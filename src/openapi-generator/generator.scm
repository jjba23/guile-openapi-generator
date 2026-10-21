;; guile-openapi-generator

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; guile-openapi-generator is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; guile-openapi-generator is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with guile-openapi-generator.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The driver: read a specification, decide what to generate, generate it, and
;; put the files where the caller asked.  Everything interesting happens in the
;; four generators; this module is the part that knows about directories, module
;; names and the report of what was written.
;;
;; Two options deserve a word:
;;
;; - @code{#:self-contained?} copies @code{(openapi-generator jso)} and
;;   @code{(openapi-generator runtime)} next to the generated code and rewrites
;;   their module names, so a generated tree can be vendored into a project that
;;   does not depend on the generator.
;; - @code{#:controllers} and @code{#:http} may be @code{'none}, which generates
;;   only the models.  A specification is often read for its data shapes alone.

(define-module (openapi-generator generator)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator schema)
  #:use-module (openapi-generator config)
  #:use-module (openapi-generator models)
  #:use-module (openapi-generator controllers)
  #:use-module (openapi-generator client)
  #:export (generate
            generate-from-file
            generate-from-string
            generator-version))

(define generator-version
  "0.1.0")

;;; Commentary:
;;; Writing files

(define (ensure-directory! path)
  "Create @var{path} and every missing parent directory."
  (unless (or (string-null? path) (file-exists? path))
    (ensure-directory! (dirname path))
    (mkdir path)))

(define (write-generated output relative content)
  "Write @var{content} to @var{relative} inside @var{output}, and return the path."
  (let ((path (string-append (string-trim-right output #\/) "/" relative)))
    (ensure-directory! (dirname path))
    (call-with-output-file path
      (lambda (port)
        (display content port)))
    path))

;;; Commentary:
;;; Module naming

(define (root-module-name spec requested)
  "Return the root module name: what the caller asked for, else the API title."
  (let ((title (or requested (spec-title spec))))
    (to-kebab title)))

;;; Commentary:
;;; Self-contained runtime

(define runtime-file
  "runtime.scm")

(define jso-file
  "jso.scm")

(define json-file
  "json.scm")

(define (find-source file-name)
  "Return the source of the runtime module called @var{file-name}.

The file is looked up next to this module first, which works however the library
was installed or loaded, and in the load path second."
  (let ((sibling (and (current-filename)
                      (string-append (dirname (current-filename))
                                     "/"
                                     file-name))))
    (or (and sibling (file-exists? sibling) sibling)
        (search-path %load-path (string-append "openapi-generator/" file-name))
        (error (format #f "Cannot find ~a next to the generator or in the load path"
                       file-name)))))

(define (rewrite-module source from to)
  "Return @var{source} with the module @var{from} renamed to @var{to}."
  (string-replace-substring
   source
   (format #f "(define-module (~{~a~^ ~})" from)
   (format #f "(define-module (~{~a~^ ~})" to)))

(define (copy-runtime! output root)
  "Copy the runtime modules into @var{output} under the @var{root} prefix.

The copied @code{jso} module keeps its own name, because it has no dependency;
the @code{runtime} module is rewritten to import the copy rather than the
installed one."
  (let* ((jso-module (append root '(jso)))
         (runtime-module (append root '(runtime)))
         (json-module (append root '(json)))
         (jso-source (find-source jso-file))
         (runtime-source (find-source runtime-file)))
    (write-generated output
                     (string-append (string-join (map symbol->string jso-module)
                                                 "/")
                                    ".scm")
                     (rewrite-module (call-with-input-file jso-source
                                       get-string-all)
                                     '(openapi-generator jso)
                                     jso-module))
    (write-generated output
                     (string-append (string-join (map symbol->string
                                                      runtime-module)
                                                 "/")
                                    ".scm")
                     (rewrite-module
                      (string-replace-substring
                       (call-with-input-file runtime-source get-string-all)
                       "(openapi-generator jso)"
                       (format #f "(~{~a~^ ~})" jso-module))
                      '(openapi-generator runtime)
                      runtime-module))
    (write-generated output
                     (string-append (string-join (map symbol->string json-module)
                                                  "/")
                                    ".scm")
                     (rewrite-module
                      (string-replace-substring
                       (call-with-input-file (find-source json-file) get-string-all)
                       "(openapi-generator jso)"
                       (format #f "(~{~a~^ ~})" jso-module))
                      '(openapi-generator json)
                      json-module))
    (list jso-module runtime-module json-module)))

;;; Commentary:
;;; Generating

(define* (generate #:key
                   (document #f)
                   (input #f)
                   (output "generated")
                   (module #f)
                   (controllers 'vanilla)
                   (http 'guile)
                   (models? #t)
                   (base-url #f)
                   (self-contained? #f)
                   (source-name #f)
                   (validate? #t))
  "Generate Guile code from an OpenAPI 3.x specification.

Exactly one of @var{document} (a parsed document, as a jso value) and
@var{input} (a file name) must be given.

@var{output} is the directory the files are written to; @var{module} is the root
module name, defaulting to the API title; @var{controllers} is @code{vanilla},
@code{artanis} or @code{none}; @var{http} is @code{guile}, @code{curl} or
@code{none}; @var{models?} says whether the record module is generated;
@var{base-url} overrides the first server of the specification;
@var{self-contained?} copies the runtime next to the generated code.

Returns an alist of component to the path that was written:

@example
(generate #:input \"petstore.yaml\" #:output \"gen\")
;; => ((models . \"gen/petstore/models.scm\")
;;     (controllers . \"gen/petstore/controllers.scm\")
;;     (client . \"gen/petstore/client.scm\"))
@end example"
  (let* ((spec (parse-spec (or document
                               (read-spec input)
                               (error "No specification given: pass #:input or #:document"))
                           #:source (or source-name input "<document>")))
         (root (root-module-name spec module))
         (root-module (list (string->symbol root)))
         (self-contained (and self-contained? #t))
         (runtime-modules (if self-contained
                              (copy-runtime! output root-module)
                              (list '(openapi-generator jso)
                                    '(openapi-generator runtime)
                                    '(openapi-generator json))))
         (config (make-config output
                              root
                              (spec-title spec)
                              controllers
                              http
                              models?
                              (cadr runtime-modules)
                              (car runtime-modules)
                              (caddr runtime-modules)
                              (or source-name input "<document>")
                              generator-version
                              self-contained
                              base-url
                              validate?))
         (written '()))
    (define (record! component relative content)
      (set! written (cons (cons component
                                (write-generated output relative content))
                          written)))
    (when models?
      (record! 'models
               (config-file config 'models)
               (emit-models spec config)))
    (when (and (not (eq? controllers 'none))
               (not (eq? controllers #f)))
      (record! 'controllers
               (config-file config 'controllers)
               (emit-controllers spec config))
      (when (eq? controllers 'artanis)
        (record! 'artanis
                 (format #f "~a/artanis.scm"
                         (string-join (map symbol->string
                                           (config-module config #f))
                                      "/"))
                 (emit-artanis-controllers spec config))))
    (when (and (not (eq? http 'none)) (not (eq? http #f)))
      (record! 'client
               (config-file config 'client)
               (emit-client spec config)))
    (record! 'readme
             (string-append (string-join (map symbol->string (config-module config #f))
                                         "/")
                            "/README.org")
             (generated-readme spec config written))
    (reverse written)))

(define* (generate-from-file path #:rest options)
  "Generate code from the specification stored in @var{path}."
  (apply generate #:input path options))

(define* (generate-from-string text #:rest options)
  "Generate code from the specification in @var{text}."
  (apply generate #:document (read-spec-string text) options))

;;; Commentary:
;;; The README of a generated tree

(define (generated-readme spec config written)
  "Return the source of the README of the generated tree."
  (let* ((root (string-join (map symbol->string (config-module config #f)) "/"))
         (name (identifier (to-kebab (config-root config))))
         (operations (spec-operations spec))
         (schemas (spec-schemas spec))
         (files (map car written)))
    (string-append
     (format #f "#+title: ~a~%#+options: toc:nil~%~%" (spec-title spec))
     (format #f "* ~a~%~%" (spec-title spec))
     (format #f "Generated from ~a by guile-openapi-generator ~a.~%~%"
             (or (config-source config) "a specification")
             (config-generator-version config))
     (let ((description (and (spec-raw spec)
                             (jso-ref (jso-ref (spec-raw spec) "info")
                                      "description"))))
       (if (and (string? description) (not (string-null? description)))
           (format #f "~a~%~%" (string-trim-both description))
           ""))
     (format #f "* Modules~%~%")
     (if (member 'models files)
         (format #f "- =~a/models.scm= --- records, validations and JSON conversion~%" root)
         "")
     (if (member 'controllers files)
         (format #f "- =~a/controllers.scm= --- route table and dispatcher~%" root)
         "")
     (if (member 'artanis files)
         (format #f "- =~a/artanis.scm= --- Artanis route registration~%" root)
         "")
     (if (member 'client files)
         (format #f "- =~a/client.scm= --- HTTP client (~a)~%"
                 root
                 (if (eq? (config-http config) 'curl) "libcurl" "plain Guile"))
         "")
     (format #f "~%* Operations~%~%")
     (format #f "| Method | Path | Procedure |~%|--------+------+-----------|~%")
     (apply string-append
            (map (lambda (operation)
                   (format #f "| ~a | ~a | =~a= |~%"
                           (string-upcase
                            (symbol->string (operation-method operation)))
                           (operation-path operation)
                           (operation-name operation)))
                 operations))
     (format #f "~%* Schemas~%~%")
     (apply string-append
            (map (lambda (schema)
                   (format #f "- =~a= --- =~a= records~%"
                           (named-schema-name schema)
                           (to-kebab (named-schema-name schema))))
                 schemas))
     (format #f "~%* Using the code~%~%#+begin_src scheme~%")
     (if (member 'models files)
         (format #f "(use-modules (~a models))~%~%" root)
         "")
     (if (and (member 'models files) (pair? schemas))
         (format #f "(define value (make-~a #:name \"...\"))~%~%"
                 (to-kebab (named-schema-name (car schemas))))
         "")
     (if (member 'client files)
         (format #f "(use-modules (~a client) (~a runtime))~%~%(define client (make-api-client))~%"
                 root root)
         "")
     (if (member 'controllers files)
         (format #f "~%(use-modules (~a controllers))~%" root)
         "")
     "#+end_src\n"
     (if (and (config-models? config)
              (not (eq? (config-http config) 'none)))
         (format #f
                 (string-append
                  "\nA constructor validates: it either returns a record that satisfies"
                  "\nthe specification, or throws =openapi-validation-error= naming the"
                  "\nfield at fault.\n"))
         ""))))
