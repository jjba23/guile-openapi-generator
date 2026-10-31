;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (openapi-generator bundle)
  #:use-module (openapi-generator json-schema)
  #:use-module (openapi-generator transforms)
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

(define (document-base source)
  "Return the locator part of @var{source}, dropping a @code{#/pointer} fragment."
  (if (and (string? source) (string-index source #\#))
      (substring source 0 (string-index source #\#))
      source))

(define* (generate #:key
                   (document #f)
                   (input #f)
                   (output "generated")
                   (module #f)
                   (controllers 'vanilla)
                   (http 'guile)
                   (models? #t)
                   (readme? #t)
                   (base-url #f)
                   (self-contained? #f)
                   (source-name #f)
                   (validate? #t)
                   (model-suffix "")
                   (operation-id-transform #f)
                   (header? #t)
                   (header-text #f)
                   (dependencies 'none)
                   (json-schema-file #f)
                   (json-schema-root-name #f)
                   (api-fragments '())
                   (headers '())
                   (resolve-external 'none)
                   (request-response-models? #f)
                   (exclude-read-only? #f)
                   (exclude-write-only? #f)
                   (x-extensible-enums? #f)
                   (fault-tolerant-enums? #f)
                   (non-null-response-payloads? #f))
  "Generate Guile code from an OpenAPI 3.x specification.

Exactly one of @var{document} (a parsed document, as a jso value),
@var{input} (a path, an http(s) URL, or the document itself) and
@var{json-schema-file} (a JSON Schema document, converted to OpenAPI first) must
be given.

@var{api-fragments} is a list of partial OpenAPI documents merged into the
primary one before generation.  @var{resolve-external} is @code{none},
@code{targeted} or @code{aggressive}: the latter two fetch and bundle external
@code{$ref}s.  @var{headers} is sent on every remote fetch.

@var{output} is the directory the files are written to; @var{module} is the root
module name, defaulting to the API title; @var{controllers} is @code{vanilla},
@code{artanis} or @code{none}; @var{http} is @code{guile}, @code{curl} or
@code{none}; @var{models?} says whether the record module is generated and
@var{readme?} whether the generated README is written; @var{base-url} overrides
the first server of the specification; @var{self-contained?} copies the runtime
next to the generated code.

@var{model-suffix} is appended to every generated model name, which keeps two
specifications that share a namespace apart.  @var{operation-id-transform} is a
procedure (or a @code{REGEX:REPLACEMENT} string) applied to operationIds before
they become procedure names.

@var{header?} controls the banner comment of every generated file and
@var{header-text} adds a paragraph to it, for a license or a disclaimer.
@var{dependencies} is @code{none} or @code{guix}: the latter writes a
@code{dependencies.scm} manifest next to the generated tree.

@var{request-response-models?} emits two model sets whose names carry
@code{Request} and @code{Response}, each filtered by the direction it describes.
With one set, @var{exclude-read-only?} and @var{exclude-write-only?} remove the
matching properties.  @var{x-extensible-enums?} treats
@code{x-extensible-enum}/{@code{x-extensible-enums} as enumerations, and
@var{fault-tolerant-enums?} maps unknown string enumeration values to an
@code{unrecognized} constant instead of failing validation.

@var{non-null-response-payloads?} makes a declared success body mandatory: an
empty or null body raises @code{openapi-empty-body} instead of decoding to
@code{#f}.

Returns an alist of component to the path that was written:

@example
(generate #:input \"petstore.yaml\" #:output \"gen\")
;; => ((models . \"gen/petstore/models.scm\")
;;     (controllers . \"gen/petstore/controllers.scm\")
;;     (client . \"gen/petstore/client.scm\"))
@end example"
  (let* ((transform (cond ((procedure? operation-id-transform)
                           operation-id-transform)
                          ((string? operation-id-transform)
                           (parse-operation-id-transform operation-id-transform))
                          (else #f)))
         (fragments (cond ((not api-fragments) '())
                          ((string? api-fragments) (list api-fragments))
                          (else api-fragments)))
         (source-label (or source-name input json-schema-file "<document>"))
         (base-locator (cond ((and json-schema-file (string? json-schema-file))
                              (document-base json-schema-file))
                             ((and input (locator? input)) input)
                             (else #f)))
         (parsed
          (cond
            (json-schema-file
             (when (or input document)
               (error (string-append
                       "Use only one of #:input, #:document and "
                       "#:json-schema-file")))
             (read-json-schema json-schema-file
                               #:headers headers
                               #:root-name json-schema-root-name))
            (else
             (let* ((primary (or document
                                 (and input
                                      (read-spec input
                                                 #:validate? #f
                                                 #:headers headers))
                                 (error (string-append
                                         "No specification given: pass "
                                         "#:input, #:json-schema-file or "
                                         "#:document"))))
                    (merged (fold (lambda (fragment result)
                                    (merge-documents
                                     result
                                     (bundle-external-refs
                                      (read-document fragment
                                                     #:headers headers)
                                      (document-base fragment)
                                      #:mode resolve-external
                                      #:headers headers)))
                                  primary
                                  fragments)))
               merged))))
         (bundled (bundle-external-refs parsed base-locator
                                        #:mode resolve-external
                                        #:headers headers))
         (document (if x-extensible-enums?
                       (enable-extensible-enums bundled)
                       bundled))
         (_ (when validate?
              ;; Validation happens after fragments, external references and
              ;; optional document transforms have been folded in, so it sees
              ;; the document that is generated.
              (validate-document document source-label)))
         (spec (parse-spec document
                           #:source source-label
                           #:operation-id-transform transform))
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
                              validate?
                              readme?
                              (or model-suffix "")
                              header?
                              header-text
                              dependencies
                              request-response-models?
                              exclude-read-only?
                              exclude-write-only?
                              x-extensible-enums?
                              fault-tolerant-enums?
                              non-null-response-payloads?))
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
    (when readme?
      (record! 'readme
               (string-append (string-join (map symbol->string
                                                (config-module config #f))
                                           "/")
                              "/README.org")
               (generated-readme spec config written)))
    (when (eq? dependencies 'guix)
      (record! 'dependencies
               "dependencies.scm"
               (dependencies-source config)))
    (reverse written)))

(define (dependencies-source config)
  "Return a Guix manifest for the runtime dependencies of the generated code.

The manifest names packages rather than store paths, so it can be used with
@code{guix shell -m dependencies.scm} on any machine.  guile-json is always
needed; guile-curl and Artanis are added only when the generated code uses them."
  (let* ((packages (append '("guile" "guile-json")
                           (if (eq? (config-http config) 'curl)
                               '("guile-curl")
                               '())
                           (if (eq? (config-controllers config) 'artanis)
                               '("artanis")
                               '())))
         (root (string-join (map symbol->string (config-module config #f)) "/")))
    (string-append
     ";; -*- mode: scheme; coding: utf-8 -*-\n"
     (format #f ";; Runtime dependencies of the generated ~a code.\n" root)
     ";; Use with: guix shell -m dependencies.scm\n\n"
     "(use-modules (gnu packages)\n"
     "             (guix packages))\n\n"
     "(packages->manifest\n"
     " (map specification->package\n"
     "      (list "
     (string-join (map (lambda (package)
                         (format #f "~s" package))
                       packages)
                  " ")
     ")))\n")))

(define* (generate-from-file path #:rest options)
  "Generate code from the specification stored in @var{path}."
  (apply generate #:input path options))

(define* (generate-from-string text #:rest options)
  "Generate code from the specification in @var{text}.

The text is parsed here but validated by @code{generate}, so a caller that passes
@code{#:validate? #f} really skips the check."
  (apply generate #:document (read-spec-string text #:validate? #f) options))

;;; Commentary:
;;; The README of a generated tree

(define (generated-readme spec config written)
  "Return the source of the README of the generated tree."
  (let* ((root (string-join (map symbol->string (config-module config #f)) "/"))
         (name (identifier (to-kebab (config-root config))))
         (suffix (or (config-model-suffix config) ""))
         (suffixes (if (config-request-response-models? config)
                       (list (string-append suffix "Request")
                             (string-append suffix "Response"))
                       (list suffix)))
         (operations (spec-operations spec))
         (schemas (spec-schemas spec))
         (first-operation (and (pair? operations)
                               (operation-name (car operations))))
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
                   (apply string-append
                          (map (lambda (direction-suffix)
                                 (let ((schema-name
                                        (string-append
                                         (named-schema-name schema)
                                         direction-suffix)))
                                   (format #f "- =~a= --- =~a= records~%"
                                           schema-name
                                           (to-kebab schema-name))))
                               suffixes)))
                 schemas))
     (format #f "~%* Using the code~%~%#+begin_src scheme~%")
     (if (member 'models files)
         (format #f "(use-modules (~a models))~%~%" root)
         "")
     (if (and (member 'models files) (pair? schemas))
         (format #f "(define value (make-~a #:name \"...\"))~%~%"
                 (to-kebab (string-append (named-schema-name (car schemas))
                                          (car suffixes))))
         "")
     (if (member 'client files)
         (format #f "(use-modules (~a client) (~{~a~^ ~}))~%~%(define client (make-api-client))~%"
                 root
                 (config-runtime-module config))
         "")
     (if (member 'controllers files)
         (format #f "~%(use-modules (~a controllers))~%" root)
         "")
     "#+end_src\n"
     (if (member 'controllers files)
         (if (member 'artanis files)
             (format #f
                     (string-append
                      "\n* Serving the API\n\n"
                      "The Artanis adapter registers every route of this tree on the\n"
                      "current Artanis application:\n\n"
                      "#+begin_src scheme\n"
                      "(use-modules (~a controllers artanis))\n"
                      "(register-routes!)\n"
                      "#+end_src\n")
                     root)
             (format #f
                     (string-append
                      "\n* Serving the API\n\n"
                      "The controllers module exports =guile-handler=, an adapter for\n"
                      "Guile's own =run-server=.  Install one handler per operation and\n"
                      "start the server:\n\n"
                      "#+begin_src scheme\n"
                      "(use-modules (web server)\n"
                      "             (~a controllers)\n"
                      "             (~{~a~^ ~}))\n\n"
                      "(parameterize ((api-handlers\n"
                      "                (list (cons '~a\n"
                      "                            (lambda (request arguments)\n"
                      "                              (ok '#()))))))\n"
                      "  (run-server (guile-handler) 'http '(#:port 8080)))\n"
                      "#+end_src\n\n"
                      "The handler receives the decoded arguments and returns a\n"
                      "response built with =ok=, =created=, =text-response=,\n"
                      "=binary-response= or =error-response=.  JSON bodies are serialised\n"
                      "for you; convert a record with its =->jso= procedure first.\n")
                     root
                     (config-runtime-module config)
                     (or first-operation 'operation)))
         "")
     (if (member 'client files)
         (format #f
                 (string-append
                  "\n* Calling the API\n\n"
                  "A client procedure returns two values: the decoded body and the\n"
                  "<http-response>.\n\n"
                  "#+begin_src scheme\n"
                  "(use-modules (ice-9 receive)\n"
                  "             (~a client))\n\n"
                  "(define client (make-api-client))\n\n"
                  "(receive (value response)\n"
                  "    (~a client)\n"
                  "  value)\n"
                  "#+end_src\n\n"
                  "Pass the keyword arguments the operation declares, for example\n"
                  "=#:limit 10=.  =make-api-client= starts from the first server URL of\n"
                  "the specification; pass =#:base-url= to point it elsewhere.\n")
                 root
                 (or first-operation 'operation))
         "")
     "\n* Packaging\n\n"
     "- =--self-contained= copies the runtime modules next to the generated code,\n"
     "  so the tree needs only Guile and guile-json at run time.\n"
     "- =--dependencies guix= writes a =dependencies.scm= manifest for\n"
     "  =guix shell -m=.\n"
     "- By default the modules import =(openapi-generator runtime)=; keep this\n"
     "  generator on the load path, or generate again with =--self-contained=.\n"
     (if (and (config-models? config)
              (not (eq? (config-http config) 'none)))
         (format #f
                 (string-append
                  "\nA constructor validates: it either returns a record that satisfies"
                  "\nthe specification, or throws =openapi-validation-error= naming the"
                  "\nfield at fault.\n"))
         ""))))
