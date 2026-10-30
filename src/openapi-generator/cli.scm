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
;; The command line: a thin shell around @code{(openapi-generator generator)}
;; and the two route readers.
;;
;; @example
;; guile -L src -- \
;;   -c '((@ (openapi-generator cli) main!)
;;        (list "generate" "--input" "petstore.yaml" "--output" "gen"
;;              "--controllers" "artanis" "--http" "curl"))'
;; @end example
;;
;; There are two commands.  @code{generate} reads a specification and writes
;; Guile; @code{spec} reads the routes of an existing Guile or Artanis server
;; and writes an OpenAPI 3.0 document in YAML.  An invocation with no command
;; word still means @code{generate}, because that was the only command the tool
;; had for a while.
;;
;; Every option has a long and a short spelling, the defaults are the ones that
;; surprise least (plain Guile controllers, plain Guile client, models included),
;; and an unknown value is an error rather than a silent fallback: a typo in
;; @code{--controllers artanis} should not leave you with a plain Guile server.

(define-module (openapi-generator cli)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator generator)
  #:use-module (openapi-generator config)
  #:use-module (openapi-generator config-file)
  #:use-module (openapi-generator bundle)
  #:use-module (openapi-generator routes)
  #:use-module (openapi-generator guile-routes)
  #:use-module (openapi-generator artanis-routes)
  #:export (main
            main!
            options
            usage
            spec-options
            spec-usage
            parse-backend
            parse-controllers
            parse-dependencies
            parse-resolution-mode
            parse-auth-header
            parse-route-source))

(define option-descriptions
  '((help . "Show this help and exit.")
    (version . "Show the version and exit.")
    (config . "Read options from a YAML or JSON file; flags override it.")
    (input . "The OpenAPI 3.x specification to read (.yaml or .json).")
    (output . "The directory to write the generated code to.")
    (module . "The root module name (default: the API title).")
    (targets . "Comma-separated: models, controllers, client, readme.")
    (controllers . "vanilla, artanis or none (default: vanilla).")
    (http . "guile, curl or none (default: guile).")
    (no-models . "Generate no record module.")
    (model-suffix . "Append SUFFIX to every generated model name.")
    (operation-id-transform . "Rename operationIds as REGEX:REPLACEMENT.")
    (base-url . "Override the first server URL of the specification.")
    (self-contained . "Copy the runtime modules next to the generated code.")
    (no-validate . "Do not check that the input is OpenAPI 3.x.")
    (no-header . "Omit the banner comment from generated files.")
    (header-file . "Add the contents of FILE to the generated banner.")
    (dependencies . "none or guix: write a dependencies.scm manifest.")
    (json-schema-file . "Read a JSON Schema document instead of an OpenAPI spec.")
    (json-schema-root-name . "Name the schema generated from a JSON Schema's top level.")
    (api-fragment . "Merge a partial OpenAPI document into the primary one.")
    (auth . "Send 'Name: value' on remote fetches; repeatable.")
    (resolve-external . "none, targeted or aggressive external $ref resolution.")
    (request-response-models . "Emit Request and Response model sets.")
    (exclude-read-only . "Drop readOnly properties from the single model set.")
    (exclude-write-only . "Drop writeOnly properties from the single model set.")
    (x-extensible-enums . "Treat x-extensible-enum(s) as enumerations.")
    (fault-tolerant-enums . "Map unknown string enum values to 'unrecognized'.")
    (non-null-response-payloads . "Fail when a declared success body is missing or null.")))

(define options
  '((help (single-char #\h))
    (version (single-char #\v))
    (config (value #t))
    (input (single-char #\i) (value #t))
    (output (single-char #\o) (value #t))
    (module (single-char #\m) (value #t))
    (targets (single-char #\t) (value #t))
    (controllers (single-char #\c) (value #t))
    (http (single-char #\H) (value #t))
    (no-models)
    (model-suffix (value #t))
    (operation-id-transform (value #t))
    (base-url (value #t))
    (self-contained)
    (no-validate)
    (no-header)
    (header-file (value #t))
    (dependencies (value #t))
    (json-schema-file (value #t))
    (json-schema-root-name (value #t))
    (api-fragment (value #t))
    (auth (value #t))
    (resolve-external (value #t))
    (request-response-models)
    (exclude-read-only)
    (exclude-write-only)
    (x-extensible-enums)
    (fault-tolerant-enums)
    (non-null-response-payloads)))

(define spec-option-descriptions
  '((help . "Show this help and exit.")
    (version . "Show the version and exit.")
    (input . "The Guile or Artanis source file whose routes are read.")
    (output . "The YAML file to write, or - for standard output.")
    (source . "guile or artanis (default: guile).")
    (title . "The info.title of the document (default: API).")
    (api-version . "The info.version of the document (default: 1.0.0).")
    (description . "The info.description of the document.")
    (server . "A servers entry; repeatable.")))

(define spec-options
  '((help (single-char #\h))
    (input (single-char #\i) (value #t))
    (output (single-char #\o) (value #t))
    (source (single-char #\s) (value #t))
    (title (single-char #\t) (value #t))
    (api-version (single-char #\V) (value #t))
    (version (single-char #\v))
    (description (value #t))
    (server (value #t))))

(define (write-option-table port descriptors descriptions)
  "Write each option of @var{descriptors} with its entry of @var{descriptions}.

The spelling and the explanation are separated by padding rather than by a tab,
because a usage text is also read as plain text in a bug report."
  (for-each (lambda (option)
              (let* ((spelling (option-spelling option))
                     (padding (make-string (max 1
                                                (- 30 (string-length spelling)))
                                           #\space)))
                (format port "~a~a~a~%"
                        spelling
                        padding
                        (or (assoc-ref descriptions (car option)) ""))))
            descriptors))

(define (usage port)
  "Write the usage of the command to @var{port}."
  (format port "guile-openapi-generator ~a~%~%" generator-version)
  (format port "Read an OpenAPI 3.x specification and generate Guile code,~%")
  (format port "or read the routes of a server and write a specification.~%~%")
  (format port "Usage: openapi-generator generate --input SPEC --output DIR~%")
  (format port "                          [options]~%")
  (format port "       openapi-generator spec --input ROUTES --output FILE~%")
  (format port "                          [options]~%~%")
  (write-option-table port options option-descriptions)
  (format port "~%Examples:~%")
  (format port
          "  openapi-generator generate -i petstore.yaml -o gen~%")
  (format port
          "  openapi-generator generate -i petstore.yaml -o gen -c artanis -H curl~%")
  (format port
          "  openapi-generator generate --config openapi-generator.yaml~%")
  (format port
          "  openapi-generator generate -i api.yaml -o gen -t models,client~%")
  (format port
          "  openapi-generator spec -i app/controllers.scm -o openapi.yaml~%")
  (format port
          "  openapi-generator spec -i app/routes.scm -s artanis -t 'My API'~%"))

(define (spec-usage port)
  "Write the usage of the @code{spec} command to @var{port}."
  (format port "guile-openapi-generator ~a~%~%" generator-version)
  (format port "Read the routes of a Guile or Artanis server and write an~%")
  (format port "OpenAPI 3.0 document in YAML.~%~%")
  (format port "Usage: openapi-generator spec --input ROUTES --output FILE~%")
  (format port "                          [options]~%~%")
  (write-option-table port spec-options spec-option-descriptions)
  (format port "~%The guile source may be a route table written by this generator or~%")
  (format port "plain (get \"/path\" handler) declarations; the artanis source may be~%")
  (format port "any file that declares routes with the Artanis method macros.  Neither~%")
  (format port "file is loaded: only the route forms are read.~%~%")
  (format port "Examples:~%")
  (format port
          "  openapi-generator spec -i app/controllers.scm -o openapi.yaml~%")
  (format port
          "  openapi-generator spec -i app/routes.scm -s artanis -o openapi.yaml~%")
  (format port
          "  openapi-generator spec -i app/controllers.scm -o - | less~%")
  (format port
          "  openapi-generator spec -i app/routes.scm -t 'Pet store' \\~%")
  (format port
          "      --api-version 2.0.0~%")
  (format port
          "      --server https://api.example.com/v1~%"))

(define (option-field option name)
  "Return the value of the @var{name} property of @var{option}, or @code{#f}.

An option specification is a list of two-element lists, so the property is the
cadr of its entry, not the cdr: @code{(single-char #\\h)} holds the character
itself, while @code{assoc-ref} would hand back the one-element list around it and
a short spelling would print as @code{(-(h))}."
  (let ((entry (assoc name (cdr option))))
    (and entry (cadr entry))))

(define (option-spelling option)
  "Return how @var{option} is spelled in the usage text, for example
@code{--input (-i) VALUE}."
  (let ((short (option-field option 'single-char))
        (value (option-field option 'value)))
    (string-append "  --" (symbol->string (car option))
                   (if short (format #f " (-~a)" short) "")
                   (if value " VALUE" ""))))

(define (parse-controllers value)
  "Return the controller backend named by @var{value}."
  (let ((backend (string->symbol
                  (string-downcase (format #f "~a" (or value "vanilla"))))))
    (if (memq backend (cons 'none controller-backends))
        backend
        (error (format #f
                       "Unknown controller backend ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string
                                         (cons 'none controller-backends))
                                    ", "))))))

(define (parse-backend value)
  "Return the HTTP backend named by @var{value}."
  (let ((backend (string->symbol
                  (string-downcase (format #f "~a" (or value "guile"))))))
    (if (memq backend (cons 'none http-backends))
        backend
        (error (format #f
                       "Unknown HTTP backend ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string
                                         (cons 'none http-backends))
                                    ", "))))))

(define (parse-dependencies value)
  "Return the dependency backend named by @var{value}."
  (let ((backend (string->symbol
                  (string-downcase (format #f "~a" (or value "none"))))))
    (if (memq backend dependency-backends)
        backend
        (error (format #f
                       "Unknown dependency backend ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string dependency-backends)
                                    ", "))))))

(define (parse-resolution-mode value)
  "Return the external reference mode named by @var{value}."
  (let ((mode (string->symbol
               (string-downcase (format #f "~a" (or value "none"))))))
    (if (memq mode resolution-modes)
        mode
        (error (format #f
                       "Unknown external reference mode ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string resolution-modes)
                                    ", "))))))

(define (substitute-env text)
  "Return @var{text} with @code{$@{NAME@}} replaced by the environment value.

A missing variable is an error: sending @code{Authorization: Bearer } because a
CI secret was not set would turn a configuration mistake into an authentication
failure at the server."
  (regexp-substitute/global
   #f
   "\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}"
   text
   'pre
   (lambda (match)
     (let* ((name (match:substring match 1))
            (value (getenv name)))
       (unless value
         (error (format #f
                        "Environment variable ~a named by --auth is not set"
                        name)))
       value))
   'post))

(define (title-case-header name)
  "Return @var{name} with each hyphen-separated part capitalised.

Guile's web client parses a lower-case @code{authorization} header as a typed
value and rejects a plain string; the title-case spelling is written through
unchanged, and HTTP header names are case-insensitive anyway."
  (string-join (map string-titlecase (string-split name #\-)) "-"))

(define (parse-auth-header value)
  "Return the @code{(name . value)} header described by a @code{--auth} argument."
  (let ((colon (string-index value #\:)))
    (unless colon
      (error (format #f
                     "Invalid --auth ~s; expected 'Name: value'"
                     value)))
    (cons (string->symbol
           (title-case-header
            (string-trim-both (substring value 0 colon))))
          (substitute-env
           (string-trim-both
            (substring value (+ colon 1) (string-length value)))))))

(define (collect-option-values arguments long-name)
  "Return every value given for @var{long-name} in @var{arguments}.

@code{getopt-long} keeps only the last occurrence of a repeatable option, so the
raw argument list is scanned for the ones that have to accumulate: @code{--auth}
and @code{--api-fragment}."
  (let loop ((rest arguments) (values '()))
    (cond
      ((null? rest) (reverse values))
      ((string=? (car rest) long-name)
       (if (pair? (cdr rest))
           (loop (cddr rest) (cons (cadr rest) values))
           values))
      ((string-prefix? (string-append long-name "=") (car rest))
       (loop (cdr rest)
             (cons (substring (car rest)
                              (+ (string-length long-name) 1)
                              (string-length (car rest)))
                   values)))
      (else (loop (cdr rest) values)))))

(define (repeatable-values arguments config key long-name)
  "Return the repeated command-line @var{key} values, or the config file's list."
  (let ((from-command-line (collect-option-values arguments long-name)))
    (if (pair? from-command-line)
        from-command-line
        (config-list (config-ref config key #f)))))

(define (option-supplied? parsed key)
  "Return @code{#t} when @var{key} was present on the command line."
  (and (assq key parsed) #t))

(define (resolve-option parsed config key default)
  "Return the command-line value of @var{key}, the config-file value, or @var{default}."
  (if (option-supplied? parsed key)
      (assq-ref parsed key)
      (config-ref config key default)))

(define (flag-or-config parsed config key)
  "Return @code{#t} when a boolean flag was given or the config file enables it."
  (if (option-supplied? parsed key)
      #t
      (config-boolean (config-ref config key #f) #f)))

(define (read-file-text path)
  "Return the contents of @var{path}, or @code{#f} when no path was given."
  (and path (call-with-input-file path get-string-all)))

(define (exception-message key arguments)
  "Return the readable message of an exception, with its arguments filled in.

Guile's @code{error} throws @code{misc-error} with a format string and the
arguments for it, so the useful text is the first element of the third part of
the arguments, formatted.  Other throws fall back to their second argument or,
when there is none, to the key itself."
  (define (apply-error payload)
    (and (pair? payload)
         (string? (car payload))
         (catch #t
           (lambda () (apply format #f (car payload) (cdr payload)))
           (lambda rest #f))))
  (or (and (pair? arguments)
           (pair? (cdr arguments))
           (pair? (cdr (cdr arguments)))
           (apply-error (car (cdr (cdr arguments)))))
      (and (pair? arguments)
           (pair? (cdr arguments))
           (format #f "~a" (car (cdr arguments))))
      (format #f "~a" key)))

(define* (generate-main #:optional (arguments (cdr (command-line))))
  "Run the @code{generate} command on @var{arguments} and return an exit code.

The arguments start after the program name, and the first of them may be the
word @code{generate}, which is accepted so that the invocation reads like the
other task runners in this project.

Options are resolved in this order: the command line, then the file named by
@code{--config}, then the built-in default.  A flag therefore only has to be
written when it differs from the file."
  (let ((arguments (if (and (pair? arguments)
                            (string=? (car arguments) "generate"))
                       (cdr arguments)
                       arguments)))
    (catch #t
      (lambda ()
        (let* ((parsed (getopt-long (cons "openapi-generator" arguments)
                                    options
                                    #:stop-at-first-non-option #t))
               (config (if (option-supplied? parsed 'config)
                           (read-config-file (option-ref parsed 'config #f))
                           '())))
          (cond
            ((option-ref parsed 'help #f)
             (usage (current-output-port))
             0)
            ((option-ref parsed 'version #f)
             (format #t "guile-openapi-generator ~a~%" generator-version)
             0)
            (else
             (let* ((targets (config-targets
                              (or (option-ref parsed 'targets #f)
                                  (config-ref config 'targets #f))))
                    (input (resolve-option parsed config 'input #f))
                    (json-schema-file (resolve-option parsed config
                                                     'json-schema-file #f))
                    (json-schema-root-name
                     (resolve-option parsed config
                                     'json-schema-root-name #f))
                    (resolve-external
                     (parse-resolution-mode
                      (resolve-option parsed config
                                      'resolve-external "none")))
                    (auth-values (repeatable-values arguments config
                                                    'auth "--auth"))
                    (headers (map parse-auth-header auth-values))
                    (fragments (repeatable-values arguments config
                                                  'api-fragment
                                                  "--api-fragment"))
                    (request-response-models?
                     (flag-or-config parsed config 'request-response-models))
                    (exclude-read-only?
                     (flag-or-config parsed config 'exclude-read-only))
                    (exclude-write-only?
                     (flag-or-config parsed config 'exclude-write-only))
                    (x-extensible-enums?
                     (flag-or-config parsed config 'x-extensible-enums))
                    (fault-tolerant-enums?
                     (flag-or-config parsed config 'fault-tolerant-enums))
                    (non-null-response-payloads?
                     (flag-or-config parsed config
                                     'non-null-response-payloads))
                    (output (resolve-option parsed config 'output "generated"))
                    (controllers-value (resolve-option parsed config
                                                       'controllers "vanilla"))
                    (http-value (resolve-option parsed config 'http "guile"))
                    (controllers (if (and targets
                                          (not (memq 'controllers targets)))
                                     'none
                                     (parse-controllers controllers-value)))
                    (http (if (and targets (not (memq 'client targets)))
                              'none
                              (parse-backend http-value)))
                    (models? (if targets
                                 (if (or (memq 'models targets)
                                         (memq 'client targets)
                                         (memq 'controllers targets))
                                     #t
                                     #f)
                                 (if (option-supplied? parsed 'no-models)
                                     #f
                                     (config-boolean
                                      (config-ref config 'models #t) #t))))
                    (readme? (if targets
                                 (if (memq 'readme targets) #t #f)
                                 (config-boolean
                                  (config-ref config 'readme #t) #t)))
                    (header-file (resolve-option parsed config
                                                 'header-file #f))
                    (header? (cond ((option-supplied? parsed 'no-header) #f)
                                   ((option-supplied? parsed 'header-file) #t)
                                   (else
                                    (config-boolean
                                     (config-ref config 'header #t) #t))))
                    (header-text (or (read-file-text header-file)
                                     (config-ref config 'header-text #f)))
                    (dependencies
                     (parse-dependencies
                      (resolve-option parsed config
                                      'dependencies "none"))))
               (unless (or input json-schema-file)
                 (usage (current-error-port))
                 (error "one of --input or --json-schema-file is required"))
               (let ((written
                      (generate #:input input
                                #:json-schema-file json-schema-file
                                #:json-schema-root-name json-schema-root-name
                                #:api-fragments fragments
                                #:headers headers
                                #:resolve-external resolve-external
                                #:request-response-models?
                                request-response-models?
                                #:exclude-read-only? exclude-read-only?
                                #:exclude-write-only? exclude-write-only?
                                #:x-extensible-enums? x-extensible-enums?
                                #:fault-tolerant-enums?
                                fault-tolerant-enums?
                                #:non-null-response-payloads?
                                non-null-response-payloads?
                                #:output output
                                #:module (resolve-option parsed config
                                                         'module #f)
                                #:controllers controllers
                                #:http http
                                #:models? models?
                                #:readme? readme?
                                #:base-url (resolve-option parsed config
                                                           'base-url #f)
                                #:self-contained?
                                (if (option-supplied? parsed 'self-contained)
                                    #t
                                    (config-boolean
                                     (config-ref config
                                                 'self-contained #f)
                                     #f))
                                #:validate?
                                (if (option-supplied? parsed 'no-validate)
                                    #f
                                    (config-boolean
                                     (config-ref config 'validate #t) #t))
                                #:model-suffix
                                (or (resolve-option parsed config
                                                    'model-suffix "")
                                    "")
                                #:operation-id-transform
                                (resolve-option parsed config
                                                'operation-id-transform #f)
                                #:header? header?
                                #:header-text header-text
                                #:dependencies dependencies)))
                 (for-each (lambda (entry)
                             (format #t "~a: ~a~%" (car entry) (cdr entry)))
                           written)
                 0))))))
      (lambda (key . arguments)
        (format (current-error-port)
                "openapi-generator: ~a~%"
                (exception-message key arguments))
        1))))

(define (parse-route-source value)
  "Return the route reader named by @var{value}.

@code{guile} reads a plain Guile server's route table or declarations,
@code{artanis} reads Artanis route forms.  An unknown name is an error, because a
typo in @code{--source artanis} should not silently read the file as Guile and
report that it has no routes."
  (let ((reader (string->symbol
                 (string-downcase (format #f "~a" (or value "guile"))))))
    (if (memq reader '(guile artanis))
        reader
        (error (format #f
                       "Unknown route source ~a; expected one of guile, artanis"
                       value)))))

(define (routes-from-source source input)
  "Return the routes declared in the source file @var{input}, read as @var{source}."
  (case source
    ((guile) (guile-routes-from-file input))
    ((artanis) (artanis-routes-from-file input))))

(define* (spec-main arguments)
  "Run the @code{spec} command on @var{arguments} and return an exit code.

The command reads the routes of an existing server and writes an OpenAPI 3.0
document in YAML.  @code{--output -} writes the document to standard output
instead of a file, which is what a pipeline wants."
  (let ((parsed (getopt-long (cons "openapi-generator spec" arguments)
                             spec-options
                             #:stop-at-first-non-option #t)))
    (cond
      ((option-ref parsed 'help #f)
       (spec-usage (current-output-port))
       0)
      ((option-ref parsed 'version #f)
       (format #t "guile-openapi-generator ~a~%" generator-version)
       0)
      (else
       (let ((input (option-ref parsed 'input #f))
             (output (option-ref parsed 'output "openapi.yaml"))
             (title (option-ref parsed 'title "API"))
             (version (option-ref parsed 'api-version "1.0.0"))
             (description (option-ref parsed 'description #f))
             (servers (collect-option-values arguments "--server")))
         (unless input
           (spec-usage (current-error-port))
           (error "spec needs --input FILE"))
         (let* ((source (parse-route-source (option-ref parsed 'source "guile")))
                (routes (routes-from-source source input)))
           (when (null? routes)
             (error (format #f
                            "No routes were found in ~a; is it a ~a source?"
                            input
                            source)))
           (if (string=? output "-")
               (display (routes->yaml routes
                                      #:title title
                                      #:version version
                                      #:description description
                                      #:servers servers))
               (begin
                 (routes->yaml-file routes
                                    output
                                    #:title title
                                    #:version version
                                    #:description description
                                    #:servers servers)
                 (format #t "specification: ~a~%" output)))
           0))))))

(define* (main #:optional (arguments (cdr (command-line))))
  "Run the command line @var{arguments} and return an exit code.

The first argument selects the command: @code{spec} reads routes out of source
and writes a specification, and anything else — including the word
@code{generate} — is the generator itself.  The @code{generate} spelling is
accepted so that an invocation reads like the other task runners in this
project, and because it is what older scripts pass.

Both commands report an error as a one-line diagnostic and return status 1
rather than letting a backtrace out; @code{main!} turns the status into the exit
status of the process."
  (if (and (pair? arguments) (string=? (car arguments) "spec"))
      (catch #t
        (lambda () (spec-main (cdr arguments)))
        (lambda (key . arguments)
          (format (current-error-port)
                  "openapi-generator: ~a~%"
                  (exception-message key arguments))
          1))
      (generate-main arguments)))

(define* (main! #:optional (arguments (cdr (command-line))))
  "Run the generator as a command and exit with its status.

@code{main} returns the status so tests can call it; this wrapper is what a
script wants, because a failed generation must not leave the caller thinking it
succeeded."
  (exit (main arguments)))
