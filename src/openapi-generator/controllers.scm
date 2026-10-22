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
;; The controllers module: everything a server needs to answer the paths of the
;; specification, and nothing about how the answers are computed.
;;
;; A controller is a *route table* plus a *dispatch* procedure.  Matching a
;; request is mechanical: the method must agree, the path must match the route's
;; compiled regular expression, and the captured groups are the path parameters.
;; What the route then does is the application's business, so it is looked up in
;; @code{api-handlers}, a parameter holding an alist of operation name to
;; procedure:
;;
;; @example
;; (parameterize ((api-handlers
;;                 (list (cons 'list-pets
;;                             (lambda (request arguments)
;;                               (ok (list (pet->jso (make-pet #:name \"Rex\")))))))))
;;   (dispatch request))
;; @end example
;;
;; An operation with no handler raises @code{openapi-not-implemented} rather than
;; guessing, which makes a half-built server obvious the first time a route is
;; hit.  The two backends share this module: the Artanis adapter turns an
;; Artanis request into an @code{<http-request>}, calls @code{dispatch}, and
;; renders the @code{<http-response>} it gets back.

(define-module (openapi-generator controllers)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator schema)
  #:use-module (openapi-generator models)
  #:use-module (openapi-generator emitter)
  #:use-module (openapi-generator config)
  #:export (emit-controllers
            emit-artanis-controllers))

;;; Commentary:
;;; Path patterns

(define (path-parameters path)
  "Return the names of the parameters in the path template @var{path}."
  (let loop ((segments (string-split path #\/))
             (names '()))
    (cond
      ((null? segments) (reverse names))
      ((and (string-prefix? "{" (car segments))
            (string-suffix? "}" (car segments)))
       (loop (cdr segments)
             (cons (substring (car segments)
                              1
                              (- (string-length (car segments)) 1))
                   names)))
      (else (loop (cdr segments) names)))))

(define (path-pattern path)
  "Return the regular expression matching @var{path}.

A placeholder becomes a capturing group, so the match yields the path
parameters in the order the template declares them."
  (string-append
   "^"
   (string-join (map (lambda (segment)
                       (if (and (string-prefix? "{" segment)
                                (string-suffix? "}" segment))
                           "([^/]+)"
                           (regexp-quote-segment segment)))
                     (string-split path #\/))
                "/")
   "/?$"))

(define (regexp-quote-segment segment)
  "Escape the characters of @var{segment} that are special in a regexp."
  (call-with-output-string
    (lambda (port)
      (string-for-each (lambda (character)
                         (if (memv character
                                   '(#\. #\? #\+ #\* #\( #\) #\[ #\] #\{ #\}
                                         #\^ #\$ #\| #\\))
                             (display (string #\\ character) port)
                             (display character port)))
                       segment))))

;;; Commentary:
;;; Parameters

(define (query-value-expression parameter raw)
  "Return source that converts RAW, a string from the request, to the declared
type of @var{parameter}."
  (let ((type (schema-type (parameter-schema parameter))))
    (case (or type 'string)
      ((integer number)
       (format #f "(let ((number (string->number ~a))) (if number number ~a))"
               raw
               raw))
      ((boolean)
       (format #f "(string=? ~a \"true\")" raw))
      (else raw))))

(define (parameter-decoder parameter)
  "Return the keyword argument declaration and decoding of @var{parameter}."
  (let* ((field (parameter-field parameter))
         (location (parameter-location parameter))
         (required? (parameter-required? parameter))
         (raw (case location
                ((header)
                 (format #f "(http-request-header request ~s)"
                         (parameter-name parameter)))
                ((cookie)
                 (format #f "(header-ref (http-request-headers request) \"Cookie\")"))
                (else
                 (format #f "(http-request-query-ref request ~s)"
                         (parameter-name parameter)))))
         (decoded (format #f "(let ((raw ~a)) (if raw ~a #f))"
                          raw
                          (query-value-expression parameter "raw"))))
    (if required?
        (format #f "(cons '~a (or ~a (validation-failure '(\"~a\" ~s) \"is required but is missing\")))"
                field
                decoded
                field
                (parameter-name parameter))
        (format #f "(cons '~a ~a)" field decoded))))

(define (path-argument-expression parameter)
  "Return source that reads @var{parameter} from the captured path."
  (let ((raw (format #f "(assoc-ref path-arguments ~s)" (parameter-name parameter))))
    (query-value-expression parameter raw)))

(define (request-body-decoder operation ctx)
  "Return the source that decodes the request body of @var{operation}."
  (let* ((media-type (operation-request-media-type operation))
         (schema (and media-type (media-type-schema media-type)))
         (shape (and schema
                     (schema-shape (context-document ctx)
                                   schema
                                   #:registry (context-registry ctx)))))
    (if shape
        (format #f
                (string-append
                 "(let ((raw (http-request-body request)))"
                 " (cond ((string? raw) (~a (json-string->jso raw)))"
                 " ((jso-object? raw) (~a raw)) (else #f)))")
                (converter-to shape)
                (converter-to shape))
        "(http-request-body request)")))

(define (converter-to shape)
  (format #f "jso->~a" (identifier (or (shape-name shape) "value"))))

;;; Commentary:
;;; Operation procedures

(define (operation-argument-declarations ctx operation)
  "Return the keyword declarations of the parameters of @var{operation}."
  (append
   (append-map (lambda (parameter)
                 (if (eq? (parameter-location parameter) 'path)
                     (list (format #f "(cons '~a ~a)"
                                   (parameter-field parameter)
                                   (path-argument-expression parameter)))
                     '()))
               (operation-parameters operation))
   (map parameter-decoder
        (filter (lambda (parameter)
                  (not (eq? (parameter-location parameter) 'path)))
                (operation-parameters operation)))
   (if (operation-request-body operation)
       (list (format #f "(cons 'body ~a)"
                     (request-body-decoder operation ctx)))
       '())))

(define (operation-docstring operation parameters body)
  "Return the docstring of the controller procedure of @var{operation}."
  (string-append
   (format #f "~a ~a"
           (string-upcase (symbol->string (operation-method operation)))
           (operation-path operation))
   (let ((summary (operation-summary operation))
         (description (operation-description operation)))
     (cond
       ((and (string? description) (not (string-null? description)))
        (format #f "~%~%~a" (string-trim-both description)))
       ((and (string? summary) (not (string-null? summary)))
        (format #f "~%~%~a" summary))
       (else "")))
   (if (null? parameters)
       ""
       (format #f "~%~%Parameters:~%~{  - ~a~^~%~}"
               (map (lambda (parameter)
                      (format #f "~a (~a~a, ~a)"
                              (parameter-name parameter)
                              (let ((type (schema-type
                                           (parameter-schema parameter))))
                                (if type (symbol->string type) "value"))
                              (if (parameter-required? parameter)
                                  ", required"
                                  "")
                              (symbol->string (parameter-location parameter))))
                    parameters)))
   (if body
       (format #f "~%~%The request body is decoded from JSON.")
       "")
   (format #f
           (string-append
            "~%~%The arguments are collected into an alist and passed to the handler"
            "~%installed in api-handlers for this operation."))))

(define (emit-operation-procedure ctx operation)
  "Emit the procedure that turns a request into the arguments of a handler."
  (let ((name (operation-name operation))
        (declarations (operation-argument-declarations ctx operation)))
    (emit "(define (~a request path-arguments)" name)
    (with-indent
     (lambda ()
       (emit-docstring
        (operation-docstring operation
                             (operation-parameters operation)
                             (operation-request-body operation)))
       (if (null? declarations)
           (emit "(invoke-handler '~a request '())" name)
           (begin
             (emit "(invoke-handler '~a request" name)
             (with-indent
              (lambda ()
                (emit "(list")
                (with-indent
                 (lambda ()
                   (for-each (lambda (declaration)
                               (emit "~a" declaration))
                             declarations)))
                (emit-close)))
             (emit-close)))))
    (emit-close)))

;;; Commentary:
;;; Dispatch

(define handler-lookup-template
  (string-append
   ";; An alist of operation name to handler procedure.\n"
   ";;\n"
   ";; A handler is called as (handler request arguments); arguments holds the\n"
   ";; decoded parameters keyed by field name and, when the operation declares a\n"
   ";; request body, a body member with the decoded record.  A handler returns an\n"
   ";; <http-response>, which ok, created, no-content and error-response build.\n"
   "(define api-handlers (make-parameter '()))\n"
   "\n"
   "(define* (invoke-handler operation request arguments)\n"
   "  \"Call the handler of OPERATION, or raise openapi-not-implemented.\n"
   "\n"
   "The api-handlers parameter holds the table, so a test can parameterize it and\n"
   "a server can install its handlers once at start up.\"\n"
   "  (let ((handler (assq-ref (api-handlers) operation)))\n"
   "    (if handler\n"
   "        (handler request arguments)\n"
   "        (unimplemented-operation operation request))))\n"))

(define dispatch-template
  (string-append
   "(define (find-route request)\n"
   "  \"Return the first route matching REQUEST, or #f.\"\n"
   "  (let ((method (http-request-method request))\n"
   "        (path (http-request-path request)))\n"
   "    (find (lambda (route)\n"
   "            (and (eq? (route-method route) method)\n"
   "                 (regexp-exec (route-pattern route) path)))\n"
   "          routes)))\n"
   "\n"
   "(define (dispatch-route route request)\n"
   "  \"Call the operation of ROUTE with the arguments captured from REQUEST.\"\n"
   "  (let* ((match (regexp-exec (route-pattern route)\n"
   "                             (http-request-path request)))\n"
   "         (captured (map (lambda (index)\n"
   "                          (match:substring match index))\n"
   "                        (iota (length (route-parameters route)) 1)))\n"
   "         (path-arguments (map cons (route-parameters route) captured)))\n"
   "    (case (route-operation route)\n"
   "~{~a~}"
   "      (else (unimplemented-operation (route-operation route) request)))))\n"
   "\n"
   "(define* (dispatch request #:key (handlers (api-handlers)))\n"
   "  \"Answer REQUEST with the response of the handler of the matching route.\n"
   "\n"
   "A method and path that match no route raises 'openapi-no-route; a matched\n"
   "route whose operation has no handler raises 'openapi-not-implemented.\"\n"
   "  (let ((route (find-route request)))\n"
   "    (unless route\n"
   "      (throw 'openapi-no-route\n"
   "             (http-request-method request)\n"
   "             (http-request-path request)))\n"
   "    (parameterize ((api-handlers handlers))\n"
   "      (dispatch-route route request))))\n"))

(define (emit-handler-lookup)
  "Emit the handler table and the procedure that consults it."
  (for-each emit-raw (string-split (string-trim-right handler-lookup-template)
                                   #\newline)))

(define (emit-dispatch operations)
  "Emit the request dispatcher and the procedures it is built from."
  (for-each emit-raw
            (string-split
             (string-trim-right
              (format #f dispatch-template
                      (map (lambda (operation)
                             (format #f "      ((~a) (~a request path-arguments))\n"
                                     (operation-name operation)
                                     (operation-name operation)))
                           operations)))
             #\newline)))

;;; Commentary:
;;; Modules

(define (emit-route-record)
  "Emit the route record shared by every generated controller."
  (emit "(define-record-type <route>")
  (with-indent
   (lambda ()
     (emit "(make-route method pattern parameters operation)")
     (emit "route?")
     (emit "(method route-method)")
     (emit "(pattern route-pattern)")
     (emit "(parameters route-parameters)")
     (emit "(operation route-operation)")))
  (emit-close))

(define (emit-controllers spec config)
  "Return the source text of the controller module of @var{spec}."
  (let* ((ctx (make-context spec config))
         (operations (spec-operations spec))
         (exports (append (list '<route>
                                'route?
                                'make-route
                                'route-method
                                'route-pattern
                                'route-parameters
                                'route-operation
                                'api-handlers
                                'invoke-handler
                                'find-route
                                'dispatch-route
                                'dispatch
                                'routes)
                          (map operation-name operations))))
    (code->string
     (lambda ()
       (when (config-header? config)
         (emit-header
          (append
           (list (format #f "Controllers generated from ~a"
                         (or (config-source config) "a specification"))
                 (format #f
                         (string-append
                          "Generated by guile-openapi-generator ~a, from the ~a ~a"
                          " specification.  Do not edit this file: regenerate it"
                          " instead.")
                         (config-generator-version config)
                         (spec-title spec)
                         (spec-version spec)))
           (if (config-header-text config)
               (list (config-header-text config))
               '()))))
       (emit-module (config-module config 'controllers)
                    (append (list (config-json-module config)
                                  (config-jso-module config)
                                  (config-runtime-module config))
                            (if (config-models? config)
                                (list (config-module config 'models))
                                '())
                            '((srfi srfi-1) (srfi srfi-9) (ice-9 regex)))
                    '()
                    exports)
       (emit-blank)
       (emit-route-record)
       (emit-blank)
       (emit-comment "The route table of the specification, in declaration order.")
       (emit "(define routes")
       (with-indent
        (lambda ()
          (emit "(list")
          (with-indent
           (lambda ()
             (for-each (lambda (operation)
                         (emit "(make-route '~a (make-regexp ~s) '~s '~a)"
                               (string-upcase
                                (symbol->string (operation-method operation)))
                               (path-pattern (operation-path operation))
                               (path-parameters (operation-path operation))
                               (operation-name operation)))
                       operations)))
          (emit-close)))
       (emit-close)
       (emit-blank)
       (emit-handler-lookup)
       (emit-blank)
       (emit-dispatch operations)
       (emit-blank)
       (for-each (lambda (operation)
                   (emit-operation-procedure ctx operation)
                   (emit-blank))
                 operations)))))

;;; Commentary:
;;; Artanis

(define artanis-support
  (string-append
   "(define (artanis-query rc)\n"
   "  \"Return the parameters of the Artanis context RC as an alist.\n"
   "\n"
   "Artanis keeps route and query parameters in one table, and hands some values\n"
   "back as symbols or numbers; this project only deals in strings.\"\n"
   "  (let ((parameters (params rc)))\n"
   "    (map (lambda (entry)\n"
   "           (cons (symbol->string (car entry))\n"
   "                 (if (string? (cdr entry))\n"
   "                     (cdr entry)\n"
   "                     (format #f \"~a\" (cdr entry)))))\n"
   "         (if (list? parameters) parameters '()))))\n"
   "\n"
   "(define (artanis-request rc method path)\n"
   "  \"Return the <http-request> that corresponds to the Artanis RC.\n"
   "\n"
   "PATH is the concrete request path, which the caller builds from the route\n"
   "template and the parameters Artanis decoded.\"\n"
   "  (let ((body (:body rc)))\n"
   "    (make-http-request method\n"
   "                       path\n"
   "                       (artanis-query rc)\n"
   "                       '()\n"
   "                       (if (string-null? (or body \"\")) #f body))))\n"
   "\n"
   "(define (artanis-response rc response)\n"
   "  \"Send the <http-response> RESPONSE through the Artanis context RC.\"\n"
   "  (let ((body (http-response-body response)))\n"
   "    (response-emit (if (or (eq? body #f) (json-null? body))\n"
   "                       \"\"\n"
   "                       (jso->json-string body))\n"
   "                   #:status (http-response-status response)\n"
   "                   #:headers (append (http-response-headers response)\n"
   "                                     '((Content-Type . \"application/json\"))))))\n"
   "\n"
   "(define (dispatch-rc rc method path)\n"
   "  \"Dispatch the Artanis request RC as METHOD and PATH.\"\n"
   "  (artanis-response rc (dispatch (artanis-request rc method path))))\n"))

(define (emit-artanis-controllers spec config)
  "Return the source text of the Artanis adapter of @var{spec}."
  (let* ((operations (spec-operations spec))
         (exports (list 'register-routes!
                        'artanis-query
                        'artanis-request
                        'artanis-response
                        'dispatch-rc)))
    (code->string
     (lambda ()
       (when (config-header? config)
         (emit-header
          (append
           (list (format #f "Artanis routes generated from ~a"
                         (or (config-source config) "a specification"))
                 (format #f
                         (string-append
                          "Generated by guile-openapi-generator ~a, from the ~a ~a"
                          " specification.  Do not edit this file: regenerate it"
                          " instead.")
                         (config-generator-version config)
                         (spec-title spec)
                         (spec-version spec))
                 (string-append
                  "The adapter is thin on purpose: routing, decoding and validation"
                  " live in the portable controller module, so this file only turns"
                  " an Artanis request into an <http-request> and back."))
           (if (config-header-text config)
               (list (config-header-text config))
               '()))))
       (emit-module (append (config-module config 'controllers) '(artanis))
                    (append (list (config-module config 'controllers)
                                  (config-json-module config)
                                  (config-jso-module config)
                                  (config-runtime-module config))
                            '((artanis artanis) (ice-9 format)))
                    '()
                    exports)
       (emit-blank)
       (for-each emit-raw (string-split artanis-support #\newline))
       (emit "(define (register-routes!)")
       (with-indent
        (lambda ()
          (emit-docstring
           "Install every operation of the specification in the Artanis server.")
          (for-each (lambda (operation)
                      (emit-wrapped
                       (format #f "(~a ~s"
                               (string-downcase
                                (symbol->string (operation-method operation)))
                               (artanis-path (operation-path operation)))
                       (list "(lambda (rc)"
                             (format #f "(dispatch-rc rc '~a"
                                     (string-upcase
                                      (symbol->string
                                       (operation-method operation))))
                             (format #f "(params->path ~s (artanis-query rc)))))"
                                     (operation-path operation)))
                       ""))
                    operations)))
       (emit-close)))))

(define (artanis-path path)
  "Return the Artanis spelling of @var{path}: @code{{petId}} becomes @code{:petId}.

The name is left exactly as the specification spells it, because the handler
passes the decoded parameters to @code{params->path}, which looks up the same
spelling inside braces."
  (string-join
   (map (lambda (segment)
          (if (and (string-prefix? "{" segment)
                   (string-suffix? "}" segment))
              (string-append ":"
                             (substring segment
                                        1
                                        (- (string-length segment) 1)))
              segment))
        (string-split path #\/))
   "/"))
