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
;;
;; A request body is decoded as the media type it actually arrived with.  When
;; an operation declares more than one, the declared names are compared exactly
;; on their essence — @code{application/json-patch+json} and
;; @code{application/merge-patch+json} are two schemas, not one — and a body
;; whose schema has no record of its own is decoded by the shape itself.

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

(define (form-field-types ctx shape)
  "Return the source of a quoted alist of field name to scalar type.

Only scalar and enumeration properties are coerced; a nested object or array is
left as the string the form carried, because a flat form has no way to say more.
The alist is passed to @code{coerce-form-values} in the generated code; its
names are written with @code{~s}, because the renderer that inserts this source
displays strings without their quotes."
  (let* ((structure (and shape
                         (schema-shape (context-document ctx)
                                       (shape-schema shape)
                                       #:registry (context-request-registry ctx)
                                       #:named? #f)))
         (types (if (and structure
                         (memq (shape-kind structure) '(record object)))
                    (filter-map
                     (lambda (property)
                       (let* ((property-shape (property-shape property))
                              (type (cond
                                      ((scalar-shape? property-shape)
                                       (or (shape-type property-shape)
                                           'string))
                                      ((enum-shape? property-shape)
                                       (or (shape-type property-shape)
                                           'string))
                                      (else #f))))
                         (and type
                              (cons (property-name property) type))))
                     (shape-properties structure))
                    '())))
    (format #f "(quote (~{~a~^ ~}))"
            (map (lambda (entry)
                   (format #f "(~s . ~a)" (car entry) (cdr entry)))
                 types))))

(define (media-type-shape ctx media-type)
  "Return the request shape of @var{media-type}, or @code{#f}."
  (let ((schema (and media-type (media-type-schema media-type))))
    (and schema
         (schema-shape (context-document ctx)
                       schema
                       #:registry (context-request-registry ctx)))))

(define (media-type-match-expression media-type variable)
  "Return source that tests whether @var{variable} names @var{media-type}.

The test is on the media type's essence: its declared name, with any parameters
ignored, so @code{application/json; charset=utf-8} still counts as
@code{application/json}.  The name is compared exactly rather than searched for a
fragment, because an operation may declare @code{application/json-patch+json} and
@code{application/merge-patch+json} side by side: both contain \"json\", and a
substring test would send every patch to the first of them."
  (let ((name (string-downcase (media-type-name media-type))))
    (format #f "(or (string=? ~a ~s) (string-prefix? ~s ~a))"
            variable name (string-append name ";") variable)))

(define (request-media-decode-expression ctx media-type)
  "Return source that decodes a request body of @var{media-type}."
  (let* ((shape (media-type-shape ctx media-type))
         (kind (media-type-kind media-type))
         (converter (and shape (converter-to shape)))
         (raw "(http-request-body request)"))
    (cond
      ((not shape) raw)
      ((eq? kind 'form)
       (format #f
               "(let ((raw ~a)) (if (string? raw) ~a #f))"
               raw
               (unbox-expression
                ctx shape
                (format #f "(coerce-form-values (parse-query raw) ~a)"
                        (form-field-types ctx shape)))))
      ((eq? kind 'multipart)
       (format #f
               "(let ((raw ~a)) ~a)"
               raw
               (unbox-expression
                ctx shape
                (format #f
                        "(coerce-form-values (parse-multipart-body raw (http-request-header request \"Content-Type\")) ~a)"
                        (form-field-types ctx shape)))))
      ((memq kind '(text binary))
       (if (shape-name shape)
           (format #f "(~a ~a)" converter raw)
           raw))
      (else
       ;; The body is decoded as the shape it declares, and an inline shape is
       ;; decoded by the form @code{unbox-expression} writes for it -- a list
       ;; body decodes its items -- instead of calling a converter that only
       ;; exists for named schemas.
       (format #f
               (string-append
                "(let ((node (let ((raw (http-request-body request)))"
                " (cond ((string? raw) (json-string->jso raw))"
                " ((jso-object? raw) raw) (else #f)))))"
                " (if (eq? node #f) #f ~a))")
               (unbox-expression ctx shape 'node))))))

(define (request-body-decoder operation ctx)
  "Return the source that decodes the request body of @var{operation}.

The actual Content-Type decides which declared media type is decoded, so a
client that sends text to an operation that also accepts JSON is understood
rather than fed to a JSON parser.  A body with no declared media type is passed
through raw."
  (let* ((body (operation-request-body operation))
         (media-types (if body
                          (ordered-media-types
                           (request-body-media-types body))
                          '())))
    (cond
      ((null? media-types) "(http-request-body request)")
      ((null? (cdr media-types))
       (request-media-decode-expression ctx (car media-types)))
      (else
       (format #f
               (string-append
                "(let ((content-type"
                " (string-downcase (format #f \"~~a\""
                " (or (http-request-header request \"Content-Type\") \"\")))))"
                " (cond ~a (else ~a)))")
               (string-join
                (map (lambda (media-type)
                       (format #f "(~a ~a)"
                               (media-type-match-expression media-type
                                                            'content-type)
                               (request-media-decode-expression ctx media-type)))
                     media-types)
                " ")
               (request-media-decode-expression ctx (car media-types)))))))

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
       (let ((media-type (operation-request-media-type operation)))
         (format #f "~%~%The request body is decoded from ~a."
                 (if media-type
                     (media-type-name media-type)
                     "the declared media type")))
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
;;; Plain Guile web server

;; The adapter between (web server) values and the runtime's request and response
;; records.  It is generated only for the portable controllers: an Artanis app
;; gets the adapter in the Artanis module instead.
(define guile-support
  (string-append
   "(define (guile-header-value value)\n"
   "  \"Return VALUE, a header parsed by (web http), as its text.\"\n"
   "  (cond\n"
   "    ((string? value) value)\n"
   "    ((pair? value)\n"
   "     (let ((parameters (cdr value)))\n"
   "       (string-append\n"
   "        (format #f \"~a\" (car value))\n"
   "        (if (list? parameters)\n"
   "            (apply string-append\n"
   "                   (map (lambda (parameter)\n"
   "                          (if (pair? parameter)\n"
   "                              (format #f \"; ~a=~a\"\n"
   "                                      (car parameter) (cdr parameter))\n"
   "                              (format #f \"; ~a\" parameter)))\n"
   "                        parameters))\n"
   "            \"\"))))\n"
   "    (else (format #f \"~a\" value))))\n"
   "\n"
   "(define (guile-request-body body content-type)\n"
   "  \"Return BODY, as (web server) read it, in the shape the runtime expects.\"\n"
   "  (if (and (bytevector? body)\n"
   "           (or (string-contains content-type \"json\")\n"
   "               (string-contains content-type \"text/\")\n"
   "               (string-contains content-type\n"
   "                                \"x-www-form-urlencoded\")))\n"
   "      (utf8->string body)\n"
   "      body))\n"
   "\n"
   "(define (guile-request request body)\n"
   "  \"Return the <http-request> that corresponds to a (web request) REQUEST.\"\n"
   "  (let* ((uri (request-uri request))\n"
   "         (headers (map (lambda (entry)\n"
   "                         (cons (format #f \"~a\" (car entry))\n"
   "                               (guile-header-value (cdr entry))))\n"
   "                       (request-headers request)))\n"
   "         (content-type (or (header-ref headers \"Content-Type\") \"\")))\n"
   "    (make-http-request\n"
   "     (request-method request)\n"
   "     (uri-path uri)\n"
   "     (parse-query (uri-query uri))\n"
   "     headers\n"
   "     (guile-request-body body content-type))))\n"
   "\n"
   "(define (guile-response-headers response)\n"
   "  \"Return the headers of RESPONSE as (web response) wants them.\"\n"
   "  (map (lambda (entry)\n"
   "         (let ((name (string->symbol\n"
   "                      (string-downcase (format #f \"~a\" (car entry))))))\n"
   "           (cons name\n"
   "                 (if (string? (cdr entry))\n"
   "                     (parse-header name (cdr entry))\n"
   "                     (cdr entry)))))\n"
   "       (http-response-headers response)))\n"
   "\n"
   "(define (guile-response-body response)\n"
   "  \"Return the body of RESPONSE as (web server) can send it.\"\n"
   "  (let ((body (http-response-body response))\n"
   "        (content-type (guile-header-value\n"
   "                       (or (http-response-header response \"Content-Type\")\n"
   "                           \"\"))))\n"
   "    (cond\n"
   "      ((or (string? body) (bytevector? body)) body)\n"
   "      ((or (eq? body #f) (json-null? body)) \"\")\n"
   "      ((string-contains content-type \"json\") (jso->json-string body))\n"
   "      (else (format #f \"~a\" body)))))\n"
   "\n"
   "(define (guile-handler)\n"
   "  \"Return a handler for (web server)'s run-server.\n"
   "\n"
   "Install the handler of every operation in api-handlers first, then pass the\n"
   "result to run-server:\n"
   "\n"
   "  (parameterize ((api-handlers (list (cons 'list-pets my-handler))))\n"
   "    (run-server (guile-handler) 'http '(#:port 8080)))\"\n"
   "  (lambda (request body)\n"
   "    (let ((response (dispatch (guile-request request body))))\n"
   "      (values (build-response\n"
   "               #:code (http-response-status response)\n"
   "               #:headers (guile-response-headers response))\n"
   "              (guile-response-body response)))))\n"))

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
                                'guile-handler
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
                            '((srfi srfi-1) (srfi srfi-9) (ice-9 regex)
                              (ice-9 format) (rnrs bytevectors)
                              (web request) (web response) (web uri)
                              (web http)))
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
       (emit "~a" guile-support)
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
