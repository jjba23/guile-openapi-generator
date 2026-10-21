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
;; The client module: one procedure per operation, typed in and typed out.
;;
;; @example
;; (define client (make-api-client
;;                 #:auth (make-auth #:api-keys '((\"api_key\" . \"s\")))))
;; (receive (pets response) (list-pets client #:limit 10)
;;   (map pet-name pets))
;; @end example
;;
;; Every procedure returns two values: the decoded body, as the record type the
;; specification declares, and the @code{<http-response>} it arrived in.  A
;; caller that only wants the value ignores the second; a caller that wants the
;; status, the headers or the raw body has them without a second request.
;;
;; Only the transport differs between the backends.  @code{client-send} is the
;; one procedure that separates them: with plain Guile it is
;; @code{(web client)}, with libcurl it is @code{curl-easy-perform}.  Everything
;; else — path substitution, query serialisation, credentials, status checking
;; and decoding — is generated once and shared, which is why the two backends
;; differ by one function call each.
;;
;; Auth is data: the security schemes of the specification become a literal
;; alist, each operation carries the requirements that authorise it, and
;; @code{authenticate} turns an @code{<auth>} into the headers and query
;; parameters that express it.

(define-module (openapi-generator client)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator schema)
  #:use-module (openapi-generator models)
  #:use-module (openapi-generator emitter)
  #:use-module (openapi-generator config)
  #:export (emit-client))

;;; Commentary:
;;; Support code
;;
;; The transport and the plumbing around it do not depend on the specification,
;; so they are written here as text that is known to be balanced.  Only the
;; per-operation procedures and the security scheme table are built from the
;; document, and those are built with string-append so that a missing
;; parenthesis cannot hide in a nested emitter call.

(define client-record-template
  (string-append
   "(define-record-type <api-client>\n"
   "  (%make-api-client base-url auth headers timeout)\n"
   "  api-client?\n"
   "  (base-url api-client-base-url)\n"
   "  (auth api-client-auth)\n"
   "  (headers api-client-headers)\n"
   "  (timeout api-client-timeout))\n"
   "\n"
   "(define* (make-api-client #:key\n"
   "                          (base-url \"~a\")\n"
   "                          (auth #f)\n"
   "                          (headers '())\n"
   "                          (timeout 30))\n"
   "  \"Create a client for this API.\n"
   "\n"
   "BASE-URL prefixes every path.  AUTH holds the credentials, or #f to call only\n"
   "public operations; HEADERS are sent with every request; TIMEOUT is the\n"
   "request timeout in seconds.\"\n"
   "  (%make-api-client base-url auth headers timeout))\n"
   "\n"
   "(define (client-url client path query)\n"
   "  \"Return the absolute URL of PATH, with the QUERY alist rendered.\"\n"
   "  (let ((base (string-trim-right (api-client-base-url client) #\\/))\n"
   "        (parameters (query-string query)))\n"
   "    (if (string-null? parameters)\n"
   "        (string-append base path)\n"
   "        (string-append base path \"?\" parameters))))\n"))

(define guile-send-template
  (string-append
   "(define* (client-send client method path #:key query headers body)\n"
   "  \"Send METHOD to PATH and return the <http-response>.\n"
   "\n"
   "The body is sent as JSON and the response body is decoded as JSON, so callers\n"
   "never deal with strings.  The transport is (web client) from Guile itself,\n"
   "which is why a generated client needs nothing but Guile and guile-json.\"\n"
   "  (let* ((url (client-url client path query))\n"
   "         (given (if (and body (not (header-ref headers \"Content-Type\")))\n"
   "                    (header-set headers \"Content-Type\" \"application/json\")\n"
   "                    headers))\n"
   "         (request-headers (append (list (cons \"Accept\" \"application/json\"))\n"
   "                                  given\n"
   "                                  (api-client-headers client)))\n"
   "         (payload (and body (jso->json-string body))))\n"
   "    (receive (response response-body)\n"
   "        (http-request url\n"
   "                      #:method method\n"
   "                      #:headers (guile-headers request-headers)\n"
   "                      #:body payload)\n"
   "      (make-http-response (response-code response)\n"
   "                          (response-headers response)\n"
   "                          (decode-body response-body)))))\n"
   "\n"
   "(define (guile-headers headers)\n"
   "  \"Return HEADERS as (web client) wants them.\n"
   "\n"
   "(web client) takes symbol names and, for the headers it knows how to parse,\n"
   "already parsed values; every other header is passed through unchanged.\"\n"
   "  (map (lambda (header)\n"
   "         (let ((name (string->symbol (string-downcase (car header))))\n"
   "               (value (cdr header)))\n"
   "           (cons name\n"
   "                 (cond\n"
   "                   ((eq? name 'content-type) (list (string->symbol value)))\n"
   "                   ((eq? name 'accept)\n"
   "                    (list (list (string->symbol value))))\n"
   "                   (else value)))))\n"
   "       headers))\n"
   "\n"
   "(define (decode-body response-body)\n"
   "  \"Return RESPONSE-BODY, a string or a bytevector, as a jso value.\n"
   "\n"
   "(web client) decodes a body to a string only for the content types it knows\n"
   "to be textual, so a JSON response usually arrives as bytes.\"\n"
   "  (let ((text (cond\n"
   "                ((string? response-body) response-body)\n"
   "                ((bytevector? response-body) (utf8->string response-body))\n"
   "                (else \"\"))))\n"
   "    (if (string-null? (string-trim-both text))\n"
   "        json-null\n"
   "        (json-string->jso text))))\n"))

(define curl-send-template
  (string-append
   "(define* (client-send client method path #:key query headers body)\n"
   "  \"Send METHOD to PATH and return the <http-response>.\n"
   "\n"
   "The body is sent as JSON and the response body is decoded as JSON.  The\n"
   "transport is libcurl through guile-curl, so proxy settings, TLS and HTTP/2 are\n"
   "libcurl's; the response is read as bytes and decoded as UTF-8.\"\n"
   "  (let* ((url (client-url client path query))\n"
   "         (payload (and body (jso->json-string body)))\n"
   "         (given (if (and payload (not (header-ref headers \"Content-Type\")))\n"
   "                    (header-set headers \"Content-Type\" \"application/json\")\n"
   "                    headers))\n"
   "         (curl (curl-easy-init)))\n"
   "    (dynamic-wind\n"
   "        (lambda () #t)\n"
   "        (lambda ()\n"
   "          (curl-easy-setopt curl 'url url)\n"
   "          (curl-easy-setopt curl 'customrequest (symbol->string method))\n"
   "          (curl-easy-setopt curl 'timeout (api-client-timeout client))\n"
   "          (curl-easy-setopt curl 'followlocation #t)\n"
   "          (curl-easy-setopt curl 'header #t)\n"
   "          (curl-easy-setopt\n"
   "           curl\n"
   "           'httpheader\n"
   "           (append (list \"Accept: application/json\")\n"
   "                   (if payload (list \"Content-Type: application/json\") '())\n"
   "                   (append (map (lambda (header)\n"
   "                                  (format #f \"~a: ~a\" (car header) (cdr header)))\n"
   "                                given)\n"
   "                           (map (lambda (header)\n"
   "                                  (format #f \"~a: ~a\" (car header) (cdr header)))\n"
   "                                (api-client-headers client)))))\n"
   "          (when payload\n"
   "            (curl-easy-setopt curl 'postfields payload))\n"
   "          (let* ((raw (curl-easy-perform curl #t))\n"
   "                 (response (read-response (open-bytevector-input-port raw)))\n"
   "                 (response-body (read-response-body response)))\n"
   "            (make-http-response (response-code response)\n"
   "                                (response-headers response)\n"
   "                                (decode-body response-body))))\n"
   "        (lambda () (curl-easy-cleanup curl)))))\n"
   "\n"
   "(define (decode-body response-body)\n"
   "  \"Return RESPONSE-BODY, a bytevector or string, as a jso value.\"\n"
   "  (let ((text (cond\n"
   "                ((string? response-body) response-body)\n"
   "                ((bytevector? response-body) (utf8->string response-body))\n"
   "                (else \"\"))))\n"
   "    (if (string-null? (string-trim-both text))\n"
   "        json-null\n"
   "        (json-string->jso text))))\n"))

;;; Commentary:
;;; From the specification

(define (descriptor-alist scheme)
  "Return the descriptor of @var{scheme}, in the shape of the specification.

Keeping the specification's own spelling means the emitted literal can be read
against the document it came from, and @code{authenticate} needs no translation
table."
  (append (list (cons "type" (security-scheme-type scheme)))
          (if (security-scheme-scheme scheme)
              (list (cons "scheme" (security-scheme-scheme scheme)))
              '())
          (if (security-scheme-bearer-format scheme)
              (list (cons "bearerFormat"
                          (security-scheme-bearer-format scheme)))
              '())
          (if (security-scheme-location scheme)
              (list (cons "in" (security-scheme-location scheme)))
              '())
          (if (security-scheme-parameter-name scheme)
              (list (cons "name" (security-scheme-parameter-name scheme)))
              '())))

(define (security-literal spec)
  "Return the source of the security scheme table of @var{spec}."
  (format #f "'~s"
          (map (lambda (scheme)
                 (cons (security-scheme-name scheme)
                       (descriptor-alist scheme)))
               (spec-security-schemes spec))))

(define (operation-statuses operation)
  "Return the success statuses declared by @var{operation}, as numbers."
  (let ((response (operation-success-response operation)))
    (if (and response (string->number (response-status response)))
        (list (string->number (response-status response)))
        '())))

(define (response-shape ctx operation)
  "Return the shape of the body of the success response of @var{operation}."
  (let ((response (operation-success-response operation)))
    (and response
         (let* ((media-types (response-media-types response))
                (media-type (or (find (lambda (candidate)
                                        (string=? (media-type-name candidate)
                                                  "application/json"))
                                      media-types)
                                (and (pair? media-types) (car media-types))))
                (schema (and media-type (media-type-schema media-type))))
           (and schema
                (schema-shape (context-document ctx)
                              schema
                              #:registry (context-registry ctx)))))))

(define (request-shape ctx operation)
  "Return the shape of the body @var{operation} sends, when it sends one."
  (let* ((media-type (operation-request-media-type operation))
         (schema (and media-type (media-type-schema media-type))))
    (and schema
         (schema-shape (context-document ctx)
                       schema
                       #:registry (context-registry ctx)))))

(define (path-parameters operation)
  (parameters-by-location (operation-parameters operation) 'path))

(define (parameter-declarations operation)
  "Return the @code{#:key} declarations of @var{operation}."
  (let* ((named (operation-parameters operation))
         (body (operation-request-body operation))
         (declarations
          (append
           (map (lambda (parameter)
                  (if (parameter-required? parameter)
                      (format #f "~a" (parameter-field parameter))
                      (format #f "(~a #f)" (parameter-field parameter))))
                named)
           (if (not body)
               '()
               (if (request-body-required? body)
                   (list "body")
                   (list "(body #f)")))
           (list "(auth (api-client-auth client))"))))
    (if (null? declarations)
        ""
        (format #f "#:key~{ ~a~}" declarations))))

(define (send-arguments ctx operation)
  "Return the keyword arguments of the @code{client-send} call of @var{operation}."
  (let* ((query (map (lambda (parameter)
                       (format #f "(cons ~s ~a)"
                               (parameter-name parameter)
                               (parameter-field parameter)))
                     (parameters-by-location (operation-parameters operation)
                                             'query)))
         (headers (map (lambda (parameter)
                         (format #f "(cons ~s ~a)"
                                 (parameter-name parameter)
                                 (parameter-field parameter)))
                       (append (parameters-by-location
                                (operation-parameters operation)
                                'header)
                               (parameters-by-location
                                (operation-parameters operation)
                                'cookie))))
         (body (and (operation-request-body operation)
                    (request-shape ctx operation))))
    (string-append
     (if (null? query)
         "\n                      #:query auth-query"
         (format #f "\n                      #:query (append (list~{ ~a~}) auth-query)"
                 query))
     (if (null? headers)
         "\n                      #:headers auth-headers"
         (format #f "\n                      #:headers (append auth-headers (list~{ ~a~}))"
                 headers))
     (if body
         (format #f "\n                      #:body ~a"
                 (box-expression ctx body "body" #f))
         ""))))

(define (path-expression operation)
  "Return the source of the request path of @var{operation}."
  (let ((parameters (path-parameters operation)))
    (if (null? parameters)
        (format #f "~s" (operation-path operation))
        (format #f "(params->path ~s (list ~{~a~^ ~}))"
                (operation-path operation)
                (map (lambda (parameter)
                       (format #f "(cons ~s ~a)"
                               (parameter-name parameter)
                               (parameter-field parameter)))
                     parameters)))))

(define (client-docstring operation shape)
  "Return the docstring of the client procedure of @var{operation}."
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
   (let ((parameters (operation-parameters operation)))
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
                                (symbol->string
                                 (parameter-location parameter))))
                      parameters))))
   (if (operation-request-body operation)
       "~%~%The request body is a record, serialised as JSON."
       "")
   (format #f
           "~%~%Returns two values: the decoded body~a and the <http-response>.~%~%Credentials come from AUTH, which defaults to the client's."
           (if shape
               (format #f " (a ~a)" (schema-display-name shape))
               ""))))

(define (operation-template ctx operation)
  "Return the source of the client procedure of @var{operation}."
  (let* ((name (operation-name operation))
         (shape (response-shape ctx operation))
         (statuses (operation-statuses operation))
         (security (operation-security operation))
         (decode (if shape
                     (unbox-expression ctx shape "(http-response-body response)")
                     "(http-response-body response)")))
    (format #f
            (string-append
             "(define* (~a client ~a)\n"
             "~a\n"
             "  (receive (auth-headers auth-query)\n"
             "      (authenticate auth api-security-schemes '~s)\n"
             "    (let ((path ~a))\n"
             "      (let ((response (client-send client '~a path~a)))\n"
             "        (expect-status response ~a)\n"
             "        (values ~a response)))))\n")
            name
            (parameter-declarations operation)
            (docstring-lines (client-docstring operation shape) 2)
            security
            (path-expression operation)
            (string-upcase (symbol->string (operation-method operation)))
            (send-arguments ctx operation)
            (if (null? statuses) "#f" (format #f "'~s" statuses))
            decode)))

(define (docstring-lines text indent)
  "Return @var{text} as a Scheme docstring literal, indented by @var{indent}.

The first line is indented like the code around it and the later lines start at
the beginning of the line, which is how Guile source is normally written and
what keeps the literal's content free of accidental indentation."
  (let* ((lines (string-split (docstring-text text) #\newline))
         (pad (make-string indent #\space)))
    (string-append (format #f "~a\"~a" pad (escape-docstring (car lines)))
                   (if (null? (cdr lines))
                       ""
                       (string-append (format #f "~%~{~a~^~%~}"
                                              (map escape-docstring (cdr lines)))
                                      "\"")))))

(define (escape-docstring line)
  "Return @var{line} escaped for a double quoted Scheme literal."
  (escape-string line))

;;; Commentary:
;;; The module

(define (emit-client spec config)
  "Return the source text of the client module of @var{spec}.

The transport is chosen by @var{config}: @code{guile} for @code{(web client)}
or @code{curl} for libcurl through guile-curl."
  (let* ((ctx (make-context spec config))
         (operations (spec-operations spec))
         (curl? (eq? (config-http config) 'curl))
         (exports (append (list '<api-client>
                                'api-client?
                                'make-api-client
                                'api-client-base-url
                                'api-client-auth
                                'api-client-headers
                                'api-client-timeout
                                'api-security-schemes
                                'client-url
                                'client-send)
                          (map operation-name operations)))
         (base-url (or (config-base-url config)
                       (spec-base-url spec)
                       "http://localhost:8080")))
    (code->string
     (lambda ()
       (emit-header
        (list (format #f "HTTP client generated from ~a"
                      (or (config-source config) "a specification"))
              (format #f
                      (string-append
                       "Generated by guile-openapi-generator ~a, from the ~a ~a"
                       " specification.  Do not edit this file: regenerate it"
                       " instead.")
                      (config-generator-version config)
                      (spec-title spec)
                      (spec-version spec))
              (if curl?
                  (string-append
                   "The transport is libcurl through guile-curl; the generated"
                   " code needs guile-curl on the load path.")
                  (string-append
                   "The transport is (web client) from Guile itself, so this file"
                   " needs nothing but Guile and guile-json."))))
       (emit-module (config-module config 'client)
                    (append (list (config-json-module config)
                                  (config-jso-module config)
                                  (config-runtime-module config))
                            (if (config-models? config)
                                (list (config-module config 'models))
                                '())
                            (if curl?
                                '((curl)
                                  (web response)
                                  (ice-9 iconv)
                                  (rnrs bytevectors))
                                '((web client)
                                  (web response)
                                  (web uri)
                                  (ice-9 iconv)
                                  (rnrs bytevectors)))
                            '((srfi srfi-1) (srfi srfi-9) (srfi srfi-13)
                              (ice-9 receive)))
                    '()
                    exports)
       (emit-blank)
       (emit-raw
        ";; The security schemes of the specification, as authenticate wants them.")
       (emit "(define api-security-schemes ~a)" (security-literal spec))
       (emit-blank)
       (for-each emit-raw
                 (string-split (string-trim-right
                                (format #f client-record-template base-url))
                               #\newline))
       (emit-blank)
       (for-each emit-raw
                 (string-split
                  (string-trim-right
                   (if curl?
                       curl-send-template
                       guile-send-template))
                  #\newline))
       (emit-blank)
       (for-each (lambda (operation)
                   (emit-raw (string-trim-right
                              (operation-template ctx operation)))
                   (emit-blank))
                 operations)))))
