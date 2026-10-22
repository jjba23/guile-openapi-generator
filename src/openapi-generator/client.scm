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
  #:use-module (openapi-generator code)
  #:use-module (openapi-generator config)
  #:export (emit-client))

;;; Commentary:
;;; Support code
;;
;; The transport and the plumbing around it do not depend on the specification,
;; so they are written here as forms: quoted lists that read like the code they
;; become.  Only the per-operation procedures and the security scheme table are
;; built from the document, and those are quasiquoted around the values taken
;; from it, so a name or a path is substituted rather than concatenated.

(define* (client-record-forms base-url)
  "Return the forms that define the client record and the URL builder.

BASE-URL is what the constructor defaults to: the first server of the
specification, unless the caller overrides it."
  (list `(define-record-type <api-client>
           (%make-api-client base-url auth headers timeout)
           api-client?
           (base-url api-client-base-url)
           (auth api-client-auth)
           (headers api-client-headers)
           (timeout api-client-timeout))
        `(define* (make-api-client #:key
                                   (base-url ,base-url)
                                   (auth #f)
                                   (headers '())
                                   (timeout 30))
           "Create a client for this API.

BASE-URL prefixes every path.  AUTH holds the credentials, or #f to call only
public operations; HEADERS are sent with every request; TIMEOUT is the request
timeout in seconds."
           (%make-api-client base-url auth headers timeout))
        `(define (client-url client path query)
           "Return the absolute URL of PATH, with the QUERY alist rendered."
           (let ((base (string-trim-right (api-client-base-url client) #\/))
                 (parameters (query-string query)))
             (if (string-null? parameters)
                 (string-append base path)
                 (string-append base path "?" parameters))))))

(define (guile-transport-forms)
  "Return the forms of the @code{(web client)} transport.

The body is sent as JSON and the response body is decoded as JSON, so callers
never deal with strings; the transport is Guile's own, which is why a generated
client needs nothing but Guile and guile-json."
  (list `(define* (client-send client method path #:key query headers body)
           "Send METHOD to PATH and return the <http-response>.

The body is sent as JSON and the response body is decoded as JSON, so callers
never deal with strings.  The transport is (web client) from Guile itself,
which is why a generated client needs nothing but Guile and guile-json."
           (let* ((url (client-url client path query))
                  (given (if (and body (not (header-ref headers "Content-Type")))
                             (header-set headers "Content-Type" "application/json")
                             headers))
                  (request-headers (append (list (cons "Accept" "application/json"))
                                           given
                                           (api-client-headers client)))
                  (payload (and body (jso->json-string body))))
             (receive (response response-body)
                 (http-request url
                               #:method method
                               #:headers (guile-headers request-headers)
                               #:body payload)
               (make-http-response (response-code response)
                                   (response-headers response)
                                   (decode-body response-body)))))
        `(define (guile-headers headers)
           "Return HEADERS as (web client) wants them.

(web client) takes symbol names and, for the headers it knows how to parse,
already parsed values; every other header is passed through unchanged."
           (map (lambda (header)
                  (let ((name (string->symbol (string-downcase (car header))))
                        (value (cdr header)))
                    (cons name
                          (cond
                            ((eq? name 'content-type)
                             (list (string->symbol value)))
                            ((eq? name 'accept)
                             (list (list (string->symbol value))))
                            (else value)))))
                headers))
        `(define (decode-body response-body)
           "Return RESPONSE-BODY, a string or a bytevector, as a jso value.

The transport decodes a body to a string only for the content types it knows to
be textual, so a JSON response usually arrives as bytes."
           (let ((text (cond
                         ((string? response-body) response-body)
                         ((bytevector? response-body)
                          (utf8->string response-body))
                         (else ""))))
             (if (string-null? (string-trim-both text))
                 json-null
                 (json-string->jso text))))))

(define (curl-transport-forms)
  "Return the forms of the libcurl transport.

libcurl brings proxy settings, TLS and HTTP/2, and the response arrives as
bytes, so the transport reads them and lets the generated code decode UTF-8."
  (list `(define* (client-send client method path #:key query headers body)
           "Send METHOD to PATH and return the <http-response>.

The body is sent as JSON and the response body is decoded as JSON.  The
transport is libcurl through guile-curl, so proxy settings, TLS and HTTP/2 are
libcurl's; the response is read as bytes and decoded as UTF-8."
           (let* ((url (client-url client path query))
                  (payload (and body (jso->json-string body)))
                  (given (if (and payload
                                  (not (header-ref headers "Content-Type")))
                             (header-set headers "Content-Type" "application/json")
                             headers))
                  (curl (curl-easy-init)))
             (dynamic-wind
                 (lambda () #t)
                 (lambda ()
                   (curl-easy-setopt curl 'url url)
                   (curl-easy-setopt curl 'customrequest
                                    (symbol->string method))
                   (curl-easy-setopt curl 'timeout (api-client-timeout client))
                   (curl-easy-setopt curl 'followlocation #t)
                   (curl-easy-setopt curl 'header #t)
                   (curl-easy-setopt
                    curl
                    'httpheader
                    (append (list "Accept: application/json")
                            (if payload
                                (list "Content-Type: application/json")
                                '())
                            (append (map (lambda (header)
                                           (format #f "~a: ~a"
                                                   (car header)
                                                   (cdr header)))
                                         given)
                                    (map (lambda (header)
                                           (format #f "~a: ~a"
                                                   (car header)
                                                   (cdr header)))
                                         (api-client-headers client)))))
                   (when payload
                     (curl-easy-setopt curl 'postfields payload))
                   (let* ((raw (curl-easy-perform curl #t))
                          (response (read-response
                                     (open-bytevector-input-port raw)))
                          (response-body (read-response-body response)))
                     (make-http-response (response-code response)
                                         (response-headers response)
                                         (decode-body response-body))))
                 (lambda () (curl-easy-cleanup curl)))))
        `(define (decode-body response-body)
           "Return RESPONSE-BODY, a bytevector or string, as a jso value."
           (let ((text (cond
                         ((string? response-body) response-body)
                         ((bytevector? response-body)
                          (utf8->string response-body))
                         (else ""))))
             (if (string-null? (string-trim-both text))
                 json-null
                 (json-string->jso text))))))

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

(define (response-status-text status)
  "Return @var{status}, which a reader may hand over as a string or a number, as text."
  (cond ((string? status) status)
        ((number? status) (number->string status))
        (else "")))

(define (status-window text)
  "Return @code{(first . last)} when @var{text} is a status family such as
@code{2XX}, otherwise @code{#f}."
  (and (= (string-length text) 3)
       (char-numeric? (string-ref text 0))
       (every (lambda (character)
                (char=? (char-upcase character) #\X))
              (list (string-ref text 1) (string-ref text 2)))
       (let ((hundreds (- (char->integer (string-ref text 0))
                          (char->integer #\0))))
         (cons (* hundreds 100) (+ (* hundreds 100) 99)))))

(define (operation-statuses operation)
  "Return the form that checks the success statuses of @var{operation}.

The result is a form for the generated client, not a value: a single declared
code is a quoted list, a family such as @code{2XX} is a call to
@code{status-range}, and @code{default} -- or no success response at all -- is
@code{#f}, which @code{expect-status} reads as \"accept any\"."
  (let ((response (operation-success-response operation)))
    (if (not response)
        #f
        (let ((text (response-status-text (response-status response))))
          (cond
            ((string->number text)
             (quoted (list (string->number text))))
            ((string-ci=? text "default") #f)
            ((status-window text)
             (let ((window (status-window text)))
               `(status-range ,(car window) ,(cdr window))))
            ;; A status that cannot be understood must not turn into a check that
            ;; accepts everything.
            (else (quoted '())))))))

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
  "Return the @code{#:key} declarations of @var{operation}, as forms.

A required parameter is a bare name and an optional one carries its default, so
the generated procedure reads like the signature it is."
  (let* ((named (operation-parameters operation))
         (body (operation-request-body operation)))
    (append (map (lambda (parameter)
                   (let ((field (parameter-field parameter)))
                     (if (parameter-required? parameter)
                         field
                         (list field #f))))
                 named)
            (if (not body)
                '()
                (if (request-body-required? body)
                    (list 'body)
                    (list (list 'body #f))))
            (list '(auth (api-client-auth client))))))

(define (header-argument-form parameter)
  "Return the form that adds @var{parameter} to the request headers.

A required parameter is always added; an optional one that the caller did not
set is left out, because a header whose value is @code{#f} is not a header at
all and the HTTP client rightly rejects it."
  (let ((name (parameter-name parameter))
        (field (parameter-field parameter)))
    (if (parameter-required? parameter)
        `(list (cons ,name ,field))
        `(if (eq? ,field #f)
             '()
             (list (cons ,name ,field))))))

(define (send-arguments ctx operation)
  "Return the keyword arguments of the @code{client-send} call of @var{operation}.

Authenticated query parameters and headers are appended to the operation's own,
so credentials and caller arguments never overwrite one another by accident."
  (let* ((query (map (lambda (parameter)
                       `(cons ,(parameter-name parameter)
                              ,(parameter-field parameter)))
                     (parameters-by-location (operation-parameters operation)
                                             'query)))
         (headers (map header-argument-form
                       (append (parameters-by-location
                                (operation-parameters operation)
                                'header)
                               (parameters-by-location
                                (operation-parameters operation)
                                'cookie))))
         (body (and (operation-request-body operation)
                    (request-shape ctx operation))))
    (append (list #:query (if (null? query)
                              'auth-query
                              `(append (list ,@query) auth-query))
                  #:headers (if (null? headers)
                                'auth-headers
                                `(append auth-headers ,@headers)))
            (if body
                (list #:body (box-expression ctx body 'body #f))
                '()))))

(define (path-expression operation)
  "Return the form that builds the request path of @var{operation}.

A path with parameters goes through @code{params->path}, which escapes each
value; a path without them is the literal string the specification gives."
  (let ((parameters (path-parameters operation)))
    (if (null? parameters)
        (operation-path operation)
        `(params->path ,(operation-path operation)
                       (list ,@(map (lambda (parameter)
                                      `(cons ,(parameter-name parameter)
                                             ,(parameter-field parameter)))
                                    parameters))))))

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
        (string-append "\n\n" (string-trim-both description)))
       ((and (string? summary) (not (string-null? summary)))
        (string-append "\n\n" summary))
       (else "")))
   (let ((parameters (operation-parameters operation)))
     (if (null? parameters)
         ""
         (string-append
          "\n\nParameters:\n"
          (string-join
           (map (lambda (parameter)
                  (let ((type (schema-type (parameter-schema parameter))))
                    (format #f "  - ~a (~a~a, ~a)"
                            (parameter-name parameter)
                            (if type (symbol->string type) "value")
                            (if (parameter-required? parameter)
                                ", required"
                                "")
                            (symbol->string
                             (parameter-location parameter)))))
                parameters)
           "\n"))))
   (if (operation-request-body operation)
       "\n\nThe request body is a record, serialised as JSON."
       "")
   (format #f
           "\n\nReturns two values: the decoded body~a and the <http-response>.\n\nCredentials come from AUTH, which defaults to the client's."
           (if shape
               (format #f " (a ~a)" (schema-display-name shape))
               ""))))

(define (method-symbol operation)
  "Return the HTTP method of @var{operation} as the symbol the runtime expects."
  (string->symbol (string-upcase (symbol->string (operation-method operation)))))

(define (operation-forms ctx operation)
  "Return the forms of the client procedure of @var{operation}.

Authentication comes first, because every generated procedure has to know which
headers and query parameters the credentials add before it can build the request;
the response is then checked against the statuses the specification declares and
decoded as the record or collection it is."
  (let* ((name (operation-name operation))
         (shape (response-shape ctx operation))
         (security (operation-security operation))
         (decode (if shape
                     (unbox-expression ctx shape '(http-response-body response))
                     '(http-response-body response))))
    (list `(define* (,name client #:key ,@(parameter-declarations operation))
             ,(client-docstring operation shape)
             (receive (auth-headers auth-query)
                 (authenticate auth api-security-schemes
                               ,(quoted security))
               (let ((path ,(path-expression operation)))
                 (let ((response (client-send client
                                              ,(quoted (method-symbol operation))
                                              path
                                              ,@(send-arguments ctx operation))))
                   (expect-status response
                                  ,(operation-statuses operation))
                   (values ,decode response))))))))

;;; Commentary:
;;; The module

(define (emit-client spec config)
  "Return the source text of the client module of @var{spec}.

The transport is chosen by @var{config}: @code{guile} for @code{(web client)}
or @code{curl} for libcurl through guile-curl.  The file is a list of forms —
the header comment, the module, the security table, the record, the transport
and one procedure per operation — rendered in one pass."
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
    (forms->string
     (append
      (if (config-header? config)
          (append
           (list (comment (format #f "HTTP client generated from ~a"
                                  (or (config-source config) "a specification")))
                 (comment
                  (string-append
                   (format #f "Generated by guile-openapi-generator ~a, from the ~a ~a specification."
                           (config-generator-version config)
                           (spec-title spec)
                           (spec-version spec))
                   "  Do not edit this file: regenerate it instead."))
                 (comment
                  (if curl?
                      (string-append
                       "The transport is libcurl through guile-curl; the generated"
                       " code needs guile-curl on the load path.")
                      (string-append
                       "The transport is (web client) from Guile itself, so this file"
                       " needs nothing but Guile and guile-json."))))
           (if (config-header-text config)
               (list (comment (config-header-text config)))
               '()))
          '())
      (list
            `(define-module ,(config-module config 'client)
               #:use-module ,(config-json-module config)
               #:use-module ,(config-jso-module config)
               #:use-module ,(config-runtime-module config)
               ,@(if (config-models? config)
                     (list #:use-module (config-module config 'models))
                     '())
               ,@(if curl?
                     (append-map (lambda (module) (list #:use-module module))
                                 '((curl) (web response) (ice-9 iconv)
                                   (rnrs bytevectors)))
                     (append-map (lambda (module) (list #:use-module module))
                                 '((web client) (web response) (web uri)
                                   (ice-9 iconv) (rnrs bytevectors))))
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-13)
               #:use-module (ice-9 receive)
               #:export ,exports)
            (comment "The security schemes of the specification, as authenticate wants them.")
            `(define api-security-schemes ,(quoted (security-table spec))))
      (client-record-forms base-url)
      (if curl?
          (curl-transport-forms)
          (guile-transport-forms))
      (concatenate (map (lambda (operation)
                          (operation-forms ctx operation))
                        operations))))))

(define (security-table spec)
  "Return the security schemes of @var{spec}, in the shape of the specification.

Keeping the specification's own spelling means the emitted literal can be read
against the document it came from, and @code{authenticate} needs no translation
table."
  (map (lambda (scheme)
         (cons (security-scheme-name scheme)
               (descriptor-alist scheme)))
       (spec-security-schemes spec)))
