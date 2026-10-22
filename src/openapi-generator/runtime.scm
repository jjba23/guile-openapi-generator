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
;; The support library generated code links against.  Everything here is
;; generic: a generated model calls the validators, a generated controller
;; builds requests and responses, a generated client applies credentials.  The
;; generated files themselves stay close to the specification they came from,
;; which is what makes them readable; the boring, repetitive parts live here,
;; once, with tests of their own.
;;
;; Three ideas run through this module:
;;
;; - *Validation raises.*  A generated constructor either returns a value that
;;   satisfies every constraint the specification declares, or it raises an
;;   @code{<openapi-validation-error>} that names the offending field path.  A
;;   half-built record never escapes.
;; - *Null is a value.*  @code{json-null} is distinct from @code{#f} and from an
;;   absent member, which is what makes RFC 7386 merge patches expressible.
;; - *Credentials are data.*  A generated client describes the security schemes
;;   of the specification as literals, and @code{authenticate} turns a
;;   @code{<auth>} into the headers and query parameters those schemes require.

(define-module (openapi-generator runtime)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:re-export (json-null json-null? json-merge-patch)
  #:export (;; errors
            validation-failure
            auth-failure
            unimplemented-operation
            ;; validation
            check-required
            check-type
            check-predicate
            check-min-length
            check-max-length
            check-pattern
            check-enum
            check-format
            check-minimum
            check-maximum
            check-exclusive-minimum
            check-exclusive-maximum
            check-multiple-of
            check-min-items
            check-max-items
            check-unique-items
            check-min-properties
            check-max-properties
            check-list
            check-map
            valid?
            ;; http
            <http-request>
            make-http-request
            http-request?
            http-request-method
            http-request-path
            http-request-query
            http-request-headers
            http-request-body
            http-request-header
            http-request-query-ref
            <http-response>
            make-http-response
            http-response?
            http-response-status
            http-response-headers
            http-response-body
            http-response-header
            json-response
            text-response
            binary-response
            ok
            created
            accepted
            no-content
            error-response
            expect-status
            require-body
            status-range
            make-multipart-boundary
            multipart-part
            multipart-body
            multipart-boundary
            parse-multipart-body
            unauthorized
            forbidden
            not-found
            uri-encode
            query-string
            form-string
            coerce-form-values
            parse-query
            header-ref
            header-set
            params->path
            ;; auth
            <auth>
            make-auth
            auth?
            auth-api-keys
            auth-bearer-token
            auth-username
            auth-password
            auth-scopes
            auth-headers
            authenticate
            auth-satisfiable?

            ;; formats
            valid-format?))

;;; Commentary:
;;; Errors
;;
;; Errors are thrown with a key rather than raised as records, so that a caller
;; catches exactly what it means to catch:
;;
;; @example
;; (catch 'openapi-validation-error
;;   (lambda () (make-pet #:name ""))
;;   (lambda (key path message)
;;     (format #t "~a: ~a~%" path message)))
;; @end example

(define (validation-failure path format-string . arguments)
  "Throw @code{openapi-validation-error} naming @var{path} and what is wrong."
  (throw 'openapi-validation-error
         path
         (apply format #f format-string arguments)))

(define (auth-failure format-string . arguments)
  "Throw @code{openapi-auth-error}."
  (throw 'openapi-auth-error (apply format #f format-string arguments)))

(define (unimplemented-operation operation request)
  "Throw @code{openapi-not-implemented} for the controller operation OPERATION."
  (throw 'openapi-not-implemented operation request))

;;; Commentary:
;;; Validation
;;
;; Every check is a procedure of a path and a value that returns #t when the
;; value is acceptable, so a generated validator reads as a list of claims about
;; one field.  A path is a list whose head is the record name and whose tail is
;; the field names leading to the value, e.g. ("pet" "category" "name").

(define (describe-path path)
  (string-join (map (lambda (element)
                      (if (string? element)
                          element
                          (format #f "~a" element)))
                    path)
               "."))

(define (check-required path value)
  "Check that @var{value} is present: not #f and not JSON null."
  (when (or (eq? value #f) (json-null? value))
    (validation-failure path "is required but is missing"))
  #t)

(define (check-type path value type)
  "Check that @var{value} has the OpenAPI @var{type}."
  (case type
    ((string) (unless (string? value)
                (validation-failure path "expected a string, got ~s" value)))
    ((integer) (unless (integer? value)
                 (validation-failure path "expected an integer, got ~s" value)))
    ((number) (unless (number? value)
                (validation-failure path "expected a number, got ~s" value)))
    ((boolean) (unless (boolean? value)
                 (validation-failure path "expected a boolean, got ~s" value)))
    ((list) (unless (list? value)
              (validation-failure path "expected a list, got ~s" value)))
    ((map) (unless (and (list? value) (every pair? value))
             (validation-failure path "expected an alist, got ~s" value)))
    (else (unless (eq? type 'any)
            (validation-failure path "unsupported type ~a" type))))
  #t)

(define (check-predicate path value predicate type-name)
  "Check that @var{value} satisfies @var{predicate}, naming @var{type-name}."
  (unless (predicate value)
    (validation-failure path "expected a ~a, got ~s" type-name value))
  #t)

(define (check-min-length path value minimum)
  (when (string? value)
    (when (< (string-length value) minimum)
      (validation-failure path "must be at least ~a characters long" minimum)))
  #t)

(define (check-max-length path value maximum)
  (when (string? value)
    (when (> (string-length value) maximum)
      (validation-failure path "must be at most ~a characters long" maximum)))
  #t)

(define (check-pattern path value pattern)
  (when (string? value)
    (unless (string-match pattern value)
      (validation-failure path "must match the pattern ~s" pattern)))
  #t)

(define (check-enum path value values)
  (unless (member value values)
    (validation-failure path "must be one of ~s, got ~s" values value))
  #t)

(define (check-format path value format-name)
  (when (string? value)
    (unless (valid-format? value format-name)
      (validation-failure path "must be a valid ~a, got ~s" format-name value)))
  #t)

(define format-patterns
  `((date . "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    (time . "^[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$")
    (date-time .
               "^[0-9]{4}-[0-9]{2}-[0-9]{2}[Tt ][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?([Zz]|[+-][0-9]{2}:[0-9]{2})$")
    (uuid .
          "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
    (email . "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$")
    (hostname . "^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$")
    (ipv4 .
          "^((25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]?[0-9])$")
    (uri . "^[a-zA-Z][a-zA-Z0-9+.-]*:")))

(define (valid-format? value format-name)
  "Return @code{#t} when @var{value} looks like the OpenAPI format
@var{format-name}.

Formats without a pattern (a binary string, for instance) accept anything, and
an unknown format is accepted rather than rejected: a validator must not invent
constraints the specification did not declare."
  (let ((entry (assq format-name format-patterns)))
    (cond
      ((not entry) #t)
      (else (and (string-match (cdr entry) value) #t)))))

(define (check-minimum path value minimum)
  (when (and (number? value) (< value minimum))
    (validation-failure path "must be at least ~a" minimum))
  #t)

(define (check-maximum path value maximum)
  (when (and (number? value) (> value maximum))
    (validation-failure path "must be at most ~a" maximum))
  #t)

(define (check-exclusive-minimum path value minimum)
  (when (and (number? value) (<= value minimum))
    (validation-failure path "must be greater than ~a" minimum))
  #t)

(define (check-exclusive-maximum path value maximum)
  (when (and (number? value) (>= value maximum))
    (validation-failure path "must be less than ~a" maximum))
  #t)

(define (multiple-of? value multiple)
  "Return @code{#t} when @var{value} is an exact multiple of @var{multiple}.

Exact numbers are divided exactly, so no tolerance is needed.  Inexact numbers
are not: @code{(/ 0.3 0.1)} is @code{2.9999999999999996}, and a check that used
@code{modulo} on that either raised a type error or rejected a value the
specification allows.  The quotient is therefore rounded and compared within a
tolerance proportional to its size."
  (let ((ratio (/ value multiple)))
    (if (and (exact? value) (exact? multiple))
        (integer? ratio)
        (let ((nearest (round ratio)))
          (<= (abs (- ratio nearest))
              (* 1e-9 (max 1 (abs nearest))))))))

(define (check-multiple-of path value multiple)
  (when (and (number? value)
             (not (zero? multiple))
             (not (multiple-of? value multiple)))
    (validation-failure path "must be a multiple of ~a" multiple))
  #t)

(define (check-min-items path value minimum)
  (when (and (list? value) (< (length value) minimum))
    (validation-failure path "must hold at least ~a items" minimum))
  #t)

(define (check-max-items path value maximum)
  (when (and (list? value) (> (length value) maximum))
    (validation-failure path "must hold at most ~a items" maximum))
  #t)

(define (check-unique-items path value)
  (when (list? value)
    (unless (= (length value) (length (delete-duplicates value)))
      (validation-failure path "must not repeat an item")))
  #t)

(define (check-min-properties path value minimum)
  (when (and (list? value) (< (length value) minimum))
    (validation-failure path "must hold at least ~a members" minimum))
  #t)

(define (check-max-properties path value maximum)
  (when (and (list? value) (> (length value) maximum))
    (validation-failure path "must hold at most ~a members" maximum))
  #t)

(define (check-list path value predicate item-name)
  "Check that every element of @var{value} satisfies @var{predicate}."
  (when (list? value)
    (let loop ((index 0) (rest value))
      (unless (null? rest)
        (unless (predicate (car rest))
          (validation-failure path
                              "item ~a must be a ~a, got ~s"
                              index
                              item-name
                              (car rest)))
        (loop (+ index 1) (cdr rest)))))
  #t)

(define (check-map path value predicate value-name)
  "Check that every value of the alist @var{value} satisfies @var{predicate}."
  (when (and (list? value) (every pair? value))
    (for-each (lambda (entry)
                (unless (predicate (cdr entry))
                  (validation-failure path
                                      "member ~s must be a ~a, got ~s"
                                      (car entry)
                                      value-name
                                      (cdr entry))))
              value))
  #t)

(define* (valid? thunk #:optional (key 'openapi-validation-error))
  "Return @code{#t} when @var{thunk} returns without throwing @var{key}.

@example
(valid? (lambda () (make-pet #:name "")))  ;; => #f
@end example"
  (catch key
    (lambda ()
      (thunk)
      #t)
    (lambda arguments #f)))

;;; Commentary:
;;; HTTP request and response values

(define-record-type <http-request>
  (make-http-request method path query headers body)
  http-request?
  (method http-request-method)
  (path http-request-path)
  (query http-request-query)
  (headers http-request-headers)
  (body http-request-body))

(define-record-type <http-response>
  (make-http-response status headers body)
  http-response?
  (status http-response-status)
  (headers http-response-headers)
  (body http-response-body))

(define (header-name-string name)
  "Return the header @var{name} as a string, whether it is a symbol or a string."
  (if (string? name) name (symbol->string name)))

(define (header-ref headers name)
  "Return the value of the header @var{name} in @var{headers}, ignoring case.

Header names are compared as strings because the transports disagree about how
to spell them: @code{(web client)} wants symbols and libcurl wants an
@code{Name: value} line."
  (let ((wanted (header-name-string name)))
    (let ((entry (find (lambda (candidate)
                         (string-ci=? (header-name-string (car candidate))
                                      wanted))
                       headers)))
      (and entry (cdr entry)))))

(define (header-set headers name value)
  "Return @var{headers} with @var{name} set to @var{value}, ignoring case."
  (let ((wanted (header-name-string name)))
    (cons (cons (if (string? name) name (symbol->string name)) value)
          (remove (lambda (candidate)
                    (string-ci=? (header-name-string (car candidate)) wanted))
                  headers))))

(define (http-request-header request name)
  "Return the header @var{name} of @var{request}."
  (header-ref (http-request-headers request) name))

(define (http-request-query-ref request name)
  "Return the query parameter @var{name} of @var{request}."
  (let ((entry (assoc name (http-request-query request))))
    (and entry (cdr entry))))

(define* (json-response status value #:optional (headers '()))
  "Return a response whose body is the jso @var{value}, as JSON."
  (make-http-response status
                      (cons (cons "Content-Type" "application/json") headers)
                      value))

(define* (text-response status text #:optional (headers '()))
  "Return a response whose body is the plain text @var{text}."
  (make-http-response status
                      (cons (cons "Content-Type" "text/plain; charset=utf-8")
                            headers)
                      text))

(define* (binary-response status bytes #:optional (headers '()))
  "Return a response whose body is the binary @var{bytes}."
  (make-http-response status
                      (cons (cons "Content-Type"
                                  "application/octet-stream")
                            headers)
                      bytes))

(define (http-response-header response name)
  "Return the header @var{name} of @var{response}, or @code{#f}."
  (header-ref (http-response-headers response) name))

(define (require-body value response)
  "Return @var{value}, or throw @code{openapi-empty-body} when it is absent.

A generated client calls this when the specification declares a success body and
the caller asked for non-null payloads: a missing or null body is a server
contract violation, not a value the caller can use."
  (if (eq? value #f)
      (throw 'openapi-empty-body
             (http-response-status response)
             (http-response-body response))
      value))

(define (make-multipart-boundary)
  "Return a boundary suitable for one multipart/form-data request."
  (format #f "----guile-openapi-~a-~a" (getpid) (random 1000000)))

(define* (multipart-part name value #:optional filename)
  "Return one multipart part: @var{name}, @var{value} and an optional filename."
  (list name value filename))

(define (ok value)
  "Return a @code{200} response carrying @var{value}."
  (json-response 200 value))

(define (created value)
  "Return a @code{201} response carrying @var{value}."
  (json-response 201 value))

(define (accepted value)
  "Return a @code{202} response carrying @var{value}."
  (json-response 202 value))

(define (no-content)
  "Return a @code{204} response with no body."
  (make-http-response 204 '() json-null))

(define (status-range first last)
  "Return the status codes from @var{first} to @var{last}, inclusive.

Generated clients use this for a response declared on a range such as
@code{2XX}: the specification names a family of statuses, and a check that only
listed one concrete code would either reject a success or accept a failure."
  (iota (+ 1 (- last first)) first))

(define (expect-status response expected)
  "Throw @code{openapi-http-error} unless @var{response} has one of @var{expected}.

@var{expected} is a list of status codes, or #f to accept any status."
  (if (or (not expected)
          (memv (http-response-status response) expected)
          (member (http-response-status response) expected))
      response
      (throw 'openapi-http-error
             (http-response-status response)
             (http-response-body response))))

(define (error-response status message)
  "Return an error response carrying @var{message} as JSON."
  (json-response status (list (cons "status" status) (cons "message" message))))

(define (unauthorized message)
  (error-response 401 message))

(define (forbidden message)
  (error-response 403 message))

(define (not-found message)
  (error-response 404 message))

;;; Commentary:
;;; One percent of an HTTP library: the parts a client needs

(define (uri-encode text)
  "Percent-encode @var{text} for use in a path segment or query parameter."
  (call-with-output-string
    (lambda (port)
      (string-for-each (lambda (character)
                         (if (or (char-alphabetic? character)
                                 (char-numeric? character)
                                 (memv character
                                       '(#\- #\_ #\. #\~ #\: #\/ #\@)))
                             (display character port)
                             (format port "%~2,0X"
                                     (char->integer character))))
                       text))))

(define (query-value-string value)
  "Return VALUE as it appears in a query string.

Booleans are spelled the way a URL spells them, not the way Scheme prints them:
@code{#t} is @code{true}, and the caller omits @code{#f} entirely."
  (cond
    ((eq? value #t) "true")
    ((eq? value #f) #f)
    ((number? value) (number->string value))
    ((string? value) value)
    (else (format #f "~a" value))))

(define (query-string parameters)
  "Render the alist @var{parameters} as a query string, without the @code{?}."
  (string-join
   (append-map (lambda (entry)
                 (let ((name (car entry))
                       (value (cdr entry)))
                   (cond
                     ((eq? value #f) '())
                     ((json-null? value) '())
                     ((list? value)
                      (filter-map (lambda (element)
                                    (let ((text (query-value-string element)))
                                      (and text
                                           (format #f "~a=~a"
                                                   (uri-encode name)
                                                   (uri-encode text)))))
                                  value))
                     (else
                      (let ((text (query-value-string value)))
                        (if text
                            (list (format #f "~a=~a"
                                          (uri-encode name)
                                          (uri-encode text)))
                            '()))))))
               parameters)
   "&"))

(define (form-value-string value)
  "Return VALUE the way an application/x-www-form-urlencoded body spells it.

Unlike a query string, a form field is not omitted when it is false: a form
that declares a boolean sends @code{false}, so the server can tell the field
was there."
  (cond
    ((eq? value #t) "true")
    ((eq? value #f) "false")
    ((number? value) (number->string value))
    ((string? value) value)
    (else (format #f "~a" value))))

(define (form-string parameters)
  "Render @var{parameters} as an application/x-www-form-urlencoded body."
  (string-join
   (append-map (lambda (entry)
                 (let ((name (car entry))
                       (value (cdr entry)))
                   (cond
                     ((json-null? value) '())
                     ((list? value)
                      (map (lambda (element)
                             (format #f "~a=~a"
                                     (uri-encode name)
                                     (uri-encode (form-value-string element))))
                           value))
                     (else
                      (list (format #f "~a=~a"
                                    (uri-encode name)
                                    (uri-encode (form-value-string value))))))))
               parameters)
   "&"))

(define (form-boolean value)
  "Return @var{value}, a form string, as a boolean when it spells one."
  (let ((text (string-downcase (format #f "~a" value))))
    (cond
      ((member text '("true" "1" "yes" "on")) #t)
      ((member text '("false" "0" "no" "off" "")) #f)
      (else value))))

(define (coerce-form-values values types)
  "Return @var{values}, a form alist, with each value coerced to its type.

@var{types} is an alist of field name to @code{integer}, @code{number},
@code{boolean} or @code{string}.  A value that does not parse is left as the
string it was, so validation reports the field rather than a coercion helper
silently inventing a value."
  (map (lambda (entry)
         (let* ((name (car entry))
                (type-entry (or (assoc name types)
                                (and (string? name)
                                     (assoc (string->symbol name) types)))))
           (cons (car entry)
                 (if (not type-entry)
                     (cdr entry)
                     (case (cdr type-entry)
                       ((integer number)
                        (let ((number (and (string? (cdr entry))
                                           (string->number (cdr entry)))))
                          (or number (cdr entry))))
                       ((boolean) (form-boolean (cdr entry)))
                       (else (cdr entry)))))))
       values))

(define (multipart-boundary content-type)
  "Return the boundary named by @var{content-type}, or @code{#f}."
  (let* ((text (if (string? content-type)
                   content-type
                   (format #f "~a" (or content-type ""))))
         (lowered (string-downcase text))
         (index (string-contains lowered "boundary=")))
    (and index
         (let* ((start (+ index (string-length "boundary=")))
                (rest (substring text start (string-length text)))
                (value (car (string-split rest #\;)))
                (trimmed (string-trim-both value)))
           (if (and (> (string-length trimmed) 1)
                    (char=? (string-ref trimmed 0) #\")
                    (char=? (string-ref trimmed
                                        (- (string-length trimmed) 1))
                            #\"))
               (substring trimmed 1 (- (string-length trimmed) 1))
               trimmed)))))

(define (split-on text delimiter)
  "Split @var{text} on every occurrence of the string @var{delimiter}."
  (let loop ((start 0) (out '()))
    (let ((index (string-contains text delimiter start)))
      (if index
          (loop (+ index (string-length delimiter))
                (cons (substring text start index) out))
          (reverse (cons (substring text start (string-length text)) out))))))

(define (multipart-part-name headers)
  "Return the form field name declared in @var{headers}, or @code{#f}."
  (let* ((lowered (string-downcase headers))
         (index (string-contains lowered "name=\"")))
    (and index
         (let* ((start (+ index (string-length "name=\"")))
                (end (string-index headers #\" start)))
           (and end (substring headers start end))))))

(define (strip-crlf text)
  "Return @var{text} without one trailing CRLF."
  (if (and (>= (string-length text) 2)
           (char=? (string-ref text (- (string-length text) 2)) #\return)
           (char=? (string-ref text (- (string-length text) 1)) #\newline))
      (substring text 0 (- (string-length text) 2))
      text))

(define (parse-multipart-body body content-type)
  "Parse a multipart/form-data @var{body} into an alist of fields.

The values are strings; a file part is returned as its content, because the
generated model describes the part's shape and not the browser's filename.  A
body without a boundary, or one that is not text, yields the empty alist."
  (let* ((text (cond
                 ((string? body) body)
                 ((bytevector? body) (bytevector->string body "ISO-8859-1"))
                 (else "")))
         (boundary (multipart-boundary content-type))
         (parts (and boundary
                     (cdr (split-on text (string-append "--" boundary))))))
    (if (not parts)
        '()
        (filter-map
         (lambda (part)
           (let* ((part (if (string-prefix? "\r\n" part)
                            (substring part 2 (string-length part))
                            part))
                  (separator (string-contains part "\r\n\r\n")))
             (and separator
                  (let* ((headers (substring part 0 separator))
                         (value (strip-crlf
                                 (substring part (+ separator 4)
                                            (string-length part))))
                         (name (multipart-part-name headers)))
                    (and name (cons name value))))))
         (filter (lambda (part)
                   (and (not (string-null? part))
                        (not (string-prefix? "--" part))))
                 parts)))))

(define (bytevector-concat parts)
  "Return the concatenation of the bytevectors @var{parts}."
  (let* ((size (apply + (map bytevector-length parts)))
         (out (make-bytevector size 0)))
    (let loop ((rest parts) (offset 0))
      (if (null? rest)
          out
          (let ((part (car rest)))
            (bytevector-copy! part 0 out offset (bytevector-length part))
            (loop (cdr rest) (+ offset (bytevector-length part))))))))

(define (multipart-part-bytes part boundary)
  "Return one multipart part as a list of bytevectors."
  (let ((name (car part))
        (value (cadr part))
        (filename (caddr part)))
    (list (string->utf8
           (string-append
            "--" boundary "\r\n"
            "Content-Disposition: form-data; name=\"" name "\""
            (if filename
                (format #f "; filename=\"~a\"" filename)
                "")
            "\r\n\r\n"))
          (cond
            ((bytevector? value) value)
            ((string? value) (string->utf8 value))
            (else (string->utf8 (format #f "~a" value))))
          (string->utf8 "\r\n"))))

(define* (multipart-body parts #:key boundary)
  "Return @var{parts}, a list of @code{multipart-part} values, as request bytes."
  (let* ((boundary (or boundary (make-multipart-boundary)))
         (chunks (append-map (lambda (part)
                               (multipart-part-bytes part boundary))
                             parts)))
    (bytevector-concat
     (append chunks
             (list (string->utf8 (string-append "--" boundary "--\r\n")))))))

(define (parse-query text)
  "Parse the query string @var{text} into an alist of decoded name/value pairs."
  (if (or (not (string? text)) (string-null? text))
      '()
      (map (lambda (pair)
             (let* ((parts (string-split pair #\=))
                    (name (uri-decode (car parts)))
                    (value (if (pair? (cdr parts))
                               (uri-decode (string-join (cdr parts) "="))
                               "")))
               (cons name value)))
           (string-split (if (string-prefix? "?" text)
                             (substring text 1 (string-length text))
                             text)
                         #\&))))

(define (uri-decode text)
  "Decode the percent escapes in @var{text}, and @code{+} as a space."
  (let ((replaced (string-replace-substring text "+" " ")))
    (call-with-output-string
      (lambda (port)
        (let loop ((index 0))
          (cond
            ((>= index (string-length replaced)) #t)
            ((and (char=? (string-ref replaced index) #\%)
                  (< (+ index 2) (string-length replaced)))
             (display (integer->char
                       (string->number (substring replaced (+ index 1)
                                                  (+ index 3))
                                       16))
                      port)
             (loop (+ index 3)))
            (else
             (display (string-ref replaced index) port)
             (loop (+ index 1)))))))))

(define (params->path template parameters)
  "Substitute @var{parameters} into the path @var{template}.

@example
(params->path \"/pets/{petId}\" '((\"petId\" . 7)))
;; => \"/pets/7\"
@end example"
  (fold (lambda (entry result)
          (string-replace-substring result
                                    (format #f "{~a}" (car entry))
                                    (uri-encode (format #f "~a" (cdr entry)))))
        template
        parameters))

;;; Commentary:
;;; Authentication
;;
;; The specification describes credentials as security schemes, and an operation
;; lists the schemes that authorise it, as alternatives of combinations.  An
;; @code{<auth>} holds what the caller has; @code{authenticate} finds the first
;; requirement the credentials satisfy and returns the headers and query
;; parameters that expressing it takes.

(define-record-type <auth>
  (%make-auth api-keys bearer-token username password scopes headers)
  auth?
  (api-keys auth-api-keys)
  (bearer-token auth-bearer-token)
  (username auth-username)
  (password auth-password)
  (scopes auth-scopes)
  (headers auth-headers))

(define* (make-auth #:key
                    (api-keys '())
                    (bearer-token #f)
                    (username #f)
                    (password #f)
                    (scopes '())
                    (headers '()))
  "Create credentials for a generated client.

@var{api-keys} is an alist of security scheme name (or header/query name) to
key; @var{bearer-token} covers @code{http} bearer, @code{oauth2} and
@code{openIdConnect}; @var{username} and @var{password} cover HTTP basic;
@var{scopes} lists the OAuth2 scopes the token was granted; @var{headers} are
extra headers to send with every request.

@example
(make-auth #:api-keys '((\"api_key\" . \"secret\")))
(make-auth #:bearer-token \"eyJhbGciOi...\" #:scopes '(\"read:pets\"))
(make-auth #:username \"alice\" #:password \"hunter2\")
@end example"
  (%make-auth api-keys bearer-token username password scopes headers))

(define (scheme-type descriptor)
  (let ((type (assoc-ref descriptor "type")))
    (and type (string->symbol type))))

(define (scheme-value security-name descriptor auth)
  "Return the credential @var{auth} holds for the scheme @var{security-name}."
  (or (let ((entry (assoc security-name (auth-api-keys auth))))
        (and entry (cdr entry)))
      (let ((name (assoc-ref descriptor "name")))
        (and name
             (let ((entry (assoc name (auth-api-keys auth))))
               (and entry (cdr entry)))))))

(define (auth-satisfiable? auth security-name descriptor)
  "Return @code{#t} when @var{auth} can satisfy the scheme @var{security-name}."
  (and auth
       (case (scheme-type descriptor)
         ((apiKey)
          (and (scheme-value security-name descriptor auth) #t))
         ((http)
          (let ((scheme (string-downcase (or (assoc-ref descriptor "scheme") ""))))
            (cond
              ((string=? scheme "basic")
               (and (auth-username auth) (auth-password auth) #t))
              ((string=? scheme "bearer")
               (and (auth-bearer-token auth) #t))
              (else (and (auth-bearer-token auth) #t)))))
         ((oauth2 openIdConnect)
          (and (auth-bearer-token auth) #t))
         (else #f))))

(define (scopes-satisfied? auth scopes)
  (every (lambda (scope)
           (member scope (auth-scopes auth)))
         scopes))

(define (scheme-headers security-name descriptor auth)
  (case (scheme-type descriptor)
    ((apiKey)
     (let ((value (scheme-value security-name descriptor auth))
           (name (assoc-ref descriptor "name"))
           (location (assoc-ref descriptor "in")))
       (cond
         ((and (string=? (or location "") "header") name)
          (list (cons name value)))
         ((and (string=? (or location "") "cookie") name)
          (list (cons "Cookie" (format #f "~a=~a" name value))))
         (else '()))))
    ((http)
     (let ((scheme (string-downcase (or (assoc-ref descriptor "scheme") ""))))
       (cond
         ((string=? scheme "basic")
          (list (cons "Authorization"
                      (string-append "Basic "
                                     (base64-encode
                                      (string-append (auth-username auth)
                                                     ":"
                                                     (auth-password auth)))))))
         (else
          (list (cons "Authorization"
                      (string-append "Bearer " (auth-bearer-token auth))))))))
    ((oauth2 openIdConnect)
     (list (cons "Authorization"
                 (string-append "Bearer " (auth-bearer-token auth)))))
    (else '())))

(define (scheme-query security-name descriptor auth)
  (if (and (eq? (scheme-type descriptor) 'apiKey)
           (string=? (or (assoc-ref descriptor "in") "") "query"))
      (let ((name (assoc-ref descriptor "name"))
            (value (scheme-value security-name descriptor auth)))
        (if (and name value)
            (list (cons name value))
            '()))
      '()))

(define* (authenticate auth schemes requirements)
  "Return @code{(values headers query)} expressing @var{auth} for an operation.

@var{schemes} is an alist of security scheme name to its specification
descriptor; @var{requirements} is a list of alternatives, each alternative a
list of schemes that must all be satisfied.  An empty @var{requirements} list
means the operation is public and no credentials are returned.

A @code{<openapi-auth-error>} is raised when no alternative can be satisfied,
because calling a secured operation anonymously is a programming error, not a
problem the server should report."
  (cond
    ((null? requirements)
     (values (auth-headers-or-none auth) '()))
    (else
     (let loop ((alternatives requirements))
       (cond
         ((null? alternatives)
          (auth-failure
           "no credentials satisfy the security requirements of this operation"))
         (else
          (let* ((requirement (car alternatives))
                 (applicable (filter (lambda (entry)
                                       (and (assoc (car entry) schemes)
                                            (auth-satisfiable?
                                             auth
                                             (car entry)
                                             (assoc-ref schemes (car entry)))
                                            (scopes-satisfied? auth (cdr entry))))
                                     requirement)))
            (if (= (length applicable) (length requirement))
                (values (append (auth-headers-or-none auth)
                                (append-map (lambda (entry)
                                              (scheme-headers
                                               (car entry)
                                               (assoc-ref schemes (car entry))
                                               auth))
                                            requirement))
                        (append-map (lambda (entry)
                                      (scheme-query
                                       (car entry)
                                       (assoc-ref schemes (car entry))
                                       auth))
                                    requirement))
                (loop (cdr alternatives))))))))))

(define (auth-headers-or-none auth)
  (if auth
      (auth-headers auth)
      '()))

(define (base64-encode text)
  "Return the base64 encoding of @var{text}, for HTTP basic authentication."
  (call-with-output-string
    (lambda (port)
      (let* ((bytes (string->utf8 text))
             (length (bytevector-length bytes))
             (alphabet
              "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
        (let loop ((index 0))
          (when (< index length)
            (let* ((remaining (- length index))
                   (first (bytevector-u8-ref bytes index))
                   (second (if (> remaining 1) (bytevector-u8-ref bytes (+ index 1)) 0))
                   (third (if (> remaining 2) (bytevector-u8-ref bytes (+ index 2)) 0))
                   (triple (+ (* first 65536) (* second 256) third)))
              (display (string-ref alphabet (quotient triple 262144)) port)
              (display (string-ref alphabet
                                   (remainder (quotient triple 4096) 64))
                       port)
              (if (> remaining 1)
                  (display (string-ref alphabet
                                       (remainder (quotient triple 64) 64))
                           port)
                  (display "=" port))
              (if (> remaining 2)
                  (display (string-ref alphabet (remainder triple 64)) port)
                  (display "=" port))
              (loop (+ index 3)))))))))
