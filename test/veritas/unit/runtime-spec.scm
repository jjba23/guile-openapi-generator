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
;; The runtime the generated code links against: the validators (including
;; their failure modes), the tiny HTTP vocabulary, and authentication.  A
;; validator that silently accepts a bad value is worse than no validator, so
;; every check is tested twice: once for the value it must accept, once for the
;; value it must reject.

(define-module (veritas unit runtime-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator runtime)
  #:export (spec))

;;; Helpers

(define (failure thunk)
  "Return @code{(path message)} when @var{thunk} throws a validation error."
  (catch 'openapi-validation-error
    (lambda ()
      (thunk)
      #f)
    (lambda (key path message)
      (list path message))))

(define (invalid? thunk)
  "Return @code{#t} when @var{thunk} throws a validation error."
  (if (failure thunk) #t #f))

(define (path-of thunk)
  "Return the path of the validation error @var{thunk} throws, if any."
  (let ((failure (failure thunk)))
    (and failure (car failure))))

(define schemes
  '(("api_key" ("type" . "apiKey") ("in" . "header") ("name" . "X-API-Key"))
    ("query_key" ("type" . "apiKey") ("in" . "query") ("name" . "api_key"))
    ("cookie_key" ("type" . "apiKey") ("in" . "cookie") ("name" . "session"))
    ("basic" ("type" . "http") ("scheme" . "basic"))
    ("bearer" ("type" . "http") ("scheme" . "bearer"))
    ("oauth" ("type" . "oauth2"))
    ("open_id" ("type" . "openIdConnect"))))

(define (headers-for auth requirements)
  (receive (headers query) (authenticate auth schemes requirements)
    (values headers query)))

(define validation-suite
  (suite "Validation"
         (test "A missing required value is reported with its path"
               (assert-equal #:expect '(("pet" "name")
                                        "is required but is missing")
                             #:got (failure (lambda ()
                                              (check-required '("pet" "name")
                                                              #f)))))

         (test "The wrong type is reported"
               (assert-equal #:expect '("pet" "age")
                             #:got (path-of (lambda ()
                                              (check-type '("pet" "age")
                                                          3.5
                                                          'integer))))
               (assert-equal #:expect '("pet" "age")
                             #:got (path-of (lambda ()
                                              (check-type '("pet" "age")
                                                          "three"
                                                          'integer)))))

         (test "A predicate is checked"
               (assert-true (check-predicate '("pet") 1 integer? "integer"))
               (assert-true (invalid? (lambda ()
                                        (check-predicate '("pet")
                                                         "one"
                                                         integer?
                                                         "integer")))))

         (test "String length and pattern"
               (assert-true (check-min-length '("s") "abc" 3))
               (assert-true (invalid? (lambda () (check-min-length '("s") "ab" 3))))
               (assert-true (invalid? (lambda () (check-max-length '("s") "abcd" 3))))
               (assert-true (check-pattern '("s") "abc-123" "^[a-z]+-[0-9]+$"))
               (assert-true (invalid? (lambda ()
                                        (check-pattern '("s") "abc" "^[0-9]+$")))))

         (test "Enumerations"
               (assert-true (check-enum '("s") "available" '("available" "sold")))
               (assert-equal #:expect '("s")
                             #:got (path-of (lambda ()
                                              (check-enum '("s")
                                                          "bogus"
                                                          '("available"))))))

         (test "Formats"
               (assert-true (valid-format? "2024-02-29T00:00:00Z" 'date-time))
               (assert-false (valid-format? "2024-02-29" 'date-time))
               (assert-true (valid-format? "2024-02-29" 'date))
               (assert-true (valid-format? "3f2504e0-4f89-11d3-9a0c-0305e82c3301"
                                           'uuid))
               (assert-true (valid-format? "someone@example.com" 'email))
               (assert-false (valid-format? "not-an-email" 'email))
               (assert-true (valid-format? "https://example.com/x" 'uri))
               (assert-true (valid-format? "anything at all" 'byte)
                            #:name "a format without a rule accepts anything"))

         (test "Numeric ranges"
               (assert-true (check-minimum '("n") 3 3))
               (assert-true (invalid? (lambda () (check-minimum '("n") 2 3))))
               (assert-true (invalid? (lambda () (check-exclusive-minimum '("n")
                                                                          3
                                                                          3))))
               (assert-true (invalid? (lambda () (check-maximum '("n") 4 3))))
               (assert-true (check-multiple-of '("n") 9 3))
               (assert-true (invalid? (lambda () (check-multiple-of '("n") 7 3))))

               (assert-true (check-multiple-of '("n") 0.3 0.1)
                            #:name "an inexact multiple is accepted within tolerance")
               (assert-true (check-multiple-of '("n") 4.0 0.1)
                            #:name "a whole inexact multiple is accepted")
               (assert-true (invalid? (lambda ()
                                        (check-multiple-of '("n") 0.35 0.1)))
                            #:name "an inexact non-multiple is still rejected")
               (assert-true (check-multiple-of '("n") 1/2 1/6)
                            #:name "an exact rational multiple is accepted"))

         (test "Collections"
               (assert-true (check-min-items '("l") '(1 2) 2))
               (assert-true (invalid? (lambda () (check-min-items '("l") '(1) 2))))
               (assert-true (check-unique-items '("l") '(1 2 3)))
               (assert-true (invalid? (lambda ()
                                        (check-unique-items '("l") '(1 1)))))
               (assert-true (check-list '("l") '(1 2) integer? "integer"))
               (assert-equal #:expect '("l")
                             #:got (path-of (lambda ()
                                              (check-list '("l")
                                                          '(1 "two")
                                                          integer?
                                                          "integer")))))

         (test "valid? answers without raising"
               (assert-true (valid? (lambda () (check-min-length '("s") "abc" 3))))
               (assert-false (valid? (lambda () (check-min-length '("s") "a" 3))))
               (assert-error (lambda () (valid? (lambda () (car '()))))
                             #:name "an unrelated error is not swallowed"))))

(define http-suite
  (suite "HTTP values"
         (test "Headers are looked up without case"
               (let ((headers '(("Content-Type" . "application/json"))))
                 (assert-equal #:expect "application/json"
                               #:got (header-ref headers "content-type"))
                 (assert-equal #:expect "application/json"
                               #:got (header-ref headers 'Content-Type))))

         (test "Setting a header replaces it"
               (assert-equal #:expect '(("ACCEPT" . "text/plain"))
                             #:got (header-set '(("Accept" . "application/json"))
                                               "ACCEPT"
                                               "text/plain")
                             #:name "the name is stored as it was given"))

         (test "Responses are built with the status they mean"
               (assert-eqv #:expect 200 #:got (http-response-status (ok '())))
               (assert-eqv #:expect 201 #:got (http-response-status (created '())))
               (assert-eqv #:expect 204
                           #:got (http-response-status (no-content)))
               (assert-true (json-null? (http-response-body (no-content))))
               (assert-eqv #:expect 401
                           #:got (http-response-status (unauthorized "no"))))

         (test "A request is a value"
               (let ((request (make-http-request 'GET
                                                 "/pets"
                                                 '(("limit" . "10"))
                                                 '(("Accept" . "application/json"))
                                                 #f)))
                 (assert-equal #:expect "10"
                               #:got (http-request-query-ref request "limit"))
                 (assert-equal #:expect "application/json"
                               #:got (http-request-header request "accept"))))

         (test "Query strings encode and decode"
               (assert-equal #:expect "a=1&c=x%20y"
                             #:got (query-string '(("a" . 1) ("b" . #f)
                                                   ("c" . "x y"))))
               (assert-equal #:expect '(("a" . "1") ("c" . "x y") ("d" . "/"))
                             #:got (parse-query "?a=1&c=x+y&d=%2F"))
               (assert-equal #:expect '()
                             #:got (parse-query "")))

         (test "Path parameters are substituted"
               (assert-equal #:expect "/pets/a%20b"
                             #:got (params->path "/pets/{petId}"
                                                 '(("petId" . "a b")))))

         (test "A status that was not declared is an error"
               (assert-equal #:expect 200
                             #:got (http-response-status
                                    (expect-status (ok '()) '(200))))
               (assert-error (lambda ()
                               (expect-status (error-response 500 "boom")
                                              '(200)))
                             #:name "500 is not 200"))

         (test "A status family stands for every code in it"
               (assert-equal #:expect '(200 201 202 299)
                             #:got (list (car (status-range 200 202))
                                         (cadr (status-range 200 202))
                                         (caddr (status-range 200 202))
                                         (car (reverse (status-range 200 299)))))
               (assert-equal #:expect 200
                             #:got (http-response-status
                                    (expect-status (ok '()) (status-range 200 299))))
               (assert-error (lambda ()
                               (expect-status (error-response 500 "boom")
                                              (status-range 200 299)))
                             #:name "a 500 is outside the 2XX family"))))

(define auth-suite
  (suite "Authentication"
         (test "A public operation needs no credentials"
               (receive (headers query) (headers-for #f '())
                 (assert-equal #:expect '() #:got headers)
                 (assert-equal #:expect '() #:got query)))

         (test "An API key goes in the header it names"
               (receive (headers query)
                        (headers-for (make-auth #:api-keys '(("api_key" . "s")))
                                     '((("api_key"))))
                 (assert-equal #:expect '(("X-API-Key" . "s")) #:got headers)
                 (assert-equal #:expect '() #:got query)))

         (test "A query API key goes in the query"
               (receive (headers query)
                        (headers-for (make-auth #:api-keys '(("query_key" . "s")))
                                     '((("query_key"))))
                 (assert-equal #:expect '() #:got headers)
                 (assert-equal #:expect '(("api_key" . "s")) #:got query)))

         (test "A cookie API key becomes a Cookie header"
               (receive (headers query)
                        (headers-for (make-auth #:api-keys '(("cookie_key" . "s")))
                                     '((("cookie_key"))))
                 (assert-equal #:expect '(("Cookie" . "session=s")) #:got headers)))

         (test "Basic authentication is base64 of user:password"
               (receive (headers query)
                        (headers-for (make-auth #:username "u" #:password "p")
                                     '((("basic"))))
                 (assert-equal #:expect '(("Authorization" . "Basic dTpw"))
                               #:got headers)))

         (test "A bearer token is sent as a bearer token"
               (receive (headers query)
                        (headers-for (make-auth #:bearer-token "t")
                                     '((("bearer")) (("oauth"))))
                 (assert-equal #:expect '(("Authorization" . "Bearer t"))
                               #:got headers)))

         (test "The first satisfiable alternative wins"
               (receive (headers query)
                        (headers-for (make-auth #:bearer-token "t")
                                     '((("api_key")) (("bearer"))))
                 (assert-equal #:expect '(("Authorization" . "Bearer t"))
                               #:got headers)))

         (test "A missing scope makes an alternative unsatisfiable"
               (assert-error (lambda ()
                               (headers-for (make-auth #:bearer-token "t")
                                            '((("oauth" "write")))))
                             #:name "the token does not carry write"))

         (test "A granted scope satisfies the alternative"
               (receive (headers query)
                        (headers-for (make-auth #:bearer-token "t"
                                                #:scopes '("write"))
                                     '((("oauth" "write"))))
                 (assert-equal #:expect '(("Authorization" . "Bearer t"))
                               #:got headers)))

         (test "Credentials that satisfy nothing raise"
               (assert-error (lambda () (headers-for #f '((("api_key")))))
                             #:name "no credentials, secured operation"))

         (test "Extra headers are always sent"
               (receive (headers query)
                        (headers-for (make-auth #:headers '(("X-Trace" . "1")))
                                     '())
                 (assert-equal #:expect '(("X-Trace" . "1")) #:got headers)))))

(define (spec)
  (veritas-run validation-suite http-suite auth-suite))
