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
;; The controllers generator: the route table, the dispatcher, and the decoding
;; of path, query, header and body parameters.  The generated controller is
;; loaded and dispatched for real, because a route table that matches the wrong
;; path is not something a text assertion would notice.
;;
;; The Artanis adapter is only checked as text: Artanis is a heavy dependency and
;; the adapter is deliberately a thin translation of a request and a response,
;; which is exactly what the assertions look at.

(define-module (veritas unit controllers-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator runtime)
  #:use-module (openapi-generator controllers)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define petstore-text
  (emit-controllers (petstore-spec) (test-config #:root "petstore")))

(define artanis-text
  (emit-artanis-controllers (petstore-spec) (test-config #:root "petstore")))

;;; Suites

(define text-suite
  (suite "Generated text"
         (test "The route table holds one route per operation"
               (assert-true (contains? petstore-text "(define routes"))
               (assert-true (contains? petstore-text
                                       "(make-route 'GET (make-regexp \"^/pets/?$\")"))
               (assert-true (contains? petstore-text
                                       "(make-route 'GET (make-regexp \"^/pets/([^/]+)/?$\")"))
               (assert-equal #:expect 4
                             #:got (occurrences petstore-text "(make-route '")))

         (test "A path parameter becomes a capturing group"
               (assert-true (contains? petstore-text "'(\"petId\")")
                            #:name "the name of the captured parameter is kept"))

         (test "Handlers are looked up, not guessed"
               (assert-true (contains? petstore-text
                                       "(define api-handlers (make-parameter '()))"))
               (assert-true (contains? petstore-text
                                       "(unimplemented-operation operation request)")))

         (test "The portable controller exports a run-server adapter"
               (assert-true (contains? petstore-text
                                       "(define (guile-handler)"))
               (assert-true (contains? petstore-text
                                       "(define (guile-request request body)"))
               (assert-true (contains? petstore-text
                                       "(utf8->string body)"))
               (assert-true (contains? petstore-text
                                       "guile-handler")))

         (test "An operation decodes its parameters"
               (assert-true (contains? petstore-text
                                       "(cons 'limit (let ((raw (http-request-query-ref request \"limit\")))"))
               (assert-true (contains? petstore-text
                                       "(cons 'pet-id (let ((number (string->number (assoc-ref path-arguments \"petId\"))))")))

         (test "A request body is decoded into the record"
               (assert-true (contains? petstore-text
                                       "(json-string->jso raw)")
                            #:name "the body is parsed as JSON")
               (assert-true (contains? petstore-text "(jso->new-pet node)")
                            #:name "the parsed node becomes the record"))

         (test "The Artanis adapter registers one route per operation"
               (assert-equal #:expect 4
                             #:got (occurrences artanis-text "(dispatch-rc rc '"))
               (assert-true (contains? artanis-text "(get \"/pets/"))
               (assert-true (contains? artanis-text "(delete \"/pets/"))
               (assert-true (contains? artanis-text "(lambda (rc)")
                            #:name "each route is a handler")
               (assert-true (contains? artanis-text "(response-emit")
                            #:name "responses go through Artanis")
               (assert-true (contains? artanis-text
                                       "(params->path \"/pets/{petId}\" (artanis-query rc))")
                            #:name "the concrete path is rebuilt from the parameters"))))

(define (controllers-program root program)
  "Generate the petstore controllers as ROOT and run PROGRAM against them."
  (with-generated root
    (lambda (directory)
      (run-program directory (list (list root 'controllers)
                                     (list root 'models)) program))))

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define behaviour-suite
  (suite "Generated dispatch"
         #:concurrent? #f

         (test "Handlers receive the decoded parameters and responses are built"
               (assert-equal #:expect '("200" "((limit . 10))" "((pet-id . 42))"
                                        "Bella" "list-pets" "(GET /nope)")
                             #:got (lines (controllers-program 'e2e-controllers
                                                              "controllers-handlers"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               text-suite behaviour-suite))
