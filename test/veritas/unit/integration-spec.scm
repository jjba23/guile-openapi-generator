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
;; Integration tests over a whole specification file.  The other suites read
;; fragments of a fixture; this one reads test/fixtures/library.yaml from start
;; to finish, generates every module the generator can write, and then uses the
;; result: records that must preserve their data, the dispatcher with the full
;; route table, and the generated client against a server this file starts.
;;
;; It is deliberately the layer where the parts meet.  A schema bug, an emitter
;; bug and a runtime bug all look the same from here, which is exactly what an
;; integration test is for.

(define-module (veritas unit integration-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator models)
  #:use-module (openapi-generator client)
  #:use-module (openapi-support)
  #:export (spec))

;;; The fixture, read once

(define library (fixture-spec "library.yaml"))

(define library-models-text
  (emit-models library (test-config #:root "library")))

(define library-client-text
  (emit-client library (test-config #:root "library" #:http 'guile)))

;;; Helpers

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

;;; Server for the generated client

(define book-id "3f2504e0-4f89-11d3-9a0c-0305e82c3301")

(define book-json
  (format #f "{\"id\":\"~a\",\"title\":\"SICP\",\"price\":12.34}" book-id))

(define books-json
  (format #f "[~a]" book-json))

(define requests '())

(define (json-response status body)
  (values (build-response #:code status
                          #:headers '((content-type . (application/json))))
          body))

(define (handler request body)
  (set! requests
        (cons (list (request-method request)
                    (uri-path (request-uri request))
                    (uri-query (request-uri request))
                    (assq-ref (request-headers request) 'x-api-key)
                    body)
              requests))
  (let* ((path (uri-path (request-uri request)))
         (query (or (uri-query (request-uri request)) "")))
    (cond
      ((and (eq? (request-method request) 'POST) (string=? path "/books"))
       (json-response 201
                      (format #f "{\"id\":\"~a\",\"title\":\"Compilers\",\"price\":9.99}"
                              book-id)))
      ((and (eq? (request-method request) 'GET) (string=? path "/search"))
       (if (string-contains query "boom")
           (json-response 500 "{\"status\":500,\"message\":\"boom\"}")
           ;; 299 is deliberately unusual: it is inside the 2XX family the
           ;; specification declares, so the generated client must accept it.
           (json-response 299 books-json)))
      ((and (eq? (request-method request) 'GET) (string=? path "/books"))
       (json-response 200 books-json))
      ((and (eq? (request-method request) 'DELETE)
            (string-prefix? "/books/" path))
       (values (build-response #:code 204) ""))
      ((and (eq? (request-method request) 'GET)
            (string-prefix? "/books/" path))
       (json-response 200 book-json))
      (else
       (json-response 404 "{\"status\":404,\"message\":\"nope\"}")))))

;; A port range of its own, so this file never meets the client suite even if a
;; cancelled server still holds its socket for a moment.
(define base-port
  (+ 9200 (modulo (getpid) 300)))

(define next-port base-port)

(define (take-port)
  (let ((port next-port))
    (set! next-port (+ port 1))
    port))

(define (port-open? port)
  (catch #t
    (lambda ()
      (let ((connection (socket PF_INET SOCK_STREAM 0)))
        (connect connection
                 (make-socket-address AF_INET
                                      (inet-pton AF_INET "127.0.0.1")
                                      port))
        (close connection)
        #t))
    (lambda (key . arguments) #f)))

(define (wait-for-server port)
  (let loop ((attempts 0))
    (cond
      ((port-open? port) #t)
      ((> attempts 200)
       (error "The integration test server never accepted a connection" port))
      (else (usleep 25000) (loop (+ attempts 1))))))

(define (with-server thunk)
  (let* ((port (take-port))
         (thread (call-with-new-thread
                  (lambda ()
                    (catch #t
                      (lambda ()
                        (run-server handler 'http (list #:port port
                                                        #:host "127.0.0.1")))
                      (lambda (key . arguments) #f))))))
    (wait-for-server port)
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk port))
      (lambda ()
        (when (thread? thread)
          (cancel-thread thread))))))

;;; Suites

(define reading-suite
  (suite "A whole specification is read"
         (test "Every operation and schema is understood"
               (assert-equal #:expect 5
                             #:got (length (spec-operations library)))
               (assert-true
                (if (every (lambda (wanted)
                             (find (lambda (schema)
                                     (string=? (named-schema-name schema) wanted))
                                   (spec-schemas library)))
                           '("Book" "NewBook" "Annotations" "Draft" "FreeForm"
                             "Labels" "Problem"))
                    #t
                    #f)))))

(define models-suite
  (suite "Generated models from a whole file"
         (test "uniqueItems is only checked when it is declared"
               ;; The Book schema has one array that may repeat and one that may
               ;; not; exactly one check must be generated.
               (assert-equal #:expect 1
                             #:got (occurrences library-models-text
                                                "(check-unique-items"))
               (assert-true (contains? library-models-text
                                       "(check-unique-items '(\"book\" \"ratings\")"))
               (assert-false (contains? library-models-text
                                        "(check-unique-items '(\"book\" \"tags\")")))

         (test "An inexact multipleOf is generated as declared"
               (assert-true (contains? library-models-text
                                       "(check-multiple-of '(\"book\" \"price\")"))
               (assert-true (contains? library-models-text "0.01")))

         (test "A map is checked against its declared size"
               (assert-true (contains? library-models-text
                                       "(check-min-properties '(\"book\" \"counters\")"))
               (assert-true (contains? library-models-text
                                       "(check-max-properties '(\"book\" \"counters\")")))

         (test "A bare object becomes a map, not an empty record"
               (assert-true (contains? library-models-text
                                       "(define (free-form? value)"))
               (assert-false (contains? library-models-text
                                        "(define-record-type <free-form>")))

         (test "A record that keeps undeclared members exports their accessor"
               (assert-true (contains? library-models-text
                                       "(define* (make-annotations #:key"))
               (assert-true (contains? library-models-text "annotations-extra"))

         (test "additionalProperties: true is understood"
               (assert-true (contains? library-models-text
                                       "(define* (make-draft #:key"))))))

(define client-suite
  (suite "Generated client from a whole file"
         (test "A 2XX response is checked against its family"
               (assert-true (contains? library-client-text
                                       "(status-range 200 299)")
                            #:name "the search operation accepts every 2xx"))
         (test "An exact response keeps its exact check"
               (assert-true (contains? library-client-text
                                       "(expect-status response '(200))"))
               (assert-true (contains? library-client-text
                                       "(expect-status response '(201))"))
               (assert-true (contains? library-client-text
                                       "(expect-status response '(204))")))
         (test "An unset optional header is left out, not sent as #f"
               (assert-true (contains? library-client-text
                                       "(if (eq? x-request-id #f)")))))

(define behaviour-suite
  (suite "The whole tree, used"
         #:concurrent? #f

         (test "Records keep their data and the route table dispatches"
               (with-generated 'library-app
                 (lambda (directory)
                   (assert-equal
                    #:expect
                    '("web"
                      "((lang . en) (note . x))"
                      "((source . web) (lang . en) (note . x))"
                      "web"
                      "((lang . en) (note . x))"
                      "(scheme scheme)"
                      "((width . 10) (unit . cm))"
                      "12.34"
                      "(book ratings)"
                      "must be a multiple of 0.01"
                      "(book counters)"
                      "((a . 1) (b . 2))"
                      "(book counters)"
                      "((a . 1) (b . two))"
                      "((x . y))"
                      "((title . rough) (pages . 3) (notes . later))"
                      "rough"
                      "((pages . 3) (notes . later))"
                      "Structure and Interpretation"
                      "((width . 10) (unit . cm))"
                      "web"
                      "((lang . en) (note . x))"
                      "200"
                      "((limit . 5) (x-request-id . #f))"
                      "((book-id . 3f2504e0-4f89-11d3-9a0c-0305e82c3301))"
                      "Compilers"
                      "200")
                    #:got (lines (run-program
                                  directory
                                  (list '(library-app models)
                                        '(library-app controllers))
                                  "full-api"))))
                 #:input (fixture "library.yaml")))

         (test "The generated client talks to a server"
               (set! requests '())
               (with-server
                (lambda (port)
                  (with-generated 'library-client
                    (lambda (directory)
                      (let ((output
                             (run-program
                              directory
                              (list '(library-client client)
                                    '(library-client models))
                              "full-client"
                              #:bindings
                              (list (cons 'base-url
                                          (format #f "~s"
                                                  (format #f "http://127.0.0.1:~a"
                                                          port)))))))
                        (assert-equal
                         #:expect '("200" "(SICP)" "SICP" "299" "(SICP)"
                                    "201" "Compilers" "500")
                         #:got (lines output))))
                    #:input (fixture "library.yaml"))
                  (let ((list-request (find (lambda (entry)
                                              (and (eq? (car entry) 'GET)
                                                   (string=? (cadr entry) "/books")))
                                            requests))
                        (create-request (find (lambda (entry)
                                                (eq? (car entry) 'POST))
                                              requests))
                        (search-request (find (lambda (entry)
                                                (and (eq? (car entry) 'GET)
                                                     (string=? (cadr entry)
                                                               "/search")
                                                     (string? (caddr entry))
                                                     (string-contains
                                                      (caddr entry)
                                                      "q=sicp")))
                                              requests)))
                    (when list-request
                      (assert-equal #:expect "secret"
                                    #:got (cadddr list-request))
                      (assert-equal #:expect "limit=5"
                                    #:got (caddr list-request)))
                    (when search-request
                      (assert-true
                       (if (string-contains (caddr search-request) "q=sicp")
                           #t
                           #f))
                      (assert-equal #:expect "secret"
                                    #:got (cadddr search-request)))
                    (when create-request
                      (assert-equal
                       #:expect "{\"title\":\"Compilers\",\"price\":9.99}"
                       #:got (utf8->string (list-ref create-request 4))))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               reading-suite models-suite client-suite behaviour-suite))
