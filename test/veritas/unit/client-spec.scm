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
;; The client generator.  The text assertions cover both transports, because the
;; difference between them is a single procedure and a reviewer should be able to
;; see which one was chosen.  The behaviour test then starts a server on
;; localhost, calls the generated client against it, and checks the whole path:
;; URL building, credentials, JSON in and JSON out, status checking and decoding.
;;
;; libcurl is not exercised against a live server here: the transport is a
;; handful of documented calls, and a test that needed libcurl would fail on a
;; machine that has none.

(define-module (veritas unit client-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator runtime)
  #:use-module (openapi-generator client)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define guile-text
  (emit-client (petstore-spec)
               (test-config #:root "petstore" #:http 'guile)))

(define curl-text
  (emit-client (petstore-spec)
               (test-config #:root "petstore" #:http 'curl)))

(define base-port 8481)

(define current-port (make-parameter base-port))

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
  (let ((path (uri-path (request-uri request))))
    (cond
      ((and (eq? (request-method request) 'POST) (string=? path "/pets"))
       (json-response 201 "{\"id\":2,\"name\":\"Cadet\"}"))
      ((and (eq? (request-method request) 'DELETE)
            (string-prefix? "/pets/" path))
       (values (build-response #:code 204) ""))
      ((and (eq? (request-method request) 'GET) (string=? path "/pets"))
       (json-response 200
                      "[{\"id\":1,\"name\":\"Rex\"},{\"id\":2,\"name\":\"Bella\"}]"))
      ((string-contains path "boom")
       (json-response 500 "{\"message\":\"boom\"}"))
      ((string-prefix? "/pets/" path)
       (json-response 200 "{\"id\":7,\"name\":\"Mia\"}"))
      (else
       (json-response 500 "{\"message\":\"boom\"}")))))

(define (with-server thunk)
  "Start the test server on a port of its own, call THUNK, then cancel it.

Each test gets a fresh port: a cancelled server may hold its socket for a moment,
and a test that failed to bind would otherwise look like a client bug."
  (let* ((port (current-port))
         (thread (call-with-new-thread
                  (lambda ()
                    (catch #t
                      (lambda ()
                        (run-server handler 'http (list #:port port
                                                        #:host "127.0.0.1")))
                      (lambda (key . arguments) #f))))))
    (current-port (+ port 1))
    (usleep 400000)
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk port))
      (lambda ()
        (when (thread? thread)
          (cancel-thread thread))))))

;;; Suites

(define text-suite
  (suite "Generated text"
         (test "The plain Guile client uses (web client)"
               (assert-true (contains? guile-text "#:use-module (web client)"))
               (assert-true (contains? guile-text "(http-request url"))
               (assert-true (contains? guile-text "(define (guile-headers headers)"))
               (assert-false (contains? guile-text "(curl)")))

         (test "The libcurl client uses guile-curl"
               (assert-true (contains? curl-text "#:use-module (curl)"))
               (assert-true (contains? curl-text "(curl-easy-init)"))
               (assert-true (contains? curl-text "(curl-easy-setopt curl 'url url)"))
               (assert-true (contains? curl-text "(curl-easy-perform curl #t)"))
               (assert-true (contains? curl-text "(curl-easy-cleanup curl)"))
               (assert-false (contains? curl-text "(web client)")))

         (test "A client is a record with a base URL and credentials"
               (assert-true (contains? guile-text "(define-record-type <api-client>"))
               (assert-true (contains? guile-text "(define* (make-api-client #:key"))
               (assert-true (contains? guile-text
                                       "(base-url \"https://api.example.com/v1\")")
                            #:name "the first server of the specification is used"))

         (test "Every operation is a procedure"
               (assert-equal #:expect 6
                             #:got (occurrences guile-text "(define* (")
                             #:name "four operations, the constructor and the transport")
               (assert-true (contains? guile-text "(define* (list-pets"))
               (assert-true (contains? guile-text "(define* (get-pet-by-id client #:key pet-id"))
               (assert-true (contains? guile-text
                                       "(define* (post-pets client #:key body")))

         (test "The security schemes are a literal"
               (assert-true (contains? guile-text "(define api-security-schemes"))
               (assert-true (contains? guile-text "(\"type\" . \"apiKey\")"))
               (assert-true (contains? guile-text "(\"scheme\" . \"bearer\")")))

         (test "An operation authenticates before it sends"
               (assert-true (contains? guile-text
                                       "(authenticate auth api-security-schemes '(((\"api_key\"))))"))
               (assert-true (contains? guile-text
                                       "(authenticate auth api-security-schemes '())")
                            #:name "the public operation asks for no credentials"))))

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define (client-program root program)
  "Generate the petstore client as ROOT and run PROGRAM against it.

The test server runs in this process, so the program, which runs in a child
process, really speaks HTTP to it."
  (with-generated root
    (lambda (directory)
      (run-program directory (list (list root 'client)
                                     (list root 'models)) program))))

(define behaviour-suite
  (suite "Generated client against a server"
         #:concurrent? #f

         (test "The client talks to the server: query, credentials, JSON out and back"
               (set! requests '())
               (with-server
                (lambda (port)
                  (setenv "BASE_URL" (format #f "http://127.0.0.1:~a" port))
                  (let ((output (client-program 'e2e-client "client-calls")))
                    (unsetenv "BASE_URL")
                    (assert-equal
                     #:expect '("200" "(Rex Bella)" "Mia" "201" "Cadet" "204")
                     #:got (lines output))
                    (let ((list-request (find (lambda (entry)
                                                (and (eq? (car entry) 'GET)
                                                     (string=? (cadr entry) "/pets")))
                                              requests))
                          (post-request (find (lambda (entry)
                                                (eq? (car entry) 'POST))
                                              requests)))
                      (assert-equal #:expect "secret"
                                    #:got (cadddr list-request))
                      (assert-equal #:expect "limit=10"
                                    #:got (caddr list-request))
                      (assert-equal #:expect 'POST #:got (car post-request))
                      (assert-equal #:expect "{\"name\":\"Cadet\"}"
                                    #:got (utf8->string
                                          (list-ref post-request 4))))))))

         (test "An undeclared status raises instead of decoding"
               (with-server
                (lambda (port)
                  (setenv "BASE_URL" (format #f "http://127.0.0.1:~a" port))
                  (let ((output (client-program 'e2e-client-error
                                                "client-error")))
                    (unsetenv "BASE_URL")
                    (assert-equal #:expect '("500")
                                  #:got (lines output))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               text-suite behaviour-suite))
