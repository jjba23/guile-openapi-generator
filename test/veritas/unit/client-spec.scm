;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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

(define base-port
  ;; A port range derived from the process id, so a run never reuses a port that
  ;; a previous run may still have in TIME_WAIT.
  (+ 8600 (modulo (getpid) 400)))

(define next-port
  ;; A plain variable rather than a parameter: parameters are per dynamic extent,
  ;; so a test that took a port inside its own extent would hand the same port to
  ;; the next one.
  base-port)

(define (take-port)
  (let ((port next-port))
    (set! next-port (+ port 1))
    port))

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

(define (port-open? port)
  "Return @code{#t} when something accepts a connection on @var{port}."
  (catch #t
    (lambda ()
      (let ((connection (socket PF_INET SOCK_STREAM 0)))
        (connect connection
                 (make-socket-address AF_INET
                                      (inet-pton AF_INET "127.0.0.1")
                                      port))
        (close connection)
        #t))
    (lambda (key . arguments)
      #f)))

(define (wait-for-server port)
  "Block until @var{port} accepts connections, or say that it never did.

Polling rather than sleeping is what makes this reliable: a fixed sleep is either
too short (a slow machine) or wasted, and when the server cannot start at all the
failure has to name the port instead of looking like a broken client."
  (let loop ((attempts 0))
    (cond
      ((port-open? port) #t)
      ((> attempts 200)
       (error "The test server never accepted a connection" port))
      (else
       (usleep 25000)
       (loop (+ attempts 1))))))

(define (with-server thunk)
  "Start the test server on a port of its own, call THUNK with it, then cancel it.

Each test gets a fresh port: a cancelled server may hold its socket for a moment,
and a test that failed to bind would otherwise look like a client bug."
  (let* ((port (take-port))
         (thread (call-with-new-thread
                  (lambda ()
                    (catch #t
                      (lambda ()
                        (run-server handler 'http (list #:port port
                                                        #:host "127.0.0.1")))
                      (lambda (key . arguments)
                        (format (current-error-port)
                                "the test server failed to start: ~a ~s~%"
                                key
                                arguments)
                        #f))))))
    (wait-for-server port)
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

(define (client-program root program port)
  "Generate the petstore client as ROOT and run PROGRAM against PORT.

The test server runs in this process, so the program, which runs in a child
process, really speaks HTTP to it.  The URL is handed over as a binding rather
than as an environment variable: the spec file runs in its own thread and
changing the environment of a multi-threaded process is undefined."
  (with-generated root
    (lambda (directory)
      (run-program directory
                   (list (list root 'client)
                         (list root 'models))
                   program
                   #:bindings (list (cons 'base-url
                                          (format #f "~s"
                                                  (format #f "http://127.0.0.1:~a"
                                                          port))))))))

(define (request-for method path)
  "Return the request the server recorded for METHOD and PATH, or #f."
  (find (lambda (entry)
          (and (eq? (car entry) method)
               (string=? (cadr entry) path)))
        requests))

(define behaviour-suite
  (suite "Generated client against a server"
         #:concurrent? #f

         (test "The client talks to the server: query, credentials, JSON out and back"
               (set! requests '())
               (with-server
                (lambda (port)
                  (let ((output (client-program 'e2e-client "client-calls" port)))
                    (assert-equal
                     #:expect '("200" "(Rex Bella)" "Mia" "201" "Cadet" "204")
                     #:got (lines output))
                    (let ((list-request (request-for 'GET "/pets"))
                          (post-request (request-for 'POST "/pets")))
                      (assert-true (if list-request #t #f)
                                   #:name "the server answered a GET /pets")
                      (assert-true (if post-request #t #f)
                                   #:name "the server answered a POST /pets")
                      (when list-request
                        (assert-equal #:expect "secret"
                                      #:got (cadddr list-request))
                        (assert-equal #:expect "limit=10"
                                      #:got (caddr list-request)))
                      (when post-request
                        (assert-equal #:expect "{\"name\":\"Cadet\"}"
                                      #:got (utf8->string
                                            (list-ref post-request 4)))))))))

         (test "An undeclared status raises instead of decoding"
               (with-server
                (lambda (port)
                  (let ((output (client-program 'e2e-client-error
                                                "client-error"
                                                port)))
                    (assert-equal #:expect '("500")
                                  #:got (lines output))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               text-suite behaviour-suite))
