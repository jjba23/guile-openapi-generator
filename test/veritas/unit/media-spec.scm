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
;; The transport slice: a client and a dispatcher that speak JSON, text, bytes,
;; forms and multipart, plus declared response headers and the non-null payload
;; option.  The generated client runs in a child process against a server this
;; file starts, because the point is the bytes on the wire as much as the value
;; the caller receives.

(define-module (veritas unit media-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (rnrs bytevectors)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (openapi-generator runtime)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define (json-result status text)
  (values (build-response #:code status
                          #:headers '((content-type . (application/json))))
          text))

(define (text-result status text)
  (values (build-response #:code status
                          #:headers '((content-type . (text/plain))))
          text))

(define (binary-result status body)
  (values (build-response #:code status
                          #:headers '((content-type . (application/octet-stream))))
          body))

(define (body-length body)
  "Return the size of BODY, whether it is a string or bytes."
  (cond
    ((bytevector? body) (bytevector-length body))
    ((string? body) (string-length body))
    (else 0)))

;;; Server

(define requests '())

(define (header-text request name)
  (let ((entry (assq name (request-headers request))))
    (if entry (format #f "~a" (cdr entry)) "")))

(define (body-text body)
  "Return the request BODY as text, whether it arrived as a string or bytes.

Binary uploads are part of this test, so a bytevector that is not valid UTF-8
falls back to a latin-1 reading for the log rather than raising."
  (cond
    ((string? body) body)
    ((bytevector? body)
     (catch #t
       (lambda () (utf8->string body))
       (lambda arguments
         (bytevector->string body "ISO-8859-1"))))
    (else "")))

(define (handler request body)
  (let* ((path (uri-path (request-uri request)))
         (method (request-method request))
         (raw-body body)
         (body (body-text body)))
    (set! requests
          (cons (list method path (header-text request 'content-type) body
                      raw-body)
                requests))
    (cond
      ((and (eq? method 'GET) (string=? path "/plain"))
       (text-result 200 "hello"))
      ((and (eq? method 'POST) (string=? path "/echo-plain"))
       (text-result 200 body))
      ((and (eq? method 'GET) (string=? path "/download"))
       (binary-result 200 #vu8(0 1 2 255)))
      ((and (eq? method 'POST) (string=? path "/upload"))
       (text-result 200 (number->string (body-length raw-body))))
      ((and (eq? method 'POST) (string=? path "/form"))
       (let* ((query (parse-query body))
              (name (or (cdr (assoc "name" query)) "")))
         (json-result 200 (format #f "{\"message\":\"~a\"}" name))))
      ((and (eq? method 'POST) (string=? path "/multipart"))
       (json-result
        200
        (if (and (string-contains body "name=\"title\"")
                 (string-contains body "name=\"file\"; filename="))
            "{\"message\":\"multipart\"}"
            "{\"message\":\"bad\"}")))
      ((and (eq? method 'GET) (string=? path "/form-response"))
       (values (build-response
                #:code 200
                #:headers
                '((content-type . (application/x-www-form-urlencoded))))
               "message=form"))
      ((and (eq? method 'GET) (string=? path "/headers"))
       (values (build-response #:code 200
                               #:headers '((content-type . (application/json))
                                           (x-rate-limit . "42")
                                           (x-request-id . "abc")))
               "{\"message\":\"headers\"}"))
      ((and (eq? method 'GET) (string=? path "/negotiated"))
       (let* ((query (parse-query (uri-query (request-uri request))))
              (representation (or (cdr (assoc "representation" query)) "")))
         (if (string=? representation "text")
             (text-result 200 "plain")
             (json-result 200 "{\"message\":\"json\"}"))))
      ((and (eq? method 'GET) (string=? path "/empty"))
       (values (build-response #:code 200
                               #:headers '((content-type . (application/json))))
               ""))
      (else (values (build-response #:code 404) "")))))

(define base-port
  (+ 9800 (modulo (getpid) 200)))

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
       (error "The media test server never accepted a connection" port))
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

(define (request-for method path)
  (find (lambda (entry)
          (and (eq? (car entry) method)
               (string=? (cadr entry) path)))
        requests))

;;; Suites

(define (curl-available?)
  "Return @code{#t} when guile-curl can be loaded.

The libcurl transport is generated code that imports @code{(curl)} at load time,
so its end-to-end test needs the module the same way the generated tree does.  A
build without it (a distribution package that does not want to depend on an
optional transport, a container image) runs every other test and skips this one
rather than failing with an unbound variable from a module that never loaded."
  (catch #t
    (lambda () (and (resolve-module '(curl) #:ensure #f) #t))
    (lambda arguments #f)))

(define runtime-suite
  (suite "Transport helpers"
         (test "A response header is read without case"
               (assert-equal
                #:expect "v"
                #:got (http-response-header
                       (make-http-response 200 '(("X-Test" . "v")) "")
                       "x-test")))
         (test "Text and binary responses set their content type"
               (assert-true
                (if (string-contains
                     (header-ref (http-response-headers (text-response 200 "x"))
                                 "Content-Type")
                     "text/plain")
                    #t
                    #f))
               (assert-true
                (if (string-contains
                     (header-ref (http-response-headers
                                  (binary-response 200 "x"))
                                 "Content-Type")
                     "application/octet-stream")
                    #t
                    #f)))
         (test "A multipart body has a boundary and a part per value"
               (let ((body (utf8->string
                            (multipart-body
                             (list (multipart-part "a" "1")
                                   (multipart-part "file" "data" "file.bin"))
                             #:boundary "B"))))
                 (assert-true (contains? body "--B"))
                 (assert-true (contains? body "name=\"a\""))
                 (assert-true (contains? body "name=\"file\"; filename=\"file.bin\""))
                 (assert-true (contains? body "data"))))
         (test "A required body is enforced"
               (assert-eq #:expect 'value
                          #:got (require-body 'value
                                              (make-http-response 200 '() "")))
               (assert-equal
                #:expect 200
                #:got (catch 'openapi-empty-body
                        (lambda ()
                          (require-body #f (make-http-response 200 '() ""))
                          'decoded)
                        (lambda (key status body) status))))))

(define client-suite
  (suite "A client across media types"
         #:concurrent? #f

         (test "Text, bytes, forms, multipart and headers travel end to end"
               (set! requests '())
               (with-server
                (lambda (port)
                  (with-generated 'media-client
                    (lambda (directory)
                      (let ((output
                             (run-program
                              directory
                              (list '(media-client client)
                                    '(media-client models))
                              "media-client"
                              #:bindings
                              (list (cons 'base-url
                                          (format #f "~s"
                                                  (format #f "http://127.0.0.1:~a"
                                                          port)))))))
                        (assert-equal
                         #:expect '("hello" "hello" "4" "4" "Ada"
                                    "multipart" "42" "abc" "form"
                                    "plain" "200")
                         #:got (lines output))))
                    #:input (fixture "media.yaml")
                    #:controllers 'none
                    #:http 'guile
                    #:non-null-response-payloads? #t)
                  (let ((plain (request-for 'POST "/echo-plain"))
                        (upload (request-for 'POST "/upload"))
                        (form (request-for 'POST "/form"))
                        (multipart (request-for 'POST "/multipart")))
                    (when plain
                      (assert-true
                       (if (string-contains (caddr plain) "text/plain")
                           #t
                           #f)))
                    (when upload
                      (assert-true
                       (if (string-contains (caddr upload)
                                            "application/octet-stream")
                           #t
                           #f)))
                    (when form
                      (assert-true
                       (if (string-contains (caddr form)
                                            "application/x-www-form-urlencoded")
                           #t
                           #f)))
                    (when multipart
                      (assert-true
                       (if (string-contains (caddr multipart)
                                            "multipart/form-data")
                           #t
                           #f)))))))

         (test "The libcurl transport sends bytes, not a latin-1 round-trip"
               #:skip? (not (curl-available?))
               (set! requests '())
               (with-server
                (lambda (port)
                  (with-generated 'media-curl
                    (lambda (directory)
                      (let ((output
                             (run-program
                              directory
                              (list '(media-curl client)
                                    '(media-curl models))
                              "media-client"
                              #:bindings
                              (list (cons 'base-url
                                          (format #f "~s"
                                                  (format #f "http://127.0.0.1:~a"
                                                          port)))))))
                        (assert-equal
                         #:expect '("hello" "hello" "4" "4" "Ada"
                                    "multipart" "42" "abc" "form"
                                    "plain" "200")
                         #:got (lines output))
                        (let ((upload (request-for 'POST "/upload")))
                          (assert-equal
                           #:expect #vu8(0 1 2 255)
                           #:got (and upload (list-ref upload 4))))))
                    #:input (fixture "media.yaml")
                    #:controllers 'none
                    #:http 'curl
                    #:non-null-response-payloads? #t))))))

(define controllers-suite
  (suite "A dispatcher across media types"
         #:concurrent? #f

         (test "Text and form bodies are decoded by media type"
               (with-generated 'media-controllers
                 (lambda (directory)
                   (assert-equal
                    #:expect '("200 {\"name\":\"Ada\",\"count\":3} (application/json)"
                               "200 bytes (text/plain (charset . utf-8))"
                               "hello" "Ada 3 #t" "T data" "hello" "json")
                    #:got (lines (run-program
                                  directory
                                  (list '(media-controllers controllers)
                                        '(media-controllers models))
                                  "media-controllers"))))
                 #:input (fixture "media.yaml")
                 #:controllers 'vanilla
                 #:http 'none))))

(define (spec)
  (veritas-run #:concurrent? #f
               runtime-suite client-suite controllers-suite))
