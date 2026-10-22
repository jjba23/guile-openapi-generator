;; The generated guile-handler adapts (web server) requests and responses.
(use-modules (web request) (web uri) (web response) (web http))

(define (serve method path content-type body)
  (parameterize ((api-handlers
                  (list (cons 'post-form
                              (lambda (request arguments)
                                (let ((form (cdr (assq 'body arguments))))
                                  (ok (list (cons "name"
                                                  (post-form-request-name form))
                                            (cons "count"
                                                  (post-form-request-count
                                                   form)))))))
                        (cons 'accept-body
                              (lambda (request arguments)
                                (let ((value (cdr (assq 'body arguments))))
                                  (text-response
                                   200
                                   (form-result-message value))))))))
    (receive (response response-body)
        ((guile-handler)
         (build-request
          (build-uri 'http #:host "localhost" #:path path)
          #:method method
          #:port 0
          #:headers (if content-type
                        (list (cons 'content-type
                                    (parse-header 'content-type content-type)))
                        '()))
         body)
      (format #t "~a ~a ~a~%"
              (response-code response)
              (if (bytevector? response-body)
                  (utf8->string response-body)
                  response-body)
              (assq-ref (response-headers response) 'content-type)))))

(serve 'POST "/form" "application/x-www-form-urlencoded"
       "name=Ada&note=x&count=3&subscribe=true")

;; A real web server hands a JSON body over as bytes; the adapter turns it back
;; into text before the generated JSON decoder runs.
(serve 'POST "/accept" "application/json"
       (string->utf8 "{\"message\":\"bytes\"}"))

;; The generated dispatcher decodes bodies by the request's content type.
(define text-response-value
  (parameterize ((api-handlers
                  (list (cons 'echo-plain
                              (lambda (request arguments)
                                (text-response
                                 200
                                 (cdr (assq 'body arguments))))))))
    (dispatch (make-http-request 'POST "/echo-plain" '()
                                 '(("Content-Type" . "text/plain"))
                                 "hello"))))
(format #t "~a~%" (http-response-body text-response-value))

;; Form fields arrive as strings and are coerced to the declared scalar types.
(define form-response-value
  (parameterize ((api-handlers
                  (list (cons 'post-form
                              (lambda (request arguments)
                                (let ((body (cdr (assq 'body arguments))))
                                  (text-response
                                   200
                                   (format #f "~a ~a ~a"
                                           (post-form-request-name body)
                                           (post-form-request-count body)
                                           (post-form-request-subscribe
                                            body)))))))))
    (dispatch (make-http-request
               'POST "/form" '()
               '(("Content-Type" . "application/x-www-form-urlencoded"))
               "name=Ada&note=x&count=3&subscribe=true"))))
(format #t "~a~%" (http-response-body form-response-value))

;; A multipart body is parsed into the request record.
(define multipart-response-value
  (parameterize ((api-handlers
                  (list (cons 'post-multipart
                              (lambda (request arguments)
                                (let ((body (cdr (assq 'body arguments))))
                                  (text-response
                                   200
                                   (format #f "~a ~a"
                                           (post-multipart-request-title body)
                                           (post-multipart-request-file
                                            body)))))))))
    (dispatch (make-http-request
               'POST "/multipart" '()
               '(("Content-Type" . "multipart/form-data; boundary=B"))
               (multipart-body
                (list (multipart-part "title" "T")
                      (multipart-part "file" "data" "file.bin"))
                #:boundary "B")))))
(format #t "~a~%" (http-response-body multipart-response-value))

;; The content type the client sent decides whether the body is text or JSON.
(define (accept-response content-type body)
  (parameterize ((api-handlers
                  (list (cons 'accept-body
                              (lambda (request arguments)
                                (let ((value (cdr (assq 'body arguments))))
                                  (text-response
                                   200
                                   (if (string? value)
                                       value
                                       (form-result-message value)))))))))
    (http-response-body
     (dispatch (make-http-request 'POST "/accept" '()
                                  (list (cons "Content-Type" content-type))
                                  body)))))

(format #t "~a~%" (accept-response "text/plain" "hello"))
(format #t "~a~%" (accept-response "application/json"
                                  "{\"message\":\"json\"}"))
