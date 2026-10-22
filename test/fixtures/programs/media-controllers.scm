;; The generated dispatcher decodes a text body and a form body by media type.
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

(define form-response-value
  (parameterize ((api-handlers
                  (list (cons 'post-form
                              (lambda (request arguments)
                                (ok (post-form-request-name
                                     (cdr (assq 'body arguments)))))))))
    (dispatch (make-http-request
               'POST "/form" '()
               '(("Content-Type" . "application/x-www-form-urlencoded"))
               "name=Ada&note=x"))))
(format #t "~a~%" (http-response-body form-response-value))
