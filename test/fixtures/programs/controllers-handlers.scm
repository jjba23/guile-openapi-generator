;; The dispatcher hands a handler the decoded parameters.
(define request (make-http-request 'GET "/pets" '(("limit" . "10")) '() #f))
(parameterize ((api-handlers (list (cons 'list-pets
                                         (lambda (request arguments)
                                           (ok arguments))))))
  (define response (dispatch request))
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (http-response-body response)))

(define path-request (make-http-request 'GET "/pets/42" '() '() #f))
(parameterize ((api-handlers (list (cons 'get-pet-by-id
                                         (lambda (request arguments)
                                           (ok arguments))))))
  (format #t "~a~%" (http-response-body (dispatch path-request))))

(define body-request (make-http-request 'POST "/pets" '() '() "{\"name\": \"Bella\"}"))
(parameterize ((api-handlers (list (cons 'post-pets
                                         (lambda (request arguments)
                                           (ok (new-pet-name (cdr (assq 'body arguments)))))))))
  (format #t "~a~%" (http-response-body (dispatch body-request))))

(format #t "~a~%" (catch 'openapi-not-implemented
                    (lambda () (dispatch (make-http-request 'GET "/pets" '() '() #f)))
                    (lambda (key operation request) operation)))
(format #t "~a~%" (catch 'openapi-no-route
                    (lambda () (dispatch (make-http-request 'GET "/nope" '() '() #f)))
                    (lambda (key method path) (list method path))))
