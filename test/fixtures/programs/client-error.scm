;; A status the specification does not declare is an error, not a value.
;; BASE-URL is defined in front of this program by the test.
(define client (make-api-client
                #:base-url base-url
                #:auth (make-auth #:api-keys '(("api_key" . "secret")))))
(format #t "~a~%" (catch 'openapi-http-error
                    (lambda () (get-pet-by-id client #:pet-id "boom"))
                    (lambda (key status body) status)))
