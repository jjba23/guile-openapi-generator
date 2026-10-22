;; Calls the generated client, from the whole library fixture, against a server
;; started by the test.  The URL is injected as the definition `base-url'.
(define client
  (make-api-client
   #:base-url base-url
   #:auth (make-auth #:api-keys '(("api_key" . "secret")))))

(receive (books response)
    (list-books client #:limit 5)
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (map book-title books)))

(receive (found response)
    (get-book client #:book-id "3f2504e0-4f89-11d3-9a0c-0305e82c3301")
  (format #t "~a~%" (book-title found)))

;; A 299 is still inside the 2XX family the specification declares.
(receive (matches response)
    (search-books client #:q "sicp")
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (map book-title matches)))

(receive (created response)
    (create-book client
                 #:body (make-new-book #:title "Compilers" #:price 9.99))
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (book-title created)))

;; A status outside the family is an error, not something decoded as a success.
(format #t "~a~%"
        (catch 'openapi-http-error
          (lambda ()
            (receive (matches response)
                (search-books client #:q "boom")
              'decoded))
          (lambda (key status body) status)))
