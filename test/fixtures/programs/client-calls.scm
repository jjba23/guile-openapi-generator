;; The generated client builds the URL, sends the credentials and decodes JSON.
(define base-url (getenv "BASE_URL"))
(define client (make-api-client
                #:base-url base-url
                #:auth (make-auth #:api-keys '(("api_key" . "secret")))))

(receive (pets response)
    (list-pets client #:limit 10)
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (map pet-name pets)))

(receive (pet response)
    (get-pet-by-id client #:pet-id 7)
  (format #t "~a~%" (pet-name pet)))

(receive (created response)
    (post-pets client #:body (make-new-pet #:name "Cadet"))
  (format #t "~a~%" (http-response-status response))
  (format #t "~a~%" (pet-name created)))

(receive (body response)
    (delete-pets-by-pet-id client #:pet-id 7)
  (format #t "~a~%" (http-response-status response)))
