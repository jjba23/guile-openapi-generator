;; The generated client against a server that speaks several media types.
(define client (make-api-client #:base-url base-url))

(receive (text response)
    (get-plain client)
  (format #t "~a~%" text))

(receive (echoed response)
    (echo-plain client #:body "hello")
  (format #t "~a~%" echoed))

(receive (bytes response)
    (download client)
  (format #t "~a~%"
          (if (bytevector? bytes)
              (bytevector-length bytes)
              (string-length bytes))))

(receive (size response)
    (upload client #:body #vu8(0 1 2 255))
  (format #t "~a~%" size))

(receive (form response)
    (post-form client
               #:body (make-post-form-request #:name "Ada"
                                              #:note "x"
                                              #:count 3
                                              #:subscribe #t))
  (format #t "~a~%" (form-result-message form)))

(receive (multi response)
    (post-multipart client
                    #:body (make-post-multipart-request #:title "T"
                                                        #:file "data"))
  (format #t "~a~%" (form-result-message multi)))

(receive (headers-result response)
    (get-headers client)
  (let ((headers (get-headers-response-headers response)))
    (format #t "~a~%" (cdr (assoc "X-Rate-Limit" headers)))
    (format #t "~a~%" (cdr (assoc "X-Request-Id" headers)))))

;; A form response is parsed into its record before the model sees it.
(receive (form-response response)
    (get-form-response client)
  (format #t "~a~%" (form-result-message form-response)))

;; The server chooses text/plain even though JSON is the preferred type; the
;; client has to decode the representation it actually received.
(receive (representation response)
    (get-negotiated client #:representation "text")
  (format #t "~a~%" representation))

;; A declared success body that comes back empty is a contract violation.
(format #t "~a~%"
        (catch 'openapi-empty-body
          (lambda () (get-empty client) 'decoded)
          (lambda (key status body) status)))
