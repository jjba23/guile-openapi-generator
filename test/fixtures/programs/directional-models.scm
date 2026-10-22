;; Uses the Request and Response model sets generated from the directional
;; fixture.  The request has no id, the response has no password.
(define request (make-account-request #:name "Ada" #:password "12345678"))
(define response (make-account-response #:id "a1" #:name "Ada" #:nickname "A"))
(format #t "~a~%" (account-request-name request))
(format #t "~a~%" (account-request-password request))
(format #t "~a~%" (account-response-id response))
(format #t "~a~%" (jso-has? (account-request->jso request) "id"))
(format #t "~a~%" (jso-has? (account-request->jso request) "password"))
(format #t "~a~%" (jso-has? (account-response->jso response) "password"))
(format #t "~a~%" (jso-has? (account-response->jso response) "nickname"))
