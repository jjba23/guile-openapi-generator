;; In the default single model set, a readOnly or writeOnly member is optional:
;; a request cannot satisfy a read-only member and a response cannot satisfy a
;; write-only one.
(define account (make-account #:name "Ada"))
(format #t "~a~%" (account-name account))
(format #t "~a~%" (account-valid? account))
