;; A server sends values the specification did not know about.  With
;; fault-tolerant enums they decode to `unrecognized` and the record still
;; builds.
(define pet (jso->pet '(("status" . "mystery") ("mood" . "curious"))))
(format #t "~a~%" (pet-status pet))
(format #t "~a~%" (status? (pet-status pet)))
(format #t "~a~%" (pet-mood pet))
(format #t "~a~%" (mood? (pet-mood pet)))
