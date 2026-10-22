;; Without fault-tolerant enums, an unknown value fails record validation.
(format #t "~a~%"
        (catch 'openapi-validation-error
          (lambda () (jso->pet '(("status" . "mystery"))) 'accepted)
          (lambda (key path message) path)))
