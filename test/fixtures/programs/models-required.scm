;; A required member that is missing must name the field.
(format #t "~a\n" (catch 'openapi-validation-error
                    (lambda () (make-cat #:kind "cat" #:name #f #:indoor #t))
                    (lambda (key path message) (list path message))))
