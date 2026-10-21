;; A value that breaks a constraint must raise, and the boolean default of a
;; required member must be accepted.
(format #t "~a\n" (car (catch 'openapi-validation-error
                          (lambda () (make-dog #:kind "dog" #:name ""))
                          (lambda (key path message) (list path message)))))
(format #t "~a\n" (dog-name (make-dog #:kind "dog" #:name "Rex" #:good-boy #f)))
