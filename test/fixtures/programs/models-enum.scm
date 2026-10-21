;; An enumeration is values, constants and a predicate.
(format #t "~a\n" fish-water-fresh)
(format #t "~a\n" (fish-water? "fresh"))
(format #t "~a\n" (car (catch 'openapi-validation-error
                          (lambda () (make-fish #:kind "fish" #:name "Nemo" #:water "soup"))
                          (lambda (key path message) (list path message)))))
