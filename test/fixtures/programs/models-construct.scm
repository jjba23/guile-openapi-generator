;; Construct a Cat and read it back through the generated accessors.
(define cat (make-cat #:kind "cat" #:name "Mia" #:indoor #t #:lives 9))
(format #t "~a\n" (cat-kind cat))
(format #t "~a\n" (cat-name cat))
(format #t "~a\n" (cat-indoor cat))
(format #t "~a\n" (cat-lives cat))
(format #t "~a\n" (cat? cat))
(format #t "~a\n" (validate-cat cat))
