;; A record serialises to jso and back; absent optional members are left out.
(define cat (make-cat #:kind "cat" #:name "Mia" #:indoor #f))
(define encoded (cat->jso cat))
(format #t "~a\n" (jso-ref encoded "name"))
(format #t "~a\n" (jso-has? encoded "nickname"))
(format #t "~a\n" (jso-has? encoded "indoor"))
(format #t "~a\n" (jso-ref encoded "indoor"))
(format #t "~a\n" (cat-name (jso->cat encoded)))
(format #t "~a\n" (jso-ref (cat->jso (jso->cat (cat->jso cat))) "name"))
