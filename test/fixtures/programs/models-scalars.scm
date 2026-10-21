;; Named scalars, named lists and named maps validate through their predicates.
(format #t "~a\n" (email? "a@b.c"))
(format #t "~a\n" (email? "nope"))
(format #t "~a\n" (name-list? '("a" "b")))
(format #t "~a\n" (name-list? '(1 2)))
(format #t "~a\n" (name-list->jso '("a" "b")))
(format #t "~a\n" (string-map? '(("a" . "b"))))
(format #t "~a\n" (jso->string-map #f))
