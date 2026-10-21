;; A discriminated union decodes into the record the discriminator names.
(define cat (jso->animal '(("kind" . "cat") ("name" . "Mia") ("indoor" . #t))))
(define dog (jso->animal '(("kind" . "dog") ("name" . "Rex"))))
(format #t "~a\n" (cat? cat))
(format #t "~a\n" (cat-name cat))
(format #t "~a\n" (dog? dog))
(format #t "~a\n" (animal? cat))
(format #t "~a\n" (animal? dog))
(format #t "~a\n" (cat-age cat))
