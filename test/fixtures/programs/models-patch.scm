;; RFC 7386: a member replaces, a null member removes.
(define cat (make-cat #:kind "cat" #:name "Mia" #:indoor #t))
(define patched (cat-apply-patch cat '(("name" . "Tom") ("indoor" . null))))
(format #t "~a\n" (cat-name patched))
(format #t "~a\n" (cat-indoor patched))
