;; The inline oneOf has no discriminator of its own; it is inherited from the
;; allOf parent of its branches.
(define circle-drawing
  (jso->drawing '(("shape" . (("kind" . "circle") ("radius" . 2))))))
(format #t "~a~%" (circle? (drawing-shape circle-drawing)))
(format #t "~a~%" (circle-radius (drawing-shape circle-drawing)))
(define square-drawing
  (jso->drawing '(("shape" . (("kind" . "square") ("side" . 3))))))
(format #t "~a~%" (square? (drawing-shape square-drawing)))
(format #t "~a~%" (square-side (drawing-shape square-drawing)))
