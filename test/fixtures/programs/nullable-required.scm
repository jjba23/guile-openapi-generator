;; A required member that accepts null may hold #f, both when it is a nullable
;; scalar and when it is a union with a null branch.

(define null-pair (make-pair #:label "x" #:note #f #:maybe #f))

(format #t "~a~%" (jso-ref (pair->jso null-pair) "note"))
(format #t "~a~%" (jso-ref (pair->jso null-pair) "maybe"))

(define decoded (jso->pair (pair->jso null-pair)))
(format #t "~a~%" (pair-note decoded))
(format #t "~a~%" (pair-maybe decoded))

(define with-widget
  (make-pair #:label "x" #:note #f #:maybe (make-widget #:name "w")))
(format #t "~a~%" (widget-name (pair-maybe with-widget)))
