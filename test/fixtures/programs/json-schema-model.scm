;; Uses the models generated from the JSON Schema fixture.
(define address (make-address #:street "Main 1" #:zip "12345"))
(define person (make-person #:name "Ada" #:age 36 #:address address))
(format #t "~a~%" (person-name person))
(format #t "~a~%" (address-street (person-address person)))
(format #t "~a~%" (person-valid? person))
(format #t "~a~%" (jso-ref (person->jso person) "age"))
