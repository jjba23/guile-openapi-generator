;; Exercises the tree generated from the whole library fixture: records with
;; arrays that may and may not repeat, an inexact multipleOf, a bare object, a
;; record that keeps undeclared members, and the full route table.
(define uuid "3f2504e0-4f89-11d3-9a0c-0305e82c3301")

;; A declared member and an extension point coexist, side by side.
(define annotations-value
  (make-annotations #:source "web"
                    #:extra '(("lang" . "en") ("note" . "x"))))
(format #t "~a~%" (annotations-source annotations-value))
(format #t "~a~%" (annotations-extra annotations-value))
(format #t "~a~%" (annotations->jso annotations-value))
(define annotations-copy (jso->annotations (annotations->jso annotations-value)))
(format #t "~a~%" (annotations-source annotations-copy))
(format #t "~a~%" (annotations-extra annotations-copy))

;; Duplicate tags are allowed; duplicate ratings are not.
(define book
  (make-book #:id uuid
             #:title "Structure and Interpretation"
             #:price 12.34
             #:tags '("scheme" "scheme")
             #:ratings '(4 5)
             #:metadata '(("width" . 10) ("unit" . "cm"))
             #:annotations annotations-value
             #:status "published"))
(format #t "~a~%" (book-tags book))
(format #t "~a~%" (book-metadata book))
(format #t "~a~%" (book-price book))

(format #t "~a~%"
        (catch 'openapi-validation-error
          (lambda ()
            (make-book #:id uuid #:title "x" #:price 1 #:ratings '(4 4))
            'accepted)
          (lambda (key path message) path)))

(format #t "~a~%"
        (catch 'openapi-validation-error
          (lambda ()
            (make-book #:id uuid #:title "x" #:price 12.345)
            'accepted)
          (lambda (key path message) message)))

;; A map honours the size the specification declares.
(format #t "~a~%"
        (catch 'openapi-validation-error
          (lambda ()
            (make-book #:id uuid #:title "x" #:price 1 #:counters '())
            'accepted)
          (lambda (key path message) path)))
(format #t "~a~%"
        (book-counters
         (make-book #:id uuid #:title "x" #:price 1
                    #:counters '(("a" . 1) ("b" . 2)))))
(format #t "~a~%"
        (catch 'openapi-validation-error
          (lambda ()
            (make-book #:id uuid #:title "x" #:price 1
                       #:counters '(("a" . 1) ("b" . 2) ("c" . 3)))
            'accepted)
          (lambda (key path message) path)))

;; A bare object is a map, so nothing it carries is lost.
(define free (jso->free-form '(("a" . 1) ("b" . "two"))))
(format #t "~a~%" (free-form->jso free))
(format #t "~a~%" (labels->jso (jso->labels '(("x" . "y")))))

;; additionalProperties: true keeps members of any type.
(define draft (make-draft #:title "rough"
                          #:extra '(("pages" . 3) ("notes" . "later"))))
(format #t "~a~%" (draft->jso draft))
(define draft-copy (jso->draft (draft->jso draft)))
(format #t "~a~%" (draft-title draft-copy))
(format #t "~a~%" (draft-extra draft-copy))

;; The whole record round-trips, extension point included.
(define round-trip (jso->book (book->jso book)))
(format #t "~a~%" (book-title round-trip))
(format #t "~a~%" (book-metadata round-trip))
(format #t "~a~%" (annotations-source (book-annotations round-trip)))
(format #t "~a~%" (annotations-extra (book-annotations round-trip)))

;; The generated dispatcher matches the whole route table.
(define list-response
  (parameterize ((api-handlers
                  (list (cons 'list-books
                              (lambda (request arguments)
                                (ok arguments))))))
    (dispatch (make-http-request 'GET "/books" '(("limit" . "5")) '() #f))))
(format #t "~a~%" (http-response-status list-response))
(format #t "~a~%" (http-response-body list-response))

(define path-response
  (parameterize ((api-handlers
                  (list (cons 'get-book
                              (lambda (request arguments)
                                (ok arguments))))))
    (dispatch (make-http-request 'GET
                                 (string-append "/books/" uuid)
                                 '() '() #f))))
(format #t "~a~%" (http-response-body path-response))

(define create-response
  (parameterize ((api-handlers
                  (list (cons 'create-book
                              (lambda (request arguments)
                                (ok (new-book-title
                                     (cdr (assq 'body arguments)))))))))
    (dispatch (make-http-request
               'POST "/books" '() '()
               "{\"title\": \"Compilers\", \"price\": 9.99}"))))
(format #t "~a~%" (http-response-body create-response))

;; A search still matches, even though its success is declared on a family.
(define search-response
  (parameterize ((api-handlers
                  (list (cons 'search-books
                              (lambda (request arguments)
                                (ok arguments))))))
    (dispatch (make-http-request 'GET "/search" '(("q" . "sicp")) '() #f))))
(format #t "~a~%" (http-response-status search-response))
