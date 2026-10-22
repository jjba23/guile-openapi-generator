;;; Call the Todo API with the generated client.  Start app.scm first.
;;;
;;;   guile -L gen -L ../../src client.scm

(use-modules (ice-9 format)
             (ice-9 receive)
             (openapi-generator runtime)
             (todo client)
             (todo models))

(define client (make-api-client))

(receive (todos response)
    (list-todos client)
  (format #t "~a todos before~%" (length todos)))

(receive (todo response)
    (create-todo client
                 #:body (make-new-todo #:title "Ship the app" #:done #f))
  (format #t "created #~a: ~a~%" (todo-id todo) (todo-title todo))
  (receive (fetched response)
      (get-todo client #:id (todo-id todo))
    (format #t "fetched #~a: ~a~%" (todo-id fetched) (todo-title fetched))
    (receive (deleted response)
        (delete-todo client #:id (todo-id fetched))
      (format #t "delete returned ~a~%"
              (http-response-status response)))))

(receive (todos response)
    (list-todos client)
  (format #t "~a todos after~%" (length todos)))
