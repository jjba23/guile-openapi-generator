;;; A complete Todo API built from openapi.yaml.
;;;
;;; Generate the tree first (see README.org), then run:
;;;
;;;   guile -L gen -L ../../src app.scm 8080
;;;
;;; The generated controllers export guile-handler, so this file is the whole
;;; server: an in-memory store, one handler per operation, and run-server.

(use-modules (ice-9 format)
             (srfi srfi-1)
             (web server)
             (openapi-generator runtime)
             (todo controllers)
             (todo models))

;;; An in-memory store.  A real app would replace this with a database.

(define todos
  (list (make-todo #:id 1 #:title "Read the generated README" #:done #t)
        (make-todo #:id 2 #:title "Write a handler" #:done #f)))

(define next-id 3)

(define (find-todo id)
  (find (lambda (todo) (= (todo-id todo) id)) todos))

;;; One handler per operation.  The argument alist holds the decoded query,
;;; path and body values; the return value is an <http-response>.

(define (list-todos-handler request arguments)
  (ok (list->vector (map todo->jso todos))))

(define (create-todo-handler request arguments)
  (let* ((new (cdr (assq 'body arguments)))
         (todo (make-todo #:id next-id
                          #:title (new-todo-title new)
                          #:done (or (new-todo-done new) #f))))
    (set! next-id (+ next-id 1))
    (set! todos (append todos (list todo)))
    (created (todo->jso todo))))

(define (get-todo-handler request arguments)
  (let ((todo (find-todo (cdr (assq 'id arguments)))))
    (if todo
        (ok (todo->jso todo))
        (not-found "No such todo"))))

(define (delete-todo-handler request arguments)
  (let ((id (cdr (assq 'id arguments))))
    (set! todos (remove (lambda (todo) (= (todo-id todo) id)) todos))
    (no-content)))

(define handlers
  (list (cons 'list-todos list-todos-handler)
        (cons 'create-todo create-todo-handler)
        (cons 'get-todo get-todo-handler)
        (cons 'delete-todo delete-todo-handler)))

;;; Serve it.  guile-handler converts between (web server) and the runtime's
;;; request and response records, including JSON serialisation.

(define (main args)
  (let ((port (if (pair? (cdr args))
                  (string->number (cadr args))
                  8080)))
    (format #t "Todo API on http://127.0.0.1:~a~%" port)
    (force-output)
    (parameterize ((api-handlers handlers))
      (run-server (guile-handler) 'http
                  (list #:host "127.0.0.1" #:port port)))))

(main (program-arguments))
