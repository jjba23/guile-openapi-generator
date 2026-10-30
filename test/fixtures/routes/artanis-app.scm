;; guile-openapi-generator

;; An Artanis application's routes.  Nothing here is loaded: the tests read
;; this file as source, so the handlers can be the usual Artanis shapes without
;; Artanis being on the load path.  Between them the declarations cover the
;; options (openapi-generator artanis-routes) understands, and a route inside a
;; procedure body, which is where an application that registers its routes in one
;; place tends to put them.

(get "/pets" (lambda (rc) (response-emit "[]" #:status 200)))

(get "/pets/:petId" (lambda (rc) (response-emit "{}" #:status 200)))

(post "/pets"
      #:from-body #:consumes 'application/json
      (lambda (rc) (response-emit "{}" #:status 201)))

(delete "/pets/:petId"
        #:with-auth (lambda (rc) #t)
        (lambda (rc) (response-emit "" #:status 204)))

(get "/pets/:petId/photo.png"
     #:produces 'image/png
     (lambda (rc) (response-emit "" #:status 200)))

(define (register-metrics)
  (get "/metrics" (lambda (rc) (response-emit "" #:status 200))))
