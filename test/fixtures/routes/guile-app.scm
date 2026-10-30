;; guile-openapi-generator

;; A plain Guile server's routes, in the two spellings
;; (openapi-generator guile-routes) understands.  Nothing here is loaded: the
;; tests read this file as source.  The first table is exactly what
;; (openapi-generator controllers) emits, patterns and all; the second is what a
;; hand-written server tends to say.

(define routes
  (list
    (make-route 'GET (make-regexp "^/pets/?$") '() 'list-pets)
    (make-route 'POST (make-regexp "^/pets/?$") '() 'post-pets)
    (make-route 'GET (make-regexp "^/pets/([^/]+)/?$") '("petId") 'get-pet-by-id)
    (make-route 'GET (make-regexp "^/v1\\.0/health/?$") '() 'get-health)
    (make-route 'DELETE (make-regexp "^/pets/([^/]+)/?$") '("petId")
                'delete-pets-by-pet-id)))

(define (router)
  (list (get "/healthz" (lambda (request) 'ok))
        (put "/pets/:petId" (lambda (request) 'ok))))
