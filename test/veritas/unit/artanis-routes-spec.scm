;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Reading an Artanis application's routes.  Artanis says a route with a method
;; macro, a path full of @code{:name} segments and options between them, so the
;; suites check that each option lands where the specification needs it:
;; @code{#:consumes} in the request body, @code{#:produces} in the response,
;; @code{#:with-auth} as an extension rather than an invented security scheme.

(define-module (veritas unit artanis-routes-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator routes)
  #:use-module (openapi-generator artanis-routes)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (fixture-routes)
  (artanis-routes-from-file (fixture "routes/artanis-app.scm")))

(define (find-route method path)
  (find (lambda (route)
          (and (eq? (route-method route) method)
               (string=? (route-path route) path)))
        (fixture-routes)))

;;; Suites

(define reading-suite
  (suite "Reading declarations"
         (test "Every declaration is read, in order"
               (assert-equal #:expect
                             '("get /pets"
                               "get /pets/{petId}"
                               "post /pets"
                               "delete /pets/{petId}"
                               "get /pets/{petId}/photo.png"
                               "get /metrics")
                             #:got (map (lambda (route)
                                          (format #f "~a ~a"
                                                  (route-method route)
                                                  (route-path route)))
                                        (fixture-routes))))

         (test "A declaration inside a procedure body is found"
               (assert-equal #:expect "getMetrics"
                             #:got (route-operation-id
                                    (find-route 'get "/metrics"))))

         (test "A call that only looks like a route is ignored"
               (assert-equal #:expect '()
                             #:got (artanis-routes-from-string
                                    "(define (lookup table)
                                       (get table 'key))")))))

(define path-suite
  (suite "Paths"
         (test "An Artanis parameter becomes a placeholder"
               (assert-equal #:expect "/pets/{petId}"
                             #:got (route-path (find-route 'get "/pets/{petId}"))))

         (test "A placeholder becomes a required path parameter"
               (let ((parameters (route-parameters (find-route 'get "/pets/{petId}"))))
                 (assert-equal #:expect '("petId")
                               #:got (map route-parameter-name parameters))
                 (assert-eq #:expect 'path
                            #:got (route-parameter-location (car parameters)))
                 (assert-true (route-parameter-required? (car parameters)))))))

(define option-suite
  (suite "Options"
         (test "Consumes makes a request body of that media type"
               (let ((body (route-request-body (find-route 'post "/pets"))))
                 (assert-equal #:expect "application/json"
                               #:got (route-body-media-type body))
                 (assert-equal #:expect '()
                               #:got (route-body-schema body)
                               #:name "the shape of the body is not guessed")))

         (test "From body alone means JSON"
               (assert-equal
                #:expect "application/json"
                #:got (route-body-media-type
                       (route-request-body
                        (car (artanis-routes-from-string
                              "(post \"/pets\" #:from-body handler)"))))))

         (test "Consumes may be written as a list"
               (assert-equal
                #:expect "application/xml"
                #:got (route-body-media-type
                       (route-request-body
                        (car (artanis-routes-from-string
                              "(post \"/pets\" #:consumes '(\"application/xml\" \"application/json\"))"))))))

         (test "A route without options has no body"
               (assert-equal #:expect #f
                             #:got (route-request-body (find-route 'get "/pets"))))

         (test "Produces becomes the media type of the response"
               (let* ((route (find-route 'get "/pets/{petId}/photo.png"))
                      (response (car (route-responses route))))
                 (assert-equal #:expect "image/png"
                               #:got (route-response-media-type response))
                 (assert-equal #:expect '()
                               #:got (route-response-schema response))))

         (test "Produces with no schema still declares the media type"
               (let ((content (jso-ref-path
                               (routes->document (fixture-routes)
                                                 #:title "Artanis API")
                               "paths" "/pets/{petId}/photo.png" "get"
                               "responses" "200" "content")))
                 (assert-equal #:expect '("image/png")
                               #:got (jso-keys content))))

         (test "With auth is kept as an extension"
               (let ((extensions (route-extensions
                                  (find-route 'delete "/pets/{petId}"))))
                 (assert-eq #:expect #t
                            #:got (assoc-ref extensions "x-artanis-with-auth"))
                 (assert-equal #:expect #f
                               #:got (route-security
                                      (find-route 'delete "/pets/{petId}"))
                               #:name "no security scheme is invented")))))

(define yaml-suite
  (suite "Specification"
         (test "The routes become a specification the reader accepts"
               (let ((parsed (parse-spec (yaml-string->scm
                                          (routes->yaml (fixture-routes)
                                                        #:title "Artanis API"))
                                         #:source "routes")))
                 (assert-equal #:expect "Artanis API" #:got (spec-title parsed))
                 (assert-equal #:expect 6 #:got (length (spec-operations parsed)))
                 (assert-equal #:expect '("getPets" "postPets" "getPetsByPetId"
                                          "deletePetsByPetId"
                                          "getPetsByPetIdPhotoPng" "getMetrics")
                               #:got (map operation-id (spec-operations parsed)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               reading-suite
               path-suite
               option-suite
               yaml-suite))
