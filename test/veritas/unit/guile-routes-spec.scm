;; guile-openapi-generator

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; guile-openapi-generator is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; guile-openapi-generator is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with guile-openapi-generator.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Reading a plain Guile server's routes.  The route table in the fixture is the
;; one the generator writes, patterns and all, so the interesting assertion is
;; that a compiled pattern is undone correctly: the captures come back as
;; placeholders, an escaped dot comes back as a dot, and a pattern whose
;; captures disagree with its parameter list is refused rather than guessed at.

(define-module (veritas unit guile-routes-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator routes)
  #:use-module (openapi-generator guile-routes)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (fixture-routes)
  (guile-routes-from-file (fixture "routes/guile-app.scm")))

(define (find-route path)
  (find (lambda (route) (string=? (route-path route) path))
        (fixture-routes)))

;;; Suites

(define pattern-suite
  (suite "Undoing a path pattern"
         (test "A capture becomes a placeholder"
               (assert-equal #:expect "/pets/{petId}"
                             #:got (regexp->path "^/pets/([^/]+)/?$" '("petId"))))

         (test "A pattern without captures keeps its path"
               (assert-equal #:expect "/pets"
                             #:got (regexp->path "^/pets/?$" '()))
               (assert-equal #:expect "/"
                             #:got (regexp->path "^//?$" '())))

         (test "An escaped character is unescaped"
               (assert-equal #:expect "/v1.0/health"
                             #:got (regexp->path "^/v1\\.0/health/?$" '()))
               (assert-equal #:expect "/a+b"
                             #:got (regexp->path "^/a\\+b$" '())))

         (test "A capture on every segment is handled"
               (assert-equal #:expect "/pets/{petId}/photos/{photoId}"
                             #:got (regexp->path
                                    "^/pets/([^/]+)/photos/([^/]+)/?$"
                                    '("petId" "photoId"))))

         (test "A pattern that disagrees with its parameters is refused"
               (assert-error (lambda ()
                               (regexp->path "^/pets/([^/]+)/?$" '())))
               (assert-error (lambda ()
                               (regexp->path "^/pets/?$" '("petId")))))))

(define file-suite
  (suite "A route table"
         (test "Every route of the table is read, in order"
               (assert-equal #:expect
                             '("get /pets"
                               "post /pets"
                               "get /pets/{petId}"
                               "get /v1.0/health"
                               "delete /pets/{petId}"
                               "get /healthz"
                               "put /pets/{petId}")
                             #:got (map (lambda (route)
                                          (format #f "~a ~a"
                                                  (route-method route)
                                                  (route-path route)))
                                        (fixture-routes))))

         (test "The handler name becomes the operationId"
               (assert-equal #:expect "listPets"
                             #:got (route-operation-id (find-route "/pets")))
               (assert-equal #:expect "getPetById"
                             #:got (route-operation-id
                                    (find-route "/pets/{petId}"))))

         (test "A capture becomes a required path parameter"
               (let ((parameters (route-parameters (find-route "/pets/{petId}"))))
                 (assert-equal #:expect '("petId")
                               #:got (map route-parameter-name parameters))
                 (assert-eq #:expect 'path
                            #:got (route-parameter-location (car parameters)))
                 (assert-true (route-parameter-required? (car parameters)))))

         (test "A quoted parameter list is read, strings or symbols"
               (assert-equal
                #:expect '("petId")
                #:got (map route-parameter-name
                           (route-parameters
                            (car (guile-routes-from-string
                                  "(make-route 'GET (make-regexp \"^/p/([^/]+)$\") '(petId) 'one)")))))
               (assert-equal
                #:expect '("petId")
                #:got (map route-parameter-name
                           (route-parameters
                            (car (guile-routes-from-string
                                  "(make-route 'GET (make-regexp \"^/p/([^/]+)$\") '(\"petId\") 'one)"))))))

         (test "A path with no captures has no parameters"
               (assert-equal #:expect '()
                             #:got (route-parameters (find-route "/pets"))))

         (test "A declaration outside a table is read too"
               (assert-equal #:expect "getHealthz"
                             #:got (route-operation-id (find-route "/healthz"))))

         (test "A call that only looks like a route is ignored"
               (assert-equal #:expect '()
                             #:got (guile-routes-from-string
                                    "(define (lookup table) (get table 'key))
                                     (define (Fetch url) (fetch url))")))))

(define yaml-suite
  (suite "Specification"
         (test "The routes become a specification the reader accepts"
               (let ((parsed (parse-spec (yaml-string->scm
                                          (routes->yaml (fixture-routes)
                                                        #:title "Guile API"
                                                        #:version "0.1.0"))
                                         #:source "routes")))
                 (assert-equal #:expect "Guile API" #:got (spec-title parsed))
                 (assert-equal #:expect 7 #:got (length (spec-operations parsed)))
                 (assert-equal #:expect '("listPets" "postPets" "getPetById"
                                          "deletePetsByPetId" "putPetsByPetId"
                                          "getHealth" "getHealthz")
                               #:got (map operation-id (spec-operations parsed)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               pattern-suite
               file-suite
               yaml-suite))
