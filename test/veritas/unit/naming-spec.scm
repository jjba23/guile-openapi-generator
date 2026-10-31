;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Naming: the rules that turn the way a specification spells things into the
;; way Guile code spells them.  The interesting cases are the ones that would
;; produce a broken program — a property called @code{else}, a schema whose name
;; starts with a digit — and the operation names, which must stay readable and
;; stable when a specification has no @code{operationId}.

(define-module (veritas unit naming-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (openapi-generator naming)
  #:export (spec))

(define words-suite
  (suite "Splitting words"
         (test "camelCase and PascalCase split"
               (assert-equal #:expect '("pet" "photo" "urls")
                             #:got (split-words "PetPhotoUrls"))
               (assert-equal #:expect '("list" "pets")
                             #:got (split-words "listPets")))

         (test "A run of capitals is one word"
               (assert-equal #:expect '("http" "response")
                             #:got (split-words "HTTPResponse")
                             #:name "the last capital starts the next word"))

         (test "Separators split too"
               (assert-equal #:expect '("pet" "id")
                             #:got (split-words "pet_id"))
               (assert-equal #:expect '("pet" "id")
                             #:got (split-words "pet-id"))
               (assert-equal #:expect '("pets" "pet" "id")
                             #:got (split-words "/pets/{petId}")))

         (test "Digits stay with their word"
               (assert-equal #:expect '("int64")
                             #:got (split-words "int64")))))

(define case-suite
  (suite "Case conversion"
         (test "kebab, snake and Pascal"
               (assert-equal #:expect "pet-photo-urls"
                             #:got (to-kebab "PetPhotoUrls"))
               (assert-equal #:expect "pet_photo_urls"
                             #:got (to-snake "PetPhotoUrls"))
               (assert-equal #:expect "PetPhotoUrls"
                             #:got (to-pascal "pet_photo_urls"))
               (assert-equal #:expect "petPhotoUrls"
                             #:got (to-camel "pet_photo_urls"))
               (assert-equal #:expect "PET_PHOTO_URLS"
                             #:got (to-constant "petPhotoUrls")))))

(define reserved-suite
  (suite "Safe identifiers"
         (test "A reserved word gains a suffix"
               (assert-equal #:expect "else-field"
                             #:got (sanitize-name "else"))
               (assert-equal #:expect "quote-field"
                             #:got (sanitize-name "quote"))
               (assert-equal #:expect "lambda-field"
                             #:got (sanitize-name "lambda")))

         (test "An identifier never starts with a digit"
               (assert-equal #:expect "n-3d-model"
                             #:got (sanitize-name "3DModel")))

         (test "An empty name becomes something bindable"
               (assert-equal #:expect "unnamed"
                             #:got (sanitize-name "")))

         (test "An ordinary name is only case-folded"
               (assert-equal #:expect "pet-photo-urls"
                             #:got (sanitize-name "PetPhotoUrls")))))

(define entity-suite
  (suite "Scheme entities"
         (test "Records, predicates, constructors and accessors"
               (assert-equal #:expect '<pet>
                             #:got (record-name "Pet"))
               (assert-equal #:expect 'pet?
                             #:got (predicate-name "Pet"))
               (assert-equal #:expect 'make-pet
                             #:got (constructor-name "Pet"))
               (assert-equal #:expect 'pet-photo-urls
                             #:got (accessor-name "Pet" "photoUrls")))

         (test "Enumeration constants carry the value"
               (assert-equal #:expect 'pet-status-in-progress
                             #:got (enum-constant-name "PetStatus" "in-progress")))

         (test "A module name is a list of symbols"
               (assert-equal #:expect '(petstore controllers artanis)
                             #:got (module-path "Petstore" "controllers" "artanis")))))

(define operation-suite
  (suite "Operations"
         (test "A path becomes a slug"
               (assert-equal #:expect "pets-by-pet-id-photos"
                             #:got (path-slug "/pets/{petId}/photos"))
               (assert-equal #:expect "root"
                             #:got (path-slug "/")))

         (test "The operationId wins when it exists"
               (assert-equal #:expect 'list-pets
                             #:got (operation-procedure-name 'get "/pets"
                                                            "listPets")))

         (test "Without an operationId, method and path decide"
               (assert-equal #:expect 'get-pets-by-pet-id
                             #:got (operation-procedure-name 'get
                                                            "/pets/{petId}"
                                                            #f))
               (assert-equal #:expect 'delete-pets-by-pet-id
                             #:got (operation-procedure-name 'delete
                                                            "/pets/{petId}"
                                                            #f)))))

(define (spec)
  (veritas-run words-suite
               case-suite
               reserved-suite
               entity-suite
               operation-suite))
