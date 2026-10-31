;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The schema IR: what a schema means.  Composition is flattened, unions keep
;; their discriminator, and inline objects are given names, because a record
;; without a name is not something generated code could refer to.

(define-module (veritas unit schema-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator schema)
  #:use-module (openapi-support)
  #:export (spec))

(define polymorphic (polymorphism-spec))
(define document (spec-raw polymorphic))
(define registry (registry-for-spec polymorphic))

(define (schema-by-name name)
  (let ((entry (assoc name (registry-named-schemas registry))))
    (and entry (cdr entry))))

(define (shape-by-name name)
  (schema-shape document (schema-by-name name) #:registry registry #:named? #f))

(define registry-suite
  (suite "Registry"
         (test "Component schemas keep their names"
               (assert-equal #:expect "Animal"
                             #:got (registry-name registry
                                                  (schema-by-name "Animal"))))

         (test "A reference becomes a record shape"
               (let ((shape (schema-shape document
                                          '(("$ref" . "#/components/schemas/Owner"))
                                          #:registry registry)))
                 (assert-true (record-shape? shape) #:name "it is a record reference")
                 (assert-equal #:expect "Owner" #:got (shape-name shape))))

         (test "An array of references is a list of records"
               (let* ((media-type (car (response-media-types
                                        (operation-response-for-status
                                         (car (spec-operations polymorphic))
                                         "200"))))
                      (shape (shape-of-media-type document media-type
                                                  #:registry registry)))
                 (assert-true (list-shape? shape) #:name "it is a list")
                 (assert-true (record-shape? (shape-items shape))
                              #:name "of records")
                 (assert-equal #:expect "Animal"
                               #:got (shape-name (shape-items shape)))))

         (test "Uses of a reference are not given a second name"
               (assert-false (assoc "list-animals-request"
                                    (registry-named-schemas registry))
                             #:name "a $ref body is not a new record"))))

(define composition-suite
  (suite "Composition"
         (test "allOf is flattened into one object"
               (let ((cat (shape-by-name "Cat")))
                 (assert-true (object-shape? cat))
                 (assert-equal #:expect '("kind" "name" "born" "age" "weight"
                                          "nickname" "tags" "owner" "extra"
                                          "indoor" "lives")
                               #:got (map property-name
                                          (shape-properties cat)))
                 (assert-true (property-required?
                               (find (lambda (property)
                                       (string=? (property-name property)
                                                 "indoor"))
                                     (shape-properties cat)))
                              #:name "the child's required member is required")))

         (test "The parent's required members stay required"
               (let ((cat (shape-by-name "Cat")))
                 (assert-true (property-required?
                               (find (lambda (property)
                                       (string=? (property-name property) "kind"))
                                     (shape-properties cat))))))

         (test "oneOf keeps its branches and discriminator"
               (let ((animal (shape-by-name "Animal")))
                 (assert-true (union-shape? animal))
                 (assert-equal #:expect "kind"
                               #:got (shape-discriminator animal))
                 (assert-equal #:expect 3
                               #:got (length (shape-branches animal)))
                 (assert-equal #:expect '(("cat" . "Cat") ("dog" . "Dog")
                                          ("fish" . "Fish"))
                               #:got (shape-discriminator-values animal))))))

(define shape-suite
  (suite "Shapes"
         (test "A nullable member is nullable"
               (let ((nickname (find (lambda (property)
                                       (string=? (property-name property)
                                                 "nickname"))
                                     (shape-properties (shape-by-name "Pet")))))
                 (assert-true (shape-nullable? (property-shape nickname)))))

         (test "A string with a format is a scalar with that format"
               (let ((born (find (lambda (property)
                                   (string=? (property-name property) "born"))
                                 (shape-properties (shape-by-name "Pet")))))
                 (assert-true (scalar-shape? (property-shape born)))
                 (assert-equal #:expect "date-time"
                               #:got (shape-format (property-shape born)))))

         (test "An enumeration is an enumeration"
               (let ((water (find (lambda (property)
                                    (string=? (property-name property) "water"))
                                  (shape-properties (shape-by-name "Fish")))))
                 (assert-true (enum-shape? (property-shape water)))
                 (assert-equal #:expect '("fresh" "salt")
                               #:got (shape-values (property-shape water)))))

         (test "A const is a one-value enumeration"
               (let ((shape (schema-shape
                             document
                             '(("type" . "string") ("const" . "accepted"))
                             #:named? #f)))
                 (assert-true (enum-shape? shape))
                 (assert-equal #:expect '("accepted")
                               #:got (shape-values shape))
                 (assert-equal #:expect "string"
                               #:got (symbol->string (shape-type shape)))))

         (test "An object of only additionalProperties is a map"
               (let ((shape (shape-by-name "StringMap")))
                 (assert-true (map-shape? shape))
                 (assert-true (scalar-shape? (shape-items shape)))))

         (test "A component array is a list of scalars"
               (let ((shape (shape-by-name "NameList")))
                 (assert-true (list-shape? shape))
                 (assert-equal #:expect 'string
                               #:got (shape-type (shape-items shape)))))

         (test "A named scalar keeps its constraints"
               (let ((shape (shape-by-name "Email")))
                 (assert-true (scalar-shape? shape))
                 (assert-equal #:expect "email" #:got (shape-format shape))
                 (assert-equal #:expect "^[^@]+@[^@]+\\.[^@]+$"
                               #:got (schema-pattern document
                                                     (schema-by-name "Email")))))))

(define constraint-suite
  (suite "Constraints"
         (test "String constraints are read"
               (let ((name (find (lambda (property)
                                   (string=? (property-name property) "name"))
                                 (shape-properties (shape-by-name "Pet")))))
                 (assert-eqv #:expect 1
                             #:got (schema-min-length document
                                                      (property-schema name)))
                 (assert-eqv #:expect 40
                             #:got (schema-max-length document
                                                      (property-schema name)))))

         (test "Numeric constraints are read"
               (let ((age (find (lambda (property)
                                  (string=? (property-name property) "age"))
                                (shape-properties (shape-by-name "Pet")))))
                 (assert-eqv #:expect 0
                             #:got (schema-minimum document
                                                   (property-schema age)))
                 (assert-eqv #:expect 200
                             #:got (schema-exclusive-maximum
                                    document (property-schema age)))))

         (test "Array constraints are read"
               (let ((tags (find (lambda (property)
                                   (string=? (property-name property) "tags"))
                                 (shape-properties (shape-by-name "Pet")))))
                 (assert-eqv #:expect 1
                             #:got (schema-min-items document
                                                     (property-schema tags)))
                 (assert-true (schema-unique-items? document
                                                    (property-schema tags)))))))

(define (spec)
  (veritas-run registry-suite composition-suite shape-suite constraint-suite))
