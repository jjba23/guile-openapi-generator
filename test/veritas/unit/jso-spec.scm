;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The value representation every other part of the project relies on: string
;; keys, vectors for arrays, and a null that is a value rather than an absence.
;; The merge patch suite is the reason the distinction exists, so it is tested
;; from both directions: a null deletes, an absent member is left alone.

(define-module (veritas unit jso-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator json)
  #:export (spec))

(define object
  '(("a" . 1) ("b" . 2) ("c" . (("d" . 4)))))

(define patch-suite
  (suite "JSON merge patch"
         (test "A member replaces the target member"
               (assert-equal #:expect '(("a" . 1) ("b" . 3))
                             #:got (json-merge-patch '(("a" . 1) ("b" . 2))
                                                     '(("b" . 3)))
                             #:name "b is replaced, a is kept"))

         (test "A null member deletes the target member"
               (assert-equal #:expect '(("a" . 1))
                             #:got (json-merge-patch '(("a" . 1) ("b" . 2))
                                                     '(("b" . null)))
                             #:name "b is removed"))

         (test "A member that is absent from the patch is untouched"
               (assert-equal #:expect '(("a" . 9) ("b" . 2) ("c" ("d" . 4)))
                             #:got (json-merge-patch object '(("a" . 9)))
                             #:name "b and c survive, only a changes"))

         (test "Objects merge recursively"
               (assert-equal #:expect '(("c" . (("d" . 5) ("e" . 6))))
                             #:got (json-merge-patch '(("c" . (("d" . 4))))
                                                     '(("c" . (("d" . 5)
                                                               ("e" . 6)))))
                             #:name "nested members merge, they do not replace"))

         (test "A nested null deletes a nested member"
               (assert-equal #:expect '(("a" . 1) ("b" . 2) ("c"))
                             #:got (json-merge-patch object
                                                     '(("c" . (("d" . null)))))
                             #:name "d is removed from the nested object"))

         (test "A patch that is not an object replaces the target"
               (assert-equal #:expect #(1 2)
                             #:got (json-merge-patch object #(1 2))
                             #:name "RFC 7386: anything else replaces"))))

(define object-suite
  (suite "Objects and arrays"
         (test "A parsed object has string keys"
               (assert-equal #:expect '(("name" . "Pet"))
                             #:got (json-string->jso "{\"name\": \"Pet\"}")))

         (test "An empty object is the empty list"
               (assert-equal #:expect '()
                             #:got (json-string->jso "{}")
                             #:name "nothing is in it"))

         (test "null is a value, not an absence"
               (let ((parsed (json-string->jso "{\"a\": null}")))
                 (assert-true (jso-has? parsed "a")
                              #:name "the member is present")
                 (assert-true (json-null? (jso-ref parsed "a"))
                              #:name "and it holds null")
                 (assert-equal #:expect 'nothing
                               #:got (jso-ref parsed "missing" 'nothing)
                               #:name "an absent member falls back to the default")))

         (test "Arrays are vectors"
               (assert-true (jso-array? (json-string->jso "[1, 2]"))
                            #:name "a JSON array is a vector"))))

(define accessor-suite
  (suite "Accessors"
         (test "Keys and values keep the document order"
               (assert-equal #:expect '("a" "b" "c")
                             #:got (jso-keys object))
               (assert-equal #:expect '(1 2 (("d" . 4)))
                             #:got (jso-values object)))

         (test "jso-ref-path walks nested objects"
               (assert-eqv #:expect 4
                           #:got (jso-ref-path object "c" "d"))
               (assert-false (jso-ref-path object "c" "missing")
                             #:name "a missing step yields #f"))

         (test "jso-put keeps the position of an existing member"
               (assert-equal #:expect '(("a" . 9) ("b" . 2))
                             #:got (jso-put '(("a" . 1) ("b" . 2)) "a" 9)))

         (test "jso-delete removes one member"
               (assert-equal #:expect '(("a" . 1) ("c" 4))
                             #:got (jso-delete '(("a" . 1) ("b" 2) ("c" 4))
                                               "b")))

         (test "jso-append lets later entries win"
               (assert-equal #:expect '(("a" . 2) ("b" . 1))
                             #:got (jso-append '(("a" . 1)) '(("b" . 1)
                                                               ("a" . 2)))))

         (test "A deep copy shares no pairs"
               (let* ((copy (jso-deep-copy object))
                      (original-nested (jso-ref object "c")))
                 (assert-equal #:expect object #:got copy)
                 (assert-false (eq? original-nested (jso-ref copy "c"))
                               #:name "the nested object is a copy")))))

(define normalise-suite
  (suite "Normalisation"
         (test "Symbol keys become strings"
               (assert-equal #:expect '(("name" . "Pet"))
                             #:got (jso-normalize '((name . "Pet")))))

         (test "Normalisation descends into arrays"
               (assert-equal #:expect #((("name" . "Pet")))
                             #:got (jso-normalize (vector '((name . "Pet"))))))

         (test "A jso object round-trips through JSON"
               (let ((round-tripped (json-string->jso (jso->json-string object))))
                 (assert-equal #:expect 1 #:got (jso-ref round-tripped "a"))
                 (assert-equal #:expect 4
                               #:got (jso-ref-path round-tripped "c" "d"))
                 (assert-equal #:expect '("a" "b" "c")
                               #:got (sort (jso-keys round-tripped)
                                           string<?))))))

(define (spec)
  (veritas-run patch-suite object-suite accessor-suite normalise-suite))
