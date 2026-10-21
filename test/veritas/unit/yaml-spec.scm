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
;; The YAML reader, exercised through the document in
;; test/fixtures/yaml-features.yaml and through small inline documents.  Each
;; test checks one construct, so a failure names the construct rather than "the
;; parser does not work".

(define-module (veritas unit yaml-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define features
  (yaml-file->scm (fixture "yaml-features.yaml")))

(define (parse text)
  (yaml-string->scm text))

;;; Suites

(define scalar-suite
  (suite "Scalars"
         (test "Numbers are numbers"
               (let ((numbers (jso-ref features "numbers")))
                 (assert-eqv #:expect 42 #:got (jso-ref numbers "integer"))
                 (assert-eqv #:expect -7 #:got (jso-ref numbers "negative"))
                 (assert-equal #:expect 3.5 #:got (jso-ref numbers "float"))
                 (assert-equal #:expect 1000.0 #:got (jso-ref numbers "exponent"))
                 (assert-eqv #:expect 31 #:got (jso-ref numbers "hex"))))

         (test "Booleans are booleans"
               (let ((booleans (jso-ref features "booleans")))
                 (assert-eq #:expect #t #:got (jso-ref booleans "yes_value"))
                 (assert-eq #:expect #f #:got (jso-ref booleans "no_value"))))

         (test "A key with no value is null"
               (assert-true (json-null? (jso-ref features "empty_value"))
                            #:name "empty_value is json-null"))

         (test "Version-like scalars stay strings"
               (assert-equal #:expect "3.0.3"
                             #:got (jso-ref (parse "openapi: 3.0.3\n") "openapi")
                             #:name "3.0.3 is not a number"))

         (test "Quoted scalars keep their content"
               (assert-equal #:expect "a: colon, \"quote\" and \n newline"
                             #:got (jso-ref features "quoted")
                             #:name "double quoted escapes")
               (assert-equal #:expect "it's fine"
                             #:got (jso-ref features "single")
                             #:name "a doubled quote is an escaped quote"))

         (test "A comment is not part of the value"
               (assert-equal #:expect '(("a" . 1) ("b" . "x#not-a-comment"))
                             #:got (parse "a: 1 # comment\nb: x#not-a-comment\n")
                             #:name "a # starts a comment only after a space"))))

(define block-suite
  (suite "Block scalars"
         (test "A literal block keeps its line breaks"
               (assert-equal #:expect "first line\nsecond line\n"
                             #:got (jso-ref features "literal")
                             #:name "the clip chomping adds one newline"))

         (test "A folded block joins its lines with spaces"
               (assert-equal #:expect "one two three\nfour"
                             #:got (jso-ref features "folded")
                             #:name "the strip chomping removes the trailing newline"))

         (test "The indentation of the block is removed"
               (assert-equal #:expect "a\n  b\n"
                             #:got (jso-ref (parse "value: |\n  a\n    b\n")
                                            "value")
                             #:name "relative indentation survives"))))

(define collection-suite
  (suite "Collections"
         (test "A flow sequence is a vector"
               (assert-equal #:expect (vector 1 2.5 #t 'null "text")
                             #:got (jso-ref (jso-ref features "flow") "list")
                             #:name "every element has the right type"))

         (test "A flow mapping nests"
               (assert-equal #:expect '(("a" . 1) ("b" . #(2 3)) ("c" ("d" . 4)))
                             #:got (jso-ref (jso-ref features "flow") "map")))

         (test "A sequence of mappings is read whole"
               (let ((sequence (jso-ref features "sequence")))
                 (assert-equal #:expect 2
                               #:got (vector-length sequence)
                               #:name "two items")
                 (assert-equal #:expect "second"
                               #:got (jso-ref (vector-ref sequence 1) "name"))
                 (assert-equal #:expect #(3 4)
                               #:got (jso-ref (vector-ref sequence 1) "values"))))

         (test "A sequence may sit at the indentation of its key"
               (assert-equal #:expect '(("k" . #("a" "b")))
                             #:got (parse "k:\n- a\n- b\n")
                             #:name "- may be at the same column as k"))

         (test "Deeply nested mappings are read"
               (assert-equal #:expect '(("a" ("b" ("c" . 1))))
                             #:got (parse "a:\n  b:\n    c: 1\n")))))

(define anchor-suite
  (suite "Anchors"
         (test "An alias repeats the anchored value"
               (let ((anchors (jso-ref features "anchors")))
                 (assert-equal #:expect (jso-ref anchors "base")
                               #:got (jso-ref anchors "copy")
                               #:name "copy equals base")))

         (test "A merge key overlays an anchored mapping"
               (let* ((anchors (jso-ref features "anchors"))
                      (merged (jso-ref anchors "merged")))
                 (assert-eq #:expect #t
                            #:got (jso-ref merged "shared")
                            #:name "the merged member is present")
                 (assert-eqv #:expect 3
                             #:got (jso-ref merged "count")
                             #:name "the explicit member wins")))

         (test "An unknown alias is an error"
               (assert-error (lambda () (parse "a: *nope\n"))
                             #:name "*nope has no anchor"))))

(define document-suite
  (suite "Documents"
         (test "Document markers and directives are ignored"
               (assert-equal #:expect '(("a" . 1))
                             #:got (parse "---\na: 1\n...\n")))

         (test "A tab cannot indent a line"
               (assert-error (lambda () (parse "a:\n\tb: 1\n"))
                             #:name "a tab raises instead of guessing"))

         (test "An empty document is null"
               (assert-true (json-null? (parse ""))
                            #:name "nothing is null"))

         (test "The petstore fixture reads"
               (let ((document (yaml-file->scm (fixture "petstore.yaml"))))
                 (assert-equal #:expect "3.0.3"
                               #:got (jso-ref document "openapi"))
                 (assert-true (jso-object? (jso-ref document "components"))
                              #:name "components is an object")))))

(define (spec)
  (veritas-run scalar-suite
               block-suite
               collection-suite
               anchor-suite
               document-suite))
