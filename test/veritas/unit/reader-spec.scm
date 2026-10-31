;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Reading a specification: format detection, the two parsers agreeing, and the
;; errors a user is most likely to hit — a Swagger 2.0 document, or something
;; that is not an OpenAPI document at all.

(define-module (veritas unit reader-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-support)
  #:export (spec))

(define format-suite
  (suite "Format detection"
         (test "A file name decides"
               (assert-eq #:expect 'json #:got (detect-format "api.json"))
               (assert-eq #:expect 'yaml #:got (detect-format "api.yaml"))
               (assert-eq #:expect 'yaml #:got (detect-format "api.yml")))

         (test "Content decides when the name does not"
               (assert-eq #:expect 'json
                          #:got (detect-format "{\"openapi\": \"3.0.0\"}"))
               (assert-eq #:expect 'yaml
                          #:got (detect-format "openapi: 3.0.0")))

         (test "An unknown extension falls back to YAML"
               (assert-eq #:expect 'yaml #:got (detect-format "api.txt")))))

(define parse-suite
  (suite "Parsing"
         (test "YAML and JSON versions of the same document agree"
               (let ((yaml (read-spec-file (fixture "mini.yaml")))
                     (json (read-spec-file (fixture "mini.json"))))
                 (assert-equal #:expect (jso-ref yaml "openapi")
                               #:got (jso-ref json "openapi"))
                 (assert-equal #:expect "object"
                               #:got (jso-ref-path json "components" "schemas"
                                                   "Thing" "type"))
                 (assert-equal #:expect
                               (jso-ref-path yaml "components" "schemas"
                                             "Thing" "required")
                               #:got (jso-ref-path json "components" "schemas"
                                                   "Thing" "required"))
                 (assert-equal #:expect #t
                               #:got (jso-true?
                                      (jso-ref-path json
                                                    "components" "schemas"
                                                    "Thing" "properties"
                                                    "notes" "nullable")))))

         (test "A string is read the same way"
               (let ((document (read-spec-string "openapi: 3.0.3\ninfo: {title: X}\n")))
                 (assert-equal #:expect "3.0.3"
                               #:got (openapi-version document))))

         (test "read-spec accepts a path and a document"
               (assert-equal #:expect "Mini"
                             #:got (document-title
                                    (read-spec (fixture "mini.yaml"))))
               (assert-equal #:expect "Mini"
                             #:got (document-title
                                    (read-spec "{\"openapi\": \"3.0.0\", \"info\": {\"title\": \"Mini\"}}"))))))

(define validation-suite
  (suite "Validation"
         (test "A Swagger 2.0 document is refused with its version"
               (assert-error (lambda ()
                               (read-spec-file (fixture "swagger2.yaml")))
                             #:name "Swagger is not OpenAPI 3"))

         (test "A document without an openapi member is refused"
               (assert-error (lambda ()
                               (read-spec-string "{\"info\": {\"title\": \"X\"}}"))))

         (test "An unknown major version is refused"
               (assert-error (lambda ()
                               (read-spec-string "openapi: 4.0.0\n"))))

         (test "3.x is accepted"
               (assert-true (supported-version? "3.0.3"))
               (assert-true (supported-version? "3.1.0"))
               (assert-false (supported-version? "2.0"))
               (assert-false (supported-version? #f)))))

(define document-suite
  (suite "Documents and fragments"
         (test "A locator is a file or URL, not document text"
               (assert-true (locator? (fixture "mini.yaml")))
               (assert-false (locator? "openapi: 3.0.0\ninfo: {title: X}\n")))

         (test "A format is read from a URL path, not its query"
               (assert-eq #:expect 'json
                          #:got (format-for-locator
                                 "https://example.com/api.json?token=1"))
               (assert-eq #:expect 'yaml
                          #:got (format-for-locator
                                 "https://example.com/api.yaml#fragment")))

         (test "A document is parsed without being validated as OpenAPI"
               (assert-equal #:expect "Old"
                             #:got (document-title
                                    (read-document (fixture "swagger2.yaml")))))

         (test "Fragments merge objects and replace values"
               (assert-equal
                #:expect '(("a" . 1) ("b" . 2) ("c" . 3))
                #:got (merge-documents '(("a" . 1) ("b" . 2))
                                       '(("c" . 3))))
               (assert-equal
                #:expect '(("a" ("x" . 1) ("y" . 2)))
                #:got (merge-documents '(("a" ("x" . 1)))
                                       '(("a" ("y" . 2))))
                #:name "objects merge recursively")
               (assert-equal
                #:expect '(("a" . 3))
                #:got (merge-documents '(("a" . 1)) '(("a" . 3)))
                #:name "a scalar is replaced"))))

(define (spec)
  (veritas-run format-suite parse-suite validation-suite document-suite))
