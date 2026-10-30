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
;; The YAML writer, checked against the reader.  The interesting cases are the
;; scalars that read back as something else when they are written plain — "200",
;; "yes", "Pet: the best" — and the structural ones, where the indentation has
;; to say which node a line belongs to.  A round trip through
;; @code{yaml-string->scm} is the strictest assertion available: it fails as soon
;; as the text means something other than the value it was written from.

(define-module (veritas unit yaml-writer-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-generator yaml-writer)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (round-trip value)
  "Return @var{value} after writing it as YAML and reading it back."
  (yaml-string->scm (jso->yaml-string value)))

(define (written value)
  "Return the YAML text of @var{value}."
  (jso->yaml-string value))

;;; Suites

(define scalar-suite
  (suite "Scalars"
         (test "A plain string stays plain"
               (assert-equal #:expect "name: Pet\n"
                             #:got (written '(("name" . "Pet")))))

         (test "The other scalars have their own spelling"
               (assert-equal #:expect "a: true\nb: false\nc: null\n"
                             #:got (written '(("a" . #t)
                                              ("b" . #f)
                                              ("c" . null))))
               (assert-equal #:expect "count: 42\nratio: 3.5\n"
                             #:got (written '(("count" . 42)
                                              ("ratio" . 3.5)))))

         (test "A version stays a string"
               (assert-equal #:expect "openapi: 3.0.3\n"
                             #:got (written '(("openapi" . "3.0.3")))
                             #:name "3.0.3 would become a number if quoted wrongly")
               (assert-equal #:expect "openapi: 3.0.3\n"
                             #:got (jso->yaml-string '(("openapi" . "3.0.3")))))

         (test "A string that reads as a number is quoted"
               (assert-equal #:expect "code: \"200\"\n"
                             #:got (written '(("code" . "200"))))
               (assert-equal #:expect "value: \"-7\"\n"
                             #:got (written '(("value" . "-7"))))
               (assert-equal #:expect "value: \"0x1f\"\n"
                             #:got (written '(("value" . "0x1f")))))

         (test "A string that reads as a boolean or null is quoted"
               (assert-equal #:expect "value: \"yes\"\n"
                             #:got (written '(("value" . "yes"))))
               (assert-equal #:expect "value: \"null\"\n"
                             #:got (written '(("value" . "null"))))
               (assert-equal #:expect "value: \"off\"\n"
                             #:got (written '(("value" . "off")))))

         (test "A colon or a hash that would end the scalar is quoted"
               (assert-equal #:expect "title: \"Pet: the best\"\n"
                             #:got (written '(("title" . "Pet: the best"))))
               (assert-equal #:expect "title: \"one #two\"\n"
                             #:got (written '(("title" . "one #two"))))
               (assert-equal #:expect "url: https://example.com/v1\n"
                             #:got (written '(("url" . "https://example.com/v1")))
                             #:name "a colon inside a URL does not need quotes"))

         (test "A leading indicator is quoted"
               (assert-equal #:expect "value: \"-x\"\n"
                             #:got (written '(("value" . "-x"))))
               (assert-equal #:expect "value: \"@home\"\n"
                             #:got (written '(("value" . "@home")))))

         (test "The empty string is quoted"
               (assert-equal #:expect "value: \"\"\n"
                             #:got (written '(("value" . "")))))

         (test "A key that needs quotes gets them"
               (assert-equal #:expect "\"a: b\": 1\n"
                             #:got (written '(("a: b" . 1)))))

         (test "A status code key stays plain"
               (assert-equal #:expect "200:\n  description: OK\n"
                             #:got (written '(("200" . (("description" . "OK")))))
                             #:name "a key is read as key text, so 200 cannot become a number"))))

(define collection-suite
  (suite "Collections"
         (test "A sequence is a block sequence"
               (assert-equal #:expect "tags:\n  - pets\n  - store\n"
                             #:got (written '(("tags" . #("pets" "store"))))))

         (test "A sequence of mappings puts the dash before the first member"
               (assert-equal
                #:expect
                (string-append "parameters:\n"
                               "  - name: petId\n"
                               "    in: path\n"
                               "  - name: limit\n"
                               "    in: query\n")
                #:got (written '(("parameters"
                                 . #((("name" . "petId") ("in" . "path"))
                                     (("name" . "limit") ("in" . "query"))))))))

         (test "A nested sequence is indented under its dash"
               (assert-equal #:expect "matrix:\n  -\n    - 1\n    - 2\n"
                             #:got (written '(("matrix" . #(#(1 2)))))))

         (test "The empty collections have their own spelling"
               (assert-equal #:expect "object: {}\narray: []\n"
                             #:got (written '(("object" . ())
                                              ("array" . #())))))

         (test "An empty document node is not a mapping"
               (assert-equal #:expect "{}\n" #:got (written '()))
               (assert-equal #:expect "[]\n" #:got (written (vector))))))

(define block-suite
  (suite "Multiline text"
         (test "Text with lines becomes a literal block"
               (assert-equal #:expect "description: |\n  first\n  second\n"
                             #:got (written '(("description" . "first\nsecond\n")))))

         (test "A text without a final newline strips the chomping"
               (assert-equal #:expect "description: |-\n  first\n  second\n"
                             #:got (written '(("description" . "first\nsecond")))))

         (test "Indentation inside the text survives"
               (assert-equal #:expect "first\n  indented\n"
                             #:got (round-trip "first\n  indented\n")))

         (test "Text that cannot be carried by a block is quoted"
               (assert-equal
                #:expect "description: \" leading space\\nsecond\"\n"
                #:got (written '(("description" . " leading space\nsecond")))
                #:name "a block would need an indentation indicator")
               (assert-equal
                #:expect "description: \"first\\nsecond\\n\\n\"\n"
                #:got (written '(("description" . "first\nsecond\n\n")))
                #:name "more than one trailing line break is spelled out"))))

(define round-trip-suite
  (suite "Round trip"
         (test "A whole document reads back unchanged"
               (let ((document
                      (list (cons "openapi" "3.0.3")
                            (cons "info" (list (cons "title" "Pets: a store")
                                               (cons "version" "1.0.0")))
                            (cons "flag" #t)
                            (cons "count" 42)
                            (cons "nothing" json-null)
                            (cons "empty-object" '())
                            (cons "empty-array" (vector))
                            (cons "description" "First line.\n  Second line.\n")
                            (cons "paths"
                                  (list (cons "/pets/{petId}"
                                              (list (cons "get"
                                                          (list (cons "tags"
                                                                      (vector "pets"))
                                                                (cons "parameters"
                                                                      (vector
                                                                       (list (cons "name" "petId")
                                                                             (cons "in" "path")
                                                                             (cons "required" #t)
                                                                             (cons "schema"
                                                                                   (list (cons "type" "string"))))))
                                                                (cons "responses"
                                                                      (list (cons "200"
                                                                                  (list (cons "description" "OK"))))))))))))))
                 (assert-equal #:expect (jso-normalize document)
                               #:got (round-trip document))))

         (test "Every ambiguous scalar reads back as a string"
               (let ((values (list "200" "-7" "0x1f" "yes" "no" "on" "off"
                                   "null" "~" "3.0.3" "1e3" "2020-01-01"
                                   "a: b" "one #two" "-x" "@home" "")))
                 (for-each (lambda (value)
                             (assert-equal #:expect value
                                           #:got (round-trip value)
                                           #:name (format #f "~s survives" value)))
                           values)))))

(define file-suite
  (suite "Files"
         (test "A document is written to a file and read back"
               (with-temporary-directory
                "yaml-writer"
                (lambda (directory)
                  (let ((path (string-append directory "/openapi.yaml"))
                        (document (list (cons "openapi" "3.0.3")
                                        (cons "paths" '()))))
                    (assert-equal #:expect path
                                  #:got (jso->yaml-file document path))
                    (assert-equal #:expect (jso-normalize document)
                                  #:got (yaml-file->scm path))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               scalar-suite
               collection-suite
               block-suite
               round-trip-suite
               file-suite))
