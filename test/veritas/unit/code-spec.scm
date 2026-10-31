;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The code writer.  Everything else in the generator hands it forms, so these
;; are the tests that decide what generated code looks like: a short form stays on
;; one line, a body is indented under the form that opens it, a keyword argument
;; is never split from its value, and data is stacked under its quote rather than
;; aligned like a call.

(define-module (veritas unit code-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator code)
  #:export (spec))

(define (lines text)
  "Return @var{text} as a list of lines, without the empty one at the end."
  (drop-right (string-split text #\newline) 1))

(define one-line-suite
  (suite "One line"
         (test "A form that fits stays on one line"
               (assert-equal #:expect "(list 1 2 3)\n"
                             #:got (code->string `(list 1 2 3))))

         (test "A call that fits keeps its arguments together"
               (assert-equal #:expect "(list 1 2 3)\n"
                             #:got (code->string `(list 1 2 3))))

         (test "An empty list is the empty list"
               (assert-equal #:expect "(list)\n"
                             #:got (code->string `(list))))

         (test "Strings are literals, quotes and all"
               (assert-equal #:expect "(display \"a \\\"quoted\\\" word\")\n"
                             #:got (code->string
                                    `(display "a \"quoted\" word"))))

         (test "A keyword prints as a keyword"
               (assert-equal #:expect "(f #:key value)\n"
                             #:got (code->string `(f #:key value))))

         (test "Quoted data prints with a quote"
               (assert-equal #:expect "'()\n"
                             #:got (code->string (quoted '())))
               (assert-equal #:expect "'((\"a\" . \"b\"))\n"
                             #:got (code->string (quoted '(("a" . "b"))))
                             #:name "an association keeps its dot"))))

(define broken-suite
  (suite "Broken forms"
         (test "A definition indents its body"
               (assert-equal
                #:expect '("(define (pet-name value)"
                           "  (jso-ref (pet->jso value) \"name\"))")
                #:got (lines (code->string
                              `(define (pet-name value)
                                 (jso-ref (pet->jso value) "name")))))
               (assert-equal #:expect '("(define empty" "  '())")
                             #:got (lines (code->string `(define empty '())))
                             #:name "a definition always breaks"))

         (test "A long argument list puts one argument per line"
               (let ((output
                      (lines (code->string
                              `(define* (make-pet #:key
                                                  id
                                                  (name "unknown")
                                                  (tags '())
                                                  (birth-day #f)
                                                  (photo-urls '())
                                                  (category #f)))))))
                 (assert-equal #:expect "(define* (make-pet #:key"
                               #:got (car output))
                 (assert-equal #:expect "id"
                               #:got (string-trim-both (cadr output))
                               #:name "an argument sits on a line of its own")
                 (assert-equal #:expect "#f)))"
                               #:got (substring (last output)
                                                (- (string-length (last output)) 5))
                               #:name "the signature is closed after its last argument")))

         (test "A long call wraps its arguments under the first one"
               (let ((output (lines
                              (code->string
                               `(client-send client
                                             'GET
                                             (params->path "/pets/{id}"
                                                           (list (cons "id"
                                                                       pet-id)))
                                             #:query (append (list (cons "limit"
                                                                         limit))
                                                             auth-query))))))
                 (assert-true (if (string-prefix? "(client-send client 'GET"
                                                  (car output))
                                  #t #f)
                              #:name "the call starts the first line")
                 (assert-true (if (every (lambda (line)
                                           (<= (string-length line) line-width))
                                         output)
                                  #t #f)
                              #:name "no line passes the width")))

         (test "A keyword argument starts a line and keeps its value"
               (let ((output (lines (code->string
                                     `(send client
                                            'GET
                                            path
                                            #:query (append (list (cons "limit"
                                                                        limit))
                                                            auth-query)
                                            #:headers auth-headers)))))
                 (assert-true (if (any (lambda (line)
                                         (string-contains line "#:query (append"))
                                       output)
                                  #t #f)
                              #:name "the keyword and its value share a line")))

         (test "A let lines up its bindings"
               (assert-equal
                #:expect '("(let ((path (params->path \"/pets\" '()))"
                           "      (response (client-send client 'GET path)))"
                           "  (values path response))")
                #:got (lines (code->string
                              `(let ((path (params->path "/pets" ,(quoted '())))
                                     (response (client-send client 'GET path)))
                                 (values path response))))))

         (test "An if lines its branches up under the test"
               (assert-equal
                #:expect '("(if (eq? value #f)"
                           "    '()"
                           "    (list value))")
                #:got (lines (code->string
                              `(if (eq? value #f)
                                   ,(quoted '())
                                   (list value))))))

         (test "A cond puts one clause per line"
               (assert-equal
                #:expect '("(cond"
                           "  ((eq? kind 'cat) (jso->cat value))"
                           "  (else (jso->dog value)))")
                #:got (lines (code->string
                              `(cond
                                ((eq? kind 'cat) (jso->cat value))
                                (else (jso->dog value)))))))

         (test "A record type names its constructor, predicate and fields"
               (assert-equal
                #:expect '("(define-record-type <pet>"
                           "  (%make-pet id name tags)"
                           "  pet?"
                           "  (id pet-id)"
                           "  (name pet-name)"
                           "  (tags pet-tags))")
                #:got (lines (code->string
                              `(define-record-type <pet>
                                 (%make-pet id name tags)
                                 pet?
                                 (id pet-id)
                                 (name pet-name)
                                 (tags pet-tags))))))

         (test "A module header puts every clause on its own line"
               (assert-equal
                #:expect '("(define-module (petstore models)"
                           "  #:use-module (srfi srfi-1)"
                           "  #:export (pet? make-pet))")
                #:got (lines (code->string
                              `(define-module (petstore models)
                                 #:use-module (srfi srfi-1)
                                 #:export (pet? make-pet))))))))

(define document-suite
  (suite "Documents"
         (test "A docstring keeps its lines"
               (assert-equal
                #:expect '("(define (f value)"
                           "  \"Check VALUE."
                           ""
                           "Returns #t.\""
                           "  value)")
                #:got (lines (code->string
                              `(define (f value)
                                 "Check VALUE.\n\nReturns #t."
                                 value)))))))

(define data-suite
  (suite "Data"
         (test "A long literal stacks its elements under the quote"
               (let ((output (lines (code->string
                                     (quoted '(("type" . "apiKeyHeader")
                                               ("name" . "a-long-header-name")
                                               ("in" . "header")
                                               ("description" . "a long one")))))))
                 (assert-equal #:expect "'((\"type\" . \"apiKeyHeader\")"
                               #:got (car output))
                 (assert-equal #:expect "  (\"description\" . \"a long one\"))"
                               #:got (last output))))

         (test "Consecutive forms are separated by a blank line"
               (assert-equal
                #:expect '("(list 1)" "" "(list 2)")
                #:got (lines (forms->string (list `(list 1)
                                                  `(list 2))))))

         (test "A comment belongs to the form below it"
               (assert-equal
                #:expect '("(list 1)" "" ";; Why b is 2" "(list 2)")
                #:got (lines (forms->string (list `(list 1)
                                                  (comment "Why b is 2")
                                                  `(list 2))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               one-line-suite
               broken-suite
               document-suite
               data-suite))
