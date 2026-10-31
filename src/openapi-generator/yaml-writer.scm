;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The writer that closes the loop with @code{(openapi-generator yaml)}: the
;; reader turns YAML into the value representation described in
;; @code{(openapi-generator jso)}, this module turns that representation back
;; into a YAML document.
;;
;; Exactly one style is produced, the one a person would write by hand: block
;; mappings and block sequences indented by two spaces, plain scalars where they
;; read back unchanged, double quotes where they would not, and a literal block
;; for text that already contains line breaks (a description, typically).
;;
;; Deciding when a scalar must be quoted is the whole job.  A plain scalar is
;; read back as a number, a boolean or null when it looks like one, and a colon
;; or a @code{#} can end it early, so @code{3.0.3} is written plain while
;; @code{200}, @code{yes} and @code{Pet: the best} are quoted.  What decides
;; each case is the reader, not taste: the test suite round trips every
;; construct through @code{yaml-string->scm}, because reading the output back has
;; to give the same value, not merely something that looks like YAML.

(define-module (openapi-generator yaml-writer)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (openapi-generator jso)
  #:export (jso->yaml-string
            jso->yaml-file))

;;; Commentary:
;;; Entry points

(define indent-width 2)

(define (jso->yaml-string value)
  "Serialise @var{value} as a YAML document.

@example
(jso->yaml-string '((\"name\" . \"Pet\") (\"legs\" . 4)))
;; => \"name: Pet\\nlegs: 4\\n\"
@end example"
  (call-with-output-string
    (lambda (port)
      (write-document port value))))

(define (jso->yaml-file value path)
  "Write the jso @var{value} to @var{path} as YAML, and return @var{path}."
  (call-with-output-file path
    (lambda (port)
      (write-document port value)))
  path)

;;; Commentary:
;;; Documents
;;
;; Every node is written on its own line, or on the lines below the member or
;; item that owns it.  INDENT is the column the first line of a node starts at;
;; PREFIX is what is printed in front of that first line, which is what lets a
;; sequence item put its @code{- } where the item's first member would otherwise
;; be indented.

(define (spaces count)
  (make-string count #\space))

(define (key-text key)
  "Return @var{key} as the text of a YAML mapping key."
  (cond
    ((string? key) key)
    ((symbol? key) (symbol->string key))
    ((keyword? key) (symbol->string (keyword->symbol key)))
    (else (format #f "~a" key))))

(define (empty-object? value)
  (and (jso-object? value) (null? value)))

(define (empty-array? value)
  (and (vector? value) (zero? (vector-length value))))

(define (write-document port value)
  "Write @var{value} to @var{port} as a YAML document."
  (cond
    ((empty-object? value) (display "{}\n" port))
    ((empty-array? value) (display "[]\n" port))
    ((jso-object? value) (write-object port value 0 ""))
    ((vector? value) (write-array port value 0))
    ((block-scalar? value) (write-block-scalar port value 0))
    (else
     (display (scalar-text value) port)
     (newline port))))

(define (write-object port object indent prefix)
  "Write the members of @var{object}, starting at @var{indent} with @var{prefix}."
  (let loop ((entries object) (prefix prefix))
    (unless (null? entries)
      (let ((entry (car entries)))
        (display prefix port)
        (display (key-scalar-text (key-text (car entry))) port)
        (display ":" port)
        (write-member-value port (cdr entry) indent)
        (loop (cdr entries) (spaces indent))))))

(define (write-member-value port value indent)
  "Write @var{value} after the colon of a member at @var{indent}."
  (let ((child-indent (+ indent indent-width)))
    (cond
      ((empty-object? value) (display " {}\n" port))
      ((empty-array? value) (display " []\n" port))
      ((jso-object? value)
       (newline port)
       (write-object port value child-indent (spaces child-indent)))
      ((vector? value)
       (newline port)
       (write-array port value child-indent))
      ((block-scalar? value)
       (display " " port)
       (write-block-scalar port value child-indent))
      (else
       (display " " port)
       (display (scalar-text value) port)
       (newline port)))))

(define (write-array port items indent)
  "Write the items of the vector @var{items}, each starting at @var{indent}."
  (let loop ((index 0))
    (when (< index (vector-length items))
      (write-item port (vector-ref items index) indent)
      (loop (+ index 1)))))

(define (write-item port item indent)
  "Write one sequence @var{item}, whose dash sits at @var{indent}."
  (let ((child-indent (+ indent indent-width))
        (prefix (string-append (spaces indent) "- ")))
    (cond
      ((empty-object? item) (display prefix port) (display "{}\n" port))
      ((empty-array? item) (display prefix port) (display "[]\n" port))
      ((jso-object? item) (write-object port item child-indent prefix))
      ((vector? item)
       (display (spaces indent) port)
       (display "-\n" port)
       (write-array port item child-indent))
      ((block-scalar? item)
       (display prefix port)
       (write-block-scalar port item child-indent))
      (else
       (display prefix port)
       (display (scalar-text item) port)
       (newline port)))))

(define (write-block-scalar port text indent)
  "Write @var{text} as a literal block scalar whose lines start at @var{indent}."
  (let* ((terminated? (string-suffix? "\n" text))
         (body (if terminated? (string-drop-right text 1) text)))
    (display (if terminated? "|" "|-") port)
    (newline port)
    (for-each (lambda (line)
                (unless (string-null? line)
                  (display (spaces indent) port)
                  (display line port))
                (newline port))
              (string-split body #\newline))))

;;; Commentary:
;;; Scalars
;;
;; A plain scalar is only used when the reader would give back exactly what went
;; in: no leading or trailing whitespace, no line break, no character that ends
;; the scalar early, and nothing a reader would resolve to a number, a boolean
;; or null.  Everything else is double quoted, except text with line breaks,
;; which becomes a literal block when its lines can be carried faithfully.

(define leading-indicators
  (string->char-set "-?:,[]{}#&*!|>'\"%@`"))

(define ambiguous-words
  '("null" "Null" "NULL" "~"
    "true" "True" "TRUE" "false" "False" "FALSE"
    "yes" "Yes" "YES" "no" "No" "NO"
    "on" "On" "ON" "off" "Off" "OFF"
    "y" "Y" "n" "N"
    ".inf" ".Inf" ".INF" "-.inf" "-.Inf" "-.INF" ".nan" ".NaN" ".NAN"))

(define number-patterns
  (map make-regexp
       '("^[-+]?[0-9]+$"
         "^0x[0-9a-fA-F]+$"
         "^0o[0-7]+$"
         "^0b[01]+$"
         "^[-+]?([0-9]+\\.[0-9]*|\\.[0-9]+|[0-9]+)([eE][-+]?[0-9]+)?$"
         "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")))

(define (ambiguous-scalar? text)
  "Return @code{#t} when a plain @var{text} would be read as a non-string."
  (or (and (member text ambiguous-words) #t)
      (any (lambda (pattern) (regexp-exec pattern text))
           number-patterns)))

(define (plain-characters? text)
  "Return @code{#t} when no character of @var{text} ends a plain scalar early.

A colon ends a plain scalar when a space follows it (or when it ends the line),
because that is where a mapping value starts, and a @code{#} does the same when
a space precedes it, because that is where a comment starts."
  (let ((length (string-length text)))
    (let loop ((index 0))
      (cond
        ((>= index length) #t)
        ((char=? (string-ref text index) #\:)
         (let ((next (+ index 1)))
           (if (or (>= next length)
                   (memv (string-ref text next) '(#\space #\tab)))
               #f
               (loop next))))
        ((char=? (string-ref text index) #\#)
         (if (and (> index 0)
                  (memv (string-ref text (- index 1)) '(#\space #\tab)))
             #f
             (loop (+ index 1))))
        (else (loop (+ index 1)))))))

(define (plain-shaped? text)
  "Return @code{#t} when @var{text} has the shape of a plain scalar.

This is everything a plain scalar needs except being readable as text: a
member key only needs the shape, because a key is a key whatever it says, while
a value that looks like a number has to be quoted to stay a string."
  (and (not (string-null? text))
       (not (string-index text char-set:iso-control))
       (not (string-contains text "\n"))
       (not (char-set-contains? leading-indicators (string-ref text 0)))
       (not (char-set-contains? char-set:whitespace (string-ref text 0)))
       (not (char-set-contains? char-set:whitespace
                               (string-ref text (- (string-length text) 1))))
       (plain-characters? text)))

(define (plain-text? text)
  "Return @code{#t} when @var{text} can be written as a plain scalar value."
  (and (plain-shaped? text)
       (not (ambiguous-scalar? text))))

(define (double-quoted text)
  "Return @var{text} between double quotes, with the escapes the reader knows."
  (call-with-output-string
    (lambda (port)
      (display "\"" port)
      (string-for-each
       (lambda (character)
         (cond
           ((char=? character #\\) (display "\\\\" port))
           ((char=? character #\") (display "\\\"" port))
           ((char=? character #\newline) (display "\\n" port))
           ((char=? character #\tab) (display "\\t" port))
           ((char=? character #\return) (display "\\r" port))
           ((char-set-contains? char-set:iso-control character)
            (format port "\\x~2,'0x" (char->integer character)))
           (else (display character port))))
       text)
      (display "\"" port))))

(define (string-text text)
  "Return @var{text} as a scalar: plain when possible, quoted when not."
  (if (plain-text? text)
      text
      (double-quoted text)))

(define (key-scalar-text text)
  "Return the mapping key @var{text} as a scalar.

A status code such as @code{200} is written plain: a mapping key is read as
key text, so it cannot turn into a number, and quoting every status code would
make a specification harder to read than the one a person writes."
  (if (plain-shaped? text)
      text
      (double-quoted text)))

(define (scalar-text value)
  "Return @var{value}, a scalar, as the text of a YAML node."
  (cond
    ((json-null? value) "null")
    ((eq? value #t) "true")
    ((eq? value #f) "false")
    ((number? value) (format #f "~a" value))
    ((string? value) (string-text value))
    ((symbol? value) (string-text (symbol->string value)))
    (else (error "Not a jso value, so it cannot be written as YAML" value))))

(define (block-scalar? value)
  "Return @code{#t} when @var{value} should be written as a literal block.

A literal block is used for text with line breaks whose lines survive the
round trip: the block keeps the line breaks, while a double quoted scalar would
spell them as @code{\\n}.  Text that starts on an empty line, that ends with
more than one line break, or that has a line with a trailing space or a tab is
written quoted instead, because a block would depend on an indentation indicator
or would lose the trailing whitespace."
  (and (string? value)
       (string-contains value "\n")
       (let* ((terminated? (string-suffix? "\n" value))
              (body (if terminated? (string-drop-right value 1) value)))
         (and (not (string-null? body))
              (not (string-suffix? "\n" body))
              (block-lines-usable? body)))))

(define (block-lines-usable? text)
  "Return @code{#t} when every line of @var{text} can go into a literal block."
  (let ((lines (string-split text #\newline)))
    (and (not (string-null? (car lines)))
         (not (char-set-contains? char-set:whitespace (string-ref (car lines) 0)))
         (every (lambda (line)
                  (and (not (string-suffix? " " line))
                       (not (string-suffix? "\t" line))
                       (not (string-index line #\tab))
                       (not (string-index line char-set:iso-control))))
                lines))))
