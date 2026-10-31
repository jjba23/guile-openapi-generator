;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Names in a specification are written for humans in other languages:
;; @code{PetPhotoUrls}, @code{pet_photo_urls}, @code{/pets/{petId}} and
;; @code{getPetById}.  Names in generated Scheme are kebab-case identifiers,
;; and a procedure that shadows @code{cond} is a bug we refuse to generate.
;; This module is the single place where the translation happens, so every part
;; of the generator spells a given schema the same way.
;;
;; The rules:
;;
;; - words are split on case changes, digits, and any punctuation;
;; - runs of capitals are treated as one word (@code{HTTPResponse} is
;;   @code{http-response});
;; - identifiers are kebab-case; record names are @code{<kebab>}, predicates
;;   @code{kebab?}, constructors @code{make-kebab}, accessors
;;   @code{kebab-field};
;; - a name that would shadow a Scheme keyword gains a suffix, so an OpenAPI
;;   property called @code{else} becomes the field @code{else-field};
;; - an identifier that would start with a digit gains an @code{n-} prefix.

(define-module (openapi-generator naming)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (openapi-generator jso)
  #:export (split-words
            to-kebab
            to-snake
            to-pascal
            to-camel
            to-constant
            sanitize-name
            reserved-word?
            identifier
            record-name
            predicate-name
            constructor-name
            accessor-name
            enum-constant-name
            operation-procedure-name
            parse-operation-id-transform
            path-slug
            module-path
            file-name
            pluralize))

;;; Commentary:
;;; Word splitting

(define separators
  (char-set-union char-set:whitespace
                  (string->char-set "-_./\\:@,;|+*#()[]{}<>=&%$!?\"'~^`")))

(define (alphanumeric? character)
  (or (char-alphabetic? character)
      (char-numeric? character)))

(define (split-words value)
  "Split @var{value} into a list of lowercase words.

@example
(split-words \"PetPhotoUrls\")  ;; => (\"pet\" \"photo\" \"urls\")
(split-words \"HTTPResponse\")   ;; => (\"http\" \"response\")
(split-words \"/pets/{petId}\")  ;; => (\"pets\" \"pet\" \"id\")
@end example"
  (let* ((text (cond
                 ((string? value) value)
                 ((symbol? value) (symbol->string value))
                 (else (format #f "~a" value))))
         (characters (string->list text))
         (words '())
         (current '()))
    (define (flush!)
      (unless (null? current)
        (set! words (cons (list->string (reverse current)) words))
        (set! current '())))
    (let loop ((rest characters) (previous #f))
      (cond
        ((null? rest)
         (flush!)
         (map string-downcase
              (filter (lambda (word)
                        (not (string-null? word)))
                      (reverse words))))
        (else
         (let* ((character (car rest))
                (next (and (pair? (cdr rest)) (cadr rest))))
           (cond
             ((not (alphanumeric? character))
              (flush!)
              (loop (cdr rest) #f))
             ((and previous
                   (char-lower-case? previous)
                   (char-upper-case? character))
              (flush!)
              (set! current (list character))
              (loop (cdr rest) character))
             ((and previous
                   (char-upper-case? previous)
                   (char-upper-case? character)
                   next
                   (char-lower-case? next))
              (flush!)
              (set! current (list character))
              (loop (cdr rest) character))
             (else
              (set! current (cons character current))
              (loop (cdr rest) character)))))))))

;;; Commentary:
;;; Case conversion

(define (to-kebab value)
  "Return @var{value} as a kebab-case string.

@example
(to-kebab \"PetPhotoUrls\")  ;; => \"pet-photo-urls\"
(to-kebab \"pet_photo\")      ;; => \"pet-photo\"
@end example"
  (string-join (split-words value) "-"))

(define (to-snake value)
  "Return @var{value} as a snake_case string."
  (string-join (split-words value) "_"))

(define (capitalize word)
  (if (string-null? word)
      word
      (string-append (string-upcase (substring word 0 1))
                     (substring word 1 (string-length word)))))

(define (to-pascal value)
  "Return @var{value} as a PascalCase string."
  (string-concatenate (map capitalize (split-words value))))

(define (to-camel value)
  "Return @var{value} as a lowerCamelCase string."
  (let ((words (split-words value)))
    (if (null? words)
        ""
        (string-concatenate (cons (car words) (map capitalize (cdr words)))))))

(define (to-constant value)
  "Return @var{value} as an UPPER_SNAKE_CASE string."
  (string-upcase (to-snake value)))

;;; Commentary:
;;; Kernel words

(define keyword-names
  '("and"
    "begin"
    "call-with-values"
    "case"
    "cond"
    "cut"
    "define"
    "define-record-type"
    "define-syntax"
    "define-syntax-rule"
    "define-values"
    "delay"
    "do"
    "dynamic-wind"
    "else"
    "guard"
    "if"
    "lambda"
    "let"
    "let*"
    "letrec"
    "letrec*"
    "named-lambda"
    "not"
    "or"
    "parameterize"
    "quasiquote"
    "quote"
    "receive"
    "set!"
    "syntax-case"
    "syntax-rules"
    "unquote"
    "unquote-splicing"
    "unless"
    "values"
    "when"
    "_"
    "=>"
    "..."
    "assert"))

(define (reserved-word? name)
  "Return @code{#t} when @var{name} would shadow a Scheme syntax keyword."
  (let ((text (cond
                ((symbol? name) (symbol->string name))
                (else name))))
    (and (member text keyword-names) #t)))

(define* (sanitize-name name #:optional (suffix "-field"))
  "Return @var{name} as an identifier that is safe to bind.

A reserved word gains @var{suffix}; an identifier that would start with a digit
gains an @code{n-} prefix; an empty identifier becomes @code{unnamed}."
  (let* ((raw (to-kebab name))
         (candidate (if (string-null? raw) "unnamed" raw))
         (prefixed (if (char-numeric? (string-ref candidate 0))
                       (string-append "n-" candidate)
                       candidate)))
    (if (reserved-word? prefixed)
        (string-append prefixed suffix)
        prefixed)))

(define (identifier name)
  "Return @var{name} as a kebab-case identifier safe for any binding."
  (sanitize-name name))

;;; Commentary:
;;; Scheme entity names

(define (record-name name)
  "Return the record type name of the schema called @var{name}: @code{<pet>}."
  (string->symbol (string-append "<" (identifier name) ">")))

(define (predicate-name name)
  "Return the predicate name of the schema called @var{name}: @code{pet?}."
  (string->symbol (string-append (identifier name) "?")))

(define (constructor-name name)
  "Return the constructor name of the schema called @var{name}: @code{make-pet}."
  (string->symbol (string-append "make-" (identifier name))))

(define (accessor-name name field)
  "Return the accessor of @var{field} on the record for @var{name}.

@example
(accessor-name \"Pet\" \"photoUrls\")  ;; => pet-photo-urls
@end example"
  (string->symbol (string-append (identifier name)
                                 "-"
                                 (identifier field))))

(define (enum-constant-name enum-name value)
  "Return the constant naming @var{value} in the enumeration @var{enum-name}."
  (string->symbol (string-append (identifier enum-name)
                                 "-"
                                 (identifier value))))

;;; Commentary:
;;; Operations

(define (path-slug path)
  "Return a procedure friendly slug for @var{path}, path parameters included.

@example
(path-slug \"/pets/{petId}/photos\")  ;; => \"pets-by-pet-id-photos\"
@end example"
  (let* ((segments (filter (lambda (segment)
                             (not (string-null? segment)))
                           (string-split path #\/)))
         (words (map (lambda (segment)
                       (if (and (string-prefix? "{" segment)
                                (string-suffix? "}" segment))
                           (string-append "by-"
                                          (identifier (substring segment
                                                                 1
                                                                 (string-length segment))))
                           (identifier segment)))
                     segments)))
    (if (null? words)
        "root"
        (string-join words "-"))))

(define (operation-procedure-name method path operation-id)
  "Return the procedure name for an operation.

The @code{operationId} in the specification wins when it is present and
distinctive; otherwise the method and the path decide, which keeps the names
stable when a specification has no identifiers at all.

@example
(operation-name 'get \"/pets/{petId}\" #f)  ;; => get-pets-by-pet-id
(operation-name 'get \"/pets\" \"listPets\")   ;; => list-pets
@end example"
  (string->symbol
   (if (and (string? operation-id) (not (string-null? operation-id)))
       (identifier operation-id)
       (string-append (string-downcase (symbol->string method))
                      "-"
                      (path-slug path)))))

;;; Commentary:
;;; Renaming operationIds

(define (replacement-items replacement)
  "Split a replacement template into @code{regexp-substitute} items.

@code{\\1} through @code{\\9} refer to the corresponding capture group; every
other character is literal text.  The items are what
@code{regexp-substitute/global} consumes after its @code{'pre} marker."
  (let loop ((index 0) (start 0) (items '()))
    (define (flush end)
      (if (< start end)
          (cons (substring replacement start end) items)
          items))
    (cond
      ((>= index (string-length replacement))
       (reverse (flush index)))
      ((and (char=? (string-ref replacement index) #\\)
            (< (+ index 1) (string-length replacement))
            (char-numeric? (string-ref replacement (+ index 1))))
       (let ((group (string->number
                     (string (string-ref replacement (+ index 1))))))
         (loop (+ index 2) (+ index 2) (cons group (flush index)))))
      (else
       (loop (+ index 1) start items)))))

(define (parse-operation-id-transform spec)
  "Return the procedure that renames an operationId as @var{spec} says.

@var{spec} is @code{REGEX:REPLACEMENT}.  The replacement may refer to capture
groups with @code{\\1} through @code{\\9}; everything else is literal.  The
returned procedure takes the operationId and returns the text the naming rules
should start from, which is how a specification full of
@code{V2_get_pets}-style identifiers is turned into stable
@code{fetch-pets}-style names without editing the document.

@example
((parse-operation-id-transform \"^V2_(.*)$:v2-\\\\1\") \"V2_pets\")
;; => \"v2-pets\"
@end example"
  (let ((colon (string-index spec #\:)))
    (unless colon
      (error (format #f
                     "--operation-id-transform expects REGEX:REPLACEMENT, got ~s"
                     spec)))
    (let* ((pattern (substring spec 0 colon))
           (replacement (substring spec (+ colon 1) (string-length spec)))
           (regexp (catch #t
                     (lambda () (make-regexp pattern))
                     (lambda (key . arguments)
                       (error (format #f
                                      "Invalid operationId transform regex ~s: ~a"
                                      pattern
                                      (or (false-if-exception
                                           (cadr arguments))
                                          key))))))
           (items (replacement-items replacement)))
      (lambda (text)
        (apply regexp-substitute/global #f regexp text
               (append (list 'pre) items (list 'post)))))))

;;; Commentary:
;;; Modules and files

(define (module-path prefix . parts)
  "Return the module name @code{(prefix part ...)}, as a list of symbols."
  (append (map (lambda (part)
                 (string->symbol (identifier part)))
               (string-split (to-kebab prefix) #\/))
          (map (lambda (part)
                 (string->symbol (identifier part)))
               parts)))

(define (file-name name)
  "Return the file name for the module component @var{name}."
  (string-append (identifier name) ".scm"))

(define (pluralize word)
  "Return a naive plural of @var{word}, for collection procedure names."
  (let ((text (identifier word)))
    (cond
      ((string-suffix? "s" text) text)
      ((string-suffix? "y" text)
       (string-append (string-drop-right text 1) "ies"))
      ((string-suffix? "ch" text)
       (string-append text "es"))
      (else (string-append text "s")))))
