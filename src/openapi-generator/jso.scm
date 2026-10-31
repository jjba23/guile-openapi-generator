;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The interchange format shared by the reader, the generator and the code it
;; emits.  A JSON/YAML document is represented as follows:
;;
;;   JSON object  -> association list with string keys, e.g. (("a" . 1))
;;   JSON array   -> vector, e.g. #(1 2 3)
;;   JSON string  -> string
;;   JSON number  -> number
;;   JSON boolean -> #t / #f
;;   JSON null    -> @code{json-null}, the symbol @code{null}
;;
;; @code{json-null} is the symbol @code{'null} because that is what guile-json
;; reads and writes for JSON null, so a document parsed by guile-json and one
;; parsed by @code{(openapi-generator yaml)} are directly comparable.
;;
;; Keeping null distinct from the empty list, from @code{#f} and from an absent
;; member is what makes RFC 7386 JSON merge patch expressible at all: "null"
;; means "remove this member", while an absent member means "leave it alone".
;; The representation doubles as the value type generated code converts to and
;; from records, so a "<name>->jso" procedure from the generator and a
;; hand-written document are the same kind of object.

(define-module (openapi-generator jso)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (json-null
            json-null?
            jso-object?
            jso-array?
            jso-scalar?
            jso-keys
            jso-values
            jso-assq
            jso-has?
            jso-ref
            jso-ref-path
            jso-default
            jso-array->list
            jso-list->array
            jso-map
            jso-filter
            jso-put
            jso-delete
            jso-append
            jso-merge
            jso-deep-copy
            jso-normalize
            jso-true?
            jso-string
            jso-number
            jso->string
            json-merge-patch))

;;; Commentary:
;;
;; @code{json-null} is a symbol, not @code{#f} and not @code{'()}: null is a
;; value, while an absent member has no value at all, and the merge patch
;; implementation below depends on telling those two apart.

(define json-null
  'null)

(define (json-null? value)
  "Return @code{#t} when @var{value} is the JSON null value."
  (eq? value json-null))

(define (jso-object? value)
  "Return @code{#t} when @var{value} is a JSON object."
  (and (list? value)
       (not (json-null? value))
       (every (lambda (entry)
                (and (pair? entry)
                     (string? (car entry))))
              value)))

(define (jso-array? value)
  "Return @code{#t} when @var{value} is a JSON array."
  (vector? value))

(define (jso-scalar? value)
  "Return @code{#t} when @var{value} is a JSON string, number, boolean or null."
  (or (string? value)
      (number? value)
      (boolean? value)
      (json-null? value)))

;;; Commentary:
;;; Accessors

(define (jso-object-or-null value)
  (if (jso-object? value)
      value
      '()))

(define (jso-keys object)
  "Return the keys of @var{object}, in document order."
  (map car (jso-object-or-null object)))

(define (jso-values object)
  "Return the values of @var{object}, in document order."
  (map cdr (jso-object-or-null object)))

(define (jso-assq object key)
  "Return the @code{(key . value)} pair of @var{key} in @var{object}, or #f."
  (and (jso-object? object)
       (assoc key object)))

(define (jso-has? object key)
  "Return @code{#t} when @var{object} has a member named @var{key}."
  (if (jso-assq object key)
      #t
      #f))

(define* (jso-ref object key #:optional (default #f))
  "Return the member @var{key} of @var{object}, or @var{default} when absent.

An explicitly null member yields @code{json-null}, never @var{default}: the two
situations are different, and the merge patch implementation depends on it."
  (let ((entry (jso-assq object key)))
    (if entry
        (cdr entry)
        default)))

(define (jso-ref-path object . keys)
  "Follow @var{keys} through nested objects, returning #f when a step is absent."
  (let loop ((node object)
             (keys keys))
    (cond
      ((null? keys)
       node)
      ((jso-object? node)
       (loop (jso-ref node (car keys)) (cdr keys)))
      (else #f))))

(define* (jso-default value #:optional (default #f))
  "Return @var{default} when @var{value} is absent or null, else @var{value}."
  (if (or (eq? value #f) (json-null? value))
      default
      value))

(define (jso-array->list array)
  "Return the elements of the JSON @var{array} as a list."
  (if (vector? array)
      (vector->list array)
      '()))

(define (jso-list->array list)
  "Return @var{list} as a JSON array."
  (list->vector list))

(define (jso-map proc object)
  "Return a new object with @var{proc} applied to every @code{(key . value)}."
  (map (lambda (entry)
         (proc (car entry) (cdr entry)))
       (jso-object-or-null object)))

(define (jso-filter predicate object)
  "Return a new object keeping the entries whose value satisfies @var{predicate}."
  (filter (lambda (entry)
            (predicate (car entry) (cdr entry)))
          (jso-object-or-null object)))

(define (jso-put object key value)
  "Return @var{object} with @var{key} set to @var{value}, keeping its position."
  (if (jso-has? object key)
      (map (lambda (entry)
             (if (string=? (car entry) key)
                 (cons key value)
                 entry))
           object)
      (append object (list (cons key value)))))

(define (jso-delete object key)
  "Return @var{object} without the member @var{key}."
  (jso-filter (lambda (existing _value)
                (not (string=? existing key)))
              object))

(define (jso-append object entries)
  "Return @var{object} with @var{entries} appended, later entries winning."
  (fold (lambda (entry result)
          (jso-put result (car entry) (cdr entry)))
        object
        entries))

(define (jso-merge first second)
  "Shallowly merge @var{second} into @var{first}."
  (jso-append first (jso-object-or-null second)))

(define (jso-deep-copy value)
  "Return a copy of @var{value} that shares no pairs or vectors with it."
  (cond
    ((json-null? value)
     json-null)
    ((jso-object? value)
     (map (lambda (entry)
            (cons (car entry) (jso-deep-copy (cdr entry))))
          value))
    ((vector? value)
     (list->vector (map jso-deep-copy (vector->list value))))
    (else value)))

;;; Commentary:
;;; Normalisation

(define (jso-normalize value)
  "Return @var{value} with every object key turned into a string.

guile-json hands back symbol keys by default; the rest of this project only
ever deals with string keys, so documents are normalised once, at the edge."
  (cond
    ((or (json-null? value) (eq? value #nil))
     json-null)
    ((eq? value #f)
     #f)
    ((vector? value)
     (list->vector (map jso-normalize (vector->list value))))
    ((and (list? value)
          (every pair? value))
     (map (lambda (entry)
            (cons (jso-key->string (car entry))
                  (jso-normalize (cdr entry))))
          value))
    (else value)))

(define (jso-key->string key)
  "Return @var{key} as the string this project uses to name object members."
  (cond
    ((string? key)
     key)
    ((symbol? key)
     (symbol->string key))
    ((keyword? key)
     (symbol->string (keyword->symbol key)))
    (else (format #f "~a" key))))

;;; Commentary:
;;; Types

(define (jso-true? value)
  "Return @code{#t} only for the JSON boolean true."
  (eq? value #t))

(define* (jso-string value #:optional (default #f))
  "Return @var{value} when it is a string, else @var{default}."
  (if (string? value)
      value
      default))

(define* (jso-number value #:optional (default #f))
  "Return @var{value} when it is a number, else @var{default}."
  (if (number? value)
      value
      default))

(define (jso->string value)
  "Return a short, log friendly rendering of @var{value}."
  (cond
    ((json-null? value)
     "null")
    ((string? value)
     (format #f "~s" value))
    ((vector? value)
     (format #f "[~a]"
             (string-join (map jso->string (vector->list value))
                          ", ")))
    ((jso-object? value)
     (format #f "{~a}"
             (string-join (map (lambda (entry)
                                 (format #f "~s: ~a"
                                         (car entry)
                                         (jso->string (cdr entry))))
                               value)
                          ", ")))
    (else (format #f "~a" value))))

;;; Commentary:
;;; JSON merge patch (RFC 7386)
;;
;; The merge patch is the reason null and absence are distinct: a null member
;; deletes the target member, a present member is merged recursively and an
;; absent member is left untouched.

(define (json-merge-patch target patch)
  "Return @var{target} with the RFC 7386 JSON merge patch @var{patch} applied.

@example
(json-merge-patch '((\"a\" . 1) (\"b\" . 2))
                  '((\"b\" . 3) (\"c\" . null)))
;; => ((\"a\" . 1) (\"b\" . 3))
@end example"
  (if (jso-object? patch)
      (fold (lambda (entry result)
              (let ((key (car entry))
                    (value (cdr entry)))
                (cond
                  ((json-null? value)
                   (jso-delete result key))
                  ((jso-object? value)
                   (let ((existing (jso-ref result key)))
                     (jso-put result key
                              (json-merge-patch (if (jso-object? existing)
                                                    existing
                                                    '())
                                                value))))
                  (else
                   (jso-put result key (jso-deep-copy value))))))
            (if (jso-object? target)
                target
                '())
            patch)
      (jso-deep-copy patch)))
