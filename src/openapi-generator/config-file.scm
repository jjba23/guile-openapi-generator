;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A generation run can be described in a file instead of on the command line.
;; The file is YAML or JSON, the same readers the specification itself goes
;; through are used, and every key is normalised to a kebab-case symbol so
;; @code{baseUrl}, @code{base_url} and @code{base-url} mean the same thing.
;;
;; The command line still wins: a flag supplied on the command line overrides the
;; file, which is what makes a checked-in config a useful default rather than a
;; straitjacket.
;;
;; @example
;; input: petstore.yaml
;; output: gen
;; module: petstore
;; targets: [models, client]
;; model-suffix: Dto
;; operation-id-transform: "^V2_(.*)$:v2-\\1"
;; @end example

(define-module (openapi-generator config-file)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator json)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-generator reader)
  #:export (read-config-file
            config-ref
            normalize-config-key
            config-targets
            config-list
            config-boolean))

(define (normalize-config-key key)
  "Return @var{key} as a kebab-case symbol.

camelCase, snake_case and kebab-case all name the same option, so a file can be
written the way its author's language spells things without the generator having
to know which language that was."
  (let* ((text (cond ((symbol? key) (symbol->string key))
                     ((string? key) key)
                     (else (format #f "~a" key))))
         (characters (string->list (string-trim-both text))))
    (let loop ((rest characters) (previous #f) (out '()))
      (if (null? rest)
          (string->symbol (list->string (reverse out)))
          (let ((character (car rest)))
            (cond
              ((char=? character #\_)
               (loop (cdr rest) previous (cons #\- out)))
              ((and (char-upper-case? character)
                    previous
                    (or (char-lower-case? previous)
                        (char-numeric? previous)))
               (loop (cdr rest) character
                     (cons (char-downcase character) (cons #\- out))))
              (else
               (loop (cdr rest) character (cons (char-downcase character) out)))))))))

(define (normalize-config-value value)
  "Return @var{value} with vectors turned into lists and objects normalised."
  (cond
    ((vector? value) (map normalize-config-value (vector->list value)))
    ((and (list? value) (every pair? value))
     (map (lambda (entry)
            (cons (normalize-config-key (car entry))
                  (normalize-config-value (cdr entry))))
          value))
    (else value)))

(define (read-config-file path)
  "Read the generation options stored in @var{path}, a YAML or JSON file.

Returns an alist of kebab-case symbols to values.  An unknown key is kept, so a
newer generator can read a file written for an older one without losing it; the
command line is where a typo is rejected."
  (let* ((text (call-with-input-file path get-string-all))
         (format (detect-format path))
         (document (case format
                     ((json) (json-string->jso text))
                     ((yaml) (yaml-string->scm text))
                     (else (error "Unknown configuration format" format)))))
    (unless (jso-object? document)
      (error (format #f
                     "~a: a configuration file must contain an object, not ~s"
                     path
                     document)))
    (map (lambda (entry)
           (cons (normalize-config-key (car entry))
                 (normalize-config-value (cdr entry))))
         document)))

(define (config-ref config key default)
  "Return the value of @var{key} in @var{config}, or @var{default} when absent."
  (let ((entry (and config (assq key config))))
    (if entry (cdr entry) default)))

(define (coerce-string-list value)
  "Return @var{value} as a list of non-empty trimmed strings.

A comma-separated string and a YAML/JSON list both work, because both are the
obvious way to write a list in a config file."
  (cond
    ((not value) '())
    ((string? value)
     (filter (lambda (item) (not (string-null? item)))
             (map string-trim-both (string-split value #\,))))
    ((list? value)
     (map (lambda (item) (string-trim-both (format #f "~a" item))) value))
    (else (list (string-trim-both (format #f "~a" value))))))

(define config-target-names
  '(models controllers client readme))

(define (config-targets value)
  "Return the targets named by @var{value}, or #f when none were given.

Accepts a comma-separated string or a list, case-insensitively.  @code{all}
expands to every target.  An unknown target is an error: a typo in
@code{--targets models,clients} should not silently generate less than asked."
  (let* ((names (map (lambda (name)
                       (string->symbol (string-downcase name)))
                     (coerce-string-list value)))
         (expanded (if (memq 'all names) config-target-names names)))
    (if (null? expanded)
        #f
        (begin
          (for-each (lambda (name)
                      (unless (memq name config-target-names)
                        (error (format #f
                                       "Unknown target ~a; expected one of ~a"
                                       name
                                       (string-join (map symbol->string
                                                         config-target-names)
                                                    ", ")))))
                    expanded)
          expanded))))

(define (config-list value)
  "Return @var{value} as a list of strings, accepting a comma-separated string."
  (coerce-string-list value))

(define (config-boolean value default)
  "Return @var{value} as a boolean, or @var{default} when it is not one.

A YAML/JSON file gives real booleans, but a string @code{\"false\"} is common
enough that it is read as false rather than as a truthy non-empty string."
  (cond
    ((boolean? value) value)
    ((string? value)
     (let ((text (string-downcase (string-trim-both value))))
       (cond ((member text '("true" "yes" "on" "1")) #t)
             ((member text '("false" "no" "off" "0")) #f)
             (else default))))
    ((not value) default)
    (else #t)))
