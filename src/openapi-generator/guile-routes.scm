;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Read the routes of a plain Guile server and describe them as OpenAPI.
;;
;; The module reads Scheme source instead of evaluating it: an application is
;; not loaded, no handler is called, and the file does not have to run.  Two
;; spellings are understood, because a plain Guile server has two:
;;
;; - the route table the generator writes, @code{(make-route 'GET (make-regexp
;;   \"^/pets/([^/]+)/?$\") '(\"petId\") 'get-pet-by-id)}, where the path is
;;   recovered by undoing the compilation of the pattern: the anchors are
;;   dropped, each capture group becomes a placeholder again, and the parameter
;;   list says which name belongs to which group;
;; - the declaration a hand-written server tends to use, @code{(get
;;   \"/pets/{petId}\" handler)}, where the method is the first element and the
;;   path template is already written out.
;;
;; What the source cannot say — a summary, a schema, a response — is not
;; invented: the result is a document with the right operations, paths and path
;; parameters, which is the skeleton the rest of the specification hangs on.
;;
;; @example
;; (use-modules (openapi-generator guile-routes)
;;              (openapi-generator routes))
;;
;; (routes->yaml-file (guile-routes-from-file \"app/controllers.scm\")
;;                    \"openapi.yaml\"
;;                    #:title \"My API\")
;; @end example

(define-module (openapi-generator guile-routes)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module ((openapi-generator naming)
               #:prefix naming:)
  #:use-module (openapi-generator routes)
  #:export (guile-routes-from-file
            guile-routes-from-string
            guile-routes-from-forms
            regexp->path))

;;; Commentary:
;;; Reading forms

(define (read-forms text)
  "Return the forms of the Scheme source @var{text}, in order."
  (call-with-input-string text
    (lambda (port)
      (let loop ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))

(define (guile-routes-from-file path)
  "Return the routes declared in the Guile source file @var{path}.

The file is read, never loaded; see the commentary of this module for the
declarations that are recognised."
  (guile-routes-from-string (call-with-input-file path get-string-all)))

(define (guile-routes-from-string text)
  "Return the routes declared in the Guile source @var{text}."
  (guile-routes-from-forms (read-forms text)))

(define (guile-routes-from-forms forms)
  "Return the routes declared among @var{forms}."
  (map route-from-form (route-forms forms)))

(define (route-forms forms)
  "Return the route declarations among @var{forms}, in source order.

Declarations are looked for at any depth, so a route table inside a procedure
body is found too, while quoted data is left alone: a list of routes in a table
is data, but a route declaration is a form."
  (let ((found '()))
    (define (visit form)
      (when (pair? form)
        (unless (memq (car form) '(quote quasiquote unquote unquote-splicing
                                   syntax))
          (when (route-declaration? form)
            (set! found (cons form found)))
          (visit-children form))))
    (define (visit-children form)
      (let loop ((rest (cdr form)))
        (cond
          ((null? rest) #f)
          ((pair? rest)
           (visit (car rest))
           (loop (cdr rest)))
          (else (visit rest)))))
    (for-each visit forms)
    (reverse found)))

(define (route-declaration? form)
  "Return @code{#t} when @var{form} declares a route of a Guile server."
  (or (controller-route? form) (method-declaration? form)))

(define (controller-route? form)
  "Return @code{#t} when @var{form} is a @code{make-route} of a route table."
  (and (eq? (car form) 'make-route)
       (pair? (cdr form))
       (pair? (cddr form))
       (regexp-form? (caddr form))))

(define (regexp-form? form)
  "Return @code{#t} when @var{form} compiles a pattern from a string literal."
  (and (pair? form)
       (eq? (car form) 'make-regexp)
       (pair? (cdr form))
       (string? (cadr form))))

(define (method-declaration? form)
  "Return @code{#t} when @var{form} is a method applied to a path template.

The path has to be a string literal that starts with a slash and the head has to
be a method, so an accessor called @code{get} on some other value is not
mistaken for a route."
  (and (memq (car form) route-methods)
       (pair? (cdr form))
       (string? (cadr form))
       (string-prefix? "/" (cadr form))))

;;; Commentary:
;;; Declarations as routes

(define (route-from-form form)
  (if (controller-route? form)
      (route-from-controller-route form)
      (route-from-method-declaration form)))

(define (route-from-controller-route form)
  "Return the route the @code{make-route} form declares."
  (let ((method (cadr form))
        (pattern (caddr form))
        (parameters (and (pair? (cdddr form)) (cadddr form)))
        (operation (and (pair? (cdddr form)) (pair? (cddddr form))
                        (car (cddddr form)))))
    (route (quoted-value method)
           (regexp->path (cadr pattern) (quoted-names parameters))
           #:operation-id (operation-id-from-name (quoted-value operation)))))

(define (route-from-method-declaration form)
  "Return the route the @code{(get \"\\/path\" handler)} form declares."
  (route (car form) (cadr form)))

(define (quoted-names form)
  "Return the names inside @var{form}, a quoted list of strings or symbols."
  (map (lambda (name)
         (if (string? name) name (format #f "~a" name)))
       (quoted-list form)))

(define (quoted-value form)
  "Return the value @var{form} quotes, or @var{form} itself when it is not quoted.

Source is read, not evaluated, so the @code{'GET} of a route table arrives as
@code{(quote GET)} and the symbol is one level down."
  (if (and (pair? form) (eq? (car form) 'quote) (pair? (cdr form)))
      (cadr form)
      form))

(define (quoted-list form)
  "Return the list @var{form} quotes, or @var{form} itself when unquoted."
  (let ((value (quoted-value form)))
    (if (list? value) value '())))

(define (operation-id-from-name operation)
  "Return the operationId that corresponds to the handler name @var{operation}.

A handler is a Scheme procedure, so its name is kebab case; an operationId is a
name in the specification, so it is camel case.

@example
(operation-id-from-name 'get-pet-by-id)  ;; => \"getPetById\"
@end example"
  (and (or (symbol? operation) (string? operation))
       (naming:to-camel (naming:identifier operation))))

;;; Commentary:
;;; Undoing a compiled path pattern

(define capture-group "([^/]+)")

(define regexp-specials
  (string->char-set ".?+*()[]{}^$|\\"))

(define (regexp->path pattern parameters)
  "Return the path template the controller @var{pattern} was compiled from.

@var{parameters} names the capture groups, in the order the pattern captures
them; the count has to agree with the pattern, because a placeholder that names
the wrong group would silently describe another route.

@example
(regexp->path \"^/pets/([^/]+)/?$\" '(\"petId\"))  ;; => \"/pets/{petId}\"
@end example"
  (let loop ((segments (split-segments (strip-anchors pattern)))
             (names parameters)
             (out '()))
    (cond
      ((null? segments)
       (when (pair? names)
         (error "The pattern has fewer capture groups than path parameters"
                pattern
                parameters))
       (string-join (reverse out) "/"))
      ((string=? (car segments) capture-group)
       (when (null? names)
         (error "The pattern has more capture groups than path parameters"
                pattern
                parameters))
       (loop (cdr segments)
             (cdr names)
             (cons (string-append "{" (car names) "}") out)))
      (else
       (loop (cdr segments)
             names
             (cons (unescape-segment (car segments)) out))))))

(define (split-segments text)
  "Split @var{text} on the slashes that separate path segments.

A capture group contains a slash — @code{([^/]+)} — so only a slash outside
parentheses separates two segments; splitting on every slash would cut the
group in three."
  (let loop ((index 0) (depth 0) (start 0) (out '()))
    (cond
      ((>= index (string-length text))
       (reverse (cons (substring text start index) out)))
      (else
       (let ((character (string-ref text index)))
         (cond
           ((char=? character #\()
            (loop (+ index 1) (+ depth 1) start out))
           ((char=? character #\))
            (loop (+ index 1) (max 0 (- depth 1)) start out))
           ((and (char=? character #\/) (zero? depth))
            (loop (+ index 1)
                  depth
                  (+ index 1)
                  (cons (substring text start index) out)))
           (else (loop (+ index 1) depth start out))))))))

(define (strip-anchors pattern)
  "Return @var{pattern} without the anchors a route pattern is compiled with."
  (let* ((text (if (string-prefix? "^" pattern)
                   (substring pattern 1 (string-length pattern))
                   pattern)))
    (cond
      ((string-suffix? "/?$" text)
       (substring text 0 (- (string-length text) 3)))
      ((string-suffix? "$" text)
       (substring text 0 (- (string-length text) 1)))
      (else text))))

(define (unescape-segment segment)
  "Return @var{segment} with the regexp escapes of a literal path segment undone."
  (let loop ((characters (string->list segment)) (out '()))
    (cond
      ((null? characters) (list->string (reverse out)))
      ((and (char=? (car characters) #\\)
            (pair? (cdr characters))
            (char-set-contains? regexp-specials (cadr characters)))
       (loop (cddr characters) (cons (cadr characters) out)))
      (else (loop (cdr characters) (cons (car characters) out))))))
