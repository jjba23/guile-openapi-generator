;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Read the routes of an Artanis application and describe them as OpenAPI.
;;
;; Artanis declares a route by applying a method macro to a path template and a
;; handler, with the route's own options in between:
;;
;; @example
;; (get \"/pets/:petId\" (lambda (rc) ...))
;; (post \"/pets\" #:from-body #:consumes 'application/json
;;       (lambda (rc) ...))
;; (delete \"/pets/:petId\" #:with-auth (lambda (rc) ...))
;; @end example
;;
;; The module reads the source as data — the application is never loaded and no
;; handler runs — and turns each declaration into a route:
;;
;; - a @code{:name} segment becomes the @code{{name}} placeholder and a required
;;   path parameter, because that is what Artanis's router captures;
;; - @code{#:consumes} says which media type the body has, and @code{#:from-body}
;;   without it means JSON, which is what a body of unknown shape is around an
;;   Artanis app;
;; - @code{#:produces}, and @code{#:mime} when it is there, become the media type
;;   of the default @code{200} response;
;; - @code{#:with-auth} is kept as the extension @code{x-artanis-with-auth},
;;   because a callback says a route is protected but not which security scheme
;;   protects it, and inventing one would be worse than saying nothing.
;;
;; Everything the source does not say — summaries, schemas, other responses — is
;; left for the person who knows it.  The result is a valid specification with the
;; right operations, which is the part that is tedious to write by hand.
;;
;; @example
;; (use-modules (openapi-generator artanis-routes)
;;              (openapi-generator routes))
;;
;; (routes->yaml-file (artanis-routes-from-file \"app/routes.scm\")
;;                    \"openapi.yaml\"
;;                    #:title \"My Artanis API\")
;; @end example

(define-module (openapi-generator artanis-routes)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator routes)
  #:export (artanis-routes-from-file
            artanis-routes-from-string
            artanis-routes-from-forms))

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

(define (artanis-routes-from-file path)
  "Return the routes declared in the Artanis source file @var{path}.

The file is read, never loaded; see the commentary of this module for the
declarations and options that are recognised."
  (artanis-routes-from-string (call-with-input-file path get-string-all)))

(define (artanis-routes-from-string text)
  "Return the routes declared in the Artanis source @var{text}."
  (artanis-routes-from-forms (read-forms text)))

(define (artanis-routes-from-forms forms)
  "Return the routes declared among @var{forms}."
  (map route-from-form (route-forms forms)))

(define (route-forms forms)
  "Return the route declarations among @var{forms}, in source order.

Declarations are looked for at any depth, so the body of @code{register-routes!}
or of a @code{define} is searched as well, while quoted data is left alone."
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
  "Return @code{#t} when @var{form} applies a method macro to a path template.

The path has to be a string literal that starts with a slash, so a call to a
procedure that happens to be called @code{get} is not mistaken for a route."
  (and (memq (car form) route-methods)
       (pair? (cdr form))
       (string? (cadr form))
       (string-prefix? "/" (cadr form))))

;;; Commentary:
;;; Route options
;;
;; The options between the path and the handler are read into an alist.  A
;; keyword followed by a value takes it, and a keyword followed by another
;; keyword, or by nothing, is a flag; that covers both @code{#:from-body} on its
;; own and @code{#:with-auth (lambda (rc) ...)}.

(define (option-pairs arguments)
  "Return the keyword options of @var{arguments} as an alist of symbol to value."
  (let loop ((rest arguments) (pairs '()))
    (cond
      ((null? rest) (reverse pairs))
      ((keyword? (car rest))
       (let ((next (cdr rest)))
         (if (and (pair? next) (not (keyword? (car next))))
             (loop (cdr next)
                   (cons (cons (keyword->symbol (car rest)) (car next)) pairs))
             (loop next
                   (cons (cons (keyword->symbol (car rest)) #t) pairs)))))
      (else (loop (cdr rest) pairs)))))

(define (option-value options name)
  "Return the value of the option @var{name} in @var{options}, or @code{#f}."
  (let ((entry (assq name options)))
    (and entry (cdr entry))))

(define (option-present? options name)
  "Return @code{#t} when @var{options} holds the option @var{name}."
  (and (assq name options) #t))

(define (media-type-of value)
  "Return the media type @var{value} names, or @code{#f}.

The value may be a symbol, a string, a quoted symbol, or a list or vector of
them, because @code{#:consumes} and @code{#:produces} are written all of those
ways; the first media type wins, the same rule the generator uses when a
specification declares several."
  (cond
    ((or (string? value) (symbol? value)) (format #f "~a" value))
    ((and (pair? value) (eq? (car value) 'quote)) (media-type-of (cadr value)))
    ((list? value) (and (pair? value) (media-type-of (car value))))
    ((vector? value) (and (positive? (vector-length value))
                          (media-type-of (vector-ref value 0))))
    (else #f)))

;;; Commentary:
;;; Declarations as routes

(define (route-from-form form)
  "Return the route the Artanis declaration @var{form} describes."
  (let* ((options (option-pairs (cddr form)))
         (consumes (media-type-of (option-value options 'consumes)))
         (produces (or (media-type-of (option-value options 'produces))
                       (media-type-of (option-value options 'mime))))
         (request-media (or consumes
                            (and (option-present? options 'from-body)
                                 "application/json"))))
    (route (car form)
           (cadr form)
           #:request-body (and request-media
                               (request-body #:media-type request-media))
           #:responses (list (response 200
                                      #:media-type (or produces
                                                        "application/json")
                                      #:schema (and produces '())))
           #:extensions (if (option-present? options 'with-auth)
                            (list (cons "x-artanis-with-auth" #t))
                            '()))))
