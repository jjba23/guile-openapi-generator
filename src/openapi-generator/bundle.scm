;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; OpenAPI documents are often split across files.  The rest of the generator
;; works on one in-memory document, so external references are *bundled*: the
;; referenced node is copied into the primary document under
;; @code{components/<type>/<name>}, the @code{$ref} is rewritten to point there,
;; and the copy is walked in turn so its own references are rewritten too.
;;
;; Two modes are useful.  @code{targeted} follows only what the primary document
;; reaches.  @code{aggressive} adds every schema in an external file as soon as
;; one reference has caused that file to be read, which is what makes a shared
;; schema file behave the way its author expected.
;;
;; Cycles are fine: a reference is recorded before its target is walked, so a
;; document that refers back to one already being bundled gets the local name
;; that was reserved for it.

(define-module (openapi-generator bundle)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator spec)
  #:export (bundle-external-refs
            resolution-modes))

(define resolution-modes
  '(none targeted aggressive))

(define-record-type <bundle>
  (%make-bundle primary base-locator mode headers documents loaded taken
                additions aggressive-seen)
  bundle?
  (primary bundle-primary)
  (base-locator bundle-base-locator)
  (mode bundle-mode)
  (headers bundle-headers)
  (documents bundle-documents)
  (loaded bundle-loaded)
  (taken bundle-taken)
  (additions bundle-additions set-bundle-additions!)
  (aggressive-seen bundle-aggressive-seen))

(define* (make-bundle primary base-locator mode headers)
  (%make-bundle primary base-locator mode headers
                (make-hash-table) (make-hash-table) (make-hash-table)
                '() (make-hash-table)))

;;; Commentary:
;;; Locators and references

(define (split-reference reference)
  "Return @code{(values locator pointer)} for @var{reference}.

The pointer does not include the @code{#}; a reference with no @code{#} names a
whole document."
  (let ((hash (string-index reference #\#)))
    (if hash
        (values (substring reference 0 hash)
                (substring reference (+ hash 1) (string-length reference)))
        (values reference ""))))

(define (url-origin url)
  "Return the scheme and authority of @var{url}, without a trailing slash."
  (let ((uri (string->uri url)))
    (if uri
        (string-append (symbol->string (uri-scheme uri))
                       "://"
                       (or (uri-host uri) "")
                       (if (uri-port uri) (format #f ":~a" (uri-port uri)) ""))
        "")))

(define (url-directory url)
  "Return @var{url} with its last path segment removed."
  (let ((slash (string-rindex url #\/)))
    (if slash (substring url 0 (+ slash 1)) url)))

(define (resolve-locator base reference)
  "Return @var{reference} resolved relative to the document at @var{base}."
  (cond
    ((http-locator? reference) reference)
    ((file-locator? reference) reference)
    ((not base) reference)
    ((http-locator? base)
     (if (string-prefix? "/" reference)
         (string-append (url-origin base) reference)
         (string-append (url-directory base) reference)))
    ((file-locator? base)
     (let ((path (substring base 7 (string-length base))))
       (if (string-prefix? "/" reference)
           reference
           (string-append (dirname path) "/" reference))))
    (else
     (if (string-prefix? "/" reference)
         reference
         (string-append (dirname base) "/" reference)))))

(define (escape-pointer segment)
  "Return @var{segment} escaped for use in a JSON pointer."
  (string-replace-substring
   (string-replace-substring segment "~" "~0")
   "/"
   "~1"))

(define (unescape-pointer segment)
  "Return @var{segment} unescaped from JSON pointer spelling."
  (string-replace-substring
   (string-replace-substring segment "~1" "/")
   "~0"
   "~"))

(define (pointer-segments pointer)
  "Return the unescaped segments of @var{pointer} (which has no leading @code{#})."
  (if (or (not pointer) (string-null? pointer))
      '()
      (map unescape-pointer
           (string-split (if (string-prefix? "/" pointer)
                             (substring pointer 1 (string-length pointer))
                             pointer)
                         #\/))))

(define (locator-stem locator)
  "Return the file name of @var{locator} without its directory or extension."
  (let* ((path (cond
                 ((http-locator? locator)
                  (or (uri-path (string->uri locator)) locator))
                 ((file-locator? locator)
                  (substring locator 7 (string-length locator)))
                 (else locator)))
         (name (car (reverse (string-split path #\/))))
         (dot (string-rindex name #\.)))
    (if dot (substring name 0 dot) name)))

(define (pointer-component pointer)
  "Return @code{(type . name)} described by @var{pointer}, or a derived name."
  (let ((segments (pointer-segments pointer)))
    (cond
      ((and (>= (length segments) 3)
            (string=? (car segments) "components"))
       (cons (cadr segments) (caddr segments)))
      ((and (= (length segments) 2)
            (member (car segments) '("definitions" "$defs")))
       (cons "schemas" (cadr segments)))
      (else
       (cons "schemas" #f)))))

(define (derived-name locator pointer)
  "Return a name for an external node that is not already a named component."
  (let* ((segments (pointer-segments pointer))
         (stem (locator-stem locator))
         (suffix (string-join (map (lambda (segment)
                                     (identifier segment))
                                   segments)
                              "-")))
    (identifier (if (string-null? suffix)
                    stem
                    (string-append stem "-" suffix)))))

;;; Commentary:
;;; The bundling context

(define (taken-key type name)
  (string-append type "/" name))

(define (seed-taken! bundle)
  "Record the component names the primary document already uses."
  (let ((components (jso-ref (bundle-primary bundle) "components"))
        (taken (bundle-taken bundle)))
    (when (jso-object? components)
      (for-each (lambda (type-entry)
                  (let ((type (car type-entry))
                        (entries (cdr type-entry)))
                    (when (jso-object? entries)
                      (for-each (lambda (entry)
                                  (hash-set! taken
                                             (taken-key type (car entry))
                                             (cdr entry)))
                                entries))))
                components))))

(define (reserve-name! bundle type name node)
  "Return a free name of @var{type} for @var{node}, and reserve it.

The second value says whether an existing component with the same content was
reused; a reused component is neither walked again nor written again."
  (let ((taken (bundle-taken bundle)))
    (let loop ((candidate name) (suffix 2))
      (let* ((key (taken-key type candidate))
             (existing (hash-ref taken key)))
        (cond
          ((not existing)
           (hash-set! taken key node)
           (values candidate #f))
          ((equal? existing node) (values candidate #t))
          (else
           (loop (format #f "~a-~a" name suffix) (+ suffix 1))))))))

(define (document-for bundle locator)
  "Return the parsed document at @var{locator}, reading it once."
  (or (hash-ref (bundle-documents bundle) locator)
      (let ((document (read-document locator
                                     #:headers (bundle-headers bundle))))
        (hash-set! (bundle-documents bundle) locator document)
        (when (and (eq? (bundle-mode bundle) 'aggressive)
                   (not (hash-ref (bundle-aggressive-seen bundle) locator)))
          (hash-set! (bundle-aggressive-seen bundle) locator #t)
          (bundle-all-schemas! bundle locator document))
        document)))

(define (bundle-all-schemas! bundle locator document)
  "Register every schema of the external @var{document} at @var{locator}."
  (define (register entries prefix)
    (when (jso-object? entries)
      (for-each (lambda (entry)
                  (bundle-target bundle locator
                                 (format #f "~a/~a"
                                         prefix
                                         (escape-pointer (car entry)))
                                 (cdr entry)))
                entries)))
  (register (jso-ref-path document "components" "schemas")
            "/components/schemas")
  (register (jso-ref document "definitions") "/definitions")
  (register (jso-ref document "$defs") "/$defs"))

(define (bundle-target bundle locator pointer node)
  "Return the local reference for @var{node}, registering it under @var{locator}.

@var{node} is the already-resolved target; @var{pointer} is only used to choose a
name that reads like the specification.  The reference is recorded before the
target is walked, so a cycle sees the name that was reserved for it."
  (let* ((key (string-append locator "#" pointer))
         (cached (hash-ref (bundle-loaded bundle) key)))
    (if cached
        cached
        (let* ((resolved (if (eq? node #f)
                             (json-pointer (document-for bundle locator)
                                           (string-append "#" pointer))
                             node))
               (component (pointer-component pointer))
               (type (car component))
               (declared-name (cdr component))
               (base-name (or declared-name
                              (derived-name locator pointer))))
          (call-with-values
              (lambda () (reserve-name! bundle type base-name resolved))
            (lambda (name reused?)
              (let ((reference (format #f "#/components/~a/~a" type name)))
                (hash-set! (bundle-loaded bundle) key reference)
                (unless reused?
                  (let ((walked (walk bundle resolved locator #f)))
                    (set-bundle-additions! bundle
                                           (cons (list type name walked)
                                                 (bundle-additions bundle)))))
                reference)))))))

(define (external-reference? reference)
  "Return @code{#t} when @var{reference} names another document."
  (and (string? reference)
       (not (string-null? reference))
       (not (string-prefix? "#" reference))))

(define (resolve-reference bundle reference current-locator)
  "Return the local reference that replaces @var{reference}."
  (call-with-values (lambda () (split-reference reference))
    (lambda (locator pointer)
      (if (string-null? locator)
          ;; A local reference inside an external document still names a node of
          ;; that document, which has to be brought along.
          (bundle-target bundle current-locator pointer #f)
          (bundle-target bundle
                         (resolve-locator current-locator locator)
                         pointer
                         #f)))))

(define (walk bundle node locator primary?)
  "Return @var{node} with every reference made local to the primary document.

@var{primary?} says whether local @code{#/...} references are already local (in
the primary document) or have to be bundled (in an external one)."
  (cond
    ((jso-object? node)
     (let ((reference (jso-ref node "$ref")))
       (cond
         ((external-reference? reference)
          (list (cons "$ref" (resolve-reference bundle reference locator))))
         ((and (string? reference)
               (string-prefix? "#" reference)
               (not primary?))
          (list (cons "$ref" (resolve-reference bundle reference locator))))
         (else
          (map (lambda (entry)
                 (if (member (car entry)
                             '("example" "examples" "default" "enum" "const"))
                     entry
                     (cons (car entry)
                           (walk bundle (cdr entry) locator primary?))))
               node)))))
    ((vector? node)
     (list->vector
      (map (lambda (element) (walk bundle element locator primary?))
           (vector->list node))))
    (else node)))

;;; Commentary:
;;; Putting the components back

(define (jso-put-path object path value)
  "Return @var{object} with @var{value} at @var{path}, creating objects on the way."
  (if (null? path)
      value
      (let* ((key (car path))
             (child (jso-ref object key))
             (updated (if (null? (cdr path))
                          value
                          (jso-put-path (if (jso-object? child) child '())
                                        (cdr path)
                                        value))))
        (jso-put object key updated))))

(define (add-components document additions)
  "Return @var{document} with the bundled @var{additions} under its components."
  (fold (lambda (addition result)
          (jso-put-path result
                        (list "components" (car addition) (cadr addition))
                        (caddr addition)))
        document
        (reverse additions)))

;;; Commentary:
;;; Entry point

(define* (bundle-external-refs document base-locator
                               #:key (mode 'none) (headers '()))
  "Return @var{document} with external references bundled into it.

@var{base-locator} is where the primary document came from, so relative
references can be resolved; @code{#f} means relative references are relative to
the current directory.  @var{mode} is @code{none} (leave the document alone),
@code{targeted} or @code{aggressive}."
  (unless (memq mode resolution-modes)
    (error (format #f
                   "Unknown external reference mode ~a; expected one of ~a"
                   mode
                   (string-join (map symbol->string resolution-modes) ", "))))
  (if (eq? mode 'none)
      document
      (let ((bundle (make-bundle document base-locator mode headers)))
        (seed-taken! bundle)
        (let ((walked (walk bundle document base-locator #t)))
          (add-components walked (bundle-additions bundle))))))
