;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Reading a specification from disk: picking the parser from the file name or,
;; when the name says nothing, from the first significant character; and
;; refusing anything that is not OpenAPI 3.x early, with an error that says what
;; was found instead.  A Swagger 2.0 document and a JSON Schema file are the two
;; mistakes this is meant to catch.

(define-module (openapi-generator reader)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator json)
  #:use-module (openapi-generator yaml)
  #:export (read-spec
            read-spec-file
            read-spec-string
            read-document
            read-document-string
            fetch-text
            locator?
            http-locator?
            file-locator?
            format-for-locator
            merge-documents
            detect-format
            openapi-version
            supported-version?
            openapi-3?
            document-title
            validate-document))

(define supported-major-version "3")

(define (supported-version? version)
  "Return @code{#t} when @var{version} names an OpenAPI 3.x specification."
  (and (string? version)
       (or (string=? version "3")
           (string-prefix? "3." version))))

(define (openapi-3? document)
  "Return @code{#t} when @var{document} declares an OpenAPI 3.x version."
  (supported-version? (openapi-version document)))

(define (openapi-version document)
  "Return the @code{openapi} member of @var{document} as a string, or #f.

A YAML author may write @code{openapi: 3.1} without quotes, which the reader
turns into the number 3.1; the string form is what the rest of the code wants."
  (let ((version (jso-ref document "openapi")))
    (cond
      ((string? version) version)
      ((number? version) (format #f "~a" version))
      (else #f))))

(define (document-title document)
  "Return the @code{info.title} of @var{document}, or @code{\"API\"}."
  (let ((title (jso-ref-path document "info" "title")))
    (if (and (string? title) (not (string-null? title)))
        title
        "API")))

(define (detect-format source)
  "Guess the format of @var{source}: @code{'json} or @code{'yaml}.

The file name wins when it says something; otherwise the first significant
character decides, because every JSON document starts with @code{@{} or
@code{[}."
  (let* ((lowered (string-downcase source))
         (trimmed (string-trim-both source)))
    (cond
      ((or (string-suffix? ".json" lowered)
           (string-prefix? "{" trimmed)
           (string-prefix? "[" trimmed))
       'json)
      ((or (string-suffix? ".yaml" lowered)
           (string-suffix? ".yml" lowered)
           (string-prefix? "---" trimmed))
       'yaml)
      (else 'yaml))))

(define (http-locator? text)
  "Return @code{#t} when @var{text} is an http or https URL."
  (and (string? text)
       (let ((lowered (string-downcase text)))
         (or (string-prefix? "http://" lowered)
             (string-prefix? "https://" lowered)))))

(define (file-locator? text)
  "Return @code{#t} when @var{text} is a file:// URL."
  (and (string? text)
       (string-prefix? "file://" (string-downcase text))))

(define (locator? text)
  "Return @code{#t} when @var{text} names a document to fetch rather than one
to parse in place."
  (and (string? text)
       (or (http-locator? text)
           (file-locator? text)
           (file-exists? text))))

(define (format-for-locator locator)
  "Return @code{'json}, @code{'yaml} or @code{#f} from a file name or URL.

The suffix of the path decides; a query string or fragment does not.  @code{#f}
means the reader should look at the first significant character instead."
  (let ((path (cond
                ((http-locator? locator)
                 (let ((uri (string->uri locator)))
                   (or (and uri (uri-path uri)) locator)))
                ((file-locator? locator)
                 (substring locator 7 (string-length locator)))
                (else locator))))
    (let ((lowered (string-downcase path)))
      (cond
        ((string-suffix? ".json" lowered) 'json)
        ((or (string-suffix? ".yaml" lowered)
             (string-suffix? ".yml" lowered))
         'yaml)
        (else #f)))))

(define (response-body-text body)
  "Return an HTTP @var{body}, a string or a bytevector, as text."
  (cond
    ((string? body) body)
    ((bytevector? body) (utf8->string body))
    (else "")))

(define* (fetch-text locator #:key (headers '()))
  "Return the contents of @var{locator}, a path or an http(s)/file URL.

@var{headers} is an alist for a remote fetch.  A non-2xx response is an error
rather than a document that later fails to parse for the wrong reason."
  (cond
    ((http-locator? locator)
     (receive (response body) (http-get locator
                                        #:headers headers
                                        #:decode-body? #t)
       (let ((status (response-code response)))
         (unless (and (>= status 200) (< status 300))
           (error (format #f "GET ~a returned HTTP ~a" locator status)))
         (response-body-text body))))
    ((file-locator? locator)
     (call-with-input-file (substring locator 7 (string-length locator))
       get-string-all))
    (else (call-with-input-file locator get-string-all))))

(define* (read-document-string text #:key (format #f) (source "<string>"))
  "Parse @var{text} as YAML or JSON, without checking what it contains.

This is the reader the generator uses for configuration files, JSON Schema
documents and external references; @code{read-spec} is the one that also insists
the result is OpenAPI 3.x."
  (let* ((format (or format (detect-format text)))
         (document (case format
                     ((json) (json-string->jso text))
                     ((yaml) (yaml-string->scm text))
                     (else (error "Unknown document format" format)))))
    document))

(define* (read-document source #:key (format #f) (headers '()))
  "Read and parse the YAML/JSON document named by @var{source}.

@var{source} may be a path, an http(s) URL, a file:// URL, or the document text
itself.  The result is not validated: callers that need an OpenAPI document use
@code{read-spec} or call @code{validate-document} themselves."
  (if (locator? source)
      (read-document-string (fetch-text source #:headers headers)
                            #:format (or format (format-for-locator source))
                            #:source source)
      (read-document-string source #:format format #:source "<document>")))

(define (merge-documents base fragment)
  "Return @var{fragment} merged into @var{base}, recursively for objects.

Objects merge key by key, so a fragment that adds a path or a schema leaves the
rest of the document alone.  A key the fragment defines wins; a list the
fragment defines replaces the base list rather than being appended to it, which
is the rule that makes a fragment's ordering of =servers= or =tags= predictable."
  (cond
    ((and (jso-object? base) (jso-object? fragment))
     (fold (lambda (entry result)
             (let* ((key (car entry))
                    (value (cdr entry))
                    (existing (jso-ref result key)))
               (jso-put result key
                        (if (and (jso-object? existing) (jso-object? value))
                            (merge-documents existing value)
                            value))))
           base
           fragment))
    ((not base) fragment)
    (else fragment)))

(define* (read-spec-string text #:key (format #f) (source "<string>") (validate? #t))
  "Parse @var{text} as an OpenAPI 3.x specification.

@var{format} is @code{'json}, @code{'yaml} or #f to detect it.  @var{source}
only labels error messages.  Unless @var{validate?} is @code{#f}, the document is
checked to be OpenAPI 3.x before it is returned."
  (let ((document (read-document-string text #:format format #:source source)))
    (when validate?
      (validate-document document source))
    document))

(define* (read-spec-file path #:key (format #f) (validate? #t))
  "Read and parse the OpenAPI 3.x specification stored in @var{path}."
  (read-spec-string (call-with-input-file path get-string-all)
                    #:format (or format (format-for-locator path))
                    #:source path
                    #:validate? validate?))

(define* (read-spec source #:key (format #f) (validate? #t) (headers '()))
  "Read a specification from @var{source}: a path, an http(s) URL, or the
document itself.

@var{headers} is sent on a remote fetch."
  (if (locator? source)
      (read-spec-string (fetch-text source #:headers headers)
                        #:format (or format (format-for-locator source))
                        #:source source
                        #:validate? validate?)
      (read-spec-string source
                        #:format format
                        #:source "<document>"
                        #:validate? validate?)))

;;; Commentary:
;;; Validation

(define (validate-document document source)
  "Raise a descriptive error when @var{document} is not an OpenAPI 3.x document."
  (unless (jso-object? document)
    (error "Not an OpenAPI document" source))
  (let ((swagger (jso-ref document "swagger")))
    (when (string? swagger)
      (error (format #f
                     "~a declares Swagger ~a; only OpenAPI 3.x is supported"
                     source
                     swagger))))
  (let ((version (openapi-version document)))
    (cond
      ((not version)
       (error (format #f "~a has no 'openapi' member; not an OpenAPI document"
                      source)))
      ((supported-version? version) #t)
      (else
       (error (format #f
                      "~a declares OpenAPI ~a; only OpenAPI 3.x is supported"
                      source
                      version))))))
