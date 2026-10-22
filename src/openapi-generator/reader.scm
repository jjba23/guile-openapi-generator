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
;; Reading a specification from disk: picking the parser from the file name or,
;; when the name says nothing, from the first significant character; and
;; refusing anything that is not OpenAPI 3.x early, with an error that says what
;; was found instead.  A Swagger 2.0 document and a JSON Schema file are the two
;; mistakes this is meant to catch.

(define-module (openapi-generator reader)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator json)
  #:use-module (openapi-generator yaml)
  #:export (read-spec
            read-spec-file
            read-spec-string
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

(define* (read-spec-string text #:key (format #f) (source "<string>") (validate? #t))
  "Parse @var{text} as an OpenAPI 3.x specification.

@var{format} is @code{'json}, @code{'yaml} or #f to detect it.  @var{source}
only labels error messages.  Unless @var{validate?} is @code{#f}, the document is
checked to be OpenAPI 3.x before it is returned.  The value returned is the
document itself, as described in @code{(openapi-generator jso)}."
  (let* ((format (or format (detect-format text)))
         (document (case format
                     ((json) (json-string->jso text))
                     ((yaml) (yaml-string->scm text))
                     (else (error "Unknown specification format" format)))))
    (when validate?
      (validate-document document source))
    document))

(define* (read-spec-file path #:key (format #f) (validate? #t))
  "Read and parse the OpenAPI 3.x specification stored in @var{path}."
  (read-spec-string (call-with-input-file path get-string-all)
                    #:format (or format (detect-format path))
                    #:source path
                    #:validate? validate?))

(define* (read-spec source #:key (format #f) (validate? #t))
  "Read a specification from @var{source}: a file name when that file exists,
otherwise the document itself."
  (if (file-exists? source)
      (read-spec-file source #:format format #:validate? validate?)
      (read-spec-string source #:format format #:validate? validate?)))

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
