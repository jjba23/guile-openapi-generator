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
;; A JSON Schema document is a description of data, not an API, but the model
;; generator only needs the former.  This module wraps a JSON Schema document
;; into the smallest OpenAPI document that can carry it: the schema itself under
;; @code{components/schemas}, and its @code{definitions}/@code{$defs} hoisted
;; next to it with their references rewritten.
;;
;; The conversion is deliberately conservative.  Draft-04 boolean
;; @code{exclusiveMinimum}/@code{exclusiveMaximum} become the numeric bounds
;; OpenAPI 3.1 understands, @code{const} becomes a one-value enumeration, and
;; schema references are rewritten.  Keywords with no OpenAPI equivalent are
;; left where they are: the generator ignores what it does not understand, and
;; inventing a meaning for @code{if}/{@code{then}/{@code{else} or
;; @code{patternProperties} would be worse than ignoring them.

(define-module (openapi-generator json-schema)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator spec)
  #:export (json-schema->openapi
            read-json-schema
            json-schema-root-name))

(define direct-schema-keys
  '("items" "additionalProperties" "not" "contains" "propertyNames"
    "if" "then" "else" "unevaluatedItems" "unevaluatedProperties"))

(define schema-map-keys
  '("properties" "patternProperties" "definitions" "$defs"
    "dependentSchemas"))

(define schema-list-keys
  '("allOf" "oneOf" "anyOf" "prefixItems"))

(define schema-content-keys
  '("type" "properties" "required" "items" "enum" "const" "$ref"
    "allOf" "oneOf" "anyOf" "additionalProperties" "not" "prefixItems"))

(define (schema-like? node)
  "Return @code{#t} when @var{node} says something about the shape of a value."
  (and (jso-object? node)
       (any (lambda (key) (jso-has? node key)) schema-content-keys)))

(define* (json-schema-root-name document #:optional requested)
  "Return the name of the schema generated from @var{document}'s top level.

@var{requested} wins, then the schema's own @code{title}, then a
@code{metadata/name} such as a Kubernetes-style manifest carries, then
@code{\"Schema\"}."
  (or requested
      (let ((title (jso-ref document "title")))
        (and (string? title) (not (string-null? title)) title))
      (let ((name (jso-ref-path document "metadata" "name")))
        (and (string? name) (not (string-null? name)) name))
      "Schema"))

(define (definition-map document)
  "Return the @code{$defs} and @code{definitions} of @var{document} as one alist."
  (let ((defs (jso-ref document "$defs"))
        (definitions (jso-ref document "definitions")))
    (append (if (jso-object? defs) defs '())
            (if (jso-object? definitions) definitions '()))))

(define (rewrite-schema-ref reference)
  "Return @var{reference} with a JSON Schema definition pointer made OpenAPI.

@code{#/definitions/X} and @code{#/$defs/X} become
@code{#/components/schemas/X}; the name is left exactly as written, escaped
characters included, so the reference and the hoisted key still agree."
  (cond
    ((and (string? reference)
          (string-prefix? "#/definitions/" reference))
     (string-append "#/components/schemas/"
                    (substring reference (string-length "#/definitions/"))))
    ((and (string? reference)
          (string-prefix? "#/$defs/" reference))
     (string-append "#/components/schemas/"
                    (substring reference (string-length "#/$defs/"))))
    (else reference)))

(define (upgrade-exclusive-bounds node)
  "Return @var{node} with draft-04 boolean exclusive bounds made numeric."
  (let* ((minimum (jso-ref node "minimum"))
         (maximum (jso-ref node "maximum"))
         (exclusive-minimum (jso-ref node "exclusiveMinimum"))
         (exclusive-maximum (jso-ref node "exclusiveMaximum"))
         (node (cond
                 ((eq? exclusive-minimum #t)
                  (if (number? minimum)
                      (jso-put (jso-delete node "minimum")
                               "exclusiveMinimum" minimum)
                      (jso-delete node "exclusiveMinimum")))
                 ((eq? exclusive-minimum #f)
                  (jso-delete node "exclusiveMinimum"))
                 (else node)))
         (node (cond
                 ((eq? exclusive-maximum #t)
                  (if (number? maximum)
                      (jso-put (jso-delete node "maximum")
                               "exclusiveMaximum" maximum)
                      (jso-delete node "exclusiveMaximum")))
                 ((eq? exclusive-maximum #f)
                  (jso-delete node "exclusiveMaximum"))
                 (else node))))
    node))

(define (const->enum node)
  "Return @var{node} with a @code{const} expressed as a one-value enumeration."
  (if (and (jso-has? node "const")
           (not (jso-has? node "enum")))
      (let ((value (jso-ref node "const")))
        (jso-put (jso-delete node "const") "enum" (vector value)))
      node))

(define (transform-schema node)
  "Return @var{node} as an OpenAPI schema.

Only keys that hold schemas are descended into, so a @code{default} or an
@code{example} that happens to be an object is data and is left alone."
  (if (not (jso-object? node))
      node
      (let* ((node (if (jso-has? node "$ref")
                       (jso-put node "$ref"
                                (rewrite-schema-ref (jso-ref node "$ref")))
                       node))
             (node (upgrade-exclusive-bounds node))
             (node (const->enum node)))
        (fold (lambda (entry result)
                (let ((key (car entry))
                      (value (cdr entry)))
                  (cond
                    ((member key direct-schema-keys)
                     (jso-put result key (transform-schema value)))
                    ((member key schema-map-keys)
                     (jso-put result key
                              (if (jso-object? value)
                                  (map (lambda (child)
                                         (cons (car child)
                                               (transform-schema (cdr child))))
                                       value)
                                  value)))
                    ((member key schema-list-keys)
                     (jso-put result key
                              (if (vector? value)
                                  (list->vector
                                   (map transform-schema (vector->list value)))
                                  value)))
                    (else result))))
              node
              node))))

(define* (json-schema->openapi document #:key root-name)
  "Return an OpenAPI 3.1 document whose schemas are the JSON Schema @var{document}.

The document's own top-level schema becomes @var{root-name} (or its title), and
its definitions become sibling component schemas."
  (if (jso-has? document "openapi")
      document
      (let* ((name (json-schema-root-name document root-name))
             (definitions (definition-map document))
             (root (fold (lambda (key result) (jso-delete result key))
                         document
                         '("$defs" "definitions" "$schema" "$id")))
             (schemas '()))
        (for-each (lambda (entry)
                    (set! schemas
                          (jso-put schemas (car entry)
                                   (transform-schema (cdr entry)))))
                  definitions)
        (when (schema-like? root)
          (set! schemas
                (jso-put schemas name (transform-schema root))))
        (unless (pair? schemas)
          (error (format #f
                         "The JSON Schema ~a describes no schema to generate"
                         name)))
        (list (cons "openapi" "3.1.0")
              (cons "info"
                    (list (cons "title" name)
                          (cons "version" "0.0.0")))
              (cons "paths" '())
              (cons "components" (list (cons "schemas" schemas)))))))

(define* (read-json-schema source #:key (headers '()) root-name)
  "Read the JSON Schema named by @var{source} and return it as OpenAPI.

@var{source} is a path or URL, optionally followed by @code{#/pointer} to select
a schema nested inside a larger document (a Nakadi @code{EventType} manifest, for
example)."
  (let* ((hash (and (string? source) (string-index source #\#)))
         (locator (if hash (substring source 0 hash) source))
         (fragment (if hash
                       (substring source (+ hash 1) (string-length source))
                       #f))
         (document (read-document locator #:headers headers))
         (document (if (and fragment (not (string-null? fragment)))
                       (json-pointer document (string-append "#" fragment))
                       document)))
    (json-schema->openapi document #:root-name root-name)))
