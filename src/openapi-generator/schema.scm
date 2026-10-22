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
;; What a schema *means*, as opposed to how it is written.
;;
;; A JSON Schema in an OpenAPI document can say "object with these properties"
;; in at least five ways: with @code{properties}, with @code{allOf}, with a
;; @code{$ref} to a named object, with @code{oneOf}/{@code{anyOf} over named
;; objects, or with @code{additionalProperties}.  The rest of the generator only
;; wants to answer three questions:
;;
;; - is this a named record, an enumeration, a list, a map or a union?
;; - which member shapes does it have?
;; - what does its raw schema say about validation?
;;
;; A @code{<shape>} answers the first two and carries the raw schema for the
;; third.  Inline object schemas are given names as well: a request body of an
;; operation called @code{create-pet} becomes the record @code{create-pet-request},
;; a nested property @code{status-detail}, and so on, because a record needs a
;; name and an anonymous record type is not something a Scheme programmer could
;; refer to.  The @code{<registry>} is what maps a schema node to the name it was
;; given.

(define-module (openapi-generator schema)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator spec)
  #:export (;; shapes
            <shape> shape? make-shape
            shape-kind
            shape-name
            shape-type
            shape-format
            shape-values
            shape-items
            shape-properties
            shape-branches
            shape-discriminator
            shape-discriminator-values
            shape-schema
            shape-nullable?
            shape-enum?
            record-shape?
            scalar-shape?
            enum-shape?
            list-shape?
            map-shape?
            union-shape?
            object-shape?
            any-shape?
            ;; properties
            <property> property? make-property
            property-name
            property-field
            property-shape
            property-required?
            property-schema
            property-description
            property-default
            property-read-only?
            property-write-only?
            ;; registry
            <registry> registry? make-registry
            registry-for-spec
            registry-name
            registry-named-schemas
            registry-register!
            registry-policy
            new-registry
            ;; shape construction
            schema-shape
            schema-shape-inline
            schema-type
            schema-nullable?
            schema-object?
            schema-enum-values
            shape-of-media-type
            constrained?
            ;; constraints, straight from the raw schema
            schema-minimum
            schema-maximum
            schema-exclusive-minimum
            schema-exclusive-maximum
            schema-multiple-of
            schema-min-length
            schema-max-length
            schema-pattern
            schema-min-items
            schema-max-items
            schema-unique-items?
            schema-min-properties
            schema-max-properties
            schema-default
            schema-example))

;;; Commentary:
;;; Shapes

(define-record-type <shape>
  (make-shape kind
              name
              type
              format
              values
              items
              properties
              branches
              discriminator
              discriminator-values
              schema
              nullable?)
  shape?
  (kind shape-kind)
  (name shape-name)
  (type shape-type)
  (format shape-format)
  (values shape-values)
  (items shape-items)
  (properties shape-properties)
  (branches shape-branches)
  (discriminator shape-discriminator)
  (discriminator-values shape-discriminator-values)
  (schema shape-schema)
  (nullable? shape-nullable?))

(define (record-shape? shape)
  "Return @code{#t} when @var{shape} stands for a named record."
  (eq? (shape-kind shape) 'record))

(define (scalar-shape? shape)
  "Return @code{#t} when @var{shape} is a scalar."
  (eq? (shape-kind shape) 'scalar))

(define (enum-shape? shape)
  "Return @code{#t} when @var{shape} is an enumeration."
  (eq? (shape-kind shape) 'enum))

(define (list-shape? shape)
  "Return @code{#t} when @var{shape} is a list of a single shape."
  (eq? (shape-kind shape) 'list))

(define (map-shape? shape)
  "Return @code{#t} when @var{shape} is a map from strings to a shape."
  (eq? (shape-kind shape) 'map))

(define (union-shape? shape)
  "Return @code{#t} when @var{shape} is a @code{oneOf} or @code{anyOf} union."
  (eq? (shape-kind shape) 'union))

(define (object-shape? shape)
  "Return @code{#t} when @var{shape} is an object with declared properties."
  (eq? (shape-kind shape) 'object))

(define (any-shape? shape)
  "Return @code{#t} when @var{shape} accepts any value."
  (eq? (shape-kind shape) 'any))

(define-record-type <property>
  (make-property name field shape required? schema description default
                 read-only? write-only?)
  property?
  (name property-name)
  (field property-field)
  (shape property-shape)
  (required? property-required?)
  (schema property-schema)
  (description property-description)
  (default property-default)
  (read-only? property-read-only?)
  (write-only? property-write-only?))

;;; Commentary:
;;; Registry

(define-record-type <registry>
  (make-registry table names policy)
  registry?
  (table registry-table)
  (names registry-names set-registry-names!)
  (policy registry-policy))

(define* (new-registry #:optional (policy 'all))
  (make-registry (make-hash-table) '() policy))

(define (registry-name registry node)
  "Return the generated name of the schema @var{node}, or #f."
  (and registry (hash-ref (registry-table registry) node)))

(define (registry-named-schemas registry)
  "Return the named schemas of @var{registry} as @code{(name . node)} pairs,
components first, in the order they were registered."
  (reverse (registry-names registry)))

(define (registry-register! registry name node)
  "Like @code{registry-add!}, but mutate in place and return the final name."
  (if (hash-ref (registry-table registry) node)
      (hash-ref (registry-table registry) node)
      (begin
        (hash-set! (registry-table registry) node name)
        (set-registry-names! registry (cons (cons name node)
                                            (registry-names registry)))
        name)))

(define* (registry-for-spec spec #:key (extra-names? #t) (model-suffix "")
                            (direction-suffix "") (policy 'all))
  "Return the registry naming every record the generator has to emit for @var{spec}.

The component schemas keep their specification names; inline schemas are named
after the place they appear in: @code{create-pet-request},
@code{list-pets-response}, @code{create-pet-request-status}.

@var{model-suffix} is appended to every model name at the point it enters the
registry, so a reference to a schema and the definition of that schema always
agree.  A nested inline model keeps the suffix of the model it was named after,
which is what keeps a second specification in the same namespace apart.

@var{direction-suffix} is appended to component names only: an operation's
inline request and response models already say which direction they belong to,
and appending @code{Request} to @code{create-pet-request} would read as a
stutter rather than as information.

@var{policy} decides which properties survive: @code{all} keeps every property
(but makes @code{readOnly} and @code{writeOnly} ones optional), @code{request}
drops @code{readOnly} properties and @code{response} drops @code{writeOnly}
ones.  The @code{exclude-read-only}, @code{exclude-write-only} and
@code{exclude-both} policies are the single-set spellings of the same idea."
  (let ((registry (new-registry policy)))
    (for-each (lambda (schema)
                (registry-register!
                 registry
                 (string-append (named-schema-name schema)
                                model-suffix
                                direction-suffix)
                 (named-schema-schema schema)))
              (spec-schemas spec))
    (when extra-names?
      (for-each (lambda (operation)
                  (register-operation! registry operation model-suffix))
                (spec-operations spec)))
    ;; Nested inline objects inside every registered schema.
    (for-each (lambda (entry)
                (register-nested! registry (car entry) (cdr entry) 0))
              (registry-named-schemas registry))
    registry))

(define (model-name base suffix)
  "Return @var{base} with @var{suffix} appended, unless it is empty."
  (if (or (not suffix) (string-null? suffix))
      base
      (string-append base suffix)))

(define (registry-includes-requests? registry)
  "Return @code{#t} when @var{registry} describes request payloads."
  (memq (registry-policy registry)
        '(all request exclude-read-only exclude-write-only exclude-both)))

(define (registry-includes-responses? registry)
  "Return @code{#t} when @var{registry} describes response payloads."
  (memq (registry-policy registry)
        '(all response exclude-read-only exclude-write-only exclude-both)))

(define (registry-fresh-name registry base)
  "Return BASE, or BASE-2 and so on when its identifier is already generated.

The registry holds specification names for components (@code{SignInResponse})
and generated kebab-case names for inline models (@code{sign-in-response});
comparing the identifiers, not the raw strings, is what keeps those two from
colliding in the generated module."
  (let loop ((candidate base) (counter 2))
    (let ((candidate-identifier (identifier candidate)))
      (if (find (lambda (entry)
                  (string=? (identifier (car entry)) candidate-identifier))
                (registry-names registry))
          (loop (format #f "~a-~a" base counter) (+ counter 1))
          candidate))))

(define (register-schema! registry base node)
  "Register NODE under BASE, or return the name it already has."
  (or (registry-name registry node)
      (registry-register! registry (registry-fresh-name registry base) node)))

(define (media-type-name-suffix media-type)
  "Return a short, stable suffix for MEDIA-TYPE."
  (case (media-type-kind media-type)
    ((json) "json")
    ((text) "text")
    ((form) "form")
    ((multipart) "multipart")
    (else "binary")))

(define (register-operation! registry operation model-suffix)
  (let ((name (symbol->string (operation-name operation))))
    (when (registry-includes-requests? registry)
      (let ((body (operation-request-body operation))
            (preferred (operation-request-media-type operation)))
        (when body
          (for-each
           (lambda (media-type)
             (let ((schema (media-type-schema media-type)))
               (when (inline-object? schema)
                 (register-schema!
                  registry
                  (model-name
                   (string-append
                    name "-request"
                    (if (eq? media-type preferred)
                        ""
                        (string-append "-"
                                       (media-type-name-suffix media-type))))
                   model-suffix)
                  schema))))
           (request-body-media-types body)))))
    (when (registry-includes-responses? registry)
      (for-each (lambda (response)
                  (let ((base (if (member (response-status response)
                                          '("200" "default"))
                                  (string-append name "-response")
                                  (string-append name "-"
                                                 (response-status response)
                                                 "-response")))
                        (first? #t))
                    (for-each (lambda (media-type)
                                (let ((schema (media-type-schema media-type)))
                                  (when (inline-object? schema)
                                    (register-schema!
                                     registry
                                     (model-name
                                      (if first?
                                          base
                                          (string-append
                                           base "-"
                                           (media-type-name-suffix media-type)))
                                      model-suffix)
                                     schema)
                                    (set! first? #f))))
                              (response-media-types response))))
                (operation-responses operation)))
    (when (registry-includes-requests? registry)
      (for-each (lambda (parameter)
                  (let ((schema (parameter-schema parameter)))
                    (when (and (jso-object? schema)
                               (jso-ref schema "properties"))
                      (registry-register! registry
                                          (model-name
                                           (string-append name "-"
                                                          (symbol->string
                                                           (parameter-field parameter)))
                                           model-suffix)
                                          schema))))
                (operation-parameters operation)))))

(define* (register-nested! registry name node #:optional (depth 0))
  "Register the inline objects nested in @var{node} under names derived from
@var{name}."
  (when (and (< depth 12) (jso-object? node))
    (let ((properties (jso-ref node "properties"))
          (items (jso-ref node "items"))
          (branches (or (jso-ref node "oneOf") (jso-ref node "anyOf")))
          (composed (jso-ref node "allOf")))
      (when (jso-object? properties)
        (for-each (lambda (entry)
                    (let ((child (cdr entry))
                          (child-name (string-append name "-"
                                                     (identifier (car entry)))))
                      (when (inline-object? child)
                        (registry-register! registry child-name child))
                      (register-nested! registry child-name child
                                        (+ depth 1))))
                  properties))
      (when (and (jso-object? items) (inline-object? items))
        (registry-register! registry (string-append name "-item") items))
      (when (jso-object? items)
        (register-nested! registry (string-append name "-item") items
                          (+ depth 1)))
      (when (vector? branches)
        (for-each (lambda (branch)
                    (register-nested! registry name branch (+ depth 1)))
                  (vector->list branches)))
      (when (vector? composed)
        (for-each (lambda (branch)
                    (register-nested! registry name branch (+ depth 1)))
                  (vector->list composed))))))

(define (inline-object? node)
  "Return @code{#t} when @var{node} is an object schema written inline."
  (and (jso-object? node)
       (not (jso-ref node "$ref"))
       (let ((type (schema-type node)))
         (or (eq? type 'object)
             (jso-ref node "properties")))
       (jso-object? (jso-ref node "properties"))))

;;; Commentary:
;;; Reading a schema

(define (schema-type node)
  "Return the @code{type} of @var{node} as a symbol, or #f."
  (let ((type (jso-ref node "type")))
    (cond
      ((string? type) (string->symbol type))
      ((vector? type)
       (let ((types (filter (lambda (element)
                              (and (string? element)
                                   (not (string=? element "null"))))
                            (vector->list type))))
         (and (pair? types) (string->symbol (car types)))))
      (else #f))))

(define (schema-nullable? document node)
  "Return @code{#t} when @var{node} allows null (OpenAPI 3.0 or 3.1 spelling)."
  (let* ((resolved (resolve-ref document node))
         (type (jso-ref resolved "type")))
    (or (jso-true? (jso-ref resolved "nullable"))
        (and (vector? type)
             (member "null" (vector->list type))
             #t))))

(define (schema-object? document node)
  "Return @code{#t} when @var{node} describes an object."
  (let ((resolved (resolve-ref document node)))
    (or (eq? (schema-type resolved) 'object)
        (jso-object? (jso-ref resolved "properties"))
        (jso-object? (jso-ref resolved "additionalProperties"))
        (jso-ref resolved "allOf"))))

(define (schema-enum-values document node)
  "Return the enumeration values of @var{node}, or #f when it has none.

OpenAPI 3.1's @code{const} is a one-value enumeration: a schema that accepts
exactly that value.  Treating it as an enumeration gives it the same generated
predicate and constants as an explicit one-value @code{enum}."
  (let ((resolved (resolve-ref document node)))
    (cond
      ((vector? (jso-ref resolved "enum"))
       (vector->list (jso-ref resolved "enum")))
      ((jso-has? resolved "const")
       (list (jso-ref resolved "const")))
      (else #f))))

;;; Commentary:
;;; Building shapes

(define* (schema-shape document node #:key (registry #f) (named? #t))
  "Return the @code{<shape>} of @var{node}.

When @var{registry} is given and knows a name for the schema, the resulting
shape is a @code{record} reference.  Pass @var{named?} as @code{#f} when the
question is what the schema *contains*, which is what the record generator
needs."
  (let* ((resolved (resolve-ref document node))
         (name (and registry named? (registry-name registry resolved))))
    (cond
      (name
       (make-shape 'record name (schema-type resolved) #f '() #f '() '() #f '()
                   resolved (schema-nullable? document resolved)))
      ((jso-ref resolved "allOf")
       (shape-from-all-of document resolved registry))
      ((or (jso-ref resolved "oneOf") (jso-ref resolved "anyOf"))
       (shape-from-union document resolved registry))
      ((schema-enum-values document resolved)
       (make-shape 'enum
                   (and registry (registry-name registry resolved))
                   (or (schema-type resolved) 'string)
                   (jso-ref resolved "format")
                   (schema-enum-values document resolved)
                   #f '() '() #f '()
                   resolved
                   (schema-nullable? document resolved)))
      ((eq? (schema-type resolved) 'array)
       (make-shape 'list #f 'array #f '()
                   (schema-shape document (or (jso-ref resolved "items")
                                              '())
                                 #:registry registry)
                   '() '() #f '() resolved
                   (schema-nullable? document resolved)))
      ((and (eq? (schema-type resolved) 'object)
            (let ((properties (jso-ref resolved "properties")))
              (not (and (jso-object? properties) (pair? properties)))))
       ;; An object that declares no members is a map, whatever its
       ;; additionalProperties says.  Reading it as any other map keeps the data
       ;; it carries: a bare `type: object` is how the specification says "any
       ;; object", and turning it into a record with no fields would silently
       ;; drop every member.
       (let ((additional (jso-ref resolved "additionalProperties")))
         (make-shape 'map #f 'object #f '()
                     (if (jso-object? additional)
                         (schema-shape document additional #:registry registry)
                         (make-shape 'any #f #f #f '() #f '() '() #f '() '()
                                     #f))
                     '() '() #f '() resolved
                     (schema-nullable? document resolved))))
      ((schema-object? document resolved)
       (make-shape 'object
                   (and registry (registry-name registry resolved))
                   'object
                   (jso-ref resolved "format")
                   '()
                   #f
                   (shape-properties-of document resolved registry)
                   '() #f '() resolved
                   (schema-nullable? document resolved)))
      ((schema-type resolved)
       (make-shape 'scalar
                   (and registry (registry-name registry resolved))
                   (schema-type resolved)
                   (jso-ref resolved "format")
                   '() #f '() '() #f '() resolved
                   (schema-nullable? document resolved)))
      (else
       (make-shape 'any
                   (and registry (registry-name registry resolved))
                   #f #f '() #f '() '() #f '() resolved
                   (schema-nullable? document resolved))))))

(define* (schema-shape-inline document node #:key (registry #f))
  "Return the shape of what @var{node} contains, ignoring its own name."
  (schema-shape document node #:registry registry #:named? #f))

(define* (shape-of-media-type document media-type #:key (registry #f))
  (let ((schema (media-type-schema media-type)))
    (if schema
        (schema-shape document schema #:registry registry)
        (make-shape 'any #f #f #f '() #f '() '() #f '() '() #f))))

(define (policy-includes-property? policy read-only? write-only?)
  "Return @code{#t} when @var{policy} keeps a property with these flags."
  (case policy
    ((request exclude-read-only) (not read-only?))
    ((response exclude-write-only) (not write-only?))
    ((exclude-both) (not (or read-only? write-only?)))
    (else #t)))

(define (policy-keeps-required? policy declared? read-only? write-only?)
  "Return @code{#t} when the property is still required under @var{policy}.

With one model set, a @code{readOnly} or @code{writeOnly} member cannot be
required: a request cannot satisfy a read-only member and a response cannot
satisfy a write-only one.  A directional set excludes the members that do not
belong and keeps the declared requiredness of the ones that do."
  (and declared?
       (if (eq? policy 'all)
           (not (or read-only? write-only?))
           #t)))

(define (shape-properties-of document node registry)
  "Return the properties of the object @var{node}, required ones marked.

The registry's policy decides which properties survive and whether a directional
member counts as required, so the same raw schema becomes the right shape for
the model set being built."
  (let* ((properties (jso-ref node "properties"))
         (required (map (lambda (element)
                          (if (string? element) element (format #f "~a" element)))
                        (jso-array->list (jso-ref node "required"))))
         (policy (if registry (registry-policy registry) 'all)))
    (filter-map
     (lambda (entry)
       (let* ((name (car entry))
              (schema (cdr entry))
              (resolved (resolve-ref document schema))
              (read-only? (jso-true? (jso-ref resolved "readOnly")))
              (write-only? (jso-true? (jso-ref resolved "writeOnly")))
              (declared? (and (member name required) #t)))
         (and (policy-includes-property? policy read-only? write-only?)
              (make-property name
                             (string->symbol (identifier name))
                             (schema-shape document schema #:registry registry)
                             (policy-keeps-required? policy declared?
                                                     read-only? write-only?)
                             resolved
                             (jso-ref resolved "description")
                             (jso-ref resolved "default")
                             read-only?
                             write-only?))))
     (if (jso-object? properties) properties '()))))

(define (shape-from-all-of document node registry)
  "Flatten @code{allOf} into a single object shape.

Composition is how OpenAPI spells inheritance, and a Scheme record has no
inheritance, so the parent properties are merged into the child: the flattened
record is what a consumer of the API wants, and the validator checks every
branch's constraints through the merged properties."
  (let* ((branches (map (lambda (branch)
                          (resolve-ref document branch))
                        (jso-array->list (jso-ref node "allOf"))))
         (all-nodes (cons node branches))
         (properties (append-map (lambda (member)
                                   (shape-properties-of document member registry))
                                 all-nodes))
         (deduplicated
          (fold (lambda (property result)
                  (if (find (lambda (existing)
                              (string=? (property-name existing)
                                        (property-name property)))
                            result)
                      result
                      (append result (list property))))
                '()
                properties)))
    (make-shape 'object
                (and registry (registry-name registry node))
                'object
                (jso-ref node "format")
                '()
                #f
                deduplicated
                '() #f '() node
                (schema-nullable? document node))))

(define (inherited-discriminator document branches)
  "Return a discriminator declared by a common @code{allOf} parent of @var{branches}.

The composition style the specification documents puts the discriminator on the
parent schema and lists the children through @code{allOf}; the @code{oneOf} that
collects them need not repeat it.  The first branch that names such a parent
decides, because a hierarchy has one discriminator."
  (let loop ((rest (jso-array->list branches)))
    (if (null? rest)
        #f
        (let* ((branch (resolve-ref document (car rest)))
               (direct (jso-ref branch "discriminator"))
               (all-of (jso-array->list (jso-ref branch "allOf")))
               (parent (find (lambda (member)
                               (let ((resolved (resolve-ref document member)))
                                 (jso-object? (jso-ref resolved
                                                       "discriminator"))))
                             all-of)))
          (or (and (jso-object? direct) direct)
              (and parent
                   (let ((resolved (resolve-ref document parent)))
                     (jso-ref resolved "discriminator")))
              (loop (cdr rest)))))))

(define (shape-from-union document node registry)
  "Build a @code{oneOf}/{@code{anyOf} shape, discriminated when possible."
  (let* ((raw (or (jso-ref node "oneOf") (jso-ref node "anyOf")))
         (branches (map (lambda (branch)
                          (schema-shape document branch #:registry registry))
                        (jso-array->list raw)))
         (discriminator (or (jso-ref node "discriminator")
                            (inherited-discriminator document raw)))
         (property-name (and (jso-object? discriminator)
                             (jso-ref discriminator "propertyName")))
         (mapping (and (jso-object? discriminator)
                       (jso-ref discriminator "mapping")))
         (values (and property-name
                      (union-discriminator-values document
                                                  raw
                                                  mapping
                                                  registry))))
    (make-shape 'union
                (and registry (registry-name registry node))
                #f #f '() #f '()
                branches
                property-name
                values
                node
                (schema-nullable? document node))))

(define (union-discriminator-values document branches mapping registry)
  "Return @code{(value . name)} pairs for the branches of a discriminated union.

The value is the @code{mapping} key when the specification provides one, and the
branch's schema name otherwise, which is the convention the discriminator
section of the specification describes."
  (append-map (lambda (branch)
                (let* ((resolved (resolve-ref document branch))
                       (reference (and (jso-object? branch)
                                       (jso-ref branch "$ref")))
                       (target-name (and registry
                                         (registry-name registry resolved)))
                       (mapped (and (pair? mapping)
                                    (find (lambda (candidate)
                                            (or (and reference
                                                     (string=? (cdr candidate)
                                                               reference))
                                                (string=? (cdr candidate)
                                                          (or target-name ""))))
                                          mapping))))
                  (if target-name
                      (list (cons (if mapped (car mapped) target-name)
                                  target-name))
                      '())))
              (jso-array->list branches)))

;;; Commentary:
;;; Constraints

(define (schema-raw document node)
  (resolve-ref document node))

(define (number-constraint key)
  (lambda (document node)
    (let ((value (jso-ref (schema-raw document node) key)))
      (and (number? value) value))))

(define (integer-constraint key)
  (lambda (document node)
    (let ((value (jso-ref (schema-raw document node) key)))
      (and (number? value) value))))

(define (string-constraint key)
  (lambda (document node)
    (let ((value (jso-ref (schema-raw document node) key)))
      (and (string? value) value))))

(define schema-minimum (number-constraint "minimum"))
(define schema-maximum (number-constraint "maximum"))
(define schema-exclusive-minimum (number-constraint "exclusiveMinimum"))
(define schema-exclusive-maximum (number-constraint "exclusiveMaximum"))
(define schema-multiple-of (number-constraint "multipleOf"))
(define schema-min-length (integer-constraint "minLength"))
(define schema-max-length (integer-constraint "maxLength"))
(define schema-pattern (string-constraint "pattern"))
(define schema-min-items (integer-constraint "minItems"))
(define schema-max-items (integer-constraint "maxItems"))
(define schema-min-properties (integer-constraint "minProperties"))
(define schema-max-properties (integer-constraint "maxProperties"))

(define (schema-unique-items? document node)
  (jso-true? (jso-ref (schema-raw document node) "uniqueItems")))

(define (schema-default document node)
  (let ((value (jso-ref (schema-raw document node) "default")))
    (if (eq? value #f) #f value)))

(define (schema-example document node)
  (let ((value (or (jso-ref (schema-raw document node) "example")
                   (jso-ref (schema-raw document node) "examples"))))
    (if (eq? value #f) #f value)))

(define (constrained? document node)
  "Return @code{#t} when @var{node} carries any validation constraint."
  (let ((raw (schema-raw document node)))
    (or (schema-minimum document node)
        (schema-maximum document node)
        (schema-exclusive-minimum document node)
        (schema-exclusive-maximum document node)
        (schema-multiple-of document node)
        (schema-min-length document node)
        (schema-max-length document node)
        (schema-pattern document node)
        (schema-min-items document node)
        (schema-max-items document node)
        (schema-unique-items? document node)
        (schema-enum-values document node)
        (jso-ref raw "required"))))
