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
;; The models module: one record type per object schema, one predicate per
;; enumeration, one validation procedure per named schema, and conversions to
;; and from the jso representation for each of them.  For a schema called
;; @code{Pet} with a required @code{name}:
;;
;; @example
;; (define-record-type <pet>
;;   (%make-pet id name status)
;;   pet?
;;   (id pet-id)
;;   (name pet-name)
;;   (status pet-status))
;;
;; (define* (make-pet #:key (id #f) (name #f) (status \"available\"))
;;   ...)
;; @end example
;;
;; Four decisions shape the output:
;;
;; - *Constructors are keyword procedures.*  A schema with eleven properties and
;;   three required ones is unreadable with positional arguments, and a missing
;;   required property is caught by validation with a message naming the field.
;; - *Construction validates.*  @code{make-pet} returns a valid record or
;;   throws; @code{%make-pet} exists for converters, which fill a record and then
;;   validate it once.
;; - *Composition is flattened.*  @code{allOf} becomes one record holding the
;;   union of its members' properties; @code{oneOf}/{@code{anyOf} become a
;;   predicate plus a decoder that dispatches on the discriminator when the
;;   specification declares one.
;; - *Enumerations are values, not types.*  A string enumeration is a list of
;;   strings, a predicate and one constant per value, which composes with Scheme
;;   better than a wrapper type would.
;;
;; Everything below is built as data.  A procedure named @code{...-form} returns
;; the form of one generated definition -- a list of symbols, strings, numbers and
;; keywords -- and a procedure named @code{...-forms} returns the forms of a
;; whole schema, in the order they are written.  @code{emit-models} puts them in a
;; list and renders that list once with @code{forms->string}, so this file reads
;; like the code it writes and the layout of the output is decided in exactly one
;; place.  Only two things stay strings: a docstring and a comment, because they
;; are prose, not code.
;;
;; The expression helpers are shared with the other generators and so are part of
;; this module's interface: @code{unbox-expression} returns the form that decodes
;; a jso value, @code{box-expression} the form that encodes one, and both take
;; their subject as a form as well -- @code{'element}, @code{'(cdr entry)} -- so
;; that a caller can nest them without rendering anything.

(define-module (openapi-generator models)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator schema)
  #:use-module ((openapi-generator code) #:select (forms->string
                                                   comment
                                                   blank
                                                   quoted))
  #:use-module (openapi-generator config)
  #:export (emit-models
            make-context
            context-document
            context-registry
            unbox-expression
            box-expression
            schema-predicate
            schema-display-name
            component-schema?))

;;; Commentary:
;;; Context

(define-record-type <context>
  (%make-context document registry required used config)
  context?
  (document context-document)
  (registry context-registry)
  (required context-required)
  (used context-used)
  (config context-config))

(define (make-context spec config)
  "Collect what the emitters need to look at while they walk the schemas."
  (let* ((document (spec-raw spec))
         (registry (registry-for-spec spec))
         (required (make-hash-table)))
    (for-each (lambda (entry)
                (let* ((name (car entry))
                       (node (cdr entry))
                       (shape (schema-shape document node
                                            #:registry registry
                                            #:named? #f)))
                  (when (object-shape? shape)
                    (hash-set! required
                               name
                               (map property-name
                                    (filter property-required?
                                            (shape-properties shape)))))))
              (registry-named-schemas registry))
    (%make-context document registry required (make-hash-table) config)))

;;; Commentary:
;;; Naming

(define (unique-name! ctx base)
  "Return @var{base}, or @var{base}-2 and so on when it was already handed out."
  (let ((used (context-used ctx)))
    (let loop ((candidate base) (suffix 2))
      (if (hash-ref used candidate)
          (loop (format #f "~a-~a" base suffix) (+ suffix 1))
          (begin
            (hash-set! used candidate #t)
            candidate)))))

(define (component-name shape)
  (or (shape-name shape) "value"))

;;; Commentary:
;;; Names of generated procedures
;;
;; A schema is emitted as a bundle of definitions named after it: the record type
;; @code{<pet>}, the predicate @code{pet?}, the constructor @code{make-pet} and
;; its unchecked twin @code{%make-pet}, the accessors @code{pet-name}, the
;; converters @code{pet->jso} and @code{jso->pet}, the validator
;; @code{validate-pet} and the merge patch @code{pet-apply-patch}.  They are built
;; as symbols in one place, so that a definition and the form that refers to it
;; cannot drift apart, and so that no emitter has to spell a name by gluing text.

(define (name-symbol name)
  "Return @var{name} as a symbol.

A name reaches these procedures in either of two shapes: as the identifier string
the naming module returns, or as the symbol a form already holds.  Both have to
become a symbol, because that is what @code{symbol-append} concatenates."
  (if (symbol? name) name (string->symbol name)))

(define (type-symbol name)
  "Return the record type of the schema called @var{name}, e.g. @code{<pet>}."
  (symbol-append '< (name-symbol name) '>))

(define (predicate-symbol name)
  "Return the predicate of the schema called @var{name}, e.g. @code{pet?}."
  (symbol-append (name-symbol name) '?))

(define (constructor-symbol name)
  "Return the validating constructor of @var{name}, e.g. @code{make-pet}."
  (symbol-append 'make- (name-symbol name)))

(define (raw-constructor-symbol name)
  "Return the unchecked constructor of @var{name}, e.g. @code{%make-pet}."
  (symbol-append '%make- (name-symbol name)))

(define (accessor-symbol name field)
  "Return the accessor of @var{field} on the record of @var{name}."
  (symbol-append (name-symbol name) '- (name-symbol field)))

(define (encoder-symbol name)
  "Return the procedure that encodes a @var{name} as a jso value."
  (symbol-append (name-symbol name) '->jso))

(define (decoder-symbol name)
  "Return the procedure that decodes a jso value into a @var{name}."
  (symbol-append 'jso-> (name-symbol name)))

(define (validator-symbol name)
  "Return the procedure that validates a @var{name}."
  (symbol-append 'validate- (name-symbol name)))

(define (valid-predicate-symbol name)
  "Return the predicate that says whether a @var{name} is valid."
  (symbol-append (name-symbol name) '-valid?))

(define (patch-symbol name)
  "Return the merge patch procedure of @var{name}, e.g. @code{pet-apply-patch}."
  (symbol-append (name-symbol name) '-apply-patch))

(define (values-symbol name)
  "Return the binding that holds the allowed values of an enumeration."
  (symbol-append (name-symbol name) '-values))

(define (constant-symbol symbol value)
  "Return the constant naming @var{value} in the enumeration @var{symbol}.

The whole name is made an identifier at once, so a value that would start with a
digit still produces the constant the export list advertises."
  (string->symbol (identifier (format #f "~a-~a" symbol value))))

;;; Commentary:
;;; Describing shapes in generated code

(define (schema-promoted-type shape)
  "Return the OpenAPI type of @var{shape} as a symbol, for check-type."
  (case (shape-kind shape)
    ((scalar enum) (or (shape-type shape) 'string))
    ((list) 'list)
    ((map) 'map)
    (else 'any)))

(define (schema-predicate ctx shape)
  "Return the form of a predicate that recognises @var{shape}.

A named shape is recognised by the predicate generated for it; a scalar by the
procedure that tests its type; anything else by a lambda, which is the one case
where nothing more specific can be said about the value."
  (case (shape-kind shape)
    ((record enum union)
     (predicate-symbol (identifier (component-name shape))))
    ((scalar)
     (case (or (shape-type shape) 'string)
       ((string) 'string?)
       ((integer) 'integer?)
       ((number) 'number?)
       ((boolean) 'boolean?)
       (else '(lambda (value) #t))))
    ((list) 'list?)
    ((map) '(lambda (value) (and (list? value) (every pair? value))))
    (else '(lambda (value) #t))))

(define (schema-display-name shape)
  "Return a human readable name for @var{shape}, for error messages."
  (case (shape-kind shape)
    ((record union) (component-name shape))
    ((enum)
     (if (shape-name shape)
         (component-name shape)
         (format #f "one of ~{~a~^, ~}" (shape-values shape))))
    ((scalar) (symbol->string (or (shape-type shape) 'value)))
    ((list) (format #f "list of ~a" (schema-display-name (shape-items shape))))
    ((map) (format #f "map counting ~a" (schema-display-name (shape-items shape))))
    (else "value")))

(define (context-required-properties ctx name)
  (or (hash-ref (context-required ctx) name) '()))

(define (union-branch-test ctx shape variable)
  "Return the form of a test that tells whether a jso node matches @var{shape}."
  (case (shape-kind shape)
    ((record)
     (let ((required (context-required-properties ctx (component-name shape))))
       (if (null? required)
           `(jso-object? ,variable)
           `(and (jso-object? ,variable)
                 ,@(map (lambda (property)
                          `(jso-has? ,variable ,property))
                        required)))))
    ((list) `(vector? ,variable))
    ((map union) `(jso-object? ,variable))
    ((scalar enum)
     (case (or (shape-type shape) 'string)
       ((string) `(string? ,variable))
       ((integer) `(integer? ,variable))
       ((number) `(number? ,variable))
       ((boolean) `(boolean? ,variable))
       (else #t)))
    (else `(jso-object? ,variable))))

;;; Commentary:
;;; Conversion, in both directions

(define (converter-to shape)
  "Return the symbol of the procedure that decodes a jso into @var{shape}."
  (decoder-symbol (identifier (component-name shape))))

(define (converter-from shape)
  "Return the symbol of the procedure that encodes @var{shape} as a jso value."
  (encoder-symbol (identifier (component-name shape))))

(define* (unbox-expression ctx shape expression)
  "Return the form that converts the jso @var{expression} into @var{shape}.

@var{expression} is itself a form -- @code{'node}, @code{'(cdr entry)} -- so a
caller can hand in the place the jso value is read from without rendering it
first.  A list and a map bind that place to @code{value} and decode each item,
because the null check happens once for the collection rather than once per
member."
  (case (shape-kind shape)
    ((union)
     (union-unbox ctx shape expression))
    ((record enum)
     (list (converter-to shape) expression))
    ((list)
     `(let ((value ,expression))
        (if (or (eq? value #f) (json-null? value))
            #f
            (map (lambda (element)
                   ,(unbox-expression ctx (shape-items shape) 'element))
                 (jso-array->list value)))))
    ((map)
     `(let ((value ,expression))
        (if (or (eq? value #f) (json-null? value))
            #f
            (map (lambda (entry)
                   (cons (car entry)
                         ,(unbox-expression ctx (shape-items shape) '(cdr entry))))
                 value))))
    ((scalar)
     `(let ((value ,expression))
        (if (json-null? value) #f value)))
    (else expression)))

(define (find-branch shape name)
  "Return the branch of the union @var{shape} called @var{name}, if any."
  (find (lambda (branch)
          (and (not (eq? (shape-kind branch) 'scalar))
               (string=? (component-name branch) name)))
        (shape-branches shape)))

(define (union-unbox ctx shape expression)
  "Return the form that decodes @var{expression} as a union.

A discriminated union dispatches on its discriminator property, which is exact;
an undiscriminated one tries the branch tests in declaration order."
  (let ((discriminator (shape-discriminator shape))
        (branches (shape-branches shape))
        (values (shape-discriminator-values shape)))
    (if (and discriminator (pair? values))
        `(cond
          ,@(map (lambda (entry)
                   (let ((branch (find-branch shape (cdr entry))))
                     (if branch
                         (list (list 'string=?
                                     (list 'jso-ref expression discriminator)
                                     (car entry))
                               (list (converter-to branch) expression))
                         ;; A mapping entry that names no branch keeps the literal
                         ;; #f this generator has always written for it, rather
                         ;; than silently changing what a specification decodes to.
                         #f)))
                 values)
          (else (error "no discriminator value in the value:" ,expression)))
        `(cond
          ,@(map (lambda (branch)
                   (list (union-branch-test ctx branch expression)
                         (unbox-expression ctx branch expression)))
                 branches)
          (else (error "no union member matches" ,expression))))))

(define* (box-expression ctx shape expression #:optional (optional? #f))
  "Return the form that converts the Scheme @var{expression} into a jso value.

When @var{optional?} is true, @code{#f} becomes @code{json-null}, which is how an
absent member is written.  A union is encoded by testing its branches in the same
order the predicate and the decoder use, so the three agree on which member a
value belongs to."
  (let ((encoded
         (case (shape-kind shape)
           ((union)
            `(cond
              ,@(map (lambda (branch)
                       (list (list (schema-predicate ctx branch) expression)
                             (box-expression ctx branch expression)))
                     (shape-branches shape))
              (else (validation-failure
                     ,(quoted (list "union"))
                     "value matches no member of the union"))))
           ((record enum)
            (list (converter-from shape) expression))
           ((list)
            `(list->vector
              (map (lambda (element)
                     ,(box-expression ctx (shape-items shape) 'element))
                   ,expression)))
           ((map)
            `(map (lambda (entry)
                    (cons (car entry)
                          ,(box-expression ctx (shape-items shape) '(cdr entry))))
                  ,expression))
           (else expression))))
    (if optional?
        `(if (eq? ,expression #f) json-null ,encoded)
        encoded)))

;;; Commentary:
;;; Validation

(define (schema-checks ctx shape path expression)
  "Return the validation forms for one value of @var{shape}.

@var{path} is the form that names the value in a message, a quoted list such as
@code{'(\"pet\" \"name\")}, and @var{expression} the form that reads it.  The forms
assume the value is present: absence is the caller's business, since only the
caller knows whether the field is required."
  (let ((document (context-document ctx)))
    (append
     (case (shape-kind shape)
       ((record)
        (list `(check-predicate ,path
                                ,expression
                                ,(schema-predicate ctx shape)
                                ,(schema-display-name shape))))
       ((enum)
        (list `(check-type ,path ,expression ',(schema-promoted-type shape))
              `(check-enum ,path ,expression ,(enum-values-expression shape))))
       ((list)
        (append
         (list `(check-type ,path ,expression 'list))
         (if (any-shape? (shape-items shape))
             '()
             (list `(check-list ,path
                                ,expression
                                ,(schema-predicate ctx (shape-items shape))
                                ,(schema-display-name (shape-items shape)))))
         (list `(check-unique-items ,path ,expression))))
       ((map)
        (append
         (list `(check-type ,path ,expression 'map))
         (if (any-shape? (shape-items shape))
             '()
             (list `(check-map ,path
                               ,expression
                               ,(schema-predicate ctx (shape-items shape))
                               ,(schema-display-name (shape-items shape)))))))
       ((union)
        (list `(check-predicate ,path
                                ,expression
                                ,(schema-predicate ctx shape)
                                ,(schema-display-name shape))))
       (else
        (list `(check-type ,path ,expression ',(schema-promoted-type shape)))))
     (string-constraints document shape path expression)
     (number-constraints document shape path expression)
     (list-constraints document shape path expression))))

(define (string-constraints document shape path expression)
  (let ((schema (shape-schema shape)))
    (if (and schema (eq? (schema-promoted-type shape) 'string))
        (let ((minimum (schema-min-length document schema))
              (maximum (schema-max-length document schema))
              (pattern (schema-pattern document schema))
              (format-name (shape-format shape)))
          (append
           (if minimum
               (list `(check-min-length ,path ,expression ,minimum))
               '())
           (if maximum
               (list `(check-max-length ,path ,expression ,maximum))
               '())
           (if (and pattern (not (string=? pattern "^.*$")))
               (list `(check-pattern ,path ,expression ,pattern))
               '())
           (if (and format-name
                    (memq (string->symbol format-name)
                          '(date date-time time uuid email hostname ipv4 uri)))
               (list `(check-format ,path ,expression ',(string->symbol format-name)))
               '())))
        '())))

(define (number-constraints document shape path expression)
  (let ((schema (shape-schema shape)))
    (if (and schema
             (memq (schema-promoted-type shape) '(integer number)))
        (let ((minimum (schema-minimum document schema))
              (maximum (schema-maximum document schema))
              (exclusive-minimum (schema-exclusive-minimum document schema))
              (exclusive-maximum (schema-exclusive-maximum document schema))
              (multiple (schema-multiple-of document schema)))
          (append
           (if minimum
               (list `(check-minimum ,path ,expression ,minimum))
               '())
           (if maximum
               (list `(check-maximum ,path ,expression ,maximum))
               '())
           (if (number? exclusive-minimum)
               (list `(check-exclusive-minimum ,path ,expression ,exclusive-minimum))
               '())
           (if (number? exclusive-maximum)
               (list `(check-exclusive-maximum ,path ,expression ,exclusive-maximum))
               '())
           (if multiple
               (list `(check-multiple-of ,path ,expression ,multiple))
               '())))
        '())))

(define (list-constraints document shape path expression)
  (let ((schema (shape-schema shape)))
    (if (and schema (eq? (shape-kind shape) 'list))
        (let ((minimum (schema-min-items document schema))
              (maximum (schema-max-items document schema)))
          (append
           (if minimum
               (list `(check-min-items ,path ,expression ,minimum))
               '())
           (if maximum
               (list `(check-max-items ,path ,expression ,maximum))
               '())))
        '())))

;;; Commentary:
;;; Enumerations

(define (enum-values-expression shape)
  "Return the form that refers to the list of allowed values of an enumeration.

A named enumeration binds its values, so the check reads that binding; an inline
one is spelled out, because there is nothing to refer to."
  (if (shape-name shape)
      (values-symbol (identifier (shape-name shape)))
      (quoted (shape-values shape))))

(define (enumeration-forms ctx shape base title)
  "Return the forms of an enumeration, in the order they are written.

An enumeration is the list of allowed values, one constant per value and a
predicate that holds for exactly those values.  @var{base} is the name the
enumeration is published under -- @code{pet-status} for the @code{status} property
of @code{Pet} -- so the constants read @code{pet-status-available} and a caller
that passes a typo gets a reader error rather than a validation error."
  (let* ((symbol (identifier base))
         (values (values-symbol symbol))
         (type (case (or (shape-type shape) 'string)
                 ((integer) 'integer)
                 ((number) 'number)
                 (else 'string))))
    (append
     (list `(define ,values
              (list ,@(shape-values shape))))
     (map (lambda (value)
            `(define ,(constant-symbol symbol value)
               ,value))
          (shape-values shape))
     (list
      `(define (,(predicate-symbol symbol) value)
         ,(format #f "Return #t when VALUE is one of the ~a values." title)
         (and (,(predicate-symbol type) value)
              (if (member value ,values) #t #f)))))))

;;; Commentary:
;;; Records

(define (record-extra-schema shape)
  "Return the schema of undeclared members, when the object keeps them."
  (let* ((schema (shape-schema shape))
         (additional (and schema (jso-ref schema "additionalProperties"))))
    (and (jso-object? additional)
         (pair? (shape-properties shape))
         additional)))

(define (jso-literal value)
  "Return @var{value}, a jso value used as a default, as a form.

A string is handed to the renderer as the string it is: the renderer knows how to
write a literal, escaping included, so there is no reason to quote anything here."
  (cond
    ((json-null? value) #f)
    ((string? value) value)
    ((boolean? value) value)
    ((number? value) value)
    ((vector? value)
     `(list ,@(map jso-literal (vector->list value))))
    (else #f)))

(define (field-default ctx property)
  "Return the form of the constructor default of @var{property}.

An optional collection defaults to the empty collection, which is what a caller
expects, unless the specification gives it a minimum size: then the default is
@code{#f}, because an empty collection would fail validation the moment the
record is built."
  (let* ((declared (property-default property))
         (shape (property-shape property))
         (schema (property-schema property))
         (document (context-document ctx)))
    (cond
      ((property-required? property)
       #f)
      ((and declared (not (json-null? declared)) (not (vector? declared)))
       (jso-literal declared))
      ((and (list-shape? shape)
            (not (schema-min-items document schema)))
       (quoted '()))
      ((and (map-shape? shape)
            (not (schema-min-properties document schema)))
       (quoted '()))
      (else #f))))

(define (shape-renamed shape name)
  "Return @var{shape} with @var{name} as its name."
  (make-shape (shape-kind shape)
              name
              (shape-type shape)
              (shape-format shape)
              (shape-values shape)
              (shape-items shape)
              (shape-properties shape)
              (shape-branches shape)
              (shape-discriminator shape)
              (shape-discriminator-values shape)
              (shape-schema shape)
              (shape-nullable? shape)))

(define (record-fields ctx symbol shape)
  "Return the properties of @var{shape}, with its inline enumerations named."
  (name-property-enums ctx symbol (shape-properties shape)))

(define (record-field-names fields)
  "Return the identifiers of @var{fields}, the names the record binds.

The binding is a variable, so it is a symbol; the name of the member in the jso
object stays a string, because that is what the object is read and written with."
  (map (lambda (property)
         (string->symbol (identifier (property-name property))))
       fields))

(define (record-type-form ctx shape name)
  "Return the @code{define-record-type} form of @var{shape}.

Record types are emitted before anything that refers to their predicate: a
reference to a record predicate written above its @code{define-record-type} is
compiled as a reference to the @code{define-record-type} syntax itself, which is
a syntax transformer rather than a procedure."
  (let* ((symbol (identifier name))
         (fields (record-fields ctx symbol shape))
         (field-names (record-field-names fields))
         (extra-schema (record-extra-schema shape))
         (all-fields (if extra-schema
                         (append field-names '(extra))
                         field-names)))
    `(define-record-type ,(type-symbol symbol)
       (,(raw-constructor-symbol symbol) ,@all-fields)
       ,(predicate-symbol symbol)
       ,@(map (lambda (field)
                (list field (accessor-symbol symbol field)))
              field-names)
       ,@(if extra-schema
             (list (list 'extra (accessor-symbol symbol 'extra)))
             '()))))

(define (record-constructor-form ctx shape name symbol fields field-names
                                extra-schema all-fields)
  "Return the form of the constructor of the record @var{symbol}.

The constructor takes keywords rather than positional arguments: a schema with
eleven properties and three required ones is unreadable with positional
arguments, and a missing required property is caught by validation with a message
naming the field.  The record is built with the unchecked constructor and
validated once, so a caller never holds an invalid record."
  (let ((bindings (map (lambda (field property)
                         (list field (field-default ctx property)))
                       field-names
                       fields)))
    `(define* (,(constructor-symbol symbol)
               ,@(if (and (null? bindings) (not extra-schema))
                     '()
                     (append (list #:key)
                             bindings
                             (if extra-schema
                                 (list (list 'extra (quoted '())))
                                 '()))))
       ,(record-docstring shape name (constructor-symbol symbol) fields)
       (let ((value (,(raw-constructor-symbol symbol) ,@all-fields)))
         (,(validator-symbol symbol) value)
         value))))

(define (record-validator-form ctx symbol name fields field-names)
  "Return the form of the procedure that validates the record @var{symbol}.

Every claim the specification makes about a property becomes one line of the
validator, under a path that names the member, so an error says which field is at
fault rather than that the record is wrong."
  `(define (,(validator-symbol symbol) value)
     ,(string-append
       "Check VALUE against the constraints the specification declares for "
       name
       ".\n\nReturns #t, or throws 'openapi-validation-error naming the field at"
       " fault.")
     (check-predicate ,(quoted (list symbol))
                      value
                      ,(predicate-symbol symbol)
                      ,name)
     ,@(append-map (lambda (field property)
                     (property-check-forms ctx symbol field property))
                   field-names
                   fields)
     #t))

(define (record-validity-form symbol name)
  "Return the form of the predicate that answers whether a value is valid.

The validator throws; this is the same answer as a boolean, for a caller that
wants to ask instead of catching."
  `(define (,(valid-predicate-symbol symbol) value)
     ,(format #f "Return #t when VALUE is a valid ~a." name)
     (valid? (lambda () (,(validator-symbol symbol) value)))))

(define (record-encoder-form ctx symbol name fields field-names extra-schema)
  "Return the form of the procedure that encodes a record as a jso object.

An optional member that holds @code{#f} is left out rather than written as null,
so a request body only carries the members the caller set; a nullable member is
written as null when it holds @code{#f}, because that is what the specification
says null means for it."
  (let ((members
         (map (lambda (property field)
                (let* ((shape (property-shape property))
                       (accessor (list (accessor-symbol symbol field) 'value))
                       (required? (property-required? property))
                       (nullable? (shape-nullable? shape)))
                  (if (or required? nullable?)
                      `(list (cons ,(property-name property)
                                   ,(box-expression ctx shape accessor
                                                    (not required?))))
                      `(if (eq? ,accessor #f)
                           ,(quoted '())
                           (list (cons ,(property-name property)
                                       ,(box-expression ctx shape accessor #f)))))))
              fields
              field-names)))
    `(define (,(encoder-symbol symbol) value)
       "Return VALUE as a jso object, ready for JSON serialisation."
       (check-predicate ,(quoted (list symbol))
                        value
                        ,(predicate-symbol symbol)
                        ,name)
       ,(if (null? fields)
            (list 'list)
            `(append
              ,@members
              ,@(if extra-schema
                    ;; The undeclared members are written last, under the name the
                    ;; specification gave the extension point.
                    (list (list 'list (list 'cons "additionalProperties" 'extra)))
                    '()))))))

(define (record-decoder-form ctx symbol name fields field-names extra-schema)
  "Return the form of the procedure that decodes a jso object into a record.

Every member is decoded into the keyword its property is named after, so the
decoder reads as the constructor call it is; a node that is null, or that is not
an object, decodes to @code{#f}, which is how the rest of the runtime spells an
absent value."
  (let ((arguments
         (append-map (lambda (property field)
                       (list (symbol->keyword field)
                             (unbox-expression
                              ctx
                              (property-shape property)
                              (list 'jso-ref 'node (property-name property)))))
                     fields
                     field-names)))
    `(define (,(decoder-symbol symbol) node)
       ,(format #f "Build a ~a from NODE, a jso object." name)
       (if (or (json-null? node) (not (jso-object? node)))
           #f
           ,(if (null? fields)
                (list (constructor-symbol symbol))
                `(,(constructor-symbol symbol)
                  ,@arguments
                  ,@(if extra-schema
                        (list #:extra
                              (extra-decode ctx extra-schema field-names))
                        '())))))))

(define (record-patch-form symbol name)
  "Return the form of the merge patch procedure of the record @var{symbol}.

A patch is decoded and then re-encoded rather than merged into the record, so a
patch that no longer satisfies the specification fails the way any other invalid
value does."
  `(define* (,(patch-symbol symbol) value patch)
     ,(string-append
       "Return VALUE with the RFC 7386 JSON merge patch PATCH applied.\n"
       "Members of PATCH set to null are removed, and the result is validated\n"
       "like any other " name ", so a patch that would leave the record invalid"
       " raises.")
     (,(decoder-symbol symbol)
      (json-merge-patch (,(encoder-symbol symbol) value) patch))))

(define (record-forms ctx shape name)
  "Return the forms of the record @var{name}, in the order they are written.

The record type itself is not here: it is emitted ahead of every definition that
refers to its predicate, and this is everything that does.  The enumerations of
enumerated properties come after them, so that the members of the record are
described in one run and the values they accept follow, the way a reader looks
for them."
  (let* ((symbol (identifier name))
         (fields (record-fields ctx symbol shape))
         (field-names (record-field-names fields))
         (extra-schema (record-extra-schema shape))
         (all-fields (if extra-schema
                         (append field-names '(extra))
                         field-names)))
    (append
     (list (record-constructor-form ctx shape name symbol fields field-names
                                   extra-schema all-fields)
           (record-validator-form ctx symbol name fields field-names)
           (record-validity-form symbol name)
           (record-encoder-form ctx symbol name fields field-names extra-schema)
           (record-decoder-form ctx symbol name fields field-names extra-schema)
           (record-patch-form symbol name))
     (append-map (lambda (property)
                   (property-enumeration-forms ctx symbol property))
                 fields))))

(define (extra-decode ctx extra-schema known-fields)
  "Return the form that decodes the undeclared members of a jso object.

The members the record already has fields for are skipped, so a value cannot be
read into two places at once."
  (let ((shape (schema-shape (context-document ctx)
                             extra-schema
                             #:registry (context-registry ctx))))
    `(map (lambda (entry)
            (cons (car entry)
                  ,(unbox-expression ctx shape '(cdr entry))))
          (filter (lambda (entry)
                    (not (member (car entry) ,(quoted known-fields))))
                  node))))

(define (boolean-shape? shape)
  "Return @code{#t} when @var{shape} is a boolean.

A required boolean accepts @code{#f}, so it is the one required property that is
not checked for presence: the value that means false is also the value that would
mean missing."
  (and (scalar-shape? shape)
       (eq? (shape-type shape) 'boolean)))

(define (property-check-forms ctx symbol field property)
  "Return the validation forms of one property of a record.

A required property is checked for presence and then checked where it sits; an
optional or nullable one is checked only when it holds something, because
@code{#f} is how an absent member is spelled and the checks are about a value that
is there."
  (let* ((path (quoted (list symbol (property-name property))))
         (expression (list (accessor-symbol symbol field) 'value))
         (shape (property-shape property))
         (required? (and (property-required? property)
                         (not (boolean-shape? shape))))
         (checks (schema-checks ctx shape path expression)))
    (append
     (if required?
         (list `(check-required ,path ,expression))
         '())
     (cond
       ((null? checks) '())
       ((or (not required?) (shape-nullable? shape))
        (list `(when ,expression ,@checks)))
       (else checks)))))

(define (property-enumeration-forms ctx symbol property)
  "Return the forms of the enumeration of @var{property}, when it is one.

An enumeration declared inline in a property is not a component schema, so nothing
else would generate its constants and its predicate; when the property refers to a
component schema, that schema emits them itself and this stays out of the way."
  (let ((shape (property-shape property)))
    (if (and (enum-shape? shape)
             (shape-name shape)
             (not (component-schema? ctx (shape-name shape))))
        (let ((base (shape-name shape))
              (title (format #f "~a.~a" symbol (property-name property))))
          (append (enumeration-forms ctx shape base title)
                  (enumeration-converter-forms (identifier base) title)))
        '())))

(define (component-schema? ctx name)
  "Return #t when @var{name} is the name of a component schema."
  (and (member name (map car (registry-named-schemas (context-registry ctx))))
       #t))

(define (name-property-enums ctx symbol properties)
  "Give every inline enumeration property a name of its own.

The constants and the predicate a generated enumeration exports need a stable
name, and the property it belongs to is the most descriptive one available:
@code{pet-status} for the @code{status} property of @code{Pet}."
  (map (lambda (property)
         (let ((shape (property-shape property)))
           (if (and (enum-shape? shape) (not (shape-name shape)))
               (make-property (property-name property)
                              (property-field property)
                              (shape-renamed
                               shape
                               (unique-name! ctx
                                             (format #f "~a-~a"
                                                     symbol
                                                     (identifier
                                                      (property-name property)))))
                              (property-required? property)
                              (property-schema property)
                              (property-description property)
                              (property-default property))
               property)))
       properties))

(define (record-docstring shape name constructor properties)
  "Return the docstring of the constructor of @var{shape}.

The docstring is prose -- it says what the record is, which fields it holds and
how to call the constructor -- so it is assembled as text and rendered as a
multi-line string literal.  The paragraphs are joined here rather than through
@code{~%} so that the text in this file reads the way it comes out in the
generated file."
  (let* ((schema (shape-schema shape))
         (description (and schema (jso-ref schema "description")))
         (title (and schema (jso-ref schema "title")))
         (summary (cond
                    ((and (string? description) (not (string-null? description)))
                     (string-trim-both description))
                    ((and (string? title) (not (string-null? title)))
                     (string-trim-both title))
                    (else (format #f "A ~a." name))))
         (fields (if (null? properties)
                     ""
                     (format #f "Fields: ~{~a~^, ~}."
                             (map (lambda (property)
                                    (format #f "~a (~a~a)"
                                            (property-name property)
                                            (schema-display-name
                                             (property-shape property))
                                            (if (property-required? property)
                                                ", required"
                                                "")))
                                  properties))))
         (example (format #f "(~a~a)"
                          constructor
                          (if (null? properties)
                              ""
                              (format #f " ~{~a~^ ~}"
                                      (map (lambda (property)
                                             (format #f "#:~a <~a>"
                                                     (identifier
                                                      (property-name property))
                                                     (schema-display-name
                                                      (property-shape property))))
                                           properties))))))
    (string-join (filter (lambda (paragraph) (not (string-null? paragraph)))
                         (list (format #f "Create a ~a." name)
                               summary
                               fields
                               (format #f "@example~%~a~%@end example" example)))
                 "\n\n")))

;;; Commentary:
;;; Named schemas that are not objects

(define (enumeration-converter-forms symbol name)
  "Return the forms of the two converters of an enumeration.

The encoding converter refuses a value outside the enumeration, so a jso is never
written with a spelling the specification does not define; the decoding one stays
lenient and lets the validator say what is wrong, because a decoder that throws
hides which member was at fault."
  (list `(define (,(encoder-symbol symbol) value)
           "Return VALUE as a jso scalar, after checking it."
           (if (,(predicate-symbol symbol) value)
               value
               (validation-failure
                ,(quoted (list symbol))
                ,(format #f "must be one of ~a-values, got ~~s" symbol)
                value)))
        `(define (,(decoder-symbol symbol) node)
           ,(format #f "Read a ~a from the jso scalar NODE." name)
           (if (json-null? node) #f node))))

(define (named-enumeration-forms ctx shape name)
  "Return the forms of a named enumeration: its values, its predicate and its
converters."
  (let ((symbol (identifier name)))
    (append (enumeration-forms ctx shape symbol name)
            (enumeration-converter-forms symbol name))))

(define (validating-predicate-form ctx shape symbol doc)
  "Return the form of a predicate that validates @var{shape} by running its checks.

A named scalar, list or map has no record behind it, so its predicate is its
validator: it is written as @code{(valid? (lambda () ...))}, which turns the first
failure into @code{#f} instead of an error."
  `(define (,(predicate-symbol symbol) value)
     ,doc
     (valid? (lambda ()
               ,@(schema-checks ctx shape (quoted (list symbol)) 'value)
               #t))))

(define (scalar-converter-forms symbol name)
  "Return the forms of the two converters of a named scalar.

A scalar is already a jso value, so encoding is the identity and decoding only
turns a JSON null into @code{#f}."
  (list `(define (,(encoder-symbol symbol) value)
           "Return VALUE as a jso scalar."
           value)
        `(define (,(decoder-symbol symbol) node)
           ,(format #f "Read a ~a from the jso scalar NODE." name)
           (if (json-null? node) #f node))))

(define (named-scalar-forms ctx shape name)
  "Return the forms of a named scalar: its predicate and its converters."
  (let ((symbol (identifier name)))
    (append
     (list (validating-predicate-form
            ctx shape symbol
            (format #f "Return #t when VALUE is a valid ~a."
                    (schema-display-name shape))))
     (scalar-converter-forms symbol name))))

(define (named-list-forms ctx shape name)
  "Return the forms of a named list: its predicate and its converters."
  (let* ((symbol (identifier name))
         (item (shape-items shape)))
    (list
     `(define (,(predicate-symbol symbol) value)
        ,(format #f "Return #t when VALUE is a list of ~a."
                 (schema-display-name item))
        (and (list? value)
             (every ,(schema-predicate ctx item) value)))
     `(define (,(encoder-symbol symbol) value)
        "Return VALUE as a jso array."
        (list->vector
         (map (lambda (element)
                ,(box-expression ctx item 'element))
              value)))
     `(define (,(decoder-symbol symbol) node)
        ,(format #f "Read a list of ~a from NODE." (schema-display-name item))
        (map (lambda (element)
               ,(unbox-expression ctx item 'element))
             (jso-array->list node))))))

(define (named-map-forms ctx shape name)
  "Return the forms of a named map: its predicate and its converters.

A map is an alist, so its predicate has to say both that the value is one and --
unless the specification accepts any value as a member -- that every member is of
the shape the specification declares."
  (let* ((symbol (identifier name))
         (value-shape (shape-items shape)))
    (list
     `(define (,(predicate-symbol symbol) value)
        ,(format #f "Return #t when VALUE maps strings to ~a."
                 (schema-display-name value-shape))
        (and (list? value)
             (every pair? value)
             ,@(if (any-shape? value-shape)
                   '()
                   (list `(every (lambda (entry)
                                   (,(schema-predicate ctx value-shape)
                                    (cdr entry)))
                                 value)))))
     `(define (,(encoder-symbol symbol) value)
        "Return VALUE as a jso object."
        (map (lambda (entry)
               (cons (car entry)
                     ,(box-expression ctx value-shape '(cdr entry))))
             value))
     `(define (,(decoder-symbol symbol) node)
        ,(format #f "Read a map counting ~a from NODE."
                 (schema-display-name value-shape))
        (if (jso-object? node)
            (map (lambda (entry)
                   (cons (car entry)
                         ,(unbox-expression ctx value-shape '(cdr entry))))
                 node)
            ,(quoted '()))))))

(define (named-union-forms ctx shape name)
  "Return the forms of a named union: its predicate, its encoder and its decoder.

A union has no record of its own, so each definition dispatches on the branch
predicates in the order the specification declares them; the decoder is the same
form any other union gets, discriminator and all."
  (let* ((symbol (identifier name))
         (branches (shape-branches shape)))
    (list
     `(define (,(predicate-symbol symbol) value)
        ,(format #f "Return #t when VALUE is one of the shapes ~a accepts." name)
        (or ,@(map (lambda (branch)
                     (list (schema-predicate ctx branch) 'value))
                   branches)))
     `(define (,(encoder-symbol symbol) value)
        "Return VALUE as a jso value."
        (cond
         ,@(map (lambda (branch)
                  (list (list (schema-predicate ctx branch) 'value)
                        (box-expression ctx branch 'value)))
                branches)
         (else (validation-failure
                ,(quoted (list symbol))
                ,(format #f "must be one of ~a" symbol)
                value))))
     `(define (,(decoder-symbol symbol) node)
        "Read the union value NODE."
        ,(unbox-expression ctx shape 'node)))))
;;; Commentary:
;;; The module

(define (replace-properties shape properties)
  "Return @var{shape} with @var{properties} in place of its own."
  (make-shape (shape-kind shape)
              (shape-name shape)
              (shape-type shape)
              (shape-format shape)
              (shape-values shape)
              (shape-items shape)
              properties
              (shape-branches shape)
              (shape-discriminator shape)
              (shape-discriminator-values shape)
              (shape-schema shape)
              (shape-nullable? shape)))

(define (prepare-models ctx)
  "Return the @code{(name . shape)} pairs the module is generated from.

Inline enumerations are named here, before anything is emitted, so that the
export list and the definitions agree on every name."
  (map (lambda (entry)
         (let* ((name (car entry))
                (symbol (identifier name))
                (shape (schema-shape (context-document ctx)
                                     (cdr entry)
                                     #:registry (context-registry ctx)
                                     #:named? #f)))
           (cons name
                 (if (object-shape? shape)
                     (replace-properties
                      shape
                      (name-property-enums ctx symbol (shape-properties shape)))
                     shape))))
       (registry-named-schemas (context-registry ctx))))

(define (schema-forms ctx name shape)
  "Return the forms a single named schema needs, in the order they are written."
  (case (shape-kind shape)
    ((object) (record-forms ctx shape name))
    ((enum) (named-enumeration-forms ctx shape name))
    ((list) (named-list-forms ctx shape name))
    ((map) (named-map-forms ctx shape name))
    ((union) (named-union-forms ctx shape name))
    ((scalar) (named-scalar-forms ctx shape name))
    (else
     (list (comment (format #f
                            "The schema ~a accepts any value, so no procedure is generated."
                            name))))))

(define (record-exports symbol shape)
  "Return the symbols a record module exports for one schema."
  (append
   (list (type-symbol symbol)
         (predicate-symbol symbol)
         (constructor-symbol symbol)
         (validator-symbol symbol)
         (valid-predicate-symbol symbol)
         (encoder-symbol symbol)
         (decoder-symbol symbol)
         (patch-symbol symbol))
   (map (lambda (property)
          (accessor-symbol symbol
                           (string->symbol (identifier (property-name property)))))
        (shape-properties shape))))

(define (enumeration-exports symbol shape)
  "Return the symbols an enumeration exports."
  (append (list (values-symbol symbol)
                (predicate-symbol symbol)
                (encoder-symbol symbol)
                (decoder-symbol symbol))
          (map (lambda (value)
                 (constant-symbol symbol value))
               (shape-values shape))))

(define (property-enumeration-exports ctx property)
  "Return the symbols the enumeration of @var{property} exports, if any."
  (let ((shape (property-shape property)))
    (if (and (enum-shape? shape)
             (shape-name shape)
             (not (component-schema? ctx (shape-name shape))))
        (enumeration-exports (shape-name shape) shape)
        '())))

(define (model-exports ctx name shape)
  "Return the symbols the models module exports for one schema."
  (let ((symbol (identifier name)))
    (case (shape-kind shape)
      ((object)
       (append (record-exports symbol shape)
               (append-map (lambda (property)
                             (property-enumeration-exports ctx property))
                           (shape-properties shape))))
      ((enum) (enumeration-exports symbol shape))
      (else
       (list (predicate-symbol symbol)
             (encoder-symbol symbol)
             (decoder-symbol symbol))))))

;;; Commentary:
;;; The module
;;
;; A generated file is three things: a banner that says where it came from, a
;; module header that says what it offers, and the forms of the schemas.  They are
;; returned as one list and rendered once, so the file has exactly the layout the
;; renderer gives a list of forms -- a blank line between them and a comment
;; directly above what it explains.

(define (wrap-comment text width)
  "Return @var{text} as a list of lines no longer than @var{width}.

A comment is rendered verbatim, so the banner has to be wrapped before it is
handed to the renderer; a generated file whose first line runs off the edge of the
console is not one a reader trusts."
  (if (string-null? text)
      (list "")
      (let loop ((words (string-split text #\space))
                 (current "")
                 (lines '()))
        (cond
          ((null? words)
           (reverse (cons current lines)))
          ((string-null? current)
           (loop (cdr words) (car words) lines))
          ((> (+ (string-length current) 1 (string-length (car words))) width)
           (loop (cdr words) (car words) (cons current lines)))
          (else
           (loop (cdr words)
                 (string-append current " " (car words))
                 lines))))))

(define (header-forms config spec)
  "Return the banner comment that opens the generated file.

@var{config} says which specification was read and by which generator, and the
banner repeats it in the file itself: a generated file that has been copied away
from its specification still says what it was generated from.  The mode line and
the paragraphs are one comment, because a comment attaches to the form below it and
a banner belongs to the file rather than to the module."
  (let ((paragraphs
         (list (format #f "Models generated from ~a"
                       (or (config-source config) "a specification"))
               (string-append
                "Generated by guile-openapi-generator "
                (config-generator-version config)
                ", from the "
                (spec-title spec)
                " "
                (spec-version spec)
                " specification.  Do not edit this file: regenerate it"
                " instead."))))
    (list (comment (string-join
                    (append (list "-*- mode: scheme; coding: utf-8 -*-" "")
                            (append-map (lambda (paragraph)
                                          (wrap-comment paragraph 74))
                                        paragraphs))
                    "\n"))
          (blank))))

(define (module-form name imports exports)
  "Return the @code{define-module} form of the generated file.

@var{name} is the module name as a list of symbols, @var{imports} the modules the
generated code calls into and @var{exports} the symbols a caller may use.  The
export list is what the rest of this file promises: a definition that is not in it
is an implementation detail of the module."
  (when (null? exports)
    (error "A generated module must export something"))
  (append (list 'define-module name)
          (append-map (lambda (importation)
                        (list #:use-module importation))
                      imports)
          (list #:export exports)))

(define (emit-models spec config)
  "Return the source text of the models module of @var{spec}.

The module name comes from @var{config}: with a root of @code{petstore} the
module is @code{(petstore models)} and the file is @code{petstore/models.scm}.

Every definition is built as a form and the file is rendered once, at the end, so
this procedure is the whole of the layout: banner, module header, the record
types, and then everything else."
  (let* ((ctx (make-context spec config))
         (models (prepare-models ctx))
         (exports (append-map (lambda (entry)
                                (model-exports ctx (car entry) (cdr entry)))
                              models)))
    (forms->string
     (append
      (header-forms config spec)
      (list (module-form (config-module config 'models)
                         (list (config-jso-module config)
                               (config-runtime-module config)
                               '(srfi srfi-1)
                               '(srfi srfi-9))
                         exports))
      ;; Record types first: a predicate referenced above its own
      ;; define-record-type compiles to the syntax, not to the procedure.
      (append-map (lambda (entry)
                    (if (object-shape? (cdr entry))
                        (list (record-type-form ctx (cdr entry) (car entry)))
                        '()))
                  models)
      (append-map (lambda (entry)
                    (schema-forms ctx (car entry) (cdr entry)))
                  models)))))