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
  #:use-module (openapi-generator emitter)
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
;;; Describing shapes in generated code

(define (schema-promoted-type shape)
  "Return the OpenAPI type of @var{shape} as a symbol, for check-type."
  (case (shape-kind shape)
    ((scalar enum) (or (shape-type shape) 'string))
    ((list) 'list)
    ((map) 'map)
    (else 'any)))

(define (schema-predicate ctx shape)
  "Return the source text of a predicate that recognises @var{shape}."
  (case (shape-kind shape)
    ((record enum union)
     (format #f "~a?" (identifier (component-name shape))))
    ((scalar)
     (case (or (shape-type shape) 'string)
       ((string) "string?")
       ((integer) "integer?")
       ((number) "number?")
       ((boolean) "boolean?")
       (else "(lambda (value) #t)")))
    ((list) "list?")
    ((map) "(lambda (value) (and (list? value) (every pair? value)))")
    (else "(lambda (value) #t)")))

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
  "Return a test that tells whether a jso node matches @var{shape}."
  (case (shape-kind shape)
    ((record)
     (let ((required (context-required-properties ctx (component-name shape))))
       (if (null? required)
           (format #f "(jso-object? ~a)" variable)
           (format #f "(and (jso-object? ~a)~{~a~})"
                   variable
                   (map (lambda (property)
                          (format #f " (jso-has? ~a ~s)" variable property))
                        required)))))
    ((list) (format #f "(vector? ~a)" variable))
    ((map union) (format #f "(jso-object? ~a)" variable))
    ((scalar enum)
     (case (or (shape-type shape) 'string)
       ((string) (format #f "(string? ~a)" variable))
       ((integer) (format #f "(integer? ~a)" variable))
       ((number) (format #f "(number? ~a)" variable))
       ((boolean) (format #f "(boolean? ~a)" variable))
       (else "#t")))
    (else (format #f "(jso-object? ~a)" variable))))

;;; Commentary:
;;; Conversion, in both directions

(define (converter-to shape)
  "Return the name of the procedure that decodes a jso into @var{shape}."
  (format #f "jso->~a" (identifier (component-name shape))))

(define (converter-from shape)
  "Return the name of the procedure that encodes @var{shape} as a jso value."
  (format #f "~a->jso" (identifier (component-name shape))))

(define* (unbox-expression ctx shape expression)
  "Return source that converts the jso @var{expression} into @var{shape}."
  (case (shape-kind shape)
    ((union)
     (union-unbox ctx shape expression))
    ((record enum)
     (format #f "(~a ~a)" (converter-to shape) expression))
    ((list)
     (format #f
             (string-append
              "(let ((value ~a))"
              " (if (or (eq? value #f) (json-null? value)) #f"
              " (map (lambda (element) ~a) (jso-array->list value))))")
             expression
             (unbox-expression ctx (shape-items shape) "element")))
    ((map)
     (format #f
             (string-append
              "(let ((value ~a))"
              " (if (or (eq? value #f) (json-null? value)) #f"
              " (map (lambda (entry) (cons (car entry) ~a)) value)))")
             expression
             (unbox-expression ctx (shape-items shape) "(cdr entry)")))
    ((scalar)
     (format #f "(let ((value ~a)) (if (json-null? value) #f value))" expression))
    (else expression)))

(define (find-branch shape name)
  "Return the branch of the union @var{shape} called @var{name}, if any."
  (find (lambda (branch)
          (and (not (eq? (shape-kind branch) 'scalar))
               (string=? (component-name branch) name)))
        (shape-branches shape)))

(define (union-unbox ctx shape expression)
  "Return source that decodes @var{expression} as a union.

A discriminated union dispatches on its discriminator property, which is exact;
an undiscriminated one tries the branch tests in declaration order."
  (let ((discriminator (shape-discriminator shape))
        (branches (shape-branches shape))
        (values (shape-discriminator-values shape)))
    (cond
      ((and discriminator (pair? values))
       (format #f
               "(cond~{ ~a~} (else (error \"no discriminator value in the value:\" ~a)))"
               (map (lambda (entry)
                      (let ((branch (find-branch shape (cdr entry))))
                        (if branch
                            (format #f "((string=? (jso-ref ~a ~s) ~s) (~a ~a))"
                                    expression
                                    discriminator
                                    (car entry)
                                    (converter-to branch)
                                    expression)
                            (format #f "#f"))))
                    values)
               expression))
      (else
       (format #f
               "(cond~{ ~a~} (else (error \"no union member matches\" ~a)))"
               (map (lambda (branch)
                      (format #f "~a ~a"
                              (union-branch-test ctx branch expression)
                              (unbox-expression ctx branch expression)))
                    branches)
               expression)))))

(define* (box-expression ctx shape expression #:optional (optional? #f))
  "Return source that converts the Scheme @var{expression} into a jso value.

When @var{optional?} is true, @code{#f} becomes @code{json-null}, which is how an
absent member is written."
  (let ((encoded
         (case (shape-kind shape)
           ((union)
            (format #f "(cond ~{~a~} (else (validation-failure '(\"union\") \"value matches no member of the union\")))"
                    (map (lambda (branch)
                           (format #f "((~a) ~a)"
                                   (schema-predicate ctx branch)
                                   (box-expression ctx branch expression)))
                         (shape-branches shape))))
           ((record enum)
            (format #f "(~a ~a)" (converter-from shape) expression))
           ((list)
            (format #f "(list->vector (map (lambda (element) ~a) ~a))"
                    (box-expression ctx (shape-items shape) "element")
                    expression))
           ((map)
            (format #f "(map (lambda (entry) (cons (car entry) ~a)) ~a)"
                    (box-expression ctx (shape-items shape) "(cdr entry)")
                    expression))
           (else expression))))
    (if optional?
        (format #f "(if (eq? ~a #f) json-null ~a)" expression encoded)
        encoded)))

;;; Commentary:
;;; Validation

(define (schema-checks ctx shape path expression)
  "Return the validation lines for one value of @var{shape}.

The lines assume the value is present: absence is the caller's business, since
only the caller knows whether the field is required."
  (let ((document (context-document ctx)))
    (append
     (case (shape-kind shape)
       ((record)
        (list (format #f "(check-predicate ~a ~a ~a ~s)"
                      path
                      expression
                      (schema-predicate ctx shape)
                      (schema-display-name shape))))
       ((enum)
        (list (format #f "(check-type ~a ~a '~a)"
                      path expression (schema-promoted-type shape))
              (format #f "(check-enum ~a ~a ~a)"
                      path expression (enum-values-expression shape))))
       ((list)
        (append
         (list (format #f "(check-type ~a ~a 'list)" path expression))
         (if (any-shape? (shape-items shape))
             '()
             (list (format #f "(check-list ~a ~a ~a ~s)"
                           path
                           expression
                           (schema-predicate ctx (shape-items shape))
                           (schema-display-name (shape-items shape)))))
         (list (format #f "(check-unique-items ~a ~a)" path expression))))
       ((map)
        (append
         (list (format #f "(check-type ~a ~a 'map)" path expression))
         (if (any-shape? (shape-items shape))
             '()
             (list (format #f "(check-map ~a ~a ~a ~s)"
                           path
                           expression
                           (schema-predicate ctx (shape-items shape))
                           (schema-display-name (shape-items shape)))))))
       ((union)
        (list (format #f "(check-predicate ~a ~a ~a ~s)"
                      path
                      expression
                      (schema-predicate ctx shape)
                      (schema-display-name shape))))
       (else
        (list (format #f "(check-type ~a ~a '~a)"
                      path expression (schema-promoted-type shape)))))
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
               (list (format #f "(check-min-length ~a ~a ~a)"
                             path expression minimum))
               '())
           (if maximum
               (list (format #f "(check-max-length ~a ~a ~a)"
                             path expression maximum))
               '())
           (if (and pattern (not (string=? pattern "^.*$")))
               (list (format #f "(check-pattern ~a ~a ~s)"
                             path expression pattern))
               '())
           (if (and format-name
                    (memq (string->symbol format-name)
                          '(date date-time time uuid email hostname ipv4 uri)))
               (list (format #f "(check-format ~a ~a '~a)"
                             path expression (string->symbol format-name)))
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
               (list (format #f "(check-minimum ~a ~a ~a)" path expression minimum))
               '())
           (if maximum
               (list (format #f "(check-maximum ~a ~a ~a)" path expression maximum))
               '())
           (if (number? exclusive-minimum)
               (list (format #f "(check-exclusive-minimum ~a ~a ~a)"
                             path expression exclusive-minimum))
               '())
           (if (number? exclusive-maximum)
               (list (format #f "(check-exclusive-maximum ~a ~a ~a)"
                             path expression exclusive-maximum))
               '())
           (if multiple
               (list (format #f "(check-multiple-of ~a ~a ~a)"
                             path expression multiple))
               '())))
        '())))

(define (list-constraints document shape path expression)
  (let ((schema (shape-schema shape)))
    (if (and schema (eq? (shape-kind shape) 'list))
        (let ((minimum (schema-min-items document schema))
              (maximum (schema-max-items document schema)))
          (append
           (if minimum
               (list (format #f "(check-min-items ~a ~a ~a)"
                             path expression minimum))
               '())
           (if maximum
               (list (format #f "(check-max-items ~a ~a ~a)"
                             path expression maximum))
               '())))
        '())))

;;; Commentary:
;;; Enumerations

(define (enum-values-expression shape)
  "Return the source of the list of allowed values of an enumeration."
  (if (shape-name shape)
      (format #f "~a-values" (identifier (shape-name shape)))
      (format #f "'~s" (shape-values shape))))

(define (emit-enumeration ctx shape base title)
  "Emit the list of values, one constant per value and the predicate."
  (emit-wrapped (format #f "(define ~a-values (list" base)
                (map (lambda (value) (format #f "~s" value))
                     (shape-values shape))
                "))")
  (emit-blank)
  (for-each (lambda (value)
              (emit "(define ~a ~s)"
                    (identifier (format #f "~a-~a" base value))
                    value))
            (shape-values shape))
  (emit-blank)
  (emit "(define (~a? value)" (identifier base))
  (with-indent
   (lambda ()
     (emit-docstring
      (format #f "Return #t when VALUE is one of the ~a values." title))
     (emit "(and (~a? value) (if (member value ~a-values) #t #f))"
           (case (or (shape-type shape) 'string)
             ((integer) "integer")
             ((number) "number")
             (else "string"))
           base)))
  (emit-close))

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
  "Return Scheme source for @var{value}, a jso value used as a default."
  (cond
    ((json-null? value) "#f")
    ((string? value) (scheme-string value))
    ((boolean? value) (if value "#t" "#f"))
    ((number? value) (format #f "~a" value))
    ((vector? value)
     (format #f "(list ~{~a~^ ~})" (map jso-literal (vector->list value))))
    (else "#f")))

(define (field-default ctx property)
  "Return the source of the constructor default of @var{property}.

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
       "#f")
      ((and declared (not (json-null? declared)) (not (vector? declared)))
       (jso-literal declared))
      ((and (list-shape? shape)
            (not (schema-min-items document schema)))
       "'()")
      ((and (map-shape? shape)
            (not (schema-min-properties document schema)))
       "'()")
      (else "#f"))))

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

(define (emit-record-type ctx shape name)
  "Emit only the @code{define-record-type} form of @var{shape}.

Record types are emitted before anything that refers to their predicate: a
reference to a record predicate written above its @code{define-record-type} is
compiled as a reference to the @code{define-record-type} syntax itself, which is
a syntax transformer rather than a procedure."
  (let* ((symbol (identifier name))
         (predicate (format #f "~a?" symbol))
         (raw-constructor (string-append "%make-" symbol))
         (fields (record-fields ctx symbol shape))
         (field-names (map (lambda (property)
                             (identifier (property-name property)))
                           fields))
         (extra-schema (record-extra-schema shape))
         (all-fields (if extra-schema
                         (append field-names '("extra"))
                         field-names)))
    (emit "(define-record-type <~a>" symbol)
    (with-indent
     (lambda ()
       (emit "(~a~{ ~a~})" raw-constructor all-fields)
       (emit "~a" predicate)
       (for-each (lambda (field)
                   (emit "(~a ~a-~a)" field symbol field))
                 field-names)
       (when extra-schema
         (emit "(extra ~a-extra)" symbol))))
    (emit-close)
    (emit-blank)))

(define (emit-record ctx shape name)
  "Emit the constructor, validator and converters of the record @var{name}."
  (let* ((symbol (identifier name))
         (predicate (format #f "~a?" symbol))
         (constructor (format #f "make-~a" symbol))
         (raw-constructor (string-append "%make-" symbol))
         (fields (record-fields ctx symbol shape))
         (field-names (map (lambda (property)
                             (identifier (property-name property)))
                           fields))
         (extra-schema (record-extra-schema shape))
         (all-fields (if extra-schema
                         (append field-names '("extra"))
                         field-names)))
    ;; The constructor
    ;; The constructor
    (if (null? all-fields)
        (emit "(define* (~a)" constructor)
        (emit-wrapped (format #f "(define* (~a" constructor)
                      (append (list "#:key")
                              (map (lambda (field property)
                                     (format #f "(~a ~a)"
                                             field
                                             (field-default ctx property)))
                                   field-names
                                   fields)
                              (if extra-schema (list "(extra '())") '()))
                      ")"))
    (with-indent
     (lambda ()
       (emit-docstring (record-docstring shape name constructor fields))
       (emit-wrapped (format #f "(let ((value (~a" raw-constructor)
                     all-fields
                     ")))")
       (with-indent
        (lambda ()
          (emit "(validate-~a value)" symbol)
          (emit "value")))
       (emit-close)))
    (emit-close)
    (emit-blank)
    ;; The validator
    (emit "(define (validate-~a value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f
                (string-append
                 "Check VALUE against the constraints the specification declares"
                 " for ~a.~%~%Returns #t, or throws 'openapi-validation-error"
                 " naming the field at fault.")
                name))
       (emit "(check-predicate '(\"~a\") value ~a \"~a\")" symbol predicate name)
       (for-each (lambda (field property)
                   (emit-property-checks ctx symbol field property))
                 field-names
                 fields)
       (emit "#t")))
    (emit-close)
    (emit-blank)
    ;; Validity predicate
    (emit "(define (~a-valid? value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring (format #f "Return #t when VALUE is a valid ~a." name))
       (emit "(valid? (lambda () (validate-~a value)))" symbol)))
    (emit-close)
    (emit-blank)
    ;; Encoder and decoder
    (for-each emit-raw
              (string-split (string-trim-right
                             (record-converters-source ctx
                                                       symbol
                                                       predicate
                                                       name
                                                       constructor
                                                       fields
                                                       field-names
                                                       extra-schema))
                            #\newline))
    (emit-blank)
    ;; JSON merge patch
    (emit "(define* (~a-apply-patch value patch)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f
                (string-append
                 "Return VALUE with the RFC 7386 JSON merge patch PATCH applied."
                 "~%Members of PATCH set to null are removed, and the result is"
                 " validated~%like any other ~a, so a patch that would leave the"
                 " record invalid raises.")
                name))
       (emit "(jso->~a (json-merge-patch (~a->jso value) patch))" symbol symbol)))
    (emit-close)
    (emit-blank)
    ;; Enumerated properties
    (for-each (lambda (property)
                (emit-property-enumeration ctx symbol property))
              fields)))
(define (record-encoder-source ctx symbol predicate name fields field-names
                               extra-schema)
  "Return the source of the procedure that encodes a record as a jso object.

An optional member that holds @code{#f} is left out rather than written as null,
so a request body only carries the members the caller set; a nullable member is
written as null when it holds @code{#f}, because that is what the specification
says null means for it."
  (let* ((members
          (map (lambda (property field)
                 (let* ((shape (property-shape property))
                        (accessor (format #f "(~a-~a value)" symbol field))
                        (required? (property-required? property))
                        (nullable? (shape-nullable? shape)))
                   (cond
                     ((or required? nullable?)
                      (format #f "    (list (cons ~s ~a))"
                              (property-name property)
                              (box-expression ctx shape accessor
                                              (not required?))))
                     (else
                      (format #f
                              (string-append
                               "    (if (eq? ~a #f)"
                               "~%        '()"
                               "~%        (list (cons ~s ~a)))")
                              accessor
                              (property-name property)
                              (box-expression ctx shape accessor #f))))))
               fields
               field-names))
         (extra (if extra-schema
                    (format #f "    (list (cons \"additionalProperties\" extra))")
                    ""))
         (body (if (null? fields)
                   "  (list)"
                   (format #f "  (append~%~{~a~%~}~a)"
                           members
                           extra))))
    (format #f
            "(define (~a->jso value)~%  ~s~%  (check-predicate '(\"~a\") value ~a ~s)~%~a)"
            symbol
            "Return VALUE as a jso object, ready for JSON serialisation."
            symbol
            predicate
            name
            body)))

(define (record-decoder-source ctx symbol name constructor fields field-names
                               extra-schema)
  "Return the source of the procedure that decodes a jso object into a record."
  (let* ((arguments
          (map (lambda (property field)
                 (format #f "       #:~a ~a"
                         field
                         (unbox-expression
                          ctx
                          (property-shape property)
                          (format #f "(jso-ref node ~s)" (property-name property)))))
               fields
               field-names))
         (extra (if extra-schema
                    (format #f "~%       #:extra ~a"
                            (extra-decode ctx extra-schema field-names))
                    ""))
         (call (if (null? fields)
                   (format #f "      (~a)" constructor)
                   (format #f "      (~a~%~{~a~%~}~a)"
                           constructor
                           arguments
                           extra))))
    (format #f
            (string-append
             "(define (jso->~a node)~%  ~s~%"
             "  (if (or (json-null? node) (not (jso-object? node)))~%"
             "      #f~%"
             "~a))")
            symbol
            (format #f "Build a ~a from NODE, a jso object." name)
            call)))

(define (record-converters-source ctx symbol predicate name constructor fields
                                  field-names extra-schema)
  "Return the source of the two converters of the record @var{symbol}."
  (string-append (record-encoder-source ctx symbol predicate name fields
                                        field-names extra-schema)
                 "\n\n"
                 (record-decoder-source ctx symbol name constructor fields
                                        field-names extra-schema)))

(define (extra-decode ctx extra-schema known-fields)
  "Return source that decodes the undeclared members of a jso object."
  (let ((shape (schema-shape (context-document ctx)
                             extra-schema
                             #:registry (context-registry ctx))))
    (format #f
            (string-append "(map (lambda (entry) (cons (car entry) ~a))"
                           " (filter (lambda (entry) (not (member (car entry) '~s)))"
                           " node))")
            (unbox-expression ctx shape "(cdr entry)")
            known-fields)))

(define (boolean-shape? shape)
  "Return @code{#t} when @var{shape} is a boolean.

A required boolean accepts @code{#f}, so it is the one required property that is
not checked for presence: the value that means false is also the value that would
mean missing."
  (and (scalar-shape? shape)
       (eq? (shape-type shape) 'boolean)))

(define (emit-property-checks ctx symbol field property)
  "Emit the validation lines of one property of a record."
  (let* ((path (format #f "'(\"~a\" ~s)" symbol (property-name property)))
         (expression (format #f "(~a-~a value)" symbol field))
         (shape (property-shape property))
         (required? (and (property-required? property)
                         (not (boolean-shape? shape))))
         (checks (schema-checks ctx shape path expression)))
    (when required?
      (emit "(check-required ~a ~a)" path expression))
    (when (pair? checks)
      (if (or (not required?) (shape-nullable? shape))
          (begin
            (emit "(when ~a" expression)
            (with-indent
             (lambda ()
               (for-each (lambda (line) (emit-raw line)) checks)))
            (emit ")"))
          (for-each (lambda (line) (emit-raw line)) checks)))))

(define (emit-property-enumeration ctx symbol property)
  "Emit the constants and predicate of an enumerated property."
  (let ((shape (property-shape property)))
    (when (and (enum-shape? shape)
               (shape-name shape)
               (not (component-schema? ctx (shape-name shape))))
      (let ((base (shape-name shape))
            (title (format #f "~a.~a" symbol (property-name property))))
        (emit-enumeration ctx shape base title)
        (emit-blank)
        (emit-enumeration-converters ctx shape base title)
        (emit-blank)))))

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
  "Return the docstring of the constructor of @var{shape}."
  (let* ((schema (shape-schema shape))
         (description (and schema (jso-ref schema "description")))
         (title (and schema (jso-ref schema "title")))
         (summary (cond
                    ((and (string? description) (not (string-null? description)))
                     description)
                    ((and (string? title) (not (string-null? title)))
                     title)
                    (else (format #f "A ~a." name)))))
    (string-append
     (format #f "Create a ~a." name)
     (if (string-null? (string-trim-both summary))
         ""
         (format #f "~%~%~a" (string-trim-both summary)))
     (if (null? properties)
         ""
         (format #f "~%~%Fields: ~{~a~^, ~}."
                 (map (lambda (property)
                        (format #f "~a (~a~a)"
                                (property-name property)
                                (schema-display-name (property-shape property))
                                (if (property-required? property)
                                    ", required"
                                    "")))
                      properties)))
     (format #f "~%~%@example~%(~a~a)~%@end example"
             constructor
             (if (null? properties)
                 ""
                 (format #f " ~{~a~^ ~}"
                         (map (lambda (property)
                                (format #f "#:~a <~a>"
                                        (identifier (property-name property))
                                        (schema-display-name
                                         (property-shape property))))
                              properties)))))))

;;; Commentary:
;;; Named schemas that are not objects

(define (emit-enumeration-converters ctx shape symbol name)
  "Emit the two converters of the enumeration @var{name}."
  (emit "(define (~a->jso value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring "Return VALUE as a jso scalar, after checking it.")
       (emit "(if (~a? value)" symbol)
       (with-indent (lambda () (emit "value")))
     (with-indent
      (lambda ()
        (emit "(validation-failure '(\"~a\") \"must be one of ~a-values, got ~~s\" value)"
              symbol symbol)))
     (emit-close)))
  (emit-close)
  (emit-blank)
  (emit "(define (jso->~a node)" symbol)
  (with-indent
   (lambda ()
     (emit-docstring (format #f "Read a ~a from the jso scalar NODE." name))
     (emit "(if (json-null? node) #f node)")))
  (emit-close))

(define (emit-named-enumeration ctx shape name)
  "Emit the enumeration, its predicate and its converters."
  (let ((symbol (identifier name)))
    (emit-enumeration ctx shape symbol name)
    (emit-blank)
    (emit-enumeration-converters ctx shape symbol name)))

(define (emit-validating-predicate ctx shape symbol doc)
  "Emit a predicate that validates @var{shape} by running its checks."
  (emit "(define (~a? value)" symbol)
  (with-indent
   (lambda ()
     (emit-docstring doc)
     (emit "(valid? (lambda ()")
     (with-indent
      (lambda ()
        (for-each emit-raw
                  (schema-checks ctx shape
                                 (format #f "'(\"~a\")" symbol)
                                 "value"))
        (emit "#t")))
     (emit-close)
     (emit-close)))
  (emit-close))

(define (emit-scalar-converters symbol name)
  "Emit the two identity-ish converters of a named scalar."
  (emit "(define (~a->jso value)" symbol)
  (with-indent
   (lambda ()
     (emit-docstring "Return VALUE as a jso scalar.")
     (emit "value")))
  (emit-close)
  (emit-blank)
  (emit "(define (jso->~a node)" symbol)
  (with-indent
   (lambda ()
     (emit-docstring (format #f "Read a ~a from the jso scalar NODE." name))
     (emit "(if (json-null? node) #f node)")))
  (emit-close))

(define (emit-named-scalar ctx shape name)
  (let ((symbol (identifier name)))
    (emit-validating-predicate
     ctx shape symbol
     (format #f "Return #t when VALUE is a valid ~a." (schema-display-name shape)))
    (emit-blank)
    (emit-scalar-converters symbol name)))

(define (emit-named-list ctx shape name)
  (let* ((symbol (identifier name))
         (item (shape-items shape)))
    (emit "(define (~a? value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f "Return #t when VALUE is a list of ~a."
                (schema-display-name item)))
       (emit "(and (list? value) (every ~a value))" (schema-predicate ctx item))))
    (emit-close)
    (emit-blank)
    (emit "(define (~a->jso value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring "Return VALUE as a jso array.")
       (emit "(list->vector (map (lambda (element) ~a) value))"
             (box-expression ctx item "element"))))
    (emit-close)
    (emit-blank)
    (emit "(define (jso->~a node)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f "Read a list of ~a from NODE." (schema-display-name item)))
       (emit "(map (lambda (element) ~a) (jso-array->list node))"
             (unbox-expression ctx item "element"))))
    (emit-close)))

(define (emit-named-map ctx shape name)
  (let* ((symbol (identifier name))
         (value-shape (shape-items shape)))
    (emit "(define (~a? value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f "Return #t when VALUE maps strings to ~a."
                (schema-display-name value-shape)))
       (if (any-shape? value-shape)
           (emit "(and (list? value) (every pair? value))")
           (emit "(and (list? value) (every pair? value)~%       (every (lambda (entry) (~a (cdr entry))) value))"
                 (schema-predicate ctx value-shape)))))
    (emit-close)
    (emit-blank)
    (emit "(define (~a->jso value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring "Return VALUE as a jso object.")
       (emit "(map (lambda (entry) (cons (car entry) ~a)) value)"
             (box-expression ctx value-shape "(cdr entry)"))))
    (emit-close)
    (emit-blank)
    (emit "(define (jso->~a node)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f "Read a map counting ~a from NODE." 
                (schema-display-name value-shape)))
       (emit "(if (jso-object? node)")
       (with-indent
        (lambda ()
          (emit "(map (lambda (entry) (cons (car entry) ~a)) node)"
                (unbox-expression ctx value-shape "(cdr entry)"))))
       (with-indent (lambda () (emit "'()")))
       (emit-close)))
    (emit-close)))

(define (emit-named-union ctx shape name)
  (let* ((symbol (identifier name))
         (branches (shape-branches shape)))
    (emit "(define (~a? value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring
        (format #f "Return #t when VALUE is one of the shapes ~a accepts." name))
       (emit "(or ~{~a~^ ~})"
             (map (lambda (branch)
                    (format #f "(~a value)" (schema-predicate ctx branch)))
                  branches))))
    (emit-close)
    (emit-blank)
    (emit "(define (~a->jso value)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring "Return VALUE as a jso value.")
       (emit "(cond")
       (with-indent
        (lambda ()
          (for-each (lambda (branch)
                      (emit "((~a value) ~a)"
                            (schema-predicate ctx branch)
                            (box-expression ctx branch "value")))
                    branches)
          (emit "(else (validation-failure '(\"~a\") \"must be one of ~a\" value))"
                symbol symbol)))
       (emit-close)))
    (emit-close)
    (emit-blank)
    (emit "(define (jso->~a node)" symbol)
    (with-indent
     (lambda ()
       (emit-docstring "Read the union value NODE.")
       (emit-raw (unbox-expression ctx shape "node"))))
    (emit-close)))
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

(define (emit-schema ctx name shape)
  "Emit everything a single named schema needs."
  (case (shape-kind shape)
    ((object) (emit-record ctx shape name))
    ((enum) (emit-named-enumeration ctx shape name))
    ((list) (emit-named-list ctx shape name))
    ((map) (emit-named-map ctx shape name))
    ((union) (emit-named-union ctx shape name))
    ((scalar) (emit-named-scalar ctx shape name))
    (else
     (emit-comment
      "The schema ~a accepts any value, so no procedure is generated." name))))

(define (record-exports symbol shape)
  "Return the symbols a record module exports for one schema."
  (append
   (list (string->symbol (format #f "<~a>" symbol))
         (string->symbol (format #f "~a?" symbol))
         (string->symbol (format #f "make-~a" symbol))
         (string->symbol (format #f "validate-~a" symbol))
         (string->symbol (format #f "~a-valid?" symbol))
         (string->symbol (format #f "~a->jso" symbol))
         (string->symbol (format #f "jso->~a" symbol))
         (string->symbol (format #f "~a-apply-patch" symbol)))
   (map (lambda (property)
          (string->symbol (format #f "~a-~a"
                                  symbol
                                  (identifier (property-name property)))))
        (shape-properties shape))))

(define (enumeration-exports symbol shape)
  "Return the symbols an enumeration exports."
  (append (list (string->symbol (format #f "~a-values" symbol))
                (string->symbol (format #f "~a?" symbol))
                (string->symbol (format #f "~a->jso" symbol))
                (string->symbol (format #f "jso->~a" symbol)))
          (map (lambda (value)
                 (string->symbol
                  (identifier (format #f "~a-~a" symbol value))))
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
       (list (string->symbol (format #f "~a?" symbol))
             (string->symbol (format #f "~a->jso" symbol))
             (string->symbol (format #f "jso->~a" symbol)))))))

(define (emit-models spec config)
  "Return the source text of the models module of @var{spec}.

The module name comes from @var{config}: with a root of @code{petstore} the
module is @code{(petstore models)} and the file is @code{petstore/models.scm}."
  (let* ((ctx (make-context spec config))
         (models (prepare-models ctx))
         (exports (append-map (lambda (entry)
                                (model-exports ctx (car entry) (cdr entry)))
                              models)))
    (code->string
     (lambda ()
       (emit-header
        (list (format #f "Models generated from ~a"
                      (or (config-source config) "a specification"))
              (format #f
                      (string-append
                       "Generated by guile-openapi-generator ~a, from the ~a ~a"
                       " specification.  Do not edit this file: regenerate it"
                       " instead.")
                      (config-generator-version config)
                      (spec-title spec)
                      (spec-version spec))))
       (emit-module (config-module config 'models)
                    (list (config-jso-module config)
                          (config-runtime-module config)
                          '(srfi srfi-1)
                          '(srfi srfi-9))
                    '()
                    exports)
       (emit-blank)
       ;; Record types first: a predicate referenced above its own
       ;; define-record-type compiles to the syntax, not to the procedure.
       (for-each (lambda (entry)
                   (let ((shape (cdr entry)))
                     (when (object-shape? shape)
                       (emit-record-type ctx shape (car entry)))))
                 models)
       (for-each (lambda (entry)
                   (emit-schema ctx (car entry) (cdr entry)))
                 models)))))