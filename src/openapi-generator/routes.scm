;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The other direction: routes written in Scheme, described as OpenAPI.
;;
;; A route is a method, a path template and what the operation takes and
;; answers.  That is all it takes to write a specification, and it is small
;; enough to write by hand:
;;
;; @example
;; (use-modules (openapi-generator routes))
;;
;; (define routes
;;   (list (route 'get \"/pets/{petId}\"
;;                #:operation-id \"getPet\"
;;                #:tags '(\"pets\")
;;                #:parameters (list (parameter \"petId\" #:in 'path #:type 'integer))
;;                #:responses (list (response 200 #:ref \"#/components/schemas/Pet\")))))
;;
;; (routes->yaml routes #:title \"Pet store\" #:version \"1.0.0\")
;; @end example
;;
;; The module fills in what follows from the route itself and refuses to guess
;; the rest: a @code{{placeholder}} in the path becomes a required string path
;; parameter, an operation with no @code{operationId} gets one derived from its
;; method and path, and an operation that declares no response gets a
;; @code{200 OK}.  What it cannot know — the schemas, the descriptions, the
;; servers — is a keyword, not a silent omission.
;;
;; Two readers build these routes out of existing code, so an application that
;; predates its specification can grow one:
;; @code{(openapi-generator guile-routes)} reads a plain Guile route table and
;; @code{(openapi-generator artanis-routes)} reads Artanis route declarations.

(define-module (openapi-generator routes)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module ((openapi-generator naming)
               #:prefix naming:)
  #:use-module (openapi-generator yaml-writer)
  #:export (;; routes
            <route> route?
            route-method
            route-path
            route-operation-id
            route-summary
            route-description
            route-tags
            route-deprecated?
            route-parameters
            route-request-body
            route-responses
            route-security
            route-extensions
            route
            ;; parameters
            <route-parameter> route-parameter?
            route-parameter-name
            route-parameter-location
            route-parameter-required?
            route-parameter-description
            route-parameter-schema
            route-parameter-deprecated?
            parameter
            ;; request bodies
            <route-body> route-body?
            route-body-required?
            route-body-description
            route-body-media-type
            route-body-schema
            request-body
            ;; responses
            <route-response> route-response?
            route-response-status
            route-response-description
            route-response-media-type
            route-response-schema
            route-response-headers
            response
            ;; answers
            route-methods
            normalize-route-path
            path-parameter-names
            derived-operation-id
            routes->document
            routes->yaml
            routes->yaml-file))

;;; Commentary:
;;; Records

(define-record-type <route>
  (make-route method
              path
              operation-id
              summary
              description
              tags
              deprecated?
              parameters
              request-body
              responses
              security
              extensions)
  route?
  (method route-method)
  (path route-path)
  (operation-id route-operation-id)
  (summary route-summary)
  (description route-description)
  (tags route-tags)
  (deprecated? route-deprecated?)
  (parameters route-parameters)
  (request-body route-request-body)
  (responses route-responses)
  (security route-security)
  (extensions route-extensions))

(define-record-type <route-parameter>
  (make-route-parameter name location required? description schema deprecated?)
  route-parameter?
  (name route-parameter-name)
  (location route-parameter-location)
  (required? route-parameter-required?)
  (description route-parameter-description)
  (schema route-parameter-schema)
  (deprecated? route-parameter-deprecated?))

(define-record-type <route-body>
  (make-route-body required? description media-type schema)
  route-body?
  (required? route-body-required?)
  (description route-body-description)
  (media-type route-body-media-type)
  (schema route-body-schema))

(define-record-type <route-response>
  (make-route-response status description media-type schema headers)
  route-response?
  (status route-response-status)
  (description route-response-description)
  (media-type route-response-media-type)
  (schema route-response-schema)
  (headers route-response-headers))

;;; Commentary:
;;; Constructors
;;
;; Each constructor takes the few members that are worth typing and fills in
;; the rest from the specification's own rules, so a route that says only what
;; it means still turns into a document a validator accepts.

(define route-methods
  '(get put post delete options head patch trace))

(define parameter-locations
  '(path query header cookie))

(define (name-text value)
  "Return @var{value}, a string or a symbol, as a string."
  (cond
    ((string? value) value)
    ((symbol? value) (symbol->string value))
    (else (format #f "~a" value))))

(define (status-text status)
  "Return @var{status}, a response status, as the text a specification uses."
  (cond
    ((string? status) status)
    ((number? status) (number->string status))
    (else (name-text status))))

(define (normalize-method method)
  "Return @var{method} as one of @code{route-methods}, or raise."
  (let ((symbol (string->symbol (string-downcase (name-text method)))))
    (unless (memq symbol route-methods)
      (error "A route needs one of the HTTP methods" method))
    symbol))

(define (normalize-location location)
  "Return @var{location} as one of @code{parameter-locations}, or raise."
  (let ((symbol (string->symbol (string-downcase (name-text location)))))
    (unless (memq symbol parameter-locations)
      (error "A parameter lives in path, query, header or cookie" location))
    symbol))

(define (normalize-route-path path)
  "Return @var{path} as an OpenAPI path template.

A path that does not start with a slash gains one, and a trailing slash is
dropped, because @code{/pets} and @code{/pets/} are the same route and only one
of them can be the key of a path item.  The Artanis spelling of a parameter,
@code{:petId}, becomes the OpenAPI one, @code{{petId}}.

@example
(normalize-route-path \"pets/:petId\")  ;; => \"/pets/{petId}\"
@end example"
  (let* ((text (name-text path))
         (text (if (string-prefix? "/" text) text (string-append "/" text)))
         (segments (map (lambda (segment)
                          (if (and (string-prefix? ":" segment)
                                   (> (string-length segment) 1))
                              (string-append "{"
                                             (substring segment 1
                                                        (string-length segment))
                                             "}")
                              segment))
                        (string-split text #\/)))
         (text (string-join segments "/")))
    (if (and (> (string-length text) 1)
             (string-suffix? "/" text))
        (substring text 0 (- (string-length text) 1))
        text)))

(define (path-parameter-names path)
  "Return the names of the @code{{placeholders}} of @var{path}, in order."
  (let loop ((segments (string-split path #\/)) (names '()))
    (cond
      ((null? segments) (reverse names))
      ((and (string-prefix? "{" (car segments))
            (string-suffix? "}" (car segments)))
       (loop (cdr segments)
             (cons (substring (car segments)
                              1
                              (- (string-length (car segments)) 1))
                   names)))
      (else (loop (cdr segments) names)))))

(define (derived-operation-id method path)
  "Return the operationId a route with @var{method} and @var{path} gets by default.

The derivation is the one the generator uses to name procedures, in camel case
because an operationId is a name, not a Scheme identifier.

@example
(derived-operation-id 'get \"/pets/{petId}\")  ;; => \"getPetsByPetId\"
@end example"
  (naming:to-camel (naming:operation-procedure-name method path #f)))

(define* (schema-value #:key schema type ref)
  "Return a schema jso built from the first of @var{schema}, @var{ref} and @var{type}.

@var{schema} is already a jso value and is used as it is; @var{ref} becomes a
@code{$ref}; @var{type} becomes a @code{type} member.  When none is given the
result is @code{#f}, so a caller can tell \"no value\" from \"any value\"."
  (cond
    ((jso-object? schema) schema)
    ((and (string? ref) (not (string-null? ref)))
     (list (cons "$ref" ref)))
    (type (list (cons "type" (string-downcase (name-text type)))))
    (else #f)))

(define* (parameter name
                    #:key
                    (in 'query)
                    required?
                    description
                    schema
                    type
                    ref
                    (deprecated? #f))
  "Describe one parameter of a route.

A parameter in the @code{path} is always required, because the route cannot be
called without it, and a parameter with no schema becomes a string.

@example
(parameter \"limit\" #:in 'query #:type 'integer)
(parameter \"petId\" #:in 'path #:type 'integer)
@end example"
  (let ((location (normalize-location in)))
    (make-route-parameter (name-text name)
                          location
                          (or (and required? #t) (eq? location 'path))
                          description
                          (or (schema-value #:schema schema #:type type #:ref ref)
                              (list (cons "type" "string")))
                          (and deprecated? #t))))

(define* (request-body #:key
                       schema
                       type
                       ref
                       (media-type "application/json")
                       (required? #f)
                       description)
  "Describe the request body of a route.

A body whose shape is unknown is an empty schema, not an absent one: the media
type says there is a body, and an empty schema says only that anything may be
in it, which is exactly what is known.

@example
(request-body #:ref \"#/components/schemas/NewPet\" #:required? #t)
@end example"
  (make-route-body (and required? #t)
                   description
                   (name-text media-type)
                   (or (schema-value #:schema schema #:type type #:ref ref)
                       '())))

(define status-descriptions
  '(("200" . "OK")
    ("201" . "Created")
    ("202" . "Accepted")
    ("204" . "No Content")
    ("301" . "Moved Permanently")
    ("304" . "Not Modified")
    ("400" . "Bad Request")
    ("401" . "Unauthorized")
    ("403" . "Forbidden")
    ("404" . "Not Found")
    ("409" . "Conflict")
    ("422" . "Unprocessable Entity")
    ("500" . "Internal Server Error")))

(define* (response status
                   #:key
                   description
                   schema
                   type
                   ref
                   (media-type "application/json")
                   headers)
  "Describe one response of a route.

A response with no schema has no content: a @code{204} is a status and a
description, and writing an empty JSON body would be a lie.  When no
description is given, the reason phrase of the status is used.

@example
(response 200 #:ref \"#/components/schemas/Pet\")
(response 404 #:description \"No pet has that id\")
@end example"
  (let ((schema (schema-value #:schema schema #:type type #:ref ref))
        (status (status-text status)))
    (make-route-response status
                         (or description
                             (assoc-ref status-descriptions status)
                             "Response")
                         (and schema (name-text media-type))
                         schema
                         headers)))

(define* (route method
                path
                #:key
                operation-id
                summary
                description
                (tags '())
                (deprecated? #f)
                (parameters '())
                (request-body #f)
                (responses '())
                security
                (extensions '()))
  "Describe one operation of a specification.

@var{path} is a path template, spelled with @code{{name}} placeholders or with
the Artanis @code{:petId} spelling; a placeholder with no matching parameter
gains a required string path parameter, so the resulting document is valid
without the caller repeating what the path already says.

@example
(route 'post \"/pets\"
       #:summary \"Create a pet\"
       #:tags '(\"pets\")
       #:request-body (request-body #:ref \"#/components/schemas/NewPet\" #:required? #t)
       #:responses (list (response 201 #:ref \"#/components/schemas/Pet\")))
@end example"
  (let* ((method (normalize-method method))
         (path (normalize-route-path path))
         (operation-id (let ((text (and operation-id (name-text operation-id))))
                         (if (and text (not (string-null? text)))
                             text
                             (derived-operation-id method path)))))
    (make-route method
                path
                operation-id
                summary
                description
                (map name-text tags)
                (and deprecated? #t)
                (complete-path-parameters path parameters)
                request-body
                responses
                security
                (validate-extensions extensions))))

(define (complete-path-parameters path parameters)
  "Return @var{parameters} with the placeholders of @var{path} all declared."
  (let* ((declared (filter (lambda (parameter)
                             (eq? (route-parameter-location parameter) 'path))
                           parameters))
         (names (map route-parameter-name declared))
         (implicit (filter-map (lambda (name)
                                 (and (not (member name names))
                                      (parameter name #:in 'path)))
                               (path-parameter-names path)))
         (all (append implicit parameters)))
    (map (lambda (parameter)
           (if (and (eq? (route-parameter-location parameter) 'path)
                    (not (route-parameter-required? parameter)))
               (make-route-parameter (route-parameter-name parameter)
                                     (route-parameter-location parameter)
                                     #t
                                     (route-parameter-description parameter)
                                     (route-parameter-schema parameter)
                                     (route-parameter-deprecated? parameter))
               parameter))
         all)))

(define (validate-extensions extensions)
  "Return @var{extensions} with string keys, raising on a key that is not one."
  (map (lambda (entry)
         (let ((key (name-text (car entry))))
           (unless (string-prefix? "x-" key)
             (error "An OpenAPI extension key starts with x-" key))
           (cons key (cdr entry))))
       extensions))

;;; Commentary:
;;; The document
;;
;; The document is built as a jso value, the same representation the reader
;; hands the generator, so a specification written here can be parsed back with
;; @code{parse-spec} and used to generate clients without a round trip through
;; a file.

(define* (routes->document routes
                           #:key
                           (title "API")
                           (version "1.0.0")
                           description
                           servers
                           schemas
                           security-schemes
                           security
                           (openapi "3.0.3"))
  "Return the OpenAPI @var{openapi} document that describes @var{routes}.

@var{servers} is a list of URL strings or of server objects, @var{schemas} and
@var{security-schemes} are alists of name to jso value, and @var{security} is a
list of requirement objects (or of scheme names, for a requirement with no
scopes) applied to every operation that declares none.

@example
(routes->document routes #:title \"Pets\" #:version \"1.0.0\"
                  #:servers '(\"https://api.example.com/v1\"))
@end example"
  (validate-routes! routes)
  (let ((components (append
                     (if (and schemas (pair? schemas))
                         (list (cons "schemas" (named-object schemas)))
                         '())
                     (if (and security-schemes (pair? security-schemes))
                         (list (cons "securitySchemes"
                                     (named-object security-schemes)))
                         '()))))
    (append
     (list (cons "openapi" openapi)
           (cons "info" (append (list (cons "title" title)
                                      (cons "version" version))
                                (if description
                                    (list (cons "description" description))
                                    '()))))
     (if (and servers (pair? servers))
         (list (cons "servers" (server-array servers)))
         '())
     (list (cons "paths" (paths-object routes)))
     (if (null? components)
         '()
         (list (cons "components" components)))
     (if (and security (pair? security))
         (list (cons "security" (security-array security)))
         '()))))

(define (named-object entries)
  "Return @var{entries}, an alist, as a jso object with string keys."
  (map (lambda (entry)
         (cons (name-text (car entry)) (cdr entry)))
       entries))

(define (server-array servers)
  "Return @var{servers} as the @code{servers} array of a document."
  (jso-list->array
   (map (lambda (server)
          (cond
            ((string? server) (list (cons "url" server)))
            ((jso-object? server) server)
            (else (error "A server is a URL string or an object" server))))
        servers)))

(define (security-array requirements)
  "Return @var{requirements} as the array a @code{security} member holds."
  (jso-list->array
   (map (lambda (requirement)
          (cond
            ((jso-object? requirement) requirement)
            ((or (string? requirement) (symbol? requirement))
             (list (cons (name-text requirement) (vector))))
            (else (error "A security requirement is a scheme name or an object"
                         requirement))))
        requirements)))

(define (paths-object routes)
  "Return the @code{paths} object of @var{routes}, in declaration order."
  (map (lambda (path)
         (cons path
               (map (lambda (route)
                      (cons (symbol->string (route-method route))
                            (operation-object route)))
                    (filter (lambda (route)
                              (string=? (route-path route) path))
                            routes))))
       (delete-duplicates (map route-path routes) string=?)))

(define (operation-object route)
  "Return the operation object of @var{route}."
  (append
   (list (cons "operationId" (route-operation-id route)))
   (if (route-summary route)
       (list (cons "summary" (route-summary route)))
       '())
   (if (route-description route)
       (list (cons "description" (route-description route)))
       '())
   (if (pair? (route-tags route))
       (list (cons "tags" (jso-list->array (route-tags route))))
       '())
   (if (route-deprecated? route)
       (list (cons "deprecated" #t))
       '())
   (if (pair? (route-parameters route))
       (list (cons "parameters"
                   (jso-list->array (map parameter-object
                                         (route-parameters route)))))
       '())
   (if (route-request-body route)
       (list (cons "requestBody" (body-object (route-request-body route))))
       '())
   (list (cons "responses" (responses-object route)))
   (if (and (route-security route) (pair? (route-security route)))
       (list (cons "security" (security-array (route-security route))))
       '())
   (route-extensions route)))

(define (parameter-object parameter)
  "Return the parameter object of @var{parameter}."
  (append
   (list (cons "name" (route-parameter-name parameter))
         (cons "in" (symbol->string (route-parameter-location parameter))))
   (if (route-parameter-required? parameter)
       (list (cons "required" #t))
       '())
   (if (route-parameter-description parameter)
       (list (cons "description" (route-parameter-description parameter)))
       '())
   (list (cons "schema" (route-parameter-schema parameter)))
   (if (route-parameter-deprecated? parameter)
       (list (cons "deprecated" #t))
       '())))

(define (body-object body)
  "Return the request body object of @var{body}."
  (append
   (if (route-body-required? body)
       (list (cons "required" #t))
       '())
   (if (route-body-description body)
       (list (cons "description" (route-body-description body)))
       '())
   (list (cons "content"
               (content-object (route-body-media-type body)
                               (route-body-schema body))))))

(define (content-object media-type schema)
  "Return a @code{content} object holding @var{schema} under @var{media-type}."
  (list (cons media-type (list (cons "schema" schema)))))

(define (responses-object route)
  "Return the @code{responses} object of @var{route}.

A route that declares no response still answers, so it gets a @code{200 OK} with
no content rather than an empty @code{responses} member, which the specification
does not allow."
  (let ((responses (route-responses route)))
    (if (null? responses)
        (list (cons "200" (list (cons "description" "OK"))))
        (map (lambda (response)
               (cons (route-response-status response)
                     (append
                      (list (cons "description"
                                  (route-response-description response)))
                      (if (route-response-schema response)
                          (list (cons "content"
                                      (content-object
                                       (or (route-response-media-type response)
                                           "application/json")
                                       (route-response-schema response))))
                          '())
                      (if (route-response-headers response)
                          (list (cons "headers"
                                      (route-response-headers response)))
                          '()))))
             responses))))

(define (validate-routes! routes)
  "Raise when @var{routes} cannot be written as a valid document."
  (let loop ((rest routes) (identifiers '()) (targets '()))
    (unless (null? rest)
      (let* ((route (car rest))
             (identifier (route-operation-id route))
             (target (cons (route-method route) (route-path route))))
        (when (member identifier identifiers)
          (error "Two routes share an operationId" identifier))
        (when (member target targets)
          (error "Two routes share a method and path"
                 (symbol->string (route-method route))
                 (route-path route)))
        (loop (cdr rest)
              (cons identifier identifiers)
              (cons target targets))))))

;;; Commentary:
;;; Output

(define* (routes->yaml routes
                       #:key
                       (title "API")
                       (version "1.0.0")
                       description
                       servers
                       schemas
                       security-schemes
                       security
                       (openapi "3.0.3"))
  "Return @var{routes} as an OpenAPI document in YAML.

The keyword arguments are the ones of @code{routes->document}.

@example
(routes->yaml routes #:title \"Pets\")   ;; => \"openapi: 3.0.3\\n...\"
@end example"
  (jso->yaml-string
   (routes->document routes
                     #:title title
                     #:version version
                     #:description description
                     #:servers servers
                     #:schemas schemas
                     #:security-schemes security-schemes
                     #:security security
                     #:openapi openapi)))

(define* (routes->yaml-file routes path
                            #:key
                            (title "API")
                            (version "1.0.0")
                            description
                            servers
                            schemas
                            security-schemes
                            security
                            (openapi "3.0.3"))
  "Write @var{routes} to @var{path} as an OpenAPI document in YAML.

Returns @var{path}, so a caller can report where the specification went."
  (call-with-output-file path
    (lambda (port)
      (display (routes->yaml routes
                             #:title title
                             #:version version
                             #:description description
                             #:servers servers
                             #:schemas schemas
                             #:security-schemes security-schemes
                             #:security security
                             #:openapi openapi)
               port)))
  path)
