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
;; The object model of the specification.  A document is an untyped jso tree;
;; this module turns it into records the generator can pattern match on, and
;; hides the places where the specification is irregular:
;;
;; - @code{$ref} may appear on a schema, a path item, a parameter, a response,
;;   a request body or a header, so every accessor resolves it before answering;
;; - parameters may be declared on the path item and overridden per operation;
;; - security may be declared globally and overridden per operation, and an
;;   empty @code{security} list means "this operation is public";
;; - a response key is a status code, a range (@code{2XX}) or @code{default}.
;;
;; Interpreting what a schema *means* is deliberately not done here: that is
;; @code{(openapi-generator schema)}.

(define-module (openapi-generator spec)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module ((openapi-generator naming)
               #:prefix naming:)
  #:use-module (openapi-generator reader)
  #:export (;; records
            <spec> spec? make-spec
            spec-version
            spec-title
            spec-description
            spec-servers
            spec-operations
            spec-schemas
            spec-security-schemes
            spec-security
            spec-raw
            spec-source
            spec-base-url
            <server> server? make-server
            server-url
            server-description
            <operation> operation? make-operation
            operation-method
            operation-path
            operation-id
            operation-name
            operation-summary
            operation-description
            operation-tags
            operation-deprecated?
            operation-parameters
            operation-request-body
            operation-responses
            operation-security
            operation-raw
            <parameter> parameter? make-parameter
            parameter-name
            parameter-field
            parameter-location
            parameter-required?
            parameter-description
            parameter-schema
            parameter-style
            parameter-explode?
            parameter-deprecated?
            parameter-raw
            <request-body> request-body? make-request-body
            request-body-required?
            request-body-description
            request-body-media-types
            <media-type> media-type? make-media-type
            media-type-name
            media-type-schema
            <response> response? make-response
            response-status
            response-description
            response-media-types
            response-headers
            <security-scheme> security-scheme? make-security-scheme
            security-scheme-name
            security-scheme-type
            security-scheme-scheme
            security-scheme-bearer-format
            security-scheme-location
            security-scheme-parameter-name
            security-scheme-open-id-url
            <named-schema> named-schema? make-named-schema
            named-schema-name
            named-schema-scheme-name
            named-schema-schema
            ;; parsing
            parse-spec
            resolve-ref
            json-pointer
            schema-name?
            spec-schema-names
            operation-response-for-status
            operation-success-response
            operation-request-media-type
            parameters-by-location
            security-schemes-by-name))

;;; Commentary:
;;; Records

(define-record-type <spec>
  (make-spec version
             title
             description
             servers
             operations
             schemas
             security-schemes
             security
             raw
             source)
  spec?
  (version spec-version)
  (title spec-title)
  (description spec-description)
  (servers spec-servers)
  (operations spec-operations)
  (schemas spec-schemas)
  (security-schemes spec-security-schemes)
  (security spec-security)
  (raw spec-raw)
  (source spec-source))

(define-record-type <server>
  (make-server url description variables)
  server?
  (url server-url)
  (description server-description)
  (variables server-variables))

(define-record-type <operation>
  (make-operation method
                  path
                  operation-id
                  name
                  summary
                  description
                  tags
                  deprecated?
                  parameters
                  request-body
                  responses
                  security
                  raw)
  operation?
  (method operation-method)
  (path operation-path)
  (operation-id operation-id)
  (name operation-name)
  (summary operation-summary)
  (description operation-description)
  (tags operation-tags)
  (deprecated? operation-deprecated?)
  (parameters operation-parameters)
  (request-body operation-request-body)
  (responses operation-responses)
  (security operation-security)
  (raw operation-raw))

(define-record-type <parameter>
  (make-parameter name
                  field
                  location
                  required?
                  description
                  schema
                  style
                  explode?
                  deprecated?
                  raw)
  parameter?
  (name parameter-name)
  (field parameter-field)
  (location parameter-location)
  (required? parameter-required?)
  (description parameter-description)
  (schema parameter-schema)
  (style parameter-style)
  (explode? parameter-explode?)
  (deprecated? parameter-deprecated?)
  (raw parameter-raw))

(define-record-type <request-body>
  (make-request-body required? description media-types)
  request-body?
  (required? request-body-required?)
  (description request-body-description)
  (media-types request-body-media-types))

(define-record-type <media-type>
  (make-media-type name schema)
  media-type?
  (name media-type-name)
  (schema media-type-schema))

(define-record-type <response>
  (make-response status description media-types headers)
  response?
  (status response-status)
  (description response-description)
  (media-types response-media-types)
  (headers response-headers))

(define-record-type <security-scheme>
  (make-security-scheme name type scheme bearer-format location parameter-name
                        open-id-url)
  security-scheme?
  (name security-scheme-name)
  (type security-scheme-type)
  (scheme security-scheme-scheme)
  (bearer-format security-scheme-bearer-format)
  (location security-scheme-location)
  (parameter-name security-scheme-parameter-name)
  (open-id-url security-scheme-open-id-url))

(define-record-type <named-schema>
  (make-named-schema name scheme-name schema)
  named-schema?
  (name named-schema-name)
  (scheme-name named-schema-scheme-name)
  (schema named-schema-schema))

;;; Commentary:
;;; JSON pointers

(define (json-pointer document pointer)
  "Follow the JSON pointer @var{pointer} through @var{document}.

@example
(json-pointer document \"#/components/schemas/Pet\")
@end example"
  (unless (and (string? pointer) (string-prefix? "#" pointer))
    (error "Only local JSON pointers are supported" pointer))
  (let ((path (substring pointer 1 (string-length pointer))))
    (let loop ((node document)
               (segments (if (string-null? path)
                             '()
                             (map unescape-pointer
                                  (string-split (substring path 1
                                                           (string-length path))
                                                #\/)))))
      (cond
        ((null? segments) node)
        ((jso-object? node)
         (let ((next (jso-ref node (car segments))))
           (if (eq? next #f)
               (error "JSON pointer does not resolve" pointer)
               (loop next (cdr segments)))))
        ((vector? node)
         (loop (vector-ref node (string->number (car segments)))
               (cdr segments)))
        (else (error "JSON pointer does not resolve" pointer))))))

(define (unescape-pointer segment)
  (string-replace-substring
   (string-replace-substring segment "~1" "/")
   "~0"
   "~"))

(define* (resolve-ref document node #:optional (depth 0))
  "Return @var{node} with its @code{$ref} resolved, when it has one."
  (let ((reference (and (jso-object? node) (jso-ref node "$ref"))))
    (cond
      ((not (string? reference)) node)
      ((> depth 20) (error "Reference cycle while resolving" reference))
      (else (resolve-ref document (json-pointer document reference)
                         (+ depth 1))))))

;;; Commentary:
;;; Parsing

(define* (parse-spec document #:key (source "<string>") (operation-id-transform #f))
  "Turn an OpenAPI 3.x @var{document} into a @code{<spec>}.

@var{operation-id-transform}, when given, is a procedure applied to every
operationId before it becomes a generated name."
  (let* ((info (jso-ref document "info"))
         (security-schemes (parse-security-schemes document))
         (global-security (parse-security (jso-ref document "security")))
         (schemas (parse-schemas document)))
    (make-spec (or (openapi-version document) "3.0.0")
               (or (jso-ref info "title") (document-title document))
               (jso-ref info "description")
               (parse-servers document)
               (parse-operations document global-security
                                 operation-id-transform)
               schemas
               security-schemes
               global-security
               document
               source)))

(define (parse-servers document)
  (map (lambda (server)
         (make-server (or (jso-ref server "url") "")
                      (jso-ref server "description")
                      (jso-ref server "variables")))
       (jso-array->list (jso-ref document "servers"))))

(define (spec-base-url spec)
  "Return the first server URL of @var{spec}, or #f when it declares none."
  (let ((servers (spec-servers spec)))
    (and (pair? servers)
         (let ((url (server-url (car servers))))
           (and (string? url)
                (not (string-null? url))
                (not (string-contains url "{"))
                url)))))

(define (parse-security node)
  "Parse a @code{security} member into a list of requirement alists.

Each element of the list is an alternative; each alist inside it must be
satisfied together with its siblings."
  (map (lambda (requirement)
         (map (lambda (entry)
                (cons (car entry)
                      (map (lambda (scope)
                             (if (string? scope) scope (format #f "~a" scope)))
                           (jso-array->list (cdr entry)))))
              (if (jso-object? requirement) requirement '())))
       (jso-array->list node)))

(define (parse-security-schemes document)
  (let ((schemes (jso-ref-path document "components" "securitySchemes")))
    (map (lambda (entry)
           (let ((name (car entry))
                 (node (resolve-ref document (cdr entry))))
             (make-security-scheme name
                                   (jso-ref node "type")
                                   (jso-ref node "scheme")
                                   (jso-ref node "bearerFormat")
                                   (jso-ref node "in")
                                   (jso-ref node "name")
                                   (jso-ref node "openIdConnectUrl"))))
         (if (jso-object? schemes) schemes '()))))

(define (security-schemes-by-name spec)
  "Return the security schemes of @var{spec} as an alist of name to record."
  (map (lambda (scheme)
         (cons (security-scheme-name scheme) scheme))
       (spec-security-schemes spec)))

(define (parse-schemas document)
  "Return the component schemas of @var{document} as @code{<named-schema>}s."
  (let ((schemas (jso-ref-path document "components" "schemas")))
    (map (lambda (entry)
           (make-named-schema (car entry)
                              (string->symbol (naming:identifier (car entry)))
                              (cdr entry)))
         (if (jso-object? schemas) schemas '()))))

(define (spec-schema-names spec)
  "Return the component schema names of @var{spec}, as strings."
  (map named-schema-name (spec-schemas spec)))

(define (schema-name? spec name)
  "Return @code{#t} when @var{name} is a component schema of @var{spec}."
  (and (member name (spec-schema-names spec)) #t))

(define (parse-operations document global-security transform)
  "Return every operation of @var{document} as a @code{<operation>}."
  (let ((paths (jso-ref document "paths")))
    (append-map (lambda (path-entry)
                  (parse-path-item document
                                   (car path-entry)
                                   (resolve-ref document (cdr path-entry))
                                   global-security
                                   transform))
                (if (jso-object? paths) paths '()))))

(define http-methods
  '(get put post delete options head patch trace))

(define (parse-path-item document path path-item global-security transform)
  (let ((path-parameters (parse-parameter-list document
                                               (jso-ref path-item "parameters"))))
    (append-map (lambda (entry)
                  (let ((method (string->symbol (string-downcase (car entry)))))
                    (if (memq method http-methods)
                        (list (parse-operation document
                                               path
                                               method
                                               (cdr entry)
                                               path-parameters
                                               global-security
                                               transform))
                        '())))
                (if (jso-object? path-item) path-item '()))))

(define (parse-operation document path method node path-parameters
                         global-security transform)
  (let* ((own-parameters (parse-parameter-list document
                                               (jso-ref node "parameters")))
         (parameters (merge-parameters path-parameters own-parameters))
         (raw-operation-id (jso-ref node "operationId"))
         (operation-id (if (and transform (string? raw-operation-id))
                           (transform raw-operation-id)
                           raw-operation-id))
         (security-node (jso-ref node "security"))
         (security (if (jso-has? node "security")
                       (parse-security security-node)
                       global-security)))
    (make-operation method
                    path
                    operation-id
                    (naming:operation-procedure-name method path operation-id)
                    (jso-ref node "summary")
                    (jso-ref node "description")
                    (map (lambda (tag) (if (string? tag) tag (format #f "~a" tag)))
                         (jso-array->list (jso-ref node "tags")))
                    (jso-true? (jso-ref node "deprecated"))
                    parameters
                    (parse-request-body document (jso-ref node "requestBody"))
                    (parse-responses document (jso-ref node "responses"))
                    security
                    node)))

(define (parse-parameter-list document node)
  (map (lambda (parameter)
         (parse-parameter document (resolve-ref document parameter)))
       (jso-array->list node)))

(define (parameter-key parameter)
  (cons (parameter-name parameter) (parameter-location parameter)))

(define (merge-parameters path-parameters operation-parameters)
  "Merge path level parameters with operation level ones, the latter winning."
  (let ((overridden (map parameter-key operation-parameters)))
    (append operation-parameters
            (remove (lambda (parameter)
                      (member (parameter-key parameter) overridden))
                    path-parameters))))

(define (parse-parameter document node)
  (let* ((name (or (jso-ref node "name") "parameter"))
         (location (string->symbol (string-downcase
                                    (or (jso-ref node "in") "query"))))
         (schema (or (jso-ref node "schema")
                     (let ((content (jso-ref node "content")))
                       (and (jso-object? content)
                            (pair? content)
                            (jso-ref-path (cdar content) "schema"))))))
    (make-parameter name
                    (string->symbol (naming:identifier name))
                    location
                    (or (jso-true? (jso-ref node "required"))
                        (eq? location 'path))
                    (jso-ref node "description")
                    (if (jso-object? schema) schema '())
                    (jso-ref node "style")
                    (jso-true? (jso-ref node "explode"))
                    (jso-true? (jso-ref node "deprecated"))
                    node)))

(define (parameters-by-location parameters location)
  "Return the parameters of @var{parameters} that live in @var{location}."
  (filter (lambda (parameter)
            (eq? (parameter-location parameter) location))
          parameters))

(define (parse-request-body document node)
  (let ((body (resolve-ref document node)))
    (if (jso-object? body)
        (make-request-body (jso-true? (jso-ref body "required"))
                           (jso-ref body "description")
                           (parse-media-types document
                                              (jso-ref body "content")))
        #f)))

(define (parse-responses document node)
  (map (lambda (entry)
         (make-response (car entry)
                        (jso-ref (cdr entry) "description")
                        (parse-media-types document
                                           (jso-ref (cdr entry) "content"))
                        (jso-ref (cdr entry) "headers")))
       (if (jso-object? node)
           (map (lambda (entry)
                  (cons (car entry) (resolve-ref document (cdr entry))))
                node)
           '())))

(define (parse-media-types document node)
  (map (lambda (entry)
         (let ((media (resolve-ref document (cdr entry))))
           (make-media-type (car entry) (jso-ref media "schema"))))
       (if (jso-object? node) node '())))

;;; Commentary:
;;; Answers the generator asks for

(define (operation-response-for-status operation status)
  "Return the response of @var{operation} declared for @var{status}."
  (let ((responses (operation-responses operation))
        (wanted (response-status-text status)))
    (or (find (lambda (response)
                (string=? (response-status-text (response-status response))
                          wanted))
              responses)
        (find (lambda (response)
                (string-ci=? (response-status-text (response-status response))
                             wanted))
              responses)
        (find (lambda (response)
                (string-ci=? (response-status-text (response-status response))
                             "default"))
              responses))))

(define (response-status-text status)
  "Return @var{status}, which a reader may hand over as a string or a number, as text."
  (cond ((string? status) status)
        ((number? status) (number->string status))
        (else "")))

(define (response-success-code? status)
  "Return @code{#t} when @var{status} names a success response.

An exact code in the 2xx range counts, and so does a family such as @code{2XX}.
@code{default} does not: it is a fallback for whatever was not declared, and
preferring it over a declared success would decode an error as a success."
  (let ((text (response-status-text status)))
    (and (= (string-length text) 3)
         (char=? (string-ref text 0) #\2)
         (every (lambda (character)
                  (or (char-numeric? character)
                      (char=? (char-upcase character) #\X)))
                (string->list text)))))

(define (operation-success-response operation)
  "Return the success response of @var{operation}, or #f when it has none.

An exact code among @code{200}, @code{201}, @code{202}, @code{204},
@code{203}, @code{205} and @code{206} wins, then any other 2xx code or a
@code{2XX} family, and only then @code{default}."
  (let ((responses (operation-responses operation)))
    (or (find (lambda (response)
                (member (response-status-text (response-status response))
                        '("200" "201" "202" "204" "203" "205" "206")
                        string=?))
              responses)
        (find (lambda (response)
                (response-success-code? (response-status response)))
              responses)
        (find (lambda (response)
                (string-ci=? (response-status-text (response-status response))
                             "default"))
              responses))))

(define (operation-request-media-type operation)
  "Return the preferred media type of the request body of @var{operation}."
  (let ((body (operation-request-body operation)))
    (and body
         (let ((media-types (request-body-media-types body)))
           (or (find (lambda (media-type)
                       (string=? (media-type-name media-type)
                                 "application/json"))
                     media-types)
               (and (pair? media-types) (car media-types)))))))
