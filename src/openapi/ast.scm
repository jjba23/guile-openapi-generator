;; guile-openapi

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; guile-openapi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; guile-openapi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with guile-openapi.  If not, see <https://www.gnu.org/licenses/>.

(define-module (openapi ast)
  #:use-module (srfi srfi-9)
  #:export (make-openapi-spec openapi-spec?
                              openapi-spec-title
                              openapi-spec-version
                              openapi-spec-paths
                              openapi-spec-schemas

                              make-operation-spec
                              operation-spec?
                              operation-spec-id
                              operation-spec-method
                              operation-spec-path
                              operation-spec-summary
                              operation-spec-description
                              operation-spec-tags
                              operation-spec-parameters
                              operation-spec-request-body
                              operation-spec-responses

                              make-parameter-spec
                              parameter-spec?
                              parameter-spec-name
                              parameter-spec-in
                              parameter-spec-required?
                              parameter-spec-description
                              parameter-spec-schema

                              make-request-body-spec
                              request-body-spec?
                              request-body-spec-required?
                              request-body-spec-content-type
                              request-body-spec-schema

                              make-response-spec
                              response-spec?
                              response-spec-status-code
                              response-spec-description
                              response-spec-content-type
                              response-spec-schema

                              make-schema-ref
                              schema-ref?
                              schema-ref-path
                              schema-ref-target
                              make-schema-primitive
                              schema-primitive?
                              schema-primitive-type
                              schema-primitive-format
                              schema-primitive-default
                              make-schema-enum
                              schema-enum?
                              schema-enum-name
                              schema-enum-type
                              schema-enum-values
                              schema-enum-default
                              make-schema-array
                              schema-array?
                              schema-array-item-schema
                              make-schema-object
                              schema-object?
                              schema-object-name
                              schema-object-properties
                              schema-object-required
                              schema-object-additional-properties))

;; Root AST Node
(define-record-type <openapi-spec>
  (make-openapi-spec title version paths schemas)
  openapi-spec?
  (title openapi-spec-title)
  (version openapi-spec-version)
  (paths openapi-spec-paths) ;List of <operation-spec>
  (schemas openapi-spec-schemas))
 ; Alist of name -> schema variant

;; Operation AST Node
(define-record-type <operation-spec>
  (make-operation-spec id
                       method
                       path
                       summary
                       description
                       tags
                       parameters
                       request-body
                       responses)
  operation-spec?
  (id operation-spec-id) ;Symbol (e.g. 'getUserAgentStats)
  (method operation-spec-method) ;Symbol ('get, 'post, etc.)
  (path operation-spec-path) ;String ("/v1/stats/user-agents")
  (summary operation-spec-summary) ;String
  (description operation-spec-description) ;String
  (tags operation-spec-tags) ;List of symbols
  (parameters operation-spec-parameters) ;List of <parameter-spec>
  (request-body operation-spec-request-body) ;<request-body-spec> or #f
  (responses operation-spec-responses))
 ; Alist of "200" -> <response-spec>

;; Parameter AST Node
(define-record-type <parameter-spec>
  (make-parameter-spec name in required? description schema)
  parameter-spec?
  (name parameter-spec-name) ;String ("domain")
  (in parameter-spec-in) ;Symbol ('query, 'path, 'header)
  (required? parameter-spec-required?) ;Boolean
  (description parameter-spec-description) ;String
  (schema parameter-spec-schema))
 ; Schema AST record

;; Request Body AST Node
(define-record-type <request-body-spec>
  (make-request-body-spec required? content-type schema)
  request-body-spec?
  (required? request-body-spec-required?)
  (content-type request-body-spec-content-type) ;String ("application/json")
  (schema request-body-spec-schema))
 ; Schema AST record

;; Response AST Node
(define-record-type <response-spec>
  (make-response-spec status-code description content-type schema)
  response-spec?
  (status-code response-spec-status-code) ;String ("200", "401")
  (description response-spec-description) ;String
  (content-type response-spec-content-type) ;String ("application/json") or #f
  (schema response-spec-schema))
 ; Schema AST record or #f

;; Schema AST Variants
(define-record-type <schema-ref>
  (make-schema-ref path) schema-ref?
  (path schema-ref-path))
 ; String ("#/components/schemas/LoginRequest")

(define (schema-ref-target ref-node)
  (let ((path (schema-ref-path ref-node)))
    (if (string-prefix? "#/components/schemas/" path)
        (substring path
                   (string-length "#/components/schemas/")) path)))

(define-record-type <schema-primitive>
  (make-schema-primitive type format default)
  schema-primitive?
  (type schema-primitive-type) ;Symbol ('string, 'integer, 'number, 'boolean)
  (format schema-primitive-format) ;Symbol ('date-time, 'uuid, 'long) or #f
  (default schema-primitive-default))

(define-record-type <schema-enum>
  (make-schema-enum name type values default)
  schema-enum?
  (name schema-enum-name) ;String or #f
  (type schema-enum-type) ;Symbol ('string)
  (values schema-enum-values) ;List of strings/values
  (default schema-enum-default))

(define-record-type <schema-array>
  (make-schema-array item-schema) schema-array?
  (item-schema schema-array-item-schema))
 ; Schema AST record

(define-record-type <schema-object>
  (make-schema-object name properties required additional-properties)
  schema-object?
  (name schema-object-name) ;String or #f
  (properties schema-object-properties) ;Alist of field-name -> schema
  (required schema-object-required) ;List of field-name strings
  (additional-properties schema-object-additional-properties))
 ; Schema AST record or boolean
