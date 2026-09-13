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

(define-module (openapi parser)
  #:use-module (json)
  #:use-module (yamlpp)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (openapi ast)
  #:export (parse-openapi-alist parse-openapi-json parse-openapi-yaml
                                parse-openapi-file))

(define (assoc-ref* alist key default)
  (if (pair? alist)
      (let ((pair (assoc key alist)))
        (if pair
            (cdr pair) default)) default))

(define (ensure-list seq)
  (cond
    ((list? seq)
     seq)
    ((vector? seq)
     (vector->list seq))
    (else '())))

;; Recursive Schema Parser
(define (parse-schema schema-alist name)
  (cond
    ;; 1. Reference Schema ($ref)
    ((assoc "$ref" schema-alist)
     (make-schema-ref (assoc-ref* schema-alist "$ref" "")))

    ;; 2. Enum Schema
    ((assoc "enum" schema-alist)
     (let ((type-str (assoc-ref* schema-alist "type" "string"))
           (vals (ensure-list (assoc-ref* schema-alist "enum"
                                          '())))
           (default (assoc-ref* schema-alist "default" #f)))
       (make-schema-enum name
                         (string->symbol type-str) vals default)))

    ;; 3. Array Schema
    ((equal? (assoc-ref* schema-alist "type" #f) "array")
     (let ((items-alist (assoc-ref* schema-alist "items"
                                    '())))
       (make-schema-array (parse-schema items-alist #f))))

    ;; 4. Object Schema (or Map/Dictionary)
    ((or (equal? (assoc-ref* schema-alist "type" #f) "object")
         (assoc "properties" schema-alist)
         (assoc "additionalProperties" schema-alist))
     (let* ((props-alist (assoc-ref* schema-alist "properties"
                                     '()))
            (req-list (ensure-list (assoc-ref* schema-alist "required"
                                               '())))
            (add-props (assoc-ref* schema-alist "additionalProperties" #f))
            (add-props-parsed (cond
                                ((pair? add-props)
                                 (parse-schema add-props #f))
                                (else add-props)))
            (props-parsed (map (lambda (pair)
                                 (cons (car pair)
                                       (parse-schema (cdr pair)
                                                     (car pair)))) props-alist)))
       (make-schema-object name props-parsed req-list add-props-parsed)))

    ;; 5. Primitive Schema (string, integer, number, boolean)
    (else (let ((type-str (assoc-ref* schema-alist "type" "string"))
                (fmt-str (assoc-ref* schema-alist "format" #f))
                (default (assoc-ref* schema-alist "default" #f)))
            (make-schema-primitive (string->symbol type-str)
                                   (and fmt-str
                                        (string->symbol fmt-str)) default)))))

;; Parameters Parser
(define (parse-parameters params-seq)
  (map (lambda (p-alist)
         (let* ((name (assoc-ref* p-alist "name" ""))
                (in-str (assoc-ref* p-alist "in" "query"))
                (req? (assoc-ref* p-alist "required" #f))
                (desc (assoc-ref* p-alist "description" ""))
                (schema-alist (assoc-ref* p-alist "schema"
                                          '()))
                (schema (parse-schema schema-alist #f)))
           (make-parameter-spec name
                                (string->symbol in-str) req? desc schema)))
       (ensure-list params-seq)))

;; Request Body Parser
(define (parse-request-body rb-alist)
  (if (not (pair? rb-alist)) #f
      (let* ((required? (assoc-ref* rb-alist "required" #f))
             (content (assoc-ref* rb-alist "content"
                                  '()))
             (ct-pair (if (pair? content)
                          (car content) #f))
             (ct-type (if ct-pair
                          (car ct-pair) "application/json"))
             (schema-alist (if ct-pair
                               (assoc-ref* (cdr ct-pair) "schema"
                                           '())
                               '()))
             (schema (if (pair? schema-alist)
                         (parse-schema schema-alist #f) #f)))
        (make-request-body-spec required? ct-type schema))))

;; Responses Parser
(define (parse-responses responses-alist)
  (map (lambda (pair)
         (let* ((code (car pair))
                (body (cdr pair))
                (desc (assoc-ref* body "description" ""))
                (content (assoc-ref* body "content"
                                     '()))
                (ct-pair (if (pair? content)
                             (car content) #f))
                (ct-type (if ct-pair
                             (car ct-pair) #f))
                (schema-alist (if ct-pair
                                  (assoc-ref* (cdr ct-pair) "schema"
                                              '())
                                  '()))
                (schema (if (pair? schema-alist)
                            (parse-schema schema-alist #f) #f)))
           (cons code
                 (make-response-spec code desc ct-type schema))))
       responses-alist))

;; Path and Operations Parser
(define (parse-paths paths-alist)
  (append-map (lambda (path-pair)
                (let ((path-str (car path-pair))
                      (methods (cdr path-pair)))
                  (filter-map (lambda (method-pair)
                                (let ((method-name (car method-pair))
                                      (op-body (cdr method-pair)))
                                  (if (member method-name
                                              '("get" "post" "put" "delete"
                                                "patch"))
                                      (make-operation-spec (let ((id (assoc-ref*
                                                                      op-body
                                                                      "operationId"
                                                                      #f)))
                                                             (and id
                                                                  (string->symbol
                                                                   id)))
                                                           (string->symbol
                                                            method-name)
                                                           path-str
                                                           (assoc-ref* op-body
                                                            "summary" "")
                                                           (assoc-ref* op-body
                                                            "description" "")
                                                           (map string->symbol
                                                            (ensure-list (assoc-ref*
                                                                          op-body
                                                                          "tags"
                                                                          '())))
                                                           (parse-parameters (assoc-ref*
                                                                              op-body
                                                                              "parameters"
                                                                              '()))
                                                           (parse-request-body
                                                            (assoc-ref*
                                                             op-body
                                                             "requestBody"
                                                             '()))
                                                           (parse-responses (assoc-ref*
                                                                             op-body
                                                                             "responses"
                                                                             '())))
                                      #f))) methods))) paths-alist))

(define (parse-openapi-alist spec-alist)
  (let* ((info (assoc-ref* spec-alist "info"
                           '()))
         (title (assoc-ref* info "title" "Untitled API"))
         (version (assoc-ref* info "version" "0.0.0"))
         (paths (parse-paths (assoc-ref* spec-alist "paths"
                                         '())))
         (components (assoc-ref* spec-alist "components"
                                 '()))
         (schemas-alist (assoc-ref* components "schemas"
                                    '()))
         (schemas (map (lambda (pair)
                         (cons (car pair)
                               (parse-schema (cdr pair)
                                             (car pair)))) schemas-alist)))
    (make-openapi-spec title version paths schemas)))

(define (parse-openapi-json json-string)
  (parse-openapi-alist (json-string->scm json-string)))

;; Entrypoint from YAML string
(define (parse-openapi-yaml yaml-string)
  (parse-openapi-alist (yaml-load yaml-string)))

;; Auto-detect format by extension or fallback
(define (parse-openapi-file file-path)
  (cond
    ((or (string-suffix? ".yaml" file-path)
         (string-suffix? ".yml" file-path))
     (let ((content (call-with-input-file file-path
                      get-string-all)))
       (parse-openapi-yaml content)))
    ((string-suffix? ".json" file-path)
     (call-with-input-file file-path
       (lambda (port)
         (parse-openapi-alist (json->scm port)))))
    (else (error "Unsupported OpenAPI file format:" file-path))))
