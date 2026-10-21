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
;; The specification model: operations with their parameters merged, responses
;; with their media types, security inherited from the document unless an
;; operation overrides it, and references resolved before anything is read.

(define-module (veritas unit spec-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-support)
  #:export (spec))

(define petstore (petstore-spec))
(define polymorphic (polymorphism-spec))

(define (operation collection name)
  (find (lambda (candidate)
          (eq? (operation-name candidate) name))
        (spec-operations collection)))

(define document-suite
  (suite "Document"
         (test "The title, version and servers are read"
               (assert-equal #:expect "Petstore" #:got (spec-title petstore))
               (assert-equal #:expect "3.0.3" #:got (spec-version petstore))
               (assert-equal #:expect "https://api.example.com/v1"
                             #:got (spec-base-url petstore))
               (assert-equal #:expect 2
                             #:got (length (spec-servers polymorphic))))

         (test "The component schemas are read in order"
               (assert-equal #:expect '("Pet" "NewPet")
                             #:got (spec-schema-names petstore))
               (assert-true (schema-name? petstore "Pet")
                            #:name "Pet is a component schema"))))

(define operation-suite
  (suite "Operations"
         (test "Every path and method becomes an operation"
               (assert-equal #:expect '(list-pets post-pets get-pet-by-id
                                                    delete-pets-by-pet-id)
                             #:got (map operation-name
                                        (spec-operations petstore))))

         (test "operationId wins, otherwise method and path decide"
               (assert-equal #:expect 'list-pets
                             #:got (operation-name (operation petstore 'list-pets)))
               (assert-equal #:expect 'post-pets
                             #:got (operation-name (operation petstore 'post-pets))
                             #:name "post-pets has no operationId"))

         (test "Method, path, summary and tags are read"
               (let ((create (operation polymorphic 'create-animal)))
                 (assert-eq #:expect 'post #:got (operation-method create))
                 (assert-equal #:expect "/animals" #:got (operation-path create))
                 (assert-equal #:expect "Create an animal"
                               #:got (operation-summary create))
                 (assert-false (operation-deprecated? create))))

         (test "Path level parameters are inherited"
               (let ((read (operation polymorphic 'get-animal)))
                 (assert-equal #:expect '("animalId")
                               #:got (map parameter-name
                                          (operation-parameters read)))
                 (assert-true (parameter-required?
                               (car (operation-parameters read)))
                              #:name "a path parameter is required")))

         (test "Operation level parameters win over path level ones"
               (let* ((document (parse-spec
                             (read-spec-string
                              (string-append
                               "openapi: 3.0.3\n"
                               "info: {title: T, version: '1'}\n"
                               "paths:\n"
                               "  /x/{id}:\n"
                               "    parameters:\n"
                               "      - {name: id, in: path, required: true,"
                               " schema: {type: string}, description: from path}\n"
                               "    get:\n"
                               "      parameters:\n"
                               "        - {name: id, in: path, required: true,"
                               " schema: {type: string}, description: from operation}\n"
                               "      responses: {'200': {description: ok}}\n"))))
                      (operation (car (spec-operations document))))
                 (assert-equal #:expect 1
                               #:got (length (operation-parameters operation))
                               #:name "the parameter is not duplicated")
                 (assert-equal #:expect "from operation"
                               #:got (parameter-description
                                      (car (operation-parameters
                                            operation))))))))

(define response-suite
  (suite "Responses"
         (test "The success response is found"
               (let ((response (operation-success-response
                                (operation petstore 'list-pets))))
                 (assert-equal #:expect "200"
                               #:got (response-status response))
                 (assert-equal #:expect "application/json"
                               #:got (media-type-name
                                      (car (response-media-types response))))))

         (test "A default response is a fallback"
               (let ((response (operation-response-for-status
                                (operation polymorphic 'list-animals) "default")))
                 (assert-equal #:expect "default"
                               #:got (response-status response))))

         (test "A response reference is resolved"
               (let ((response (operation-response-for-status
                                (operation polymorphic 'list-animals) "default")))
                 (assert-equal #:expect "Something went wrong"
                               #:got (response-description response))
                 (assert-equal #:expect 1
                               #:got (length (response-media-types response)))))))

(define security-suite
  (suite "Security"
         (test "Global security applies to an operation without its own"
               (assert-equal #:expect '((("api_key")))
                             #:got (operation-security
                                    (operation polymorphic 'list-animals))))

         (test "An empty operation security means public"
               (assert-equal #:expect '()
                             #:got (operation-security
                                    (operation polymorphic 'delete-animal))
                             #:name "deleteAnimal opts out"))

         (test "Operation security wins over the global one"
               (assert-equal #:expect '((("oauth2" "write:animals")))
                             #:got (operation-security
                                    (operation polymorphic 'create-animal))))

         (test "The security schemes are read"
               (assert-equal #:expect '("api_key" "basic_auth" "bearer_auth"
                                        "oauth2" "open_id")
                             #:got (map security-scheme-name
                                        (spec-security-schemes polymorphic)))
               (let ((bearer (assoc-ref (security-schemes-by-name polymorphic)
                                        "bearer_auth")))
                 (assert-equal #:expect "http"
                               #:got (security-scheme-type bearer))
                 (assert-equal #:expect "bearer"
                               #:got (security-scheme-scheme bearer))
                 (assert-equal #:expect "JWT"
                               #:got (security-scheme-bearer-format bearer))))))

(define reference-suite
  (suite "References"
         (test "A local pointer resolves"
               (let ((document (spec-raw petstore)))
                 (assert-equal #:expect "object"
                               #:got (jso-ref (resolve-ref document
                                                           '(("$ref" . "#/components/schemas/Pet")))
                                              "type"))))

         (test "An unresolvable pointer raises"
               (assert-error (lambda ()
                               (resolve-ref (spec-raw petstore)
                                            '(("$ref" . "#/components/schemas/Nope"))))))

         (test "A pointer escape is understood"
               (assert-equal #:expect "string"
                             #:got (jso-ref (json-pointer
                                             '(("components" ("schemas"
                                                              ("a/b" ("type" . "string")))))
                                            "#/components/schemas/a~1b")
                                            "type")
                             #:name "the escape sequence for a slash is understood"))))

(define (spec)
  (veritas-run #:concurrent? #f
               document-suite
               operation-suite
               response-suite
               security-suite
               reference-suite))
