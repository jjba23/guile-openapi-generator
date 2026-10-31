;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Routes written in Scheme, and the document they turn into.  The suites check
;; the two halves of the promise: what a route says is read back exactly (method,
;; path, parameters, responses), and what it does not say is filled in by a rule
;; that is written down — a placeholder becomes a required path parameter, a
;; missing operationId is derived, a missing response is a 200 OK.  The last
;; suite feeds the YAML back through @code{parse-spec}, because a document that
;; this project's own reader cannot read would be useless whatever it looked
;; like.

(define-module (veritas unit routes-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator routes)
  #:use-module (openapi-generator cli)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define pet-schema
  '(("type" . "object")
    ("required" . #("id" "name"))
    ("properties" . (("id" . (("type" . "integer")))
                     ("name" . (("type" . "string")))))))

(define (example-routes)
  "Return a small set of routes, one per shape the writer has to handle."
  (list (route 'get "/pets"
               #:summary "List pets"
               #:tags '("pets")
               #:parameters (list (parameter "limit"
                                             #:in 'query
                                             #:type 'integer
                                             #:description "How many")))
        (route 'post "/pets"
               #:tags '("pets")
               #:request-body (request-body #:ref "#/components/schemas/Pet"
                                            #:required? #t)
               #:responses (list (response 201
                                           #:ref "#/components/schemas/Pet")))
        (route 'get "/pets/:petId"
               #:operation-id "getPet"
               #:tags '("pets")
               #:responses (list (response 200 #:ref "#/components/schemas/Pet")
                                (response 404
                                          #:description "No pet has that id")))))

(define (document routes)
  (routes->document routes
                    #:title "Pet store"
                    #:version "1.0.0"
                    #:servers '("https://api.example.com/v1")
                    #:schemas (list (cons "Pet" pet-schema))))

(define (quiet-main arguments)
  "Run the command line with @var{arguments}, returning its status and output.

Both ports are captured, because the command reports what it wrote on standard
output and its mistakes on standard error, and a test that only captured one of
them would miss half of what it says."
  (let ((output (open-output-string))
        (errors (open-output-string)))
    (let ((status (parameterize ((current-output-port output)
                                 (current-error-port errors))
                    (main arguments))))
      (cons status
            (string-append (get-output-string output)
                           (get-output-string errors))))))

(define (operations routes)
  (parse-spec (yaml-string->scm
               (routes->yaml routes
                             #:title "Pet store"
                             #:version "1.0.0"
                             #:schemas (list (cons "Pet" pet-schema))))
              #:source "routes"))

;;; Suites

(define route-suite
  (suite "Routes"
         (test "A method may be spelled either way"
               (assert-eq #:expect 'get
                          #:got (route-method (route 'get "/pets")))
               (assert-eq #:expect 'delete
                          #:got (route-method (route "DELETE" "/pets"))))

         (test "An unsupported method is refused"
               (assert-error (lambda () (route 'fetch "/pets"))))

         (test "A path is normalised"
               (assert-equal #:expect "/pets"
                             #:got (route-path (route 'get "pets")))
               (assert-equal #:expect "/pets"
                             #:got (route-path (route 'get "/pets/")))
               (assert-equal #:expect "/pets/{petId}"
                             #:got (route-path (route 'get "/pets/:petId")))
               (assert-equal #:expect "/"
                             #:got (route-path (route 'get "/"))))

         (test "A placeholder becomes a required path parameter"
               (let* ((route (route 'get "/pets/{petId}/photos/{photoId}"))
                      (parameters (route-parameters route)))
                 (assert-equal #:expect '("petId" "photoId")
                               #:got (map route-parameter-name parameters))
                 (assert-equal #:expect '(path path)
                               #:got (map route-parameter-location parameters))
                 (assert-equal #:expect '(#t #t)
                               #:got (map route-parameter-required? parameters))
                 (assert-equal #:expect "string"
                               #:got (jso-ref (route-parameter-schema
                                               (car parameters))
                                              "type"))))

         (test "A declared path parameter is kept where it was written"
               (let* ((route (route 'get "/pets/{petId}"
                                    #:parameters (list (parameter "limit"
                                                                  #:in 'query)
                                                       (parameter "petId"
                                                                  #:in 'path
                                                                  #:type 'integer))))
                      (parameters (route-parameters route)))
                 (assert-equal #:expect '("limit" "petId")
                               #:got (map route-parameter-name parameters))
                 (assert-equal #:expect "integer"
                               #:got (jso-ref (route-parameter-schema
                                               (cadr parameters))
                                              "type"))
                 (assert-true (route-parameter-required? (cadr parameters))
                              #:name "a path parameter is required even when it was not said so")))

         (test "A missing operationId is derived from the method and the path"
               (assert-equal #:expect "getPetsByPetId"
                             #:got (route-operation-id
                                    (route 'get "/pets/{petId}")))
               (assert-equal #:expect "getPets"
                             #:got (derived-operation-id 'get "/pets")))

         (test "A declared operationId wins"
               (assert-equal #:expect "getPet"
                             #:got (route-operation-id
                                    (route 'get "/pets/{petId}"
                                           #:operation-id "getPet"))))

         (test "Tags are text whatever they were written as"
               (assert-equal #:expect '("pets" "store")
                             #:got (route-tags (route 'get "/pets"
                                                      #:tags '(pets "store")))))))

(define parameter-suite
  (suite "Parameters and bodies"
         (test "A location has to be one of the four"
               (assert-equal #:expect 'header
                             #:got (route-parameter-location
                                    (parameter "X-Trace" #:in "header")))
               (assert-error (lambda () (parameter "body" #:in 'body))))

         (test "A parameter without a schema is a string"
               (assert-equal #:expect "string"
                             #:got (jso-ref (route-parameter-schema
                                             (parameter "name"))
                                            "type"))
               (assert-equal #:expect 'query
                             #:got (route-parameter-location (parameter "name"))))

         (test "A type or a schema becomes the schema"
               (assert-equal #:expect "integer"
                             #:got (jso-ref (route-parameter-schema
                                             (parameter "limit" #:type 'integer))
                                            "type"))
               (assert-equal #:expect "#/components/schemas/Pet"
                             #:got (jso-ref (route-parameter-schema
                                             (parameter "pet"
                                                         #:ref "#/components/schemas/Pet"))
                                            "$ref")))

         (test "A request body has a media type and a schema"
               (let ((body (request-body #:ref "#/components/schemas/Pet"
                                         #:required? #t)))
                 (assert-true (route-body-required? body))
                 (assert-equal #:expect "application/json"
                               #:got (route-body-media-type body))
                 (assert-equal #:expect "#/components/schemas/Pet"
                               #:got (jso-ref (route-body-schema body) "$ref")))
               (assert-equal #:expect "text/plain"
                             #:got (route-body-media-type
                                    (request-body #:media-type "text/plain")))
               (assert-equal #:expect '()
                             #:got (route-body-schema (request-body))
                             #:name "a body of unknown shape is an empty schema"))

         (test "A response without a description gets the reason phrase"
               (assert-equal #:expect "Created"
                             #:got (route-response-description (response 201)))
               (assert-equal #:expect "OK"
                             #:got (route-response-description (response "200")))
               (assert-equal #:expect "Response"
                             #:got (route-response-description (response 299))))

         (test "A response with no schema has no content"
               (assert-equal #:expect #f
                             #:got (route-response-schema (response 204)))
               (assert-equal #:expect "integer"
                             #:got (jso-ref (route-response-schema
                                             (response 200 #:type 'integer))
                                            "type")))))

(define document-suite
  (suite "Document"
         (test "The document has the OpenAPI members and the serviced paths"
               (let ((document (document (example-routes))))
                 (assert-equal #:expect "3.0.3"
                               #:got (jso-ref document "openapi"))
                 (assert-equal #:expect "Pet store"
                               #:got (jso-ref-path document "info" "title"))
                 (assert-equal #:expect "https://api.example.com/v1"
                               #:got (jso-ref (vector-ref (jso-ref document
                                                                  "servers")
                                                          0)
                                              "url"))
                 (assert-equal #:expect '("/pets" "/pets/{petId}")
                               #:got (jso-keys (jso-ref document "paths")))
                 (assert-equal #:expect "string"
                               #:got (jso-ref-path document "components"
                                                   "schemas" "Pet" "properties"
                                                   "name" "type")
                               #:name "the schemas are carried into components")))

         (test "Two methods on one path share the path item"
               (let ((paths (jso-ref (document (example-routes)) "paths")))
                 (assert-equal #:expect '("get" "post")
                               #:got (jso-keys (assoc-ref paths "/pets")))))

         (test "An operation declares its parameters, body and responses"
               (let* ((document (document (example-routes)))
                      (operation (jso-ref-path document "paths" "/pets/{petId}"
                                               "get")))
                 (assert-equal #:expect "getPet"
                               #:got (jso-ref operation "operationId"))
                 (assert-equal #:expect "petId"
                               #:got (jso-ref (vector-ref (jso-ref operation
                                                                   "parameters")
                                                          0)
                                              "name"))
                 (assert-equal #:expect '(200 404)
                               #:got (map string->number
                                          (jso-keys (jso-ref operation
                                                             "responses"))))
                 (assert-equal #:expect #f
                               #:got (jso-ref operation "requestBody"))))

         (test "A request body becomes a content object"
               (let* ((document (document (example-routes)))
                      (body (jso-ref-path document "paths" "/pets" "post"
                                          "requestBody")))
                 (assert-eq #:expect #t #:got (jso-ref body "required"))
                 (assert-equal #:expect "application/json"
                               #:got (car (jso-keys (jso-ref body "content"))))))

         (test "A route with no response still answers"
               (let* ((document (routes->document (list (route 'get "/ping"))))
                      (responses (jso-ref-path document "paths" "/ping" "get"
                                               "responses")))
                 (assert-equal #:expect '("200") #:got (jso-keys responses))))

         (test "Security is written when it is given"
               (let ((document (routes->document
                                (list (route 'get "/pets"))
                                #:security-schemes
                                (list (cons "bearerAuth"
                                            (list (cons "type" "http")
                                                  (cons "scheme" "bearer"))))
                                #:security '("bearerAuth"))))
                 (assert-equal #:expect "bearerAuth"
                               #:got (car (jso-keys
                                           (vector-ref (jso-ref document
                                                                "security")
                                                       0))))
                 (assert-equal #:expect "bearer"
                               #:got (jso-ref-path document "components"
                                                   "securitySchemes" "bearerAuth"
                                                   "scheme"))))))

(define command-line-suite
  (suite "Command line"
         (test "A route table becomes a file"
               (with-temporary-directory
                "spec-command"
                (lambda (directory)
                  (let* ((path (string-append directory "/openapi.yaml"))
                         (result (quiet-main
                                  (list "spec"
                                        "--input" (fixture "routes/guile-app.scm")
                                        "--output" path
                                        "--title" "Guile API"
                                        "--api-version" "2.1.0"
                                        "--server" "https://a.example.com"
                                        "--server" "https://b.example.com"))))
                    (assert-eqv #:expect 0 #:got (car result))
                    (assert-contains #:expect (cdr result)
                                     #:got "specification:")
                    (let ((document (yaml-file->scm path)))
                      (assert-equal #:expect "Guile API"
                                    #:got (jso-ref-path document "info" "title"))
                      (assert-equal #:expect "2.1.0"
                                    #:got (jso-ref-path document "info" "version"))
                      (assert-equal #:expect 2
                                    #:got (vector-length
                                           (jso-ref document "servers")))
                      (assert-equal #:expect "https://b.example.com"
                                    #:got (jso-ref (vector-ref
                                                    (jso-ref document "servers")
                                                    1)
                                                   "url")))))))

         (test "The source reader defaults to Guile"
               (with-temporary-directory
                "spec-command-default"
                (lambda (directory)
                  (let ((result (quiet-main
                                 (list "spec"
                                       "--input" (fixture "routes/guile-app.scm")
                                       "--output"
                                       (string-append directory "/a.yaml")))))
                    (assert-eqv #:expect 0 #:got (car result))))))

         (test "An Artanis source is read with --source artanis"
               (with-temporary-directory
                "spec-command-artanis"
                (lambda (directory)
                  (let* ((path (string-append directory "/openapi.yaml"))
                         (result (quiet-main
                                  (list "spec"
                                        "-i" (fixture "routes/artanis-app.scm")
                                        "-s" "artanis"
                                        "-o" path))))
                    (assert-eqv #:expect 0 #:got (car result))
                    (assert-equal #:expect 6
                                  #:got (length (spec-operations
                                                 (parse-spec
                                                  (yaml-file->scm path)))))))))

         (test "The document can go to standard output"
               (let ((result (quiet-main
                              (list "spec"
                                    "-i" (fixture "routes/guile-app.scm")
                                    "-o" "-"))))
                 (assert-eqv #:expect 0 #:got (car result))
                 (assert-contains #:expect (cdr result)
                                  #:got "openapi: 3.0.3")
                 (assert-contains #:expect (cdr result)
                                  #:got "/pets/{petId}")))

         (test "The usage lists the spec command"
               (let ((result (quiet-main (list "spec" "--help"))))
                 (assert-eqv #:expect 0 #:got (car result))
                 (assert-contains #:expect (cdr result)
                                  #:got "openapi-generator spec")
                 (assert-contains #:expect (cdr result)
                                  #:got "--source")))

         (test "A file with no routes is an error"
               (with-temporary-directory
                "spec-command-empty"
                (lambda (directory)
                  (let ((path (string-append directory "/app.scm")))
                    (call-with-output-file path
                      (lambda (port)
                        (display "(define (hello) 'world)\n" port)))
                    (let ((result (quiet-main
                                   (list "spec" "-i" path "-o" "-"))))
                      (assert-eqv #:expect 1 #:got (car result))
                      (assert-contains #:expect (cdr result)
                                       #:got "No routes were found"))))))

         (test "A source that does not exist is an error"
               (assert-eqv #:expect 1
                           #:got (car (quiet-main
                                       (list "spec"
                                             "-i" "/tmp/no-such-routes.scm"
                                             "-o" "-")))))

         (test "An unknown source is an error"
               (with-temporary-directory
                "spec-command-source"
                (lambda (directory)
                  (let ((result (quiet-main
                                 (list "spec"
                                       "-i" (fixture "routes/guile-app.scm")
                                       "-s" "sinatra"
                                       "-o" (string-append directory "/a.yaml")))))
                    (assert-eqv #:expect 1 #:got (car result))
                    (assert-contains #:expect (cdr result)
                                     #:got "Unknown route source")))))

         (test "The input is required"
               (let ((result (quiet-main (list "spec" "-o" "-"))))
                 (assert-eqv #:expect 1 #:got (car result))
                 (assert-contains #:expect (cdr result)
                                  #:got "--input")))

         (test "Both commands report their version"
               (let ((spec (quiet-main (list "spec" "--version")))
                     (generate (quiet-main (list "--version"))))
                 (assert-eqv #:expect 0 #:got (car spec))
                 (assert-eqv #:expect 0 #:got (car generate))
                 (assert-contains #:expect (cdr spec)
                                  #:got "guile-openapi-generator")
                 (assert-contains #:expect (cdr generate)
                                  #:got "guile-openapi-generator")))))

(define yaml-suite
  (suite "YAML"
         (test "The generated document is read back by the reader"
               (let ((operations (operations (example-routes))))
                 (assert-equal #:expect "Pet store" #:got (spec-title operations))
                 (assert-equal #:expect '(get-pets post-pets get-pet)
                               #:got (map operation-name
                                          (spec-operations operations)))
                 (assert-equal #:expect '("Pet") #:got (spec-schema-names operations))
                 (assert-equal #:expect '("petId")
                               #:got (map parameter-name
                                          (operation-parameters
                                           (car (filter (lambda (operation)
                                                          (eq? (operation-name operation)
                                                               'get-pet))
                                                        (spec-operations operations))))))))

         (test "A specification is written to a file"
               (with-temporary-directory
                "routes"
                (lambda (directory)
                  (let ((path (string-append directory "/openapi.yaml")))
                    (assert-equal #:expect path
                                  #:got (routes->yaml-file (example-routes)
                                                           path
                                                           #:title "Pet store"))
                    (assert-equal #:expect "Pet store"
                                  #:got (spec-title
                                         (parse-spec (yaml-file->scm path)
                                                     #:source path)))))))))

(define error-suite
  (suite "Mistakes"
         (test "Two routes with one operationId are refused"
               (assert-error (lambda ()
                               (routes->document
                                (list (route 'get "/a" #:operation-id "same")
                                      (route 'get "/b" #:operation-id "same"))))))

         (test "Two routes with one method and path are refused"
               (assert-error (lambda ()
                               (routes->document (list (route 'get "/pets")
                                                       (route 'get "/pets/"))))))

         (test "An extension that is not one is refused"
               (assert-error (lambda ()
                               (route 'get "/pets"
                                      #:extensions (list (cons "artanis" #t))))))

         (test "A server has to be a URL or an object"
               (assert-error (lambda ()
                               (routes->document (list (route 'get "/pets"))
                                                 #:servers (list 42)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               route-suite
               parameter-suite
               document-suite
               yaml-suite
               command-line-suite
               error-suite))
