;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Slice B of the input surface: JSON Schema documents, API fragments, external
;; references and remote specifications.  These tests are about *where a
;; document comes from* and *what its references point at*, so most of them end
;; by generating a tree and reading the models back, rather than stopping at the
;; parsed document.

(define-module (veritas unit input-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (openapi-generator generator)
  #:use-module (openapi-generator bundle)
  #:use-module (openapi-generator json-schema)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator cli)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define (join directory relative)
  (string-append directory "/" relative))

(define (quiet-main arguments)
  "Run the command line with @var{arguments}, with its report silenced."
  (parameterize ((current-output-port (open-output-string)))
    (main arguments)))

(define* (generate-into directory #:key input json-schema-file fragments
                        (resolve-external 'targeted))
  "Generate models from INPUT or JSON-SCHEMA-FILE into DIRECTORY under module app."
  (generate #:input input
            #:json-schema-file json-schema-file
            #:api-fragments (or fragments '())
            #:resolve-external resolve-external
            #:output directory
            #:module "app"
            #:controllers 'none
            #:http 'none
            #:readme? #f))

(define* (models-text directory #:optional (module "app"))
  (file-contents (join directory (string-append module "/models.scm"))))

;;; Remote specification server

(define remote-api
  (string-append
   "openapi: 3.0.3\n"
   "info: {title: Remote, version: 1.0.0}\n"
   "paths:\n"
   "  /things:\n"
   "    get:\n"
   "      operationId: listThings\n"
   "      responses:\n"
   "        '200':\n"
   "          description: things\n"
   "          content:\n"
   "            application/json:\n"
   "              schema:\n"
   "                type: array\n"
   "                items:\n"
   "                  $ref: 'schemas.yaml#/components/schemas/Remote'\n"))

(define remote-schemas
  (string-append
   "components:\n"
   "  schemas:\n"
   "    Remote:\n"
   "      type: object\n"
   "      required: [id]\n"
   "      properties:\n"
   "        id: {type: string}\n"))

(define requests '())

(define (yaml-response text)
  (values (build-response #:code 200
                          #:headers '((content-type . (text/yaml))))
          text))

(define (handler request body)
  (let ((path (uri-path (request-uri request)))
        (authorization (let ((entry (assq 'authorization
                                         (request-headers request))))
                         (and entry (cdr entry)))))
    (set! requests (cons (cons path authorization) requests))
    (cond
      ((string=? path "/api.yaml") (yaml-response remote-api))
      ((string=? path "/schemas.yaml") (yaml-response remote-schemas))
      (else (values (build-response #:code 404) "")))))

(define base-port
  (+ 9500 (modulo (getpid) 300)))

(define next-port base-port)

(define (take-port)
  (let ((port next-port))
    (set! next-port (+ port 1))
    port))

(define (port-open? port)
  (catch #t
    (lambda ()
      (let ((connection (socket PF_INET SOCK_STREAM 0)))
        (connect connection
                 (make-socket-address AF_INET
                                      (inet-pton AF_INET "127.0.0.1")
                                      port))
        (close connection)
        #t))
    (lambda (key . arguments) #f)))

(define (wait-for-server port)
  (let loop ((attempts 0))
    (cond
      ((port-open? port) #t)
      ((> attempts 200)
       (error "The input test server never accepted a connection" port))
      (else (usleep 25000) (loop (+ attempts 1))))))

(define (with-server thunk)
  (let* ((port (take-port))
         (thread (call-with-new-thread
                  (lambda ()
                    (catch #t
                      (lambda ()
                        (run-server handler 'http (list #:port port
                                                        #:host "127.0.0.1")))
                      (lambda (key . arguments) #f))))))
    (wait-for-server port)
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk port))
      (lambda ()
        (when (thread? thread)
          (cancel-thread thread))))))

;;; Suites

(define json-schema-suite
  (suite "JSON Schema input"
         #:concurrent? #f

         (test "A JSON Schema becomes a component and its definitions"
               (with-temporary-directory "json-schema"
                 (lambda (directory)
                   (generate-into directory
                                  #:json-schema-file
                                  (fixture "json-schema/person.json"))
                   (let ((text (models-text directory)))
                     (assert-true (contains? text "(define-record-type <person>"))
                     (assert-true (contains? text "(define-record-type <address>"))
                     (assert-true (contains? text "(define (jso->address node)"))
                     (assert-true (contains? text "(person-address value)"))))))

         (test "The root name and a nested pointer are honoured"
               (with-temporary-directory "json-schema-pointer"
                 (lambda (directory)
                   (generate #:json-schema-file
                             (string-append
                              (fixture "json-schema/wrapper.json")
                              "#/spec/schemaObject")
                             #:json-schema-root-name "Wrapped"
                             #:output directory
                             #:module "app"
                             #:controllers 'none
                             #:http 'none
                             #:readme? #f)
                   (let ((text (models-text directory)))
                     (assert-true (contains? text
                                             "(define-record-type <wrapped>"))
                     (assert-true (contains? text "(wrapped-id value)"))))))

         (test "Draft-04 bounds and const are understood"
               (with-temporary-directory "json-schema-draft4"
                 (lambda (directory)
                   (generate-into directory
                                  #:json-schema-file
                                  (fixture "json-schema/draft4.json"))
                   (let ((text (models-text directory)))
                     (assert-true (contains? text
                                             "(check-exclusive-minimum"))
                     (assert-true (contains? text "measure-unit-values"))
                     (assert-true (contains? text "check-enum"))))))

         (test "The generated records load and validate"
               (with-temporary-directory "json-schema-model"
                 (lambda (directory)
                   (generate-into directory
                                  #:json-schema-file
                                  (fixture "json-schema/person.json"))
                   (assert-equal
                    #:expect '("Ada" "Main 1" "#t" "36")
                    #:got (lines (run-program directory
                                              (list '(app models))
                                              "json-schema-model"))))))

         (test "The command line accepts --json-schema-file"
               (with-temporary-directory "json-schema-cli"
                 (lambda (directory)
                   (let ((status (quiet-main
                                  (list "generate"
                                        "--json-schema-file"
                                        (fixture "json-schema/person.json")
                                        "--output" directory
                                        "--module" "app"
                                        "--targets" "models"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (contains? (models-text directory)
                                             "<person>"))))))

         (test "Input and JSON Schema are mutually exclusive"
               (assert-eqv #:expect 1
                           #:got (quiet-main
                                  (list "generate"
                                        "--input" (fixture "mini.yaml")
                                        "--json-schema-file"
                                        (fixture "json-schema/person.json")
                                        "--output" "/tmp/never"
                                        "--targets" "models"))))))

(define external-suite
  (suite "External references"
         #:concurrent? #f

         (test "Targeted resolution follows only what is referenced"
               (with-temporary-directory "external-targeted"
                 (lambda (directory)
                   (generate-into directory
                                  #:input (fixture "external/main.yaml"))
                   (let ((text (models-text directory)))
                     (assert-true (contains? text "(define-record-type <user>"))
                     (assert-true (contains? text "(define-record-type <address>"))
                     (assert-false (contains? text
                                              "(define-record-type <unused>"))))))

         (test "Aggressive resolution adds every schema of a referenced file"
               (with-temporary-directory "external-aggressive"
                 (lambda (directory)
                   (generate #:input (fixture "external/main.yaml")
                             #:resolve-external 'aggressive
                             #:output directory
                             #:module "app"
                             #:controllers 'none
                             #:http 'none
                             #:readme? #f)
                   (let ((text (models-text directory)))
                     (assert-true (contains? text
                                             "(define-record-type <unused>"))
                     (assert-true (contains? text
                                             "(define-record-type <local-user>"))))))

         (test "A reference cycle terminates and points both ways"
               (with-temporary-directory "external-cycle"
                 (lambda (directory)
                   (generate-into directory
                                  #:input (fixture "external/cycle-a.yaml"))
                   (let ((text (models-text directory)))
                     (assert-true (contains? text "(define-record-type <a>"))
                     (assert-true (contains? text "(define-record-type <b>"))
                     (assert-true (contains? text "(jso->b (jso-ref node \"b\"))"))))))

         (test "An unknown resolution mode is an error"
               (assert-eqv #:expect 1
                           #:got (quiet-main
                                  (list "generate"
                                        "--input" (fixture "external/main.yaml")
                                        "--output" "/tmp/never"
                                        "--resolve-external" "sometimes"))))))

(define fragment-suite
  (suite "API fragments"
         #:concurrent? #f

         (test "A fragment adds paths and schemas to the primary document"
               (with-temporary-directory "fragments"
                 (lambda (directory)
                   (generate #:input (fixture "fragment/base.yaml")
                             #:api-fragments
                             (list (fixture "fragment/extra.yaml"))
                             #:output directory
                             #:module "app"
                             #:readme? #t)
                   (let ((models (models-text directory))
                         (controllers (file-contents
                                       (join directory "app/controllers.scm"))))
                     (assert-true (contains? models "(define-record-type <pet>"))
                     (assert-true (contains? models "(define-record-type <toy>"))
                     (assert-true (contains? controllers "list-pets"))
                     (assert-true (contains? controllers "list-toys"))))))

         (test "The command line accepts a fragment"
               (with-temporary-directory "fragments-cli"
                 (lambda (directory)
                   (let ((status (quiet-main
                                  (list "generate"
                                        "--input" (fixture "fragment/base.yaml")
                                        "--api-fragment"
                                        (fixture "fragment/extra.yaml")
                                        "--output" directory
                                        "--module" "app"
                                        "--targets" "models"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (contains? (models-text directory)
                                             "(define-record-type <toy>"))))))))

(define remote-suite
  (suite "Remote input and authentication"
         #:concurrent? #f

         (test "A remote spec and its relative reference are fetched with auth"
               (set! requests '())
               (with-server
                (lambda (port)
                  (with-temporary-directory "remote"
                    (lambda (directory)
                      (let* ((base (format #f "http://127.0.0.1:~a" port))
                             (status (quiet-main
                                      (list "generate"
                                            "--input"
                                            (string-append base "/api.yaml")
                                            "--auth" "Authorization: Bearer secret"
                                            "--resolve-external" "targeted"
                                            "--output" directory
                                            "--module" "app"
                                            "--targets" "models"))))
                        (assert-eqv #:expect 0 #:got status)
                        (assert-true (contains?
                                      (models-text directory)
                                      "(define-record-type <remote>")))))))
               (let ((api (find (lambda (entry)
                                  (string=? (car entry) "/api.yaml"))
                                requests))
                     (schemas (find (lambda (entry)
                                      (string=? (car entry) "/schemas.yaml"))
                                    requests)))
                 (assert-true (if api #t #f)
                              #:name "the primary spec was fetched")
                 (assert-true (if schemas #t #f)
                              #:name "the external schema was fetched")
                 (when api
                   (assert-equal #:expect '(bearer secret)
                                 #:got (cdr api)))
                 (when schemas
                   (assert-equal #:expect '(bearer secret)
                                 #:got (cdr schemas)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               json-schema-suite external-suite fragment-suite remote-suite))
