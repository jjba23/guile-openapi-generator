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
;; The driver and the command line: that a run writes what it says it wrote,
;; that the choices really change the output, that a self-contained tree loads
;; with nothing but its own directory on the load path, and that a typo in an
;; option is an error rather than a silently different program.

(define-module (veritas unit generator-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator generator)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator config-file)
  #:use-module (openapi-generator naming)
  #:use-module (openapi-generator cli)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define (join directory relative)
  (string-append directory "/" relative))

(define (component report name)
  "Return the path of @var{name} in the report of a generation run."
  (let ((entry (assq name report)))
    (and entry (cdr entry))))

(define* (generate-and-check name
                             #:key
                             (input (fixture "petstore.yaml"))
                             (module "petstore")
                             (controllers 'vanilla)
                             (http 'guile)
                             (self-contained? #f)
                             (expected '())
                             (absent '()))
  "Generate into a fresh directory and check which files it wrote.

Returns the report of the run.  The directory is removed on the way out, so the
report is only useful for its keys and for the assertions made here."
  (with-temporary-directory name
    (lambda (directory)
      (let ((report (generate #:input input
                              #:output directory
                              #:module module
                              #:controllers controllers
                              #:http http
                              #:self-contained? self-contained?)))
        (for-each (lambda (relative)
                    (assert-true (file-exists? (join directory relative))
                                 #:name relative))
                  expected)
        (for-each (lambda (relative)
                    (assert-false (file-exists? (join directory relative))
                                  #:name relative))
                  absent)
        report))))

(define (quiet thunk)
  "Call @var{thunk} with the standard output discarded."
  (parameterize ((current-output-port (open-output-string)))
    (thunk)))

;;; Suites

(define report-suite
  (suite "Generation report"
         #:concurrent? #f
         (test "A run writes models, controllers, a client and a README"
               (let ((report (generate-and-check
                              "report"
                              #:expected '("petstore/models.scm"
                                           "petstore/controllers.scm"
                                           "petstore/client.scm"
                                           "petstore/README.org"))))
                 (assert-equal #:expect 4 #:got (length report))
                 (assert-equal #:expect 'models #:got (caar report))))

         (test "The README names the API and its operations"
               (with-temporary-directory "readme"
                 (lambda (directory)
                   (generate #:input (fixture "petstore.yaml")
                             #:output directory
                             #:module "petstore")
                   (let ((readme (file-contents (join directory
                                                      "petstore/README.org"))))
                     (assert-true (contains? readme "#+title: Petstore"))
                     (assert-true (contains? readme
                                             "| GET | /pets | =list-pets= |"))))))

         (test "Models only, when asked"
               (let ((report (generate-and-check
                              "models-only"
                              #:module "only"
                              #:controllers 'none
                              #:http 'none
                              #:expected '("only/models.scm"
                                           "only/README.org")
                              #:absent '("only/controllers.scm"
                                         "only/client.scm"))))
                 (assert-equal #:expect '(models readme)
                               #:got (map car report))))

         (test "The Artanis adapter is written when it is asked for"
               (generate-and-check "artanis"
                                   #:module "artanis-app"
                                   #:controllers 'artanis
                                   #:expected '("artanis-app/artanis.scm"
                                                "artanis-app/controllers.scm")))

         (test "The client backend changes the generated client"
               (let ((text (with-temporary-directory
                            "curl"
                            (lambda (directory)
                              (generate #:input (fixture "petstore.yaml")
                                        #:output directory
                                        #:module "curl-app"
                                        #:http 'curl)
                              (file-contents (join directory
                                                   "curl-app/client.scm"))))))
                 (assert-true (contains? text "#:use-module (curl)")
                              #:name "the curl backend was chosen")))

         (test "The module name defaults to the API title"
               (let ((report (generate-and-check "auto-module"
                                                 #:input (fixture "mini.yaml")
                                                 #:module #f
                                                 #:expected '("mini/models.scm"))))
                 (assert-true (if (component report 'models) #t #f))))))

(define self-contained-suite
  (suite "Self-contained trees"
         #:concurrent? #f
         (test "The runtime is copied next to the generated code"
               (generate-and-check "self-contained"
                                   #:module "vendored"
                                   #:self-contained? #t
                                   #:expected '("vendored/jso.scm"
                                                "vendored/runtime.scm"
                                                "vendored/models.scm")))

         (test "The copy imports the copy"
               (with-generated 'copied
                 (lambda (directory)
                   (let ((text (file-contents (join directory
                                                    "copied/runtime.scm"))))
                     (assert-true (contains? text "#:use-module (copied jso)"))))
                 #:self-contained? #t))

         (test "The copied tree loads with only its own directory on the path"
               (with-generated 'vendored-app
                 (lambda (directory)
                   (assert-equal
                    #:expect '("cat" "Mia" "#t" "9" "#t" "#t")
                    #:got (lines (run-program directory
                                              (list '(vendored-app models))
                                              "models-construct"))))
                 #:input (fixture "polymorphism.yaml")
                 #:self-contained? #t))))

(define cli-suite
  (suite "Command line"
         (test "--help and --version succeed"
               (assert-eqv #:expect 0 #:got (quiet (lambda () (main (list "--help")))))
               (assert-eqv #:expect 0
                           #:got (quiet (lambda () (main (list "--version"))))))

         (test "A missing --input is an error"
               (assert-eqv #:expect 1
                           #:got (quiet (lambda () (main (list "generate"))))))

         (test "An unknown controller backend is an error"
               (assert-eqv #:expect 1
                           #:got (quiet
                                  (lambda ()
                                    (main (list "generate"
                                                "--input" (fixture "petstore.yaml")
                                                "--output" "/tmp/never"
                                                "--controllers" "phoenix"))))))

         (test "Backend names are parsed"
               (assert-eq #:expect 'artanis #:got (parse-controllers "artanis"))
               (assert-eq #:expect 'none #:got (parse-controllers "NONE"))
               (assert-eq #:expect 'curl #:got (parse-backend "curl")))

         (test "generate is accepted as the first word"
               (with-temporary-directory "cli"
                 (lambda (directory)
                   (let ((status (quiet
                                  (lambda ()
                                    (main (list "generate"
                                                "--input" (fixture "petstore.yaml")
                                                "--output" directory
                                                "--module" "cli-app"
                                                "--no-models"
                                                "--http" "none"))))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-false (file-exists? (join directory
                                                       "cli-app/models.scm")))))))))

(define version-suite
  (suite "Version"
         (test "The generator reports its own version"
               (assert-true (string? generator-version))
               (assert-false (string-null? generator-version)))))

(define validation-suite
  (suite "Validating the specification"
         #:concurrent? #f
         (test "#:validate? decides whether an in-memory document is checked"
               (with-temporary-directory "validate"
                 (lambda (directory)
                   (let ((document (read-spec-string "swagger: '2.0'"
                                                     #:validate? #f)))
                     (assert-error
                      (lambda ()
                        (quiet (lambda ()
                                 (generate #:document document
                                           #:output directory
                                           #:module "strict"))))
                      #:name "a Swagger document is refused by default")
                     (let ((report (quiet
                                    (lambda ()
                                      (generate #:document document
                                                #:output directory
                                                #:module "lax"
                                                #:models? #f
                                                #:controllers 'none
                                                #:http 'none
                                                #:validate? #f)))))
                       (assert-equal #:expect '(readme)
                                     #:got (map car report)
                                     #:name "skipping validation still runs"))))))))

(define (run-main arguments)
  "Run the command line with @var{arguments}, with its report silenced."
  (quiet (lambda () (main arguments))))

(define configuration-suite
  (suite "Configuration"
         #:concurrent? #f

         (test "A config file is read with normalised keys"
               (let ((config (read-config-file
                              (fixture "config/generator.yaml"))))
                 (assert-equal #:expect "configured"
                               #:got (config-ref config 'module #f))
                 (assert-equal #:expect "Dto"
                               #:got (config-ref config 'model-suffix #f))
                 (assert-equal #:expect '(models readme)
                               #:got (config-targets
                                      (config-ref config 'targets #f)))
                 (assert-equal #:expect 'guix
                               #:got (string->symbol
                                      (format #f "~a"
                                              (config-ref config
                                                          'dependencies
                                                          #f))))))

         (test "A JSON config file is read the same way"
               (let ((config (read-config-file
                              (fixture "config/generator.json"))))
                 (assert-equal #:expect "json-configured"
                               #:got (config-ref config 'module #f))
                 (assert-equal #:expect "Json"
                               #:got (config-ref config 'model-suffix #f))
                 (assert-equal #:expect '(models)
                               #:got (config-targets
                                      (config-ref config 'targets #f)))
                 (assert-false (config-ref config 'header #t))))

         (test "The command line overrides the config file"
               (with-temporary-directory "config-cli"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--config"
                                        (fixture "config/generator.yaml")
                                        "--output" directory
                                        "--module" "from-cli"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (file-exists?
                                   (join directory "from-cli/models.scm")))
                     (assert-true (file-exists?
                                   (join directory "dependencies.scm"))
                                  #:name "the config file's guix dependencies are written")
                     (assert-false (file-exists?
                                    (join directory "configured/models.scm")))))))

         (test "--targets selects the parts to write"
               (with-temporary-directory "targets"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "petstore.yaml")
                                        "--output" directory
                                        "--targets" "models"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (file-exists?
                                   (join directory "petstore/models.scm")))
                     (assert-false (file-exists?
                                    (join directory "petstore/client.scm")))
                     (assert-false (file-exists?
                                    (join directory "petstore/controllers.scm")))
                     (assert-false (file-exists?
                                    (join directory "petstore/README.org")))))))

         (test "A client target brings the models it needs"
               (with-temporary-directory "targets-client"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "petstore.yaml")
                                        "--output" directory
                                        "--targets" "client"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (file-exists?
                                   (join directory "petstore/models.scm")))
                     (assert-true (file-exists?
                                   (join directory "petstore/client.scm")))
                     (assert-false (file-exists?
                                    (join directory "petstore/README.org")))))))

         (test "An unknown target is an error"
               (assert-eqv #:expect 1
                           #:got (run-main
                                  (list "generate"
                                        "--input" (fixture "petstore.yaml")
                                        "--output" "/tmp/never"
                                        "--targets" "models,clients"))))

         (test "--model-suffix renames the models"
               (with-temporary-directory "suffix"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "mini.yaml")
                                        "--output" directory
                                        "--module" "suffixed"
                                        "--model-suffix" "Dto"
                                        "--targets" "models"))))
                     (assert-eqv #:expect 0 #:got status)
                     (let ((text (file-contents
                                  (join directory "suffixed/models.scm"))))
                       (assert-true (contains? text
                                               "(define-record-type <thing-dto>"))
                       (assert-true (contains? text "(define* (make-thing-dto")))))))

         (test "--operation-id-transform renames the procedures"
               (with-temporary-directory "transform"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "petstore.yaml")
                                        "--output" directory
                                        "--module" "renamed"
                                        "--operation-id-transform"
                                        "getPetById:fetch-pet"
                                        "--targets" "models,client"))))
                     (assert-eqv #:expect 0 #:got status)
                     (let ((text (file-contents
                                  (join directory "renamed/client.scm"))))
                       (assert-true (contains? text "(define* (fetch-pet"))
                       (assert-false (contains? text "(define* (get-pet-by-id")))))))

         (test "A replacement template can refer to a capture group"
               (assert-equal #:expect "v2-pets"
                             #:got ((parse-operation-id-transform
                                     "^V2_(.*)$:v2-\\1")
                                    "V2_pets"))
               (assert-equal #:expect "create-pet"
                             #:got ((parse-operation-id-transform
                                     "^create_(.*)$:create-\\1")
                                    "create_pet")))

         (test "--no-validate skips the OpenAPI check"
               (with-temporary-directory "novalidate"
                 (lambda (directory)
                   (assert-eqv #:expect 1
                               #:got (run-main
                                      (list "generate"
                                            "--input" (fixture "swagger2.yaml")
                                            "--output" directory
                                            "--targets" "readme")))
                   (assert-eqv #:expect 0
                               #:got (run-main
                                      (list "generate"
                                            "--input" (fixture "swagger2.yaml")
                                            "--output" directory
                                            "--targets" "readme"
                                            "--no-validate"))))))

         (test "--no-header omits the banner and --header-file adds a paragraph"
               (with-temporary-directory "header"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "mini.yaml")
                                        "--output" directory
                                        "--module" "plain"
                                        "--targets" "models"
                                        "--no-header"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-false
                      (contains? (file-contents
                                  (join directory "plain/models.scm"))
                                 "Generated by guile-openapi-generator")))
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "mini.yaml")
                                        "--output" directory
                                        "--module" "licensed"
                                        "--targets" "models"
                                        "--header-file"
                                        (fixture "config/header.txt")))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true
                      (contains? (file-contents
                                  (join directory "licensed/models.scm"))
                                 "SPDX-License-Identifier"))))))

         (test "--dependencies guix writes a manifest"
               (with-temporary-directory "deps"
                 (lambda (directory)
                   (let ((status (run-main
                                  (list "generate"
                                        "--input" (fixture "petstore.yaml")
                                        "--output" directory
                                        "--targets" "models"
                                        "--dependencies" "guix"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (file-exists?
                                   (join directory "dependencies.scm")))
                     (assert-true
                      (contains? (file-contents
                                  (join directory "dependencies.scm"))
                                 "guile-json"))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               report-suite self-contained-suite cli-suite version-suite
               validation-suite configuration-suite))
