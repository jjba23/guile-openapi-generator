;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Directional models, enumerations that tolerate new values, and discriminators
;; inherited from a composition parent.  The first group is the one that changes
;; the shape of a generated model: a `readOnly` member is not something a request
;; can send, and a `writeOnly` member is not something a response can return.
;;
;; The tests read the generated text for the contract (which accessor exists and
;; which required check is emitted), and then run the records for the behaviour,
;; because "the field is gone" and "the constructor no longer asks for it" are
;; different claims.

(define-module (veritas unit directional-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator cli)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (lines output)
  (filter (lambda (line) (not (string-null? line)))
          (map string-trim-both (string-split output #\newline))))

(define (join directory relative)
  (string-append directory "/" relative))

(define (module-text directory root part)
  (file-contents (join directory (format #f "~a/~a.scm" root part))))

(define (models-text directory root)
  (module-text directory root "models"))

(define (client-text directory root)
  (module-text directory root "client"))

(define (controllers-text directory root)
  (module-text directory root "controllers"))

(define (quiet-main arguments)
  "Run the command line with @var{arguments}, with its report silenced."
  (parameterize ((current-output-port (open-output-string)))
    (main arguments)))

;;; Suites

(define directional-suite
  (suite "Directional models"
         #:concurrent? #f

         (test "A single model set keeps readOnly and writeOnly members optional"
               (with-generated 'directional-default
                 (lambda (directory)
                   (assert-equal
                    #:expect '("Ada" "#t")
                    #:got (lines (run-program
                                  directory
                                  (list '(directional-default models))
                                  "directional-default"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none))

         (test "Excluding readOnly members leaves a request-shaped model"
               (with-generated 'directional-read-only
                 (lambda (directory)
                   (let ((text (models-text directory "directional-read-only")))
                     (assert-false (contains? text "(id account-id)"))
                     (assert-false (contains?
                                    text "(nickname account-nickname)"))
                     (assert-true (contains?
                                   text "(password account-password)"))
                     (assert-true
                      (contains?
                       text "(check-required '(\"account\" \"password\")"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:exclude-read-only? #t))

         (test "Excluding writeOnly members leaves a response-shaped model"
               (with-generated 'directional-write-only
                 (lambda (directory)
                   (let ((text (models-text directory "directional-write-only")))
                     (assert-false (contains?
                                    text "(password account-password)"))
                     (assert-true (contains? text "(id account-id)"))
                     (assert-true
                      (contains?
                       text "(check-required '(\"account\" \"id\")"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:exclude-write-only? #t))

         (test "Request and Response sets differ in the right members"
               (with-generated 'directional-sets
                 (lambda (directory)
                   (let ((text (models-text directory "directional-sets")))
                     (assert-true
                      (contains? text "(define-record-type <account-request>"))
                     (assert-true
                      (contains? text "(define-record-type <account-response>"))
                     (assert-false
                      (contains? text "(id account-request-id)"))
                     (assert-true
                      (contains? text "(password account-request-password)"))
                     (assert-false
                      (contains? text "(password account-response-password)"))
                     (assert-true
                      (contains? text "(id account-response-id)"))
                     (assert-true
                      (contains?
                       text "(check-required '(\"account-request\" \"password\")"))
                     (assert-true
                      (contains?
                       text "(check-required '(\"account-response\" \"id\")"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:request-response-models? #t))

         (test "Inline request and response models are not duplicated"
               (with-generated 'directional-inline
                 (lambda (directory)
                   (let ((text (models-text directory "directional-inline")))
                     (assert-equal
                      #:expect 1
                      #:got (occurrences
                             text
                             "(define-record-type <create-profile-request>"))
                     (assert-equal
                      #:expect 1
                      #:got (occurrences
                             text
                             "(define-record-type <create-profile-201-response>"))
                     (assert-true
                      (contains? text
                                 "(secret create-profile-request-secret)"))
                     (assert-false
                      (contains? text
                                 "(secret create-profile-201-response-secret)"))
                     (assert-true
                      (contains? text
                                 "(id create-profile-201-response-id)"))
                     (assert-false
                      (contains? text "(id create-profile-request-id)"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:request-response-models? #t))

         (test "An operation response does not overwrite a component response"
               (with-generated 'directional-collision
                 (lambda (directory)
                   (let ((text (models-text directory
                                            "directional-collision")))
                     (assert-equal
                      #:expect 1
                      #:got (occurrences
                             text
                             "(define-record-type <sign-in-response>"))
                     (assert-equal
                      #:expect 1
                      #:got (occurrences
                             text
                             "(define-record-type <sign-in-response-2>"))))
                 #:input (fixture "directional-collision.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:request-response-models? #t))

         (test "The client sends a request model and decodes a response model"
               (with-generated 'directional-client
                 (lambda (directory)
                   (let ((text (client-text directory "directional-client")))
                     (assert-true (contains? text "(account-request->jso body)"))
                     (assert-true (contains? text "(jso->account-response")))
                   (let ((controllers
                          (controllers-text directory "directional-client")))
                     (assert-true
                      (contains? controllers "(jso->account-request"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'vanilla
                 #:http 'guile
                 #:request-response-models? #t))

         (test "The Request and Response records load and round-trip"
               (with-generated 'directional-models
                 (lambda (directory)
                   (assert-equal
                    #:expect '("Ada" "12345678" "a1" "#f" "#t" "#f" "#t")
                    #:got (lines (run-program
                                  directory
                                  (list '(directional-models models))
                                  "directional-models"))))
                 #:input (fixture "directional.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:request-response-models? #t))

         (test "The command line exposes the directional options"
               (with-temporary-directory "directional-cli"
                 (lambda (directory)
                   (let ((status (quiet-main
                                  (list "generate"
                                        "--input" (fixture "directional.yaml")
                                        "--output" directory
                                        "--module" "app"
                                        "--targets" "models"
                                        "--request-response-models"))))
                     (assert-eqv #:expect 0 #:got status)
                     (assert-true (contains? (models-text directory "app")
                                             "<account-request>"))))))))

(define enums-suite
  (suite "Enumerations"
         #:concurrent? #f

         (test "An extensible enum is only an enum when asked"
               (with-generated 'enums-off
                 (lambda (directory)
                   (assert-false (contains? (models-text directory "enums-off")
                                            "mood-values")))
                 #:input (fixture "enums.yaml")
                 #:controllers 'none
                 #:http 'none)
               (with-generated 'enums-on
                 (lambda (directory)
                   (let ((text (models-text directory "enums-on")))
                     (assert-true (contains? text "mood-values"))
                     (assert-true (contains? text "\"happy\""))))
                 #:input (fixture "enums.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:x-extensible-enums? #t))

         (test "Fault-tolerant enums map unknown values to unrecognized"
               (with-generated 'enums-tolerant
                 (lambda (directory)
                   (assert-equal
                    #:expect '("unrecognized" "#t" "unrecognized" "#t")
                    #:got (lines (run-program
                                  directory
                                  (list '(enums-tolerant models))
                                  "models-enums"))))
                 #:input (fixture "enums.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:x-extensible-enums? #t
                 #:fault-tolerant-enums? #t))

         (test "Without fault tolerance an unknown enum still fails validation"
               (with-generated 'enums-strict
                 (lambda (directory)
                   (assert-equal
                    #:expect '("(pet status)")
                    #:got (lines (run-program
                                  directory
                                  (list '(enums-strict models))
                                  "models-enums-strict"))))
                 #:input (fixture "enums.yaml")
                 #:controllers 'none
                 #:http 'none
                 #:x-extensible-enums? #t))))

(define inherited-discriminator-suite
  (suite "Inherited discriminators"
         #:concurrent? #f

         (test "A oneOf inherits the discriminator of its branches' parent"
               (with-generated 'inherited-discriminator
                 (lambda (directory)
                   (assert-equal
                    #:expect '("#t" "2" "#t" "3")
                    #:got (lines (run-program
                                  directory
                                  (list '(inherited-discriminator models))
                                  "models-inherited-discriminator"))))
                 #:input (fixture "inherited-discriminator.yaml")
                 #:controllers 'none
                 #:http 'none))))

(define (spec)
  (veritas-run #:concurrent? #f
               directional-suite enums-suite inherited-discriminator-suite))
