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
;; The generator against specifications written for real APIs, not for this test
;; suite.  The three fixtures under @file{test/fixtures/real} are subsets of the
;; GitHub, Stripe and Kubernetes APIs: they bring dotted schema names, form
;; encoded bodies, cursor pagination, unions, allOf composition, three JSON
;; content types on one operation and response envelopes.  A synthetic fixture
;; written while the generator was being built tends to look like the generator;
;; these do not.
;;
;; Every suite generates a whole tree and runs a child Guile process against it,
;; so compilation and loading are part of the test rather than something a text
;; assertion hopes for.  The Kubernetes patch operation is then dispatched for
;; real, once per declared content type.

(define-module (veritas unit real-specs-spec)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator jso)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator yaml)
  #:use-module (openapi-generator yaml-writer)
  #:use-module (openapi-generator controllers)
  #:use-module (openapi-generator routes)
  #:use-module (openapi-generator guile-routes)
  #:use-module (openapi-support)
  #:export (spec))

;;; Helpers

(define (real name)
  (fixture (string-append "real/" name)))

(define (real-spec name)
  (fixture-spec (string-append "real/" name)))

(define (reported output key)
  "Return the value of the @code{KEY=VALUE} line of @var{output}, or #f."
  (let* ((prefix (string-append key "="))
         (line (find (lambda (line) (string-prefix? prefix line))
                     (string-split output #\newline))))
    (and line (substring line (string-length prefix)))))

(define (generated-report root input)
  "Generate a full tree for @var{input} and run the report program against it."
  (with-generated root
    (lambda (directory)
      (run-program directory
                   (list (list root 'models)
                         (list root 'controllers)
                         (list root 'client))
                   "real-specs"))
    #:input input))

(define (assert-generation root input operations)
  "Assert that a tree generated from @var{input} compiles and routes."
  (let ((output (generated-report root input)))
    (assert-equal #:expect (number->string operations)
                  #:got (reported output "routes")
                  #:name "one route per operation")
    (assert-equal #:expect "#t"
                  #:got (reported output "dispatch")
                  #:name "the dispatcher is a procedure")
    (assert-true (> (string->number (reported output "models")) 10)
                 #:name "the models module exports converters")
    (assert-false (contains? output "warning:")
                  #:name "generation is warning-free")))

(define (operation-keys spec)
  "Return the @code{method path} keys of the operations of @var{spec}, sorted."
  (sort (map (lambda (operation)
               (format #f "~a ~a"
                       (operation-method operation)
                       (operation-path operation)))
             (spec-operations spec))
        string<?))

(define (route-keys routes)
  "Return the @code{method path} keys of @var{routes}, sorted."
  (sort (map (lambda (route)
               (format #f "~a ~a" (route-method route) (route-path route)))
             routes)
        string<?))

(define (path-parameters spec path method)
  "Return the names of the path parameters of one operation of @var{spec}."
  (map parameter-name
       (filter (lambda (parameter)
                 (eq? (parameter-location parameter) 'path))
               (operation-parameters
                (find (lambda (operation)
                        (and (string=? (operation-path operation) path)
                             (eq? (operation-method operation) method)))
                      (spec-operations spec))))))

;;; Suites

(define github-suite
  (suite "GitHub"
         (test "The document parses into the operations it declares"
               (let ((spec (real-spec "github.yaml")))
                 (assert-equal #:expect "GitHub REST API (issues subset)"
                               #:got (spec-title spec))
                 (assert-equal #:expect 5
                               #:got (length (spec-operations spec)))
                 (assert-equal #:expect 8
                               #:got (length (spec-schemas spec)))))

         (test "A path level parameter belongs to every operation of the path"
               (assert-equal #:expect '("owner" "repo" "issue_number")
                             #:got (path-parameters (real-spec "github.yaml")
                                                    "/repos/{owner}/{repo}/issues/{issue_number}"
                                                    'patch)))

         (test "A response that refers to a component response is resolved"
               (let* ((spec (real-spec "github.yaml"))
                      (operation (find (lambda (operation)
                                         (eq? (operation-method operation) 'post))
                                       (filter (lambda (operation)
                                                 (string=? (operation-path operation)
                                                           "/repos/{owner}/{repo}/issues"))
                                               (spec-operations spec))))
                      (response (operation-response-for-status operation "422")))
                 (assert-true (response? response))
                 (assert-equal #:expect "Validation failed"
                               #:got (response-description response))
                 (assert-equal #:expect "application/json"
                               #:got (media-type-name
                                      (car (response-media-types response))))))

         (test "The generated tree compiles, loads and routes"
               (assert-generation 'real-github (real "github.yaml") 5))))

(define stripe-suite
  (suite "Stripe"
         (test "The document parses into the operations it declares"
               (let ((spec (real-spec "stripe.yaml")))
                 (assert-equal #:expect "Stripe API (customers and charges subset)"
                               #:got (spec-title spec))
                 (assert-equal #:expect 9
                               #:got (length (spec-operations spec)))
                 (assert-equal #:expect 13
                               #:got (length (spec-schemas spec)))))

         (test "A deepObject query parameter keeps its style and explode"
               (let* ((spec (real-spec "stripe.yaml"))
                      (operation (find (lambda (operation)
                                         (eq? (operation-method operation) 'get))
                                       (filter (lambda (operation)
                                                 (string=? (operation-path operation)
                                                           "/v1/charges"))
                                               (spec-operations spec))))
                      (created (find (lambda (parameter)
                                       (string=? (parameter-name parameter) "created"))
                                     (operation-parameters operation))))
                 (assert-equal #:expect "deepObject"
                               #:got (parameter-style created))
                 (assert-true (parameter-explode? created))))

         (test "A form and a JSON body are both declared for one operation"
               (let* ((spec (real-spec "stripe.yaml"))
                      (operation (find (lambda (operation)
                                         (string=? (operation-path operation) "/v1/charges"))
                                       (filter (lambda (operation)
                                                 (eq? (operation-method operation) 'post))
                                               (spec-operations spec))))
                      (media-types (map media-type-name
                                        (request-body-media-types
                                         (operation-request-body operation)))))
                 (assert-equal #:expect '("application/json"
                                          "application/x-www-form-urlencoded")
                               #:got (sort media-types string<?))))

         (test "The generated tree compiles, loads and routes"
               (assert-generation 'real-stripe (real "stripe.yaml") 9))))

(define kubernetes-suite
  (suite "Kubernetes"
         (test "The document parses into the operations it declares"
               (let ((spec (real-spec "kubernetes.yaml")))
                 (assert-equal #:expect "Kubernetes API (pods subset)"
                               #:got (spec-title spec))
                 (assert-equal #:expect 6
                               #:got (length (spec-operations spec)))
                 (assert-equal #:expect 14
                               #:got (length (spec-schemas spec)))))

         (test "A dotted schema name is kept as it is written"
               (assert-true
                (and (member "io.k8s.api.core.v1.Pod"
                             (map named-schema-name
                                  (spec-schemas (real-spec "kubernetes.yaml"))))
                     #t)))

         (test "The generated tree compiles, loads and routes"
               (assert-generation 'real-kubernetes (real "kubernetes.yaml") 6))

         (test "Three JSON media types are dispatched apart"
               (let ((text (emit-controllers (real-spec "kubernetes.yaml")
                                             (test-config #:root "kubernetes")))
                     (names '("application/json-patch+json"
                              "application/merge-patch+json"
                              "application/strategic-merge-patch+json")))
                 (for-each
                  (lambda (name)
                    (assert-true
                     (contains? text
                                (format #f "(string=? content-type ~s)" name))
                     #:name (format #f "~a is tested by name" name)))
                  names)
                 (assert-equal #:expect 3
                               #:got (occurrences
                                      text
                                      "(string=? content-type \"application/"))))

         (test "A body with no record behind it is decoded without a converter"
               (let ((text (emit-controllers (real-spec "kubernetes.yaml")
                                             (test-config #:root "kubernetes"))))
                 (assert-false (contains? text "jso->value")
                               #:name "no converter is named that does not exist")))))

(define negotiation-suite
  (suite "Content negotiation"
         #:concurrent? #f

         (test "Each declared content type reaches its handler as its own shape"
               (let ((output
                      (with-generated 'real-kubernetes-patch
                        (lambda (directory)
                          (run-program directory
                                       (list '(real-kubernetes-patch models)
                                             '(real-kubernetes-patch controllers))
                                       "kubernetes-patch"))
                        #:input (real "kubernetes.yaml"))))
                 (assert-equal #:expect '("json-patch list=#t items=2"
                                          "merge pod=#t policy=Never"
                                          "merge pod=#t policy=OnFailure")
                               #:got (remove string-null?
                                             (map (lambda (line)
                                                    (if (string-prefix? ";;;" line)
                                                        ""
                                                        line))
                                                  (string-split output #\newline))))))))

(define round-trip-suite
  (suite "Reading back what was written"
         (test "A real document survives the YAML writer"
               (for-each
                (lambda (name)
                  (let* ((path (real name))
                         (document (yaml-file->scm path))
                         (written (jso->yaml-string document)))
                    (assert-equal #:expect (jso-normalize document)
                                  #:got (jso-normalize
                                         (yaml-string->scm written))
                                  #:name (format #f "~a round trips" name))))
                '("github.yaml" "stripe.yaml" "kubernetes.yaml")))

         (test "The controllers of a real API read back as the same document"
               (for-each
                (lambda (name)
                  (let* ((spec (real-spec name))
                         (routes (guile-routes-from-string
                                  (emit-controllers
                                   spec
                                   (test-config #:root "recovered"))))
                         (recovered
                          (parse-spec
                           (yaml-string->scm
                            (routes->yaml routes
                                          #:title (spec-title spec)
                                          #:version "1.0.0"))
                           #:source "recovered")))
                    (assert-equal #:expect (operation-keys spec)
                                  #:got (operation-keys recovered)
                                  #:name (format #f "~a keeps its operations" name))))
                '("github.yaml" "stripe.yaml" "kubernetes.yaml")))))

(define (spec)
  (veritas-run #:concurrent? #f
               github-suite
               stripe-suite
               kubernetes-suite
               negotiation-suite
               round-trip-suite))
