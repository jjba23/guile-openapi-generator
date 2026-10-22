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
;; What a generation run is configured with.  The record lives in its own module
;; so the model, controller and client generators can all read it without one of
;; them having to depend on the driver that calls them.

(define-module (openapi-generator config)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (openapi-generator naming)
  #:export (<config>
            config?
            make-config
            config-output
            config-root
            config-project
            config-controllers
            config-http
            config-models?
            config-readme?
            config-runtime-module
            config-jso-module
            config-json-module
            config-source
            config-generator-version
            config-self-contained?
            config-base-url
            config-module
            config-file
            config-validate?
            config-model-suffix
            config-header?
            config-header-text
            config-dependencies
            config-request-response-models?
            config-exclude-read-only?
            config-exclude-write-only?
            config-x-extensible-enums?
            config-fault-tolerant-enums?
            config-non-null-response-payloads?
            controller-backends
            http-backends
            dependency-backends))

(define controller-backends
  '(vanilla artanis))

(define http-backends
  '(guile curl))

(define dependency-backends
  '(none guix))

(define-record-type <config>
  (make-config output
               root
               project
               controllers
               http
               models?
               runtime-module
               jso-module
               json-module
               source
               generator-version
               self-contained?
               base-url
               validate?
               readme?
               model-suffix
               header?
               header-text
               dependencies
               request-response-models?
               exclude-read-only?
               exclude-write-only?
               x-extensible-enums?
               fault-tolerant-enums?
               non-null-response-payloads?)
  config?
  (output config-output)
  (root config-root)
  (project config-project)
  (controllers config-controllers)
  (http config-http)
  (models? config-models?)
  (runtime-module config-runtime-module)
  (jso-module config-jso-module)
  (json-module config-json-module)
  (source config-source)
  (generator-version config-generator-version)
  (self-contained? config-self-contained?)
  (base-url config-base-url)
  (validate? config-validate?)
  (readme? config-readme?)
  (model-suffix config-model-suffix)
  (header? config-header?)
  (header-text config-header-text)
  (dependencies config-dependencies)
  (request-response-models? config-request-response-models?)
  (exclude-read-only? config-exclude-read-only?)
  (exclude-write-only? config-exclude-write-only?)
  (x-extensible-enums? config-x-extensible-enums?)
  (fault-tolerant-enums? config-fault-tolerant-enums?)
  (non-null-response-payloads? config-non-null-response-payloads?))

(define* (config-module config part)
  "Return the module name of one generated component, e.g. @code{(petstore models)}.

Passing @code{#f} returns the root module, @code{(petstore)}."
  (append (map string->symbol (string-split (to-kebab (config-root config))
                                            #\/))
          (if part (list part) '())))

(define* (config-file config part #:optional (suffix ""))
  "Return the path of a generated component, relative to the output directory.

@example
(config-file config 'models)  ;; => \"petstore/models.scm\"
(config-file config #f)       ;; => \"petstore.scm\"
@end example"
  (let ((directory (string-join (map symbol->string
                                     (map string->symbol
                                          (string-split (to-kebab (config-root config))
                                                        #\/)))
                                "/")))
    (string-append directory
                   (if part (format #f "/~a" (to-kebab part)) "")
                   suffix
                   ".scm")))
