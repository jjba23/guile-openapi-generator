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
;; The public face of the generator.  Two entry points cover almost every use:
;;
;; @example
;; (use-modules (openapi-generator))
;;
;; ;; From a file, with the defaults: models, plain Guile controllers and a
;; ;; plain Guile client, in ./generated.
;; (generate-from-file \"petstore.yaml\" #:output \"gen\")
;;
;; ;; From a document already in memory, with an Artanis server and libcurl.
;; (generate #:document document
;;           #:output \"gen\"
;;           #:module \"petstore\"
;;           #:controllers 'artanis
;;           #:http 'curl)
;; @end example
;;
;; Everything else the library can do — parsing a specification into records,
;; turning a schema into a shape, generating only the models — is available from
;; the modules below, which are stable enough to be used directly.

(define-module (openapi-generator)
  #:declarative? #t
  #:use-module (openapi-generator generator)
  #:use-module (openapi-generator reader)
  #:use-module (openapi-generator spec)
  #:use-module (openapi-generator config)
  #:re-export (generate
               generate-from-file
               generate-from-string
               generator-version
               read-spec
               read-spec-file
               read-spec-string
               parse-spec
               spec-title
               spec-version
               spec-operations
               spec-schemas
               spec-security-schemes
               operation-name
               operation-method
               operation-path)
  #:export (yaml-available?))

(define (yaml-available?)
  "Return @code{#t}: the YAML reader is implemented in Scheme, so YAML
specifications are always supported."
  #t)
