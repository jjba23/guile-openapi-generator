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
;; A package definition for the working tree.  Build or enter it with:
;;
;;   guix shell -f guix.scm -- guile -L ./src -c '(use-modules (openapi-generator))'
;;
;; Generated code depends on `(openapi-generator runtime)' and
;; `(openapi-generator jso)' at run time, which is why guile-json is a
;; propagated input.

(use-modules (guix packages)
             (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix build-system guile)
             ((guix licenses)
              #:prefix license:))

(define %source-dir
  (dirname (current-filename)))

(define-public guile-openapi-generator
  (package
    (name "guile-openapi-generator")
    (version "dev")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"))
    (propagated-inputs (list guile-json-4))
    (inputs (list guile-3.0))
    (synopsis "OpenAPI 3 to Guile Scheme code generator")
    (description
     "guile-openapi-generator reads OpenAPI 3.x specifications written in YAML or
JSON and generates Guile Scheme code: validated record data models, controllers
(plain Guile or Artanis) and HTTP clients (plain Guile or libcurl), including
support for polymorphism, enumerations, JSON merge patch and the security
schemes described by the specification.")
    (home-page "https://codeberg.org/jjba23/guile-openapi-generator")
    (license license:agpl3+)))

guile-openapi-generator
