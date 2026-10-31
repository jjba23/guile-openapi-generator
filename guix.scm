;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
    (license license:expat)))

guile-openapi-generator
