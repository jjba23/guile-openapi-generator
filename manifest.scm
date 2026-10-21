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
;; The development environment: Guile, guile-json (JSON parsing),
;; guile-veritas (tests), guile-curl (to load and exercise generated libcurl
;; clients) and the documentation tooling used by `maak docs'.

(use-modules (guix packages)
             (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages curl)
             (gnu packages rust-apps)
             (gnu packages texinfo)
             (guix build-system guile)
             ((guix licenses)
              #:prefix license:))

(define-public guile-veritas
  (package
    (name "guile-veritas")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/veritas.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11n9gyqb7nc0zw4z0fadmw4bd6k673314wpdd66mllwxs2xfcs0q"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers guile-json-4))
    (home-page "https://codeberg.org/jjba23/veritas")
    (synopsis "Testing framework for Guile")
    (description
     "Veritas is a testing framework for Guile with an @acronym{EDSL,
embedded domain specific language} to define test suites.  Emphasis is placed
on legible, maintainable tests.  Veritas shuffles tests and runs them
concurrently by default.")
    (license license:lgpl3+)))

(packages->manifest (list (specification->package "make")
                          guile-next
                          guile-json-4
                          guile-curl
                          guile-documenta
                          texinfo
                          watchexec
                          guile-veritas))
