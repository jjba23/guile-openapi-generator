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

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; The container helpers are imported under a prefix because the tasks
  ;; below are named the way maak exposes them on the command line.
  #:use-module ((maak container) #:prefix ct:)
  #:export (test
            check
            generate
            fmt
            docs
            deploy
            repl
            hot-reload
            hr
            dev
            clean
            default
            container-build
            container-run
            package-docker))

;; ====== Maak file constants ======

(define project-name
  'guile-openapi-generator)

;; The URL for the CSS style-sheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

;; The absolute path to the directory where the API documentation is deployed.
(define deploy-api-docs-dir
  "/srv/http/jointhefreeworld.org")

(define hot-reload-file-types
  '(scm yaml json org))

;; The example specification the `generate' task runs the generator on.
(define example-spec
  "examples/petstore.yaml")

(define example-output
  "dist/petstore")

;; ====== Maak file helpers ======

(define (guile-run . code)
  "Runs CODE, a list of forms as a single string, with src/ and test/ loaded."
  (time-machine-manifest-shell (list "guile -L ./src -L ./test"
                                     (~ "-c '~a'"
                                        (apply string-append code)))))

;; ====== Maak file tasks ======

(define (test)
  "Run the project's Veritas test suites."
  (guile-run "((@ (veritas runner) run-tests))"))

(define (check)
  "Compile every source and test module to catch syntax and reference errors.

The trees the test suite generates under test/generated and the fixture programs
in test/fixtures/programs are skipped: they are output or fragments, and they
only make sense with a generated tree on the load path.

The load path is exported for the whole pipeline, not just for `find`: an
assignment in front of a pipeline applies to that one command, so `guild compile`
would otherwise run without src/ and test/ on its path."
  (time-machine-manifest-shell
   (list (string-append
          "export GUILE_LOAD_PATH=./src:./test:$GUILE_LOAD_PATH; "
          "find src test -name '*.scm' "
          "-not -path 'test/generated/*' "
          "-not -path 'test/fixtures/programs/*' -print0 "
          "| xargs -0 -n 1 guild compile "
          "-o \"${TMPDIR:-/tmp}/guile-openapi-generator-check.go\""))))

(define (generate)
  "Generate a client, controllers and models from the example specification."
  (delete-file-recursively example-output)
  (mkdir-p example-output)
  (time-machine-manifest-shell
   (list (~ "guile -L ./src -c '((@ (openapi-generator cli) main!) (list"
            " \"generate\" \"--input\" \"~a\" \"--output\" \"~a\""
            " \"--module\" \"petstore\" \"--controllers\" \"artanis\""
            " \"--http\" \"curl\"))'"
            example-spec example-output))))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (docs)
  "Generate technical API documentation."
  (delete-file-recursively "doc")
  (time-machine-manifest-shell (list (~ "documenta api ./src/~a" project-name)
                                    (~ "texi2any -v --html --css-ref=~a -o index_html"
                                       api-docs-css-url)
                                    "./doc/api/index.texi"))
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Generate the API documentation and publish it to the web directory."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*" (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (delete-file-recursively "doc"))

(define (repl)
  "Development REPL with src/ and test/ on the load path."
  (time-machine-manifest-shell '("guile -L ./src -L ./test")))

(define (hot-reload)
  "Re-run the test suite whenever a source, fixture or spec file changes."
  (time-machine-manifest-shell
   (list "watchexec 'maak test'"
         "--clear"
         (~ "--exts ~a"
            (string-join (map symbol->string hot-reload-file-types) ","))
         "--on-busy-update=restart"
         "--shell bash"
         "--watch .")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (clean)
  "Remove generated artefacts."
  (delete-file-recursively "dist")
  (delete-file-recursively "doc")
  (delete-file-recursively "reports"))

(define (default)
  "Default task to be run, list available tasks."
  (list-tasks))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for guile-openapi-generator."
  (ct:container-build "." #:tag "guile-openapi-generator:latest" #:verbose? #t))

(define (container-run)
  "Run the container image for guile-openapi-generator."
  (ct:container-run "guile-openapi-generator" #:interactive? #t #:tty? #t #:verbose? #t))

(define (package-docker)
  "Build the container image for guile-openapi-generator."
  (container-build))
