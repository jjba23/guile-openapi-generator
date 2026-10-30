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
  ;; The container, archive and git helpers are imported under prefixes because
  ;; the tasks below are named the way maak exposes them on the command line,
  ;; and because `package-tar' and friends read better than `tar-create'.
  #:use-module ((maak container) #:prefix ct:)
  #:use-module ((maak git) #:prefix git:)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-13)
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
            package-docker
            package-tar
            package-zip
            package-smoke
            package-deb
            package-rpm
            package-arch))

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

;; The code the generator runs under `guile -c' is one shell word, so it keeps
;; its single quotes inside the token while its own arguments stay separate.
(define (guile-run . code)
  "Runs CODE, a list of forms as a single string, with src/ and test/ loaded."
  (time-machine-manifest-shell
   (list "guile" "-L" "./src" "-L" "./test"
         "-c" (~ "'~a'" (apply string-append code)))))

;; ====== Maak file tasks ======

(define (test)
  "Run the project's Veritas test suites."
  (guile-run "((@ (veritas runner) run-tests))"))

(define (check)
  "Compile every source and test module to catch syntax and reference errors.

The trees the test suite generates under test/generated and the fixture programs
in test/fixtures/programs are skipped: they are output or fragments, and they
only make sense with a generated tree on the load path.  The route fixtures in
test/fixtures/routes are skipped for a different reason: they are source that the
route readers are meant to read, not code that anything should compile.

The load path is exported in the shell line rather than put in front of `find':
an assignment in front of a pipeline applies to that one command, so `guild
compile' would otherwise run without src/ and test/ on its path."
  ;; A shell is what this needs: an `export' that outlives the first command,
  ;; and a `find -print0 | xargs -0' pair.  Every part below is one shell word,
  ;; quoted where the shell must keep it one.
  (time-machine-manifest-shell
   (list "export" "GUILE_LOAD_PATH=./src:./test:$GUILE_LOAD_PATH;"
         "find" "src" "test" "-name" "'*.scm'"
         "-not" "-path" "'test/generated/*'"
         "-not" "-path" "'test/fixtures/programs/*'"
         "-not" "-path" "'test/fixtures/routes/*'"
         "-print0"
         "|"
         "xargs" "-0" "-n" "1" "guild" "compile"
         "-o" (shell-quote
               (~ "~a/guile-openapi-generator-check.go" (tmp-dir))))
   #:verbose? #t))

(define (generate)
  "Generate a client, controllers and models from the example specification."
  (delete-file-recursively example-output)
  (mkdir-p example-output)
  (time-machine-manifest-shell
   (list "guile" "-L" "./src"
         "-c" (~ "'((@ (openapi-generator cli) main!) (list \"generate\" \"--input\" \"~a\" \"--output\" \"~a\" \"--module\" \"petstore\" \"--controllers\" \"artanis\" \"--http\" \"curl\"))'"
                 example-spec example-output))))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (docs)
  "Generate technical API documentation."
  (delete-file-recursively "doc")
  ;; Documenta writes ./doc/api/index.texi, and texi2any turns that into the
  ;; HTML tree; two calls, so a failure names the step that failed.
  (time-machine-manifest-shell (list "documenta" "api"
                                     (~ "./src/~a" project-name)))
  (time-machine-manifest-shell
   (list "texi2any" "-v" "--html"
         (~ "--css-ref=~a" api-docs-css-url)
         "-o" "index_html" "./doc/api/index.texi"))
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Generate the API documentation and publish it to the web directory."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*" (~ "~a/api-docs/~a" deploy-api-docs-dir project-name)
      #:expand? #t)
  (delete-file-recursively "doc"))

(define (repl)
  "Development REPL with src/ and test/ on the load path."
  (time-machine-manifest-shell (list "guile" "-L" "./src" "-L" "./test")))

(define (hot-reload)
  "Re-run the test suite whenever a source, fixture or spec file changes."
  ;; `maak test' is one word to watchexec, so it stays quoted inside its token.
  (time-machine-manifest-shell
   (list "watchexec" "'maak test'"
         "--clear"
         "--exts" (string-join (map symbol->string hot-reload-file-types) ",")
         "--on-busy-update=restart"
         "--shell" "bash"
         "--watch" ".")))

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

;; ====== Packaging tasks ======
;;
;; The recipes in packaging/ and the Dockerfile all install the program the same
;; way: by running scripts/install-tree.  These tasks build from that one
;; description, so a release is `maak package-smoke' first (everything the
;; recipes do, without a distribution tool) and then whichever recipe the host
;; or a container can actually run.  Each recipe also runs the unit suite on its
;; own, so a package that builds but does not pass the suite fails there.

(define (project-version)
  "Return the version to put on an archive or an image tag.

The version is read from the sources and not from the last tag: it is the
string `guile-openapi-generator --version' prints, and the recipes pin it too,
so an archive, an image tag and the command agree even while a release is being
prepared and the newest tag is still the previous one."
  (call-with-input-file "src/openapi-generator/generator.scm"
    (lambda (port)
      (let loop ()
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            (error "no generator-version in src/openapi-generator/generator.scm"))
           ((string-contains line "(define generator-version")
            (let* ((declaration (read-line port))
                   (start (string-index declaration #\"))
                   (end (string-rindex declaration #\")))
              (substring declaration (+ start 1) end)))
           (else (loop))))))))

(define (package-smoke)
  "Stage the installation into dist/root and run it from there.

This is the check the recipes depend on.  It runs the same installer they run,
so the launcher, the site directory, the byte-compiled copies and both commands
are proved to work before a package exists to get them wrong.  The tree is
staged rather than installed, so what it proves is the layout and the launcher's
prefix, not this machine's module cache."
  (delete-file-recursively "dist/root")
  (time-machine-manifest-shell
   (list "sh" "-c"
         (shell-quote
          (string-append
           "set -eu\n"
           "root=$PWD/dist/root\n"
           "program=$root/usr/bin/guile-openapi-generator\n"
           "sh scripts/install-tree --destdir \"$root\"\n"
           "\"$program\" --version | grep -q '^guile-openapi-generator '\n"
           "\"$program\" --help | grep -q 'Usage:'\n"
           "\"$program\" spec --help | grep -q 'Usage:'\n"
           "work=$(mktemp -d)\n"
           "trap 'rm -rf \"$work\"' EXIT\n"
           "\"$program\" generate -i examples/petstore.yaml \\\n"
           "    -o \"$work/gen\" -m smoke\n"
           "test -f \"$work/gen/smoke/models.scm\"\n"
           "test -f \"$work/gen/smoke/controllers.scm\"\n"
           "printf '%s\\n' '(get \"/ping\" ping-handler)' >\"$work/app.scm\"\n"
           "\"$program\" spec -i \"$work/app.scm\" -o \"$work/openapi.yaml\"\n"
           "grep -q '/ping' \"$work/openapi.yaml\"\n"
           "echo 'the staged installation works in both directions'")))
   #:verbose? #t))

;; Both archives pack the last commit, so what ships is what the release commit
;; contains: the tracked files, without .git and without anything untracked or
;; ignored.  The prefix is the directory the RPM spec's `%autosetup' expects to
;; unpack into.

(define (package-tar)
  "Write the last commit as dist/guile-openapi-generator-VERSION.tar.gz."
  (let ((name (~ "~a-~a" project-name (project-version))))
    (mkdir-p "dist")
    (git:git-archive-tar (~ "dist/~a.tar.gz" name) #:prefix (~ "~a/" name))))

(define (package-zip)
  "Write the last commit as dist/guile-openapi-generator-VERSION.zip."
  (let ((name (~ "~a-~a" project-name (project-version))))
    (mkdir-p "dist")
    (git:git-archive-zip (~ "dist/~a.zip" name) #:prefix (~ "~a/" name))))

(define (package-deb)
  "Build the Debian package, unsigned, with debuild.

The recipe expects its debian/ directory at the top of the source tree, so the
packaging directory is symlinked for the length of the build and removed again;
packaging/debian is never moved or edited.  Needs debhelper, devscripts and the
build dependencies from the control file, which is what a build chroot has."
  (dynamic-wind
   (lambda () (ln-s "packaging/debian" "debian" #:force? #f #:verbose? #t))
   (lambda ()
     (run (list "debuild" "-b" "-uc" "-us") #:verbose? #t))
   (lambda () (rm-rf "debian" #:verbose? #t))))

(define (package-rpm)
  "Build the openSUSE/Fedora RPM with rpmbuild, from the spec in packaging/.

The tarball comes from the same `git archive' the source packages use, so the
spec's Source0 is the release commit and not a working tree with leftovers."
  (let* ((version* (project-version))
         (top (~ "~a/rpmbuild" (home-dir)))
         (sources (~ "~a/SOURCES" top))
         (specs (~ "~a/SPECS" top)))
    ;; mkdir-p takes one directory, so the two trees are created one at a time.
    (mkdir-p sources)
    (mkdir-p specs)
    (git:git-archive-tar (~ "~a/~a-~a.tar.gz" sources project-name version*)
                          #:prefix (~ "~a-~a/" project-name version*))
    (cp "packaging/opensuse/guile-openapi-generator.spec" specs #:verbose? #t)
    (run (list "rpmbuild" "-ba"
               (~ "~a/guile-openapi-generator.spec" specs))
         #:verbose? #t)))

(define (package-arch)
  "Build the Arch package with makepkg, from the recipe in packaging/arch.

makepkg needs an Arch host, or `podman run --rm -v $PWD:/src archlinux:base-devel'
with the same command; the checksum in the recipe has to be filled in with
`updpkgsums' once the tag it builds exists."
  (run (list "makepkg" "--cleanbuild" "--nodeps" "--force"
             "--dir" "packaging/arch")
       #:verbose? #t))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for guile-openapi-generator, tagged latest."
  (ct:container-build "." #:tag (~ "~a:latest" project-name) #:verbose? #t))

(define (container-run)
  "Run the container image for guile-openapi-generator."
  (ct:container-run project-name #:interactive? #t #:tty? #t #:verbose? #t))

(define (package-docker)
  "Build the container image, tagged with the project version and latest.

The image carries the version it was built from as a label and asserts it
against `--version' while building, so a tag that lies about the code it holds
fails the build instead of shipping."
  (let ((version* (project-version)))
    (ct:container-build "."
                        #:tag (~ "~a:~a" project-name version*)
                        #:build-args (list (~ "VERSION=~a" version*))
                        #:verbose? #t)
    (ct:container (list "tag"
                        (~ "~a:~a" project-name version*)
                        (~ "~a:latest" project-name))
                  #:verbose? #t)))
