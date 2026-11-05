;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
            package-arch agents))

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
compile' would otherwise run without src/ and test/ on its path.  The whole line
runs under `sh -c' because `guix shell' takes the first word after `--' as the
command: an `export' there is looked up as a program and the rest of the line is
never run in the same shell."
  ;; A shell is what this needs: an `export' that outlives the first command,
  ;; and a `find -print0 | xargs -0' pair.  The script is one shell word, so its
  ;; double quotes reach the shell that reads it.
  (time-machine-manifest-shell
   (list "sh" "-c"
         (shell-quote
          (string-append
           "set -eu\n"
           "export GUILE_LOAD_PATH=./src:./test:$GUILE_LOAD_PATH\n"
           "find src test -name \"*.scm\""
           " -not -path \"test/generated/*\""
           " -not -path \"test/fixtures/programs/*\""
           " -not -path \"test/fixtures/routes/*\""
           " -print0 | xargs -0 -n 1 guild compile -o "
           (shell-quote (~ "~a/guile-openapi-generator-check.go" (tmp-dir)))
           "\n")))
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

;; ====== Agent skills ======

;; AGENTS.md is generated, never edited by hand.  The section has two halves:
;; the data below, which is this project's, and the task itself, which is not.
;; Another repository copies the whole section and replaces the data - the
;; header, the pinned skills (repository, commit, path, licence) and the
;; practices - and `maak agents' writes its own AGENTS.md.  Regeneration is
;; deterministic: the same pins and the same text produce the same file, byte
;; for byte, and it needs nothing but curl.  The skill URLs assume GitHub, so a
;; skill hosted elsewhere needs a URL of its own.

(define ponytail-commit
  "96608e078b2d5ea607c5c3b8b5a54d1c1fba6846")

(define tdd-commit
  "22f0b463e7a5e579904dd6211a520321035526f7")

;; Name, repository, commit, the path the repository keeps the skill at and the
;; licence.  A skill arrives whole: trimming it means scanning for headings, and
;; such a scan cannot tell a heading from a `##' line inside a code fence.
(define agent-skills
  (list (list "ponytail"
              "DietrichGebert/ponytail"
              ponytail-commit
              "skills/ponytail/SKILL.md"
              "MIT")
        (list "test-driven-development"
              "obra/superpowers"
              tdd-commit
              "skills/test-driven-development/SKILL.md"
              "MIT")))

(define agent-skills-dir
  "reports/agent-skills")

(define agents-header
  "# AGENTS.md

Generated by `maak agents` from the pinned skills and the practices in
`maak.scm`; edit that task, never this file.  The pins and the text are the whole
input, so regenerating gives the same document.  The repository's own rules win
over general advice, and the narrowest rule here wins over the broadest.
")

;; Practices this project keeps.  They are short on purpose: a rule a reader
;; checks in a second is a rule that gets followed.  The pinned skills are the
;; long form of the same habits.
(define agent-practices
  (list
   "## Documentation

- Every user-visible change reaches the documentation: the page that owns the
  feature, the manual page for a flag or an environment variable, and the
  release notes under the unreleased heading.
- Keep the table of contents in step with the headings.  A section with no entry
  is a section nobody finds.
- The guide is Org: one `*` heading per page, `**` for sections, `#+TITLE` and
  `#+EMAIL` at the top, a `CUSTOM_ID` on anything linked from elsewhere, and
  `file:` links between pages.
- Explain why and when, never what: the code already says what.  A docstring's
  first line works as a summary on its own.
- Spell an identifier the way the code spells it - the exact name, not a
  description of it.
- An example must run as written, copied from a real invocation.
- No promises about the future and no changelog in the guide; history lives in
  the release notes.
"
   "## Security

- No secrets in the tree.  Tokens, keys and passwords arrive through the
  environment, and no recipe, workflow or example embeds one.
- Validate at the edge: anything from a file, a report, a path or a subprocess
  is untrusted until it has been checked.
- Never build a shell string out of data.  Tasks pass argument lists, and Guile
  calls `(system \"program\" \"arg\")` or `open-pipe*`, never a command
  assembled with `string-append`.
- Do not `eval` or `read` input you did not write; parse with `(json)` or a
  reader built for the format.
- Escape what leaves the process: a name that came from input is escaped when
  it is written to XML, HTML or a data file.
- Fail closed.  A file that cannot be read is an error, not an empty result.
- Input never chooses the path of a written file; writes stay inside the
  directory reserved for them.
- A dependency is a security decision: it must be needed, MIT compatible and
  pinned.
- Report a vulnerability privately, not in a public issue.
"
   "## Performance

- Measure first.  A before and after belongs in the benchmark, and a speedup
  claimed without a number is not a speedup.
- Prefer not doing the work: cache it, skip it, batch it, or delete the call,
  before making the call faster.
- Then the algorithm, then the data structure (hash table, vector), then the
  micro-optimisation.  In that order, for that reason.
- Guile specifics: keep module resolution and long `assoc` chains out of loops,
  use `#:declarative?` modules and compiled `.go` files at startup, and reach
  for `statprof` instead of guessing where the time goes.
- A shortcut that buys speed costs clarity; it gets a measurement and a
  `shortcut:` comment saying what it costs and when to revisit it.
- Coverage instrumentation is slow by design: iterate without it, and measure
  before finishing.
"
   "## Architecture decisions

- A decision that is expensive to reverse gets a short record: the context, the
  decision, the consequences, and the alternatives that lost.  A small decision
  belongs in the commit body instead.
- Two paragraphs beat a design document; record the choice while it is still a
  choice, not after it has become a fact.
- An accepted record is not rewritten.  Supersede it with a new one and link the
  two directions.
- The record lives next to its subject: the documentation for anything a user
  would notice, the release notes for the release-level result.
- Changes to the public API, a file format, the pipeline or the licence get this
  treatment at a minimum in the commit body.
"
   "## Semantic versioning

- The project follows SemVer 2.0.0 for its public API: the exported functions,
  the CLI flags and environment variables, and the shape of its file formats.
- A breaking change bumps MAJOR, a new feature MINOR, a fix PATCH.  A new export
  is MINOR; removing or changing one is MAJOR.
- One version string, named everywhere it appears: the source, the build
  recipes, the manual page and the packaging.
- A release is a dated release-notes section, an annotated `vX.Y.Z` tag on the
  release commit, and a packaging checksum updated afterwards from the published
  archive.
"
   "## Conventional commits

- `type(scope): summary`, imperative, lower case, no full stop, about seventy
  characters.
- Types in use: `feat`, `fix`, `perf`, `refactor`, `test`, `docs`, `build`, `ci`,
  `chore`.  A `!` after the type or a `BREAKING CHANGE:` footer marks a MAJOR
  change, and says what breaks.
- The subject says what changed.  The body says why, and what was rejected.
- One logical change per commit: a refactor and a fix travel apart, so either
  can be reverted alone.
- Run the full validation task before committing, and never commit a red suite
  without saying so in the message.
- Reference the issue in the footer - `Closes #12` - not in the subject.
"
   "## Functional programming and immutability

- Pure functions in the core, effects at the edges.  Whatever touches the file
  system or the clock sits at the edge; the core does not.
- Prefer `let` and new values to `set!`.  Build results with `map`, `filter`,
  `fold` and named `let`, not with index loops that push into a list.
- A record is a value: construct a new one instead of mutating a shared one, and
  pass configuration as arguments or parameters rather than as globals.
- Handle an exception at the boundary: catch it there, turn it into a result
  record, and let the caller decide what it means.
- A function that both computes and prints is two functions.
- Temporary effects - a changed directory, a bound environment variable - go
  through `dynamic-wind`.
- Prefer the expression style: `if` and `cond` that return a value, not a branch
  that assigns one.
"
   "## Do not reinvent the wheel

- Guile's standard library first: `(ice-9 ...)` for the runtime, `(srfi srfi-1)`
  for lists, `(srfi srfi-9)` for records, `(srfi srfi-13)` for strings, `(json)`
  for JSON.  The project already depends on all of them.
- Then this codebase: read the existing modules before writing a helper, and use
  the one that is nearly right instead of adding a second one.
- A new dependency needs a reason the standard library cannot meet, a licence
  the project can carry, and a paragraph in the commit body.
- Prefer deleting a special case to writing code that handles it.
- The best diff removes lines and keeps the behaviour; the second best is a
  small diff that is obvious a year later.
"))

(define (slurp path)
  "Return the whole of @var{path} as a string."
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '())
                 (line (read-line port)))
        (if (eof-object? line)
            (string-concatenate-reverse lines)
            (loop (cons (string-append line "\n") lines)
                  (read-line port)))))))

(define (fetch-skill name repository commit path)
  "Download one skill pinned at @var{commit}, into the reports tree.

  Returns the skill's text."
  (let ((target (~ "~a/~a.md" agent-skills-dir name))
        (url (~ "https://raw.githubusercontent.com/~a/~a/~a"
                repository commit path)))
    (run (list "curl" "--fail" "--silent" "--show-error" "--location"
               "--output" target url)
         #:verbose? #t)
    (slurp target)))

(define (agents)
  "Regenerate AGENTS.md from the pinned skills and the practices."
  (run (list "mkdir" "-p" agent-skills-dir))
  ;; Every download happens before the file is opened, so a fetch that fails
  ;; leaves the previous AGENTS.md in place rather than a half-written one.
  (let ((skills (map (lambda (skill)
                       (append skill
                               (list (fetch-skill (list-ref skill 0)
                                                  (list-ref skill 1)
                                                  (list-ref skill 2)
                                                  (list-ref skill 3)))))
                     agent-skills)))
    (call-with-output-file "AGENTS.md"
      (lambda (port)
        (display agents-header port)
        (for-each (lambda (skill)
                    (format port
                            "~%## ~a~%~%`~a` at `~a`, `~a`, ~a licensed.~%~%"
                            (list-ref skill 0)
                            (list-ref skill 1)
                            (list-ref skill 2)
                            (list-ref skill 3)
                            (list-ref skill 4))
                    (display (list-ref skill 5) port))
                  skills)
        (for-each (lambda (practice)
                    (newline port)
                    (display practice port))
                  agent-practices)))))
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
