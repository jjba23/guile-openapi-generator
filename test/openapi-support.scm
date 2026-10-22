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
;; Helpers shared by the test suites.  This file is not a suite: the Veritas
;; runner only walks test/veritas, so it is never picked up as one.
;;
;; The interesting helper is @code{with-generated}: it runs the generator into a
;; throwaway directory, puts that directory on the load path, and hands the test
;; the interface of the generated module.  That is how the tests check that the
;; code the generator writes is not merely well spelled but actually loads, and
;; that the records it defines behave.

(define-module (openapi-support)
  ;; The helpers load generated files and move the load path, so this module is
  ;; deliberately not declarative.
  #:declarative? #f
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator)
  #:use-module (openapi-generator config)
  #:use-module (openapi-generator models)
  #:use-module (openapi-generator controllers)
  #:use-module (openapi-generator client)
  #:use-module (openapi-generator schema)
  #:export (fixture
            fixture-spec
            petstore-spec
            polymorphism-spec
            mini-spec
            test-config
            with-temporary-directory
            with-generated
            with-polymorphic
            run-generated
            run-program
            contains?
            occurrences
            file-contents
            temporary-root))

;;; Commentary:
;;; Fixtures

(define (fixture name)
  "Return the path of the fixture called @var{name}."
  (format #f "test/fixtures/~a" name))

(define (fixture-spec name)
  "Parse the fixture called @var{name} into a @code{<spec>}."
  (parse-spec (read-spec-file (fixture name)) #:source (fixture name)))

(define (petstore-spec)
  (fixture-spec "petstore.yaml"))

(define (polymorphism-spec)
  (fixture-spec "polymorphism.yaml"))

(define (mini-spec)
  (fixture-spec "mini.yaml"))

;;; Commentary:
;;; Configuration

(define* (test-config #:key
                      (output "generated")
                      (root "petstore")
                      (controllers 'vanilla)
                      (http 'guile)
                      (models? #t)
                      (readme? #t)
                      (base-url #f)
                      (self-contained? #f)
                      (model-suffix "")
                      (header? #t)
                      (header-text #f)
                      (dependencies 'none)
                      (runtime-module '(openapi-generator runtime))
                      (jso-module '(openapi-generator jso)))
  "Return a configuration for the tests."
  (make-config output
               root
               "Petstore"
               controllers
               http
               models?
               runtime-module
               jso-module
               '(openapi-generator json)
               "petstore.yaml"
               "0.0.0-test"
               self-contained?
               base-url
               #t
               readme?
               model-suffix
               header?
               header-text
               dependencies))

;;; Commentary:
;;; Temporary directories and generated trees

(define counter 0)

(define (temporary-root name)
  "Return a fresh directory path for a test called @var{name}.

The stamp has to be unique across runs, not only within one: Guile caches the
compiled form of a file under a path derived from the file's own path, so reusing
a path would hand a later run the compiled form of an earlier one."
  (set! counter (+ counter 1))
  (format #f "/tmp/guile-openapi-generator/~a-~a-~a-~a"
          name
          (getpid)
          counter
          (car (gettimeofday))))

(define* (with-temporary-directory name thunk)
  "Create a directory called @var{name}, call @var{thunk} with it, then remove it."
  (let ((directory (temporary-root name)))
    (when (file-exists? directory)
      (delete-file-recursively directory))
    (ensure-directory! directory)
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk directory))
      (lambda ()
        (when (and (file-exists? directory)
                   (not (getenv "KEEP_GENERATED")))
          (delete-file-recursively directory))))))

(define (ensure-directory! path)
  "Create @var{path} and its missing parents, tolerating a concurrent creator."
  (unless (or (string-null? path) (file-exists? path))
    (ensure-directory! (dirname path))
    (catch #t
      (lambda () (mkdir path))
      (lambda (key . arguments)
        (unless (file-exists? path)
          (apply throw key arguments))))))

(define (delete-file-recursively path)
  "Remove @var{path}, descending into directories."
  (cond
    ((file-is-directory? path)
     (for-each (lambda (entry)
                 (unless (member entry '("." ".."))
                   (delete-file-recursively (string-append path "/" entry))))
               (directory-listing path))
     (rmdir path))
    ((file-exists? path)
     (delete-file path))
    (else #t)))

(define (directory-listing path)
  "Return the names of the entries of @var{path}."
  (let ((port (opendir path)))
    (let loop ((names '()))
      (let ((name (readdir port)))
        (if (eof-object? name)
            (begin
              (closedir port)
              (reverse names))
            (loop (cons name names)))))))

(define* (run-program directory module-names program #:key (bindings '()))
  "Run the program called @var{program} against the tree in @var{directory}.

@var{module-names} is the list of modules the program works in: the fixture
programs are plain top-level code, and the module form written here is what gives
them the generated modules, so a program never has to know which root it is
testing.

@var{bindings} is a list of @code{(name . source)} pairs that become definitions
in front of the program.  A test hands a value over that way rather than through
the environment: Veritas runs each spec file in its own thread, and mutating the
environment of a multi-threaded process has no defined meaning, so an environment
variable would not reach the child reliably."
  (let ((body (file-contents (fixture (string-append "programs/" program ".scm")))))
    (run-generated directory
                   (string-append
                    (string-append
                     "(define-module (check)\n"
                     (string-join
                      (map (lambda (module-name)
                             (format #f "  #:use-module (~{~a~^ ~})\n"
                                     module-name))
                           (append module-names
                                   (list '(openapi-generator jso)
                                         '(openapi-generator runtime)
                                         '(ice-9 format)
                                         '(ice-9 receive))))
                      "")
                     ") ; end of module\n")
                    (string-join
                     (map (lambda (binding)
                            (format #f "(define ~a ~a)\n"
                                    (car binding)
                                    (cdr binding)))
                          bindings)
                     "")
                    body))))

(define (shell-quote text)
  "Return @var{text} as a single shell word, quotes and all."
  (string-append "'"
                 (string-replace-substring text "'" "'\\''")
                 "'"))

(define (child-load-path directory)
  "Return the load path a generated tree's program needs, as one string.

The tests may have been started with @code{-L} flags rather than a
@code{GUILE_LOAD_PATH} variable — @code{maak test} does exactly that — and those
flags are not inherited by a child process.  The parent's load path is therefore
copied into the environment of the child explicitly, with the generated tree in
front."
  (string-join (cons directory (remove string-null? %load-path)) ":"))

(define (run-generated directory program)
  "Run @var{program}, a Scheme source string, against the tree in @var{directory}.

The program runs in a fresh Guile process with @var{directory} on its load path,
and its standard output is returned as a string.  A fresh process is what makes
these tests independent: modules, load paths and record types all start empty,
so nothing one test generates can be seen by another."
  (let* ((work (string-append directory "/check"))
         (path (string-append work "/check.scm")))
    (ensure-directory! work)
    (call-with-output-file path
      (lambda (port)
        (display program port)))
    (let* ((guile (or (getenv "GUILE") "guile"))
           (command (format #f "GUILE_LOAD_PATH=~a GUILE_AUTO_COMPILE=1 ~a -L ~a ~a 2>&1"
                            (shell-quote (child-load-path directory))
                            (shell-quote guile)
                            (shell-quote directory)
                            (shell-quote path)))
           (port (open-input-pipe command))
           (output (get-string-all port)))
      (let ((status (close-pipe port)))
        (when (not (zero? status))
          (format #t "~%[program failed: ~a]~%" command)))
      (string-join
       (filter (lambda (line)
                 (not (string-prefix? ";;;" line)))
               (string-split output #\newline))
       "\n"))))

(define generated-root
  "test/generated")

(define* (with-generated root thunk
                         #:key
                         (input (fixture "petstore.yaml"))
                         (controllers 'vanilla)
                         (http 'guile)
                         (self-contained? #f))
  "Generate a tree called @var{root} under test/generated and call THUNK with it.

The directory is left in place: it is git-ignored, a failing test stays
inspectable, and a path that only changes when the tree changes keeps the child
processes of @code{run-program} away from the compiled files Guile caches by
path."
  (let ((directory (format #f "~a/~a" generated-root root)))
    (when (file-exists? directory)
      (delete-file-recursively directory))
    (ensure-directory! directory)
    (generate #:input input
              #:output directory
              #:module (symbol->string root)
              #:controllers controllers
              #:http http
              #:self-contained? self-contained?)
    (thunk directory)))


;;; Commentary:
;;; Small assertions helpers

(define (contains? text needle)
  "Return #t when @var{text} contains @var{needle}."
  (and (string? text) (string-contains text needle) #t))

(define (occurrences text needle)
  "Count how often @var{needle} appears in @var{text}."
  (let loop ((start 0) (count 0))
    (let ((index (string-contains text needle start)))
      (if index
          (loop (+ index (string-length needle)) (+ count 1))
          count))))

(define (file-contents path)
  "Return the contents of @var{path} as a string."
  (call-with-input-file path
    (lambda (port)
      (let loop ((characters '()))
        (let ((character (read-char port)))
          (if (eof-object? character)
              (list->string (reverse characters))
              (loop (cons character characters))))))))
