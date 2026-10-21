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
;; The command line: a thin shell around @code{(openapi-generator generator)}.
;;
;; @example
;; guile -L src -- \
;;   -c '((@ (openapi-generator cli) main)
;;        (list "generate" "--input" "petstore.yaml" "--output" "gen"
;;              "--controllers" "artanis" "--http" "curl"))'
;; @end example
;;
;; Every option has a long and a short spelling, the defaults are the ones that
;; surprise least (plain Guile controllers, plain Guile client, models included),
;; and an unknown value is an error rather than a silent fallback: a typo in
;; @code{--controllers artanis} should not leave you with a plain Guile server.

(define-module (openapi-generator cli)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (openapi-generator generator)
  #:use-module (openapi-generator config)
  #:export (main
            options
            usage
            parse-backend
            parse-controllers))

(define option-descriptions
  '((help . "Show this help and exit.")
    (version . "Show the version and exit.")
    (input . "The OpenAPI 3.x specification to read (.yaml or .json).")
    (output . "The directory to write the generated code to.")
    (module . "The root module name (default: the API title).")
    (controllers . "vanilla, artanis or none (default: vanilla).")
    (http . "guile, curl or none (default: guile).")
    (no-models . "Generate no record module.")
    (base-url . "Override the first server URL of the specification.")
    (self-contained . "Copy the runtime modules next to the generated code.")))

(define options
  '((help (single-char #\h))
    (version (single-char #\v))
    (input (single-char #\i) (value #t))
    (output (single-char #\o) (value #t))
    (module (single-char #\m) (value #t))
    (controllers (single-char #\c) (value #t))
    (http (single-char #\H) (value #t))
    (no-models)
    (base-url (value #t))
    (self-contained)))

(define (usage port)
  "Write the usage of the command to @var{port}."
  (format port "guile-openapi-generator ~a~%~%" generator-version)
  (format port "Read an OpenAPI 3.x specification and generate Guile code.~%~%")
  (format port "Usage: openapi-generator generate --input SPEC --output DIR~%")
  (format port "                          [options]~%~%")
  (for-each (lambda (option)
              (let* ((spelling (option-spelling option))
                     (padding (make-string (max 1
                                                (- 30 (string-length spelling)))
                                           #\space)))
                (format port "~a~a~a~%"
                        spelling
                        padding
                        (or (assoc-ref option-descriptions (car option)) ""))))
            options)
  (format port "~%Examples:~%")
  (format port
          "  openapi-generator generate -i petstore.yaml -o gen~%")
  (format port
          "  openapi-generator generate -i petstore.yaml -o gen -c artanis -H curl~%"))

(define (option-field option name)
  "Return the value of the @var{name} property of @var{option}, or @code{#f}.

An option specification is a list of two-element lists, so the property is the
cadr of its entry, not the cdr: @code{(single-char #\\h)} holds the character
itself, while @code{assoc-ref} would hand back the one-element list around it and
a short spelling would print as @code{(-(h))}."
  (let ((entry (assoc name (cdr option))))
    (and entry (cadr entry))))

(define (option-spelling option)
  "Return how @var{option} is spelled in the usage text, for example
@code{--input (-i) VALUE}."
  (let ((short (option-field option 'single-char))
        (value (option-field option 'value)))
    (string-append "  --" (symbol->string (car option))
                   (if short (format #f " (-~a)" short) "")
                   (if value " VALUE" ""))))

(define (parse-controllers value)
  "Return the controller backend named by @var{value}."
  (let ((backend (string->symbol (string-downcase value))))
    (if (memq backend (cons 'none controller-backends))
        backend
        (error (format #f
                       "Unknown controller backend ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string
                                         (cons 'none controller-backends))
                                    ", "))))))

(define (parse-backend value)
  "Return the HTTP backend named by @var{value}."
  (let ((backend (string->symbol (string-downcase value))))
    (if (memq backend (cons 'none http-backends))
        backend
        (error (format #f
                       "Unknown HTTP backend ~a; expected one of ~a"
                       value
                       (string-join (map symbol->string
                                         (cons 'none http-backends))
                                    ", "))))))

(define* (main #:optional (arguments (cdr (command-line))))
  "Run the generator from the command line @var{arguments} and return an exit code.

The arguments start after the program name, and the first of them may be the
word @code{generate}, which is accepted so that the invocation reads like the
other task runners in this project."
  (let ((arguments (if (and (pair? arguments)
                            (string=? (car arguments) "generate"))
                       (cdr arguments)
                       arguments)))
    (catch #t
      (lambda ()
        (let ((parsed (getopt-long (cons "openapi-generator" arguments)
                                   options
                                   #:stop-at-first-non-option #t)))
          (cond
            ((option-ref parsed 'help #f)
             (usage (current-output-port))
             0)
            ((option-ref parsed 'version #f)
             (format #t "guile-openapi-generator ~a~%" generator-version)
             0)
            (else
             (let ((input (option-ref parsed 'input #f))
                   (output (option-ref parsed 'output "generated")))
               (unless input
                 (usage (current-error-port))
                 (error "--input is required"))
               (let ((written
                      (generate #:input input
                                #:output output
                                #:module (option-ref parsed 'module #f)
                                #:controllers
                                (parse-controllers
                                 (option-ref parsed 'controllers "vanilla"))
                                #:http (parse-backend
                                        (option-ref parsed 'http "guile"))
                                #:models? (not (option-ref parsed 'no-models #f))
                                #:base-url (option-ref parsed 'base-url #f)
                                #:self-contained?
                                (option-ref parsed 'self-contained #f))))
                 (for-each (lambda (entry)
                             (format #t "~a: ~a~%" (car entry) (cdr entry)))
                           written)
                 0))))))
      (lambda (key . arguments)
        (let ((message (false-if-exception (cadr arguments))))
          (format (current-error-port)
                  "openapi-generator: ~a~%"
                  (or message key)))
        1))))
