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
;; The code writer.  Generated code is built as data — a tree of lists, symbols,
;; strings, numbers and keywords — and rendered here, so a generator reads like
;; the code it writes:
;;
;; @example
;; (forms->string
;;  (list `(define (pet-name value)
;;           (jso-ref (pet->jso value) "name"))
;;        `(define (unbox-pet value)
;;           (let ((payload (jso-ref value "data")))
;;             (jso->pet payload)))))
;; @end example
;;
;; Rendering is where indentation, line breaks, string escapes and docstrings are
;; decided, once, so every generated file looks as if the same person wrote it.
;; A form is printed on one line when it fits and broken at its head when it does
;; not: a definition keeps its signature on the first line and indents its body,
;; a @code{let} puts one binding per line, a @code{cond} one clause per line, and
;; a procedure call lines its arguments up under the first one.
;;
;; Nothing here knows what OpenAPI is.  It knows Scheme layout.

(define-module (openapi-generator code)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (code->string
            forms->string
            comment
            comment?
            blank
            blank?
            quoted
            symbol-append
            escape-string
            scheme-string
            line-width
            indent-width))

;;; Commentary:
;;; Lines

;; How far a body is indented from the form that opens it.
(define indent-width 2)

;; The column a rendered line may not pass without being broken.
(define line-width 78)

;; A rendered line knows what it is: a closing parenthesis must land on a line of
;; code, never on a comment, or the parenthesis would be commented out.
(define-record-type <line>
  (make-line kind text)
  line?
  (kind line-kind)
  (text line-text))

(define (code-line text)
  (make-line 'code text))

(define (comment-line text)
  (make-line 'comment text))

(define (blank-line)
  (make-line 'blank ""))

(define (indent-to column text)
  (string-append (make-string column #\space) text))

(define (fits? text column)
  (<= (+ column (string-length text)) line-width))

;;; Commentary:
;;; Markers

;; A comment is a value an emitter can put in a list of forms, so a section of
;; generated code can say what it is.
(define-record-type <comment>
  (comment text)
  comment?
  (text comment-text))

;; A blank line inside a body, which is how sections of a file are separated.
(define-record-type <blank>
  (blank)
  blank?)

;; One clause of a define-module body: the keyword and what it applies to.  The
;; two are separate elements of the form, but one line of output.
(define-record-type <clause>
  (clause keyword value)
  clause?
  (keyword clause-keyword)
  (value clause-value))

(define (quoted datum)
  "Return the form that renders as @var{datum} quoted, for example @code{'()}.

An emitter that knows a list of values at generation time writes
@code{(quoted values)} and gets the source of those values, rather than a string
that happens to look like it."
  (list 'quote datum))

(define (symbol-append . parts)
  "Return the symbol spelled by concatenating @var{parts}.

@example
(symbol-append 'pet '- 'id)
@result{} pet-id
@end example"
  (string->symbol
   (apply string-append
          (map (lambda (part)
                 (cond
                   ((symbol? part) (symbol->string part))
                   ((number? part) (number->string part))
                   (else part)))
               parts))))

;;; Commentary:
;;; Printing one line

(define (escape-string text)
  "Return @var{text} escaped for inclusion in a double quoted Scheme literal."
  (list->string
   (append-map (lambda (character)
                 (case character
                   ((#\") (list #\\ #\"))
                   ((#\\) (list #\\ #\\))
                   (else (list character))))
               (string->list text))))

(define (scheme-string text)
  "Return @var{text} as a Scheme string literal, escapes included."
  (string-append "\""
                 (escape-string (if (string? text)
                                    text
                                    (format #f "~a" text)))
                 "\""))

(define (flat form)
  "Return @var{form} as one line of source, or @code{#f} when it needs several.

A string holding a line break needs several lines, which is why this can fail:
the caller then renders the form in full."
  (cond
    ((keyword? form) (format #f "~a" form))

    ((symbol? form) (symbol->string form))

    ((string? form)
     (if (string-index form #\newline)
         #f
         (scheme-string form)))

    ((boolean? form) (if form "#t" "#f"))

    ((char? form) (format #f "~s" form))

    ((number? form) (number->string form))

    ((null? form) "()")

    ((vector? form)
     (let ((parts (map flat (vector->list form))))
       (and (every (lambda (part) part) parts)
            (string-append "#(" (string-join parts " ") ")"))))

    ((pair? form)
     (let ((head (car form))
           (rest (cdr form)))
       (cond
         ((and (memq head '(quote quasiquote))
               (pair? rest)
               (null? (cdr rest)))
          (let ((inner (flat (car rest))))
            (and inner
                 (string-append (if (eq? head 'quote) "'" "`") inner))))
         ((and (memq head '(unquote unquote-splicing))
               (pair? rest)
               (null? (cdr rest)))
          (let ((inner (flat (car rest))))
            (and inner
                 (string-append (if (eq? head 'unquote) "," ",@") inner))))
         (else
          (let ((parts (flat-parts form)))
            (and parts (string-append "(" (string-join parts " ") ")")))))))

    (else #f)))

(define (flat-parts form)
  "Return the printed parts of @var{form}, or @code{#f} if one needs two lines.

An improper list keeps its dot, so the association @code{(\"type\" .
\"apiKey\")} comes out the way it was written."
  (let loop ((rest form) (parts '()))
    (cond
      ((null? rest) (reverse parts))
      ((pair? rest)
       (let ((part (flat (car rest))))
         (and part (loop (cdr rest) (cons part parts)))))
      (else
       (let ((part (flat rest)))
         (and part (reverse (cons (string-append ". " part) parts))))))))

;;; Commentary:
;;; Printing a form

;; The form heads that a reader expects to see broken over several lines: a
;; definition, a binding form and a conditional are blocks even when they would
;; fit on one line, because the body is what a reader looks for.
(define block-heads
  '(define define* define-values define-record-type
    lambda let let* letrec letrec* parameterize receive
    cond case if when unless begin))

(define (render form column)
  "Return the lines that print @var{form}, which starts at @var{column}."
  (cond
    ((comment? form)
     (map (lambda (line)
            (comment-line (indent-to column
                                     (if (string-null? line)
                                         ";;"
                                         (string-append ";; " line)))))
          (string-split (comment-text form) #\newline)))

    ((blank? form) (list (blank-line)))

    ((clause? form) (render-clause form column))

    ((and (string? form) (string-index form #\newline))
     (render-multiline-string form column))

    ((and (pair? form) (memq (car form) block-heads))
     (render-broken form column))

    (else
     (let ((text (flat form)))
       (cond
         ((and text (fits? text column))
          (list (code-line (indent-to column text))))
         ((not (pair? form))
          (list (code-line (indent-to column (or text "")))))
         (else (render-broken form column)))))))

(define (render-multiline-string text column)
  "Return the lines of a string literal whose content spans several lines.

The first line is indented like the code around it and the later ones start at
the beginning of the line, which is how Guile source is normally written and what
keeps the literal's content free of accidental indentation."
  (let ((lines (string-split (escape-string text) #\newline)))
    (let loop ((rest (cdr lines))
               (rendered
                (list (code-line (indent-to column
                                            (string-append "\"" (car lines)))))))
      (cond
        ((null? rest) (last-code-line-append rendered "\""))
        ((null? (cdr rest))
         (append rendered (list (code-line (string-append (car rest) "\"")))))
        (else
         (loop (cdr rest) (append rendered (list (code-line (car rest))))))))))

(define (last-code-line-append lines text)
  "Append @var{text} to the last line of @var{lines} that holds code.

Comments and blank lines keep their place: a closing parenthesis appended to a
comment would be commented out."
  (let loop ((reversed (reverse lines)) (trailing '()))
    (cond
      ((null? reversed) (list (code-line text)))
      ((eq? (line-kind (car reversed)) 'code)
       (append (reverse (cdr reversed))
               (list (code-line (string-append (line-text (car reversed)) text)))
               trailing))
      (else (loop (cdr reversed) (cons (car reversed) trailing))))))

(define (prepend-at column indent text lines)
  "Put @var{text} at @var{column} in front of @var{lines}.

The first of @var{lines} was rendered at @var{indent}, and loses exactly that
much indentation, because it now follows @var{text} on the same line."
  (cond
    ((null? lines) (list (code-line (indent-to column text))))
    (else
     (cons (code-line (string-append (indent-to column text)
                                     (substring (line-text (car lines)) indent)))
           (cdr lines)))))

(define* (render-block column open forms #:optional (step indent-width))
  "Return the lines of a block that opens with @var{open} at @var{column}.

Each of @var{forms} gets lines of its own, @var{step} columns further in, and the
block is closed on the line that holds the last of them."
  (let loop ((rest forms) (lines (list (code-line (indent-to column open)))))
    (if (null? rest)
        (last-code-line-append lines ")")
        (loop (cdr rest) (append lines (render (car rest) (+ column step)))))))

(define* (render-with-prefix column indent text prefix body
                             #:optional (body-step indent-width))
  "Return the lines of a form that opens with @var{text} and continues to @var{body}.

@var{text} starts at @var{column}; @var{prefix} are the lines of the form that
follow it on the same line, rendered at @var{indent}.  @var{body} is indented
@var{body-step} columns past the form, and the form is closed after its last
form, which is where a Scheme reader expects the parenthesis."
  (let loop ((rest body)
             (lines (prepend-at column indent text prefix)))
    (if (null? rest)
        (last-code-line-append lines ")")
        (loop (cdr rest)
              (append lines (render (car rest) (+ column body-step)))))))

(define (render-head-and-body column first-line body)
  "Return the lines of a form that opens with @var{first-line} at @var{column}.

@var{body} is indented one level and the form is closed after its last form."
  (render-with-prefix column (+ column (string-length first-line)) first-line '() body))

;;; Commentary:
;;; Breaking a form

(define (render-broken form column)
  "Return the lines of @var{form}, which the caller knows does not fit on one."
  (case (car form)
    ((define define* define-values) (render-definition form column))
    ((define-record-type) (render-record-type form column))
    ((lambda) (render-lambda form column))
    ((let let* letrec letrec* parameterize) (render-let form column))
    ((receive) (render-receive form column))
    ((cond) (render-block column "(cond" (cdr form)))
    ((case) (render-case form column))
    ((if) (render-if form column))
    ((when unless) (render-conditional form column))
    ((begin) (render-block column "(begin" (cdr form)))
    ((define-module) (render-module-form form column))
    ((quote) (render-quoted (cadr form) column))
    ((else) (render-head-and-body column "(else" (cdr form)))
    (else (render-call form column))))

(define (render-signature name arguments column)
  "Return the lines of the procedure signature @code{(NAME ARGUMENTS ...)}.

An argument list that does not fit is put one argument per line, lined up under
the name.  The @code{#:key} marker stays beside the name: it belongs to the
signature, not to the values a caller passes."
  (let ((one-line (flat (cons name arguments))))
    (cond
      ((and one-line (fits? one-line column))
       (list (code-line (indent-to column one-line))))
      (else
       (let* ((argument-column (+ column 1 (string-length (flat name)) 1))
              (marker (and (pair? arguments)
                           (keyword? (car arguments))
                           (flat (car arguments))))
              (first-line (string-append "("
                                         (flat name)
                                         (if marker
                                             (string-append " " marker)
                                             "")))
              (rest (if marker (cdr arguments) arguments)))
         (let loop ((rest rest)
                    (lines (list (code-line (indent-to column first-line)))))
           (if (null? rest)
               (last-code-line-append lines ")")
               (loop (cdr rest)
                     (append lines (render (car rest) argument-column))))))))))

(define (render-arguments arguments column)
  "Return the lines of a parenthesised argument list at @var{column}."
  (let ((one-line (flat arguments)))
    (if (and one-line (fits? one-line column))
        (list (code-line (indent-to column one-line)))
        (render-sequence column arguments render))))

(define (render-sequence column elements render-element)
  "Return the lines of a parenthesised list at @var{column}, one element per line.

The first element follows the opening parenthesis, so a list of bindings, or of
data, reads the way it would if it were typed by hand."
  (if (null? elements)
      (list (code-line (indent-to column "()")))
      (let loop ((rest (cdr elements))
                 (lines (prepend-at column
                                    (+ column 1)
                                    "("
                                    (render-element (car elements) (+ column 1)))))
        (if (null? rest)
            (last-code-line-append lines ")")
            (loop (cdr rest)
                  (append lines
                          (render-element (car rest) (+ column 1))))))))

(define (render-definition form column)
  "Return the lines of a @code{define} or @code{define*} form at @var{column}.

The signature stays on the first line; only a signature that does not fit is put
one argument per line."
  (let* ((head-text (string-append "(" (symbol->string (car form)) " "))
         (signature (cadr form))
         (body (cddr form))
         (prefix-column (+ column (string-length head-text)))
         (prefix
          (if (pair? signature)
              (render-signature (car signature) (cdr signature) prefix-column)
              (list (code-line (indent-to prefix-column
                                          (or (flat signature) "")))))))
    (render-with-prefix column prefix-column head-text prefix body)))

(define (render-record-type form column)
  "Return the lines of a @code{define-record-type} form at @var{column}.

The constructor, the predicate and the fields each get a line, which is how a
record is normally written even when it would fit on one."
  (render-block column
                (string-append "(define-record-type " (flat (cadr form)))
                (cddr form)))

(define (render-lambda form column)
  "Return the lines of a @code{lambda} at @var{column}."
  (let* ((head-text "(lambda ")
         (prefix-column (+ column (string-length head-text)))
         (prefix (render-arguments (cadr form) prefix-column)))
    (render-with-prefix column prefix-column head-text prefix (cddr form))))

(define (render-let form column)
  "Return the lines of a @code{let} and its relatives at @var{column}.

The bindings line up under the first one, so a two-binding @code{let} reads as a
block rather than as a very long first line."
  (let* ((head-text (string-append "(" (symbol->string (car form)) " "))
         (prefix-column (+ column (string-length head-text)))
         (bindings (cadr form))
         (prefix (if (null? bindings)
                     (list (code-line (indent-to prefix-column "()")))
                     (render-sequence prefix-column bindings render-binding)))
         (body (cddr form)))
    (render-with-prefix column prefix-column head-text prefix body)))

(define (render-binding binding column)
  "Return the lines of one binding of a @code{let} at @var{column}.

A value that does not fit on the binding's line goes on the next one, two columns
in, rather than under the value of the binding above it."
  (let ((one-line (flat binding)))
    (cond
      ((and one-line (fits? one-line column))
       (list (code-line (indent-to column one-line))))
      ((pair? binding)
       (render-head-and-body column
                             (string-append "(" (or (flat (car binding)) ""))
                             (cdr binding)))
      (else (list (code-line (indent-to column (or one-line ""))))))))

(define (render-receive form column)
  "Return the lines of a @code{receive} form at @var{column}.

The value expression sits one level deeper than the body, which is what tells a
reader where the multiple-value expression ends and the body begins."
  (let* ((head-text "(receive ")
         (prefix-column (+ column (string-length head-text)))
         (prefix (render-arguments (cadr form) prefix-column))
         (expression (caddr form))
         (body (cdddr form))
         (lines (append (prepend-at column prefix-column head-text prefix)
                        (render expression (+ column 4)))))
    (let loop ((rest body) (lines lines))
      (if (null? rest)
          (last-code-line-append lines ")")
          (loop (cdr rest)
                (append lines (render (car rest) (+ column indent-width))))))))

(define (render-if form column)
  "Return the lines of an @code{if} at @var{column}.

The branches line up under the test, so which form is the alternative is visible
at a glance."
  (let* ((test (cadr form))
         (branches (cddr form))
         (branch-column (+ column 4))
         (lines (prepend-at column
                            branch-column
                            "(if "
                            (render test branch-column))))
    (let loop ((rest branches) (lines lines))
      (if (null? rest)
          (last-code-line-append lines ")")
          (loop (cdr rest)
                (append lines (render (car rest) branch-column)))))))

(define (render-conditional form column)
  "Return the lines of a @code{when} or @code{unless} at @var{column}."
  (let* ((head-text (string-append "(" (symbol->string (car form)) " "))
         (prefix-column (+ column (string-length head-text)))
         (test (cadr form))
         (body (cddr form)))
    (render-with-prefix column
                        prefix-column
                        head-text
                        (render test prefix-column)
                        body)))

(define (render-case form column)
  "Return the lines of a @code{case} at @var{column}."
  (render-block column
                (string-append "(case " (or (flat (cadr form)) ""))
                (cddr form)))

(define (render-module-form form column)
  "Return the lines of a @code{define-module} form at @var{column}.

Every @code{#:use-module} and @code{#:export} clause gets a line of its own: the
header of a generated file is the first thing a reader looks at.  The clauses are
flat in the form itself — the keyword and the value that follows it are separate
elements — so they are paired up again here."
  (render-block column
                (string-append "(define-module " (or (flat (cadr form)) ""))
                (module-clauses (cddr form))))

(define (module-clauses elements)
  "Return the clauses of a @code{define-module} body as one value per keyword.

@code{(#:use-module (srfi srfi-1) #:export (pet? make-pet))} becomes two clauses,
so that a clause is never split across the keyword and what it applies to."
  (cond
    ((null? elements) '())
    ((null? (cdr elements)) (list (car elements)))
    (else
     (cons (clause (car elements) (cadr elements))
           (module-clauses (cddr elements))))))

(define (render-clause form column)
  "Return the lines of one @code{define-module} clause at @var{column}."
  (let* ((keyword (flat (clause-keyword form)))
         (value (clause-value form))
         (one-line (string-append keyword " " (or (flat value) ""))))
    (cond
      ((fits? one-line column) (list (code-line (indent-to column one-line))))
      (else
       (prepend-at column
                   (+ column (string-length keyword) 1)
                   (string-append keyword " ")
                   (render-arguments value
                                     (+ column (string-length keyword) 1)))))))

(define (argument-groups arguments)
  "Return @var{arguments} with each keyword argument kept beside its value.

A keyword and the value that follows it are one argument as far as the reader is
concerned, so a line must never end between them."
  (cond
    ((null? arguments) '())
    ((and (keyword? (car arguments)) (pair? (cdr arguments)))
     (cons (list (car arguments) (cadr arguments))
           (argument-groups (cddr arguments))))
    (else (cons (list (car arguments)) (argument-groups (cdr arguments))))))

(define (group-text group)
  "Return @var{group} as one line of text, or @code{#f} if a part needs several."
  (let ((parts (map flat group)))
    (and (every (lambda (part) part) parts)
         (string-join parts " "))))

(define (keyword-group? group)
  (and (pair? (cdr group)) (keyword? (car group))))

(define (render-call form column)
  "Return the lines of a procedure call at @var{column}.

Arguments fill the line they are on and wrap onto the next, lined up under the
first one: a call that is a little too long for one line should read as one
expression, not as a column of words.  A keyword argument starts a line of its
own, so @code{#:key value} pairs are never split."
  (let* ((head-text (or (flat (car form)) ""))
         (arguments (cdr form))
         (argument-column (+ column 1 (string-length head-text) 1))
         (head-length (+ column 1 (string-length head-text))))
    (if (null? arguments)
        (list (code-line (indent-to column (string-append "(" head-text ")"))))
        (let loop ((rest (argument-groups arguments))
                   (finished '())
                   (current (indent-to column (string-append "(" head-text))))
          (cond
            ((null? rest)
             (last-code-line-append
              (map code-line (reverse (cons current finished)))
              ")"))
            (else
             (let* ((group (car rest))
                    (text (group-text group))
                    (fresh? (and (keyword-group? group)
                                 (> (string-length current) head-length))))
               (cond
                 ((and text
                       (not fresh?)
                       (<= (+ (string-length current) 1 (string-length text))
                           line-width))
                  (loop (cdr rest) finished (string-append current " " text)))
                 ((and text (fits? text argument-column))
                  (loop (cdr rest)
                        (cons current finished)
                        (indent-to argument-column text)))
                 (else
                  (let* ((rendered
                          (append-map (lambda (element)
                                        (map line-text
                                             (render element argument-column)))
                                      group))
                         (all (append (reverse (cons current finished))
                                      rendered)))
                    (loop (cdr rest)
                          (reverse (drop-right all 1))
                          (car (last-pair all)))))))))))))

;;; Commentary:
;;; Printing data

(define (render-quoted datum column)
  "Return the lines of @var{datum} quoted, at @var{column}.

A quoted list that does not fit keeps its quote on the first line and stacks its
elements under it, which is how a literal table is usually written."
  (let ((one-line (flat (list 'quote datum))))
    (cond
      ((and one-line (fits? one-line column))
       (list (code-line (indent-to column one-line))))
      ((and (pair? datum) (list? datum))
       (prepend-at column
                   (+ column 1)
                   "'"
                   (render-sequence (+ column 1) datum render-datum)))
      (else
       (list (code-line (indent-to column (or one-line ""))))))))

(define (render-datum datum column)
  "Return the lines of @var{datum}, a value rather than a form, at @var{column}.

Data that is printed by the generated code is stacked under its opening
parenthesis rather than aligned like the arguments of a call: an association list
is a table, not a call."
  (let ((one-line (flat datum)))
    (cond
      ((and one-line (fits? one-line column))
       (list (code-line (indent-to column one-line))))
      ((and (pair? datum) (list? datum))
       (render-sequence column datum render-datum))
      (else
       (list (code-line (indent-to column (or one-line ""))))))))

;;; Commentary:
;;; Files

(define (forms->string forms)
  "Return the source of @var{forms}, one top-level form after another.

A blank line separates consecutive forms.  A comment is the exception: the blank
line goes in front of it, so it sits directly above the form it explains."
  (let loop ((rest forms) (lines '()) (previous #f))
    (cond
      ((null? rest) (lines->string lines))
      (else
       (let* ((form (car rest))
              (rendered (render form 0))
              (separated? (or (null? lines) (comment? previous)))
              (lines (if separated?
                         (append lines rendered)
                         (append lines (list (blank-line)) rendered))))
         (loop (cdr rest) lines form))))))

(define (code->string form)
  "Return @var{form} as source, with a trailing newline."
  (forms->string (list form)))

(define (lines->string lines)
  "Return @var{lines}, which carry what they are, as the text of a file."
  (if (null? lines)
      ""
      (string-append (string-join (map line-text lines) "\n") "\n")))
