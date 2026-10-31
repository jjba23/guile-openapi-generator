;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A small code writer: every generator module emits lines through these
;; procedures, which own indentation, alignment, wrapping and the escaping of
;; strings.  Keeping it in one place is what makes the output of the four
;; generators look as if the same person wrote it, and what makes it possible to
;; assert on the generated text in the tests without whitespace noise.
;;
;; Lines are collected rather than written straight out, because a form that is
;; opened with one call should be closed on the line that holds its last
;; argument: @code{emit-close} appends a parenthesis to the previous line.  That
;; is the difference between generated code that reads like Scheme and generated
;; code that reads like a template.

(define-module (openapi-generator emitter)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (code->string
            emit
            emit-blank
            emit-close
            emit-close-with
            emit-comment
            emit-docstring
            docstring-text
            escape-string
            emit-raw
            emit-wrapped
            emit-list
            emit-header
            emit-module
            with-indent
            with-indent-by
            indentation
            scheme-string
            scheme-symbol
            symbol-list
            inline-list
            line-width))

;;; Commentary:
;;; State

(define current-lines
  (make-parameter '()))

(define indentation
  (make-parameter 0))

(define indent-width
  2)

(define line-width
  78)

(define* (push-line text #:optional (protected? #f))
  (unless (current-lines)
    (error "emit called outside code->string"))
  (current-lines (cons (cons text protected?) (current-lines))))

(define (pad)
  (make-string (indentation) #\space))

(define (emit format-string . arguments)
  "Write one line of code, prefixed with the current indentation."
  (push-line (string-append (pad)
                            (apply format #f format-string arguments))))

(define (emit-raw line)
  "Write @var{line} verbatim, with the current indentation."
  (push-line (string-append (pad) line)))

(define (emit-blank)
  "Write an empty line."
  (push-line ""))

(define (emit-close)
  "Append a closing parenthesis to the line just written."
  (emit-close-with ")"))

(define (emit-close-with text)
  "Append @var{text} to the line just written."
  (let ((lines (current-lines)))
    (when (null? lines)
      (error "emit-close-with called before emit"))
    (current-lines (cons (cons (string-append (car (car lines)) text)
                               (cdr (car lines)))
                         (cdr lines)))))

(define (with-indent thunk)
  "Evaluate @var{thunk} with the indentation increased by one level."
  (parameterize ((indentation (+ (indentation) indent-width)))
    (thunk)))

(define (with-indent-by amount thunk)
  "Evaluate @var{thunk} with the indentation increased by @var{amount} columns."
  (parameterize ((indentation (+ (indentation) amount)))
    (thunk)))

(define (code->string thunk)
  "Return, as a string, the code @var{thunk} writes with the emit procedures."
  (parameterize ((current-lines '())
                 (indentation 0))
    (thunk)
    (let ((lines (map car (pull-up-closers (reverse (current-lines))))))
      (if (null? lines)
          ""
          (string-append (string-join lines "\n") "\n")))))

(define (pull-up-closers lines)
  "Join lines that hold nothing but closing parentheses to the line above.

Emitting a block with @code{with-indent} and closing it with @code{emit-close}
would otherwise leave a line holding a lone parenthesis."
  (if (null? lines)
      '()
      (let loop ((rest (cdr lines)) (result (list (car lines))))
        (cond
          ((null? rest) (reverse result))
          ((closing-only? (car rest))
           (loop (cdr rest)
                 (cons (cons (string-append (car (car result))
                                            (string-trim-both (car (car rest))))
                             (cdr (car result)))
                       (cdr result))))
          (else (loop (cdr rest) (cons (car rest) result)))))))

(define (closing-only? line)
  "Return #t for a line that holds nothing but closing parentheses."
  (let ((text (car line)))
    (if (cdr line)
        #f
        (let ((trimmed (string-trim-both text)))
          (and (not (string-null? trimmed))
               (string-every (lambda (character)
                               (memv character '(#\))))
                             trimmed))))))

(define* (emit-wrapped prefix parts #:optional (suffix ""))
  "Write @var{prefix} followed by @var{parts}, wrapping at 78 columns.

Continuation lines line up under the first part, which is how a long argument
list should look in Scheme."
  (let* ((start (pad))
         (column (+ (indentation) (string-length prefix) 1))
         (width (max line-width (+ column 24))))
    (let loop ((remaining parts)
               (line (string-append start prefix))
               (started 0))
      (cond
        ((null? remaining)
         (push-line (string-append line suffix)))
        (else
         (let* ((part (car remaining))
                (tentative (+ (string-length line) 1 (string-length part))))
           (cond
             ((and (> started 0) (> tentative width))
              (push-line line)
              (loop (cdr remaining)
                    (string-append (make-string column #\space) part)
                    1))
             (else
              (loop (cdr remaining)
                    (string-append line " " part)
                    (+ started 1))))))))))

;;; Commentary:
;;; Comments and documents

(define (emit-comment format-string . arguments)
  "Write @var{format-string} as one or more comment lines."
  (let ((text (apply format #f format-string arguments)))
    (for-each (lambda (line)
                (if (string-null? line)
                    (emit-raw ";;")
                    (emit-raw (string-append ";; " line))))
              (string-split text #\newline))))

(define (emit-header lines)
  "Write the banner comment that opens a generated file.

@var{lines} is a list of paragraphs: the first names the file, the rest explain
where it came from."
  (emit-raw ";; -*- mode: scheme; coding: utf-8 -*-")
  (emit-blank)
  (for-each (lambda (line)
              (emit-comment "~a" line))
            (append-map (lambda (paragraph)
                          (wrap-lines paragraph 74))
                        lines))
  (emit-blank))

(define (wrap-lines paragraph width)
  "Wrap one paragraph at @var{width} columns."
  (if (string-null? paragraph)
      (list "")
      (let loop ((words (string-split paragraph #\space))
                 (current "")
                 (result '()))
        (cond
          ((null? words)
           (reverse (cons current result)))
          ((string-null? current)
           (loop (cdr words) (car words) result))
          ((> (+ (string-length current) 1 (string-length (car words))) width)
           (loop (cdr words) (car words) (cons current result)))
          (else
           (loop (cdr words)
                 (string-append current " " (car words))
                 result))))))

(define (docstring-text text)
  "Return @var{text} with the @code{~%} spelling of a newline expanded.

Docstrings are literal text, but writing them as one long line is unreadable in
the generator, so the two generators spell line breaks the way a format string
would.  A second blank line is spelled @code{~%~%}."
  (string-replace-substring
   (string-replace-substring (if (string? text) text "") "~%~%" "\n\n")
   "~%" "\n"))

(define (emit-docstring text)
  "Write @var{text} as a Scheme docstring literal, keeping its line structure."
  (let ((trimmed (string-trim-both (docstring-text text))))
    (unless (string-null? trimmed)
      (let ((lines (string-split trimmed #\newline)))
        (push-line (string-append (pad) "\"" (escape-string (car lines))) #t)
        (for-each (lambda (line)
                    (push-line (escape-string line) #t))
                  (cdr lines))
        (emit-close-with "\"")))))

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
  (format #f "~s" (if (string? text) text (format #f "~a" text))))

(define (scheme-symbol name)
  "Return @var{name} as a quoted Scheme symbol."
  (format #f "'~a" name))

(define (symbol-list symbols)
  "Return @var{symbols} as the source text of a quoted list."
  (if (null? symbols)
      "'()"
      (format #f "'(~a)" (string-join (map (lambda (symbol)
                                              (format #f "~a" symbol))
                                            symbols)
                                       " "))))

(define (inline-list items)
  "Return @var{items}, already strings, as a parenthesised expression."
  (format #f "(~a)" (string-join items " ")))

;;; Commentary:
;;; Modules

(define (emit-module name imports re-exports exports)
  "Write the @code{define-module} form of the generated file.

@var{name} is a list of symbols, @var{imports} a list of module names,
@var{re-exports} and @var{exports} lists of symbols."
  (emit "(define-module (~{~a~^ ~})" name)
  (with-indent
   (lambda ()
     (for-each (lambda (import)
                 (emit "#:use-module (~{~a~^ ~})"
                       (if (list? import) import (list import))))
               imports)
     (unless (null? re-exports)
       (emit "#:re-export (~{~a~^ ~})" re-exports))
     (when (null? exports)
       (error "A generated module must export something"))
     (emit-exports exports)))
  (emit-close))

(define (emit-exports exports)
  "Write an @code{#:export} clause for @var{exports}."
  (if (<= (length exports) 3)
      (emit "#:export (~{~a~^ ~})" exports)
      (begin
        (emit-raw "#:export (")
        (with-indent
         (lambda ()
           (let loop ((rest exports))
             (cond
               ((null? rest) #t)
               ((null? (cdr rest)) (emit "~a)" (car rest)))
               (else
                (emit "~a" (car rest))
                (loop (cdr rest))))))))))

(define (emit-list items)
  "Write each of @var{items}, already strings, on its own line."
  (for-each emit-raw items))
