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
;; A YAML reader written in Guile, with no C dependency and no FFI.  It covers
;; the part of YAML 1.1/1.2 that OpenAPI documents use in practice:
;;
;; - block mappings and block sequences, nested to any depth, including a
;;   sequence written at the same indentation as the key that owns it;
;; - flow sequences and flow mappings, possibly spread over several lines;
;; - plain, single quoted and double quoted scalars, with escapes;
;; - literal (@code{|}) and folded (@code{>}) block scalars, with the
;;   @code{-} and @code{+} chomping indicators and an explicit indentation
;;   indicator;
;; - comments, document start/end markers and directive lines;
;; - anchors (@code{&name}) and aliases (@code{*name}), including merge keys
;;   (@code{<<}), which JSON Schema authors use to avoid repetition;
;; - tags (@code{!!str} and friends), which are dropped: OpenAPI schemas carry
;;   their types in @code{type} members, so the tag adds no information.
;;
;; The reader produces the value representation described in
;; @code{(openapi-generator jso)}: mappings are association lists with string
;; keys, sequences are vectors, scalars are strings, numbers or booleans, and
;; @code{#nil} stands for YAML's null.
;;
;; The implementation is a recursive-descent parser over pre-scanned lines.  A
;; line is remembered as @code{(indent content raw)}: @code{content} has its
;; comment removed and drives the structural decisions, while @code{raw} keeps
;; the original text and is what a block scalar is rebuilt from.

(define-module (openapi-generator yaml)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (openapi-generator jso)
  #:export (yaml-string->scm
            yaml-file->scm
            yaml-available?))

;;; Commentary:
;;; Entry points

(define (yaml-available?)
  "Return @code{#t}: this reader is implemented in Scheme, so YAML support is
never missing."
  #t)

(define (yaml-file->scm path)
  "Read the YAML document in @var{path} and return its value."
  (yaml-string->scm (call-with-input-file path get-string-all)))

(define (yaml-string->scm text)
  "Parse @var{text} as a YAML document.

Only the first document of a multi-document stream is returned, because an
OpenAPI specification is a single document.

@example
(yaml-string->scm \"name: Pet\\nlegs: 4\\n\")
;; => ((\"name\" . \"Pet\") (\"legs\" . 4))
@end example"
  (let* ((lines (list->vector (map make-line (split-lines text))))
         (count (vector-length lines))
         (position 0)
         (pending #f)
         (anchors '()))

    ;; ====== Line access ======

    (define (line-indent index)
      (vector-ref (vector-ref lines index) 0))

    (define (line-content index)
      (vector-ref (vector-ref lines index) 1))

    (define (line-raw index)
      (vector-ref (vector-ref lines index) 2))

    (define (skip-blanks!)
      (unless pending
        (while (and (< position count)
                    (string-null? (line-content position)))
          (set! position (+ position 1)))))

    ;; The next structural line, without consuming it: either the synthetic
    ;; line a block sequence item pushed, or the next non-blank source line.
    (define (current-line)
      (skip-blanks!)
      (cond
        (pending pending)
        ((< position count)
         (list (line-indent position) (line-content position)))
        (else #f)))

    (define (advance!)
      (if pending
          (set! pending #f)
          (set! position (+ position 1))))

    (define (peek-continuation indent)
      "Return the next line's content when it continues below INDENT."
      (skip-blanks!)
      (if (and (< position count)
               (not pending)
               (> (line-indent position) indent))
          (line-content position)
          #f))

    (define (peek-continuation-at-least indent)
      "Return the next line's content when its indentation is at least INDENT.

A plain or quoted scalar may continue at the indentation of its first line;
what makes it a continuation is that it is not a mapping or sequence entry, and
that the scalar has not ended yet."
      (skip-blanks!)
      (if (and (< position count)
               (not pending)
               (>= (line-indent position) indent))
          (line-content position)
          #f))

    (define (fetch-continuation! indent)
      (let ((content (peek-continuation indent)))
        (when content
          (set! position (+ position 1)))
        content))

    (define (fetch-continuation-at-least! indent)
      (let ((content (peek-continuation-at-least indent)))
        (when content
          (set! position (+ position 1)))
        content))

    ;; ====== Scalars ======

    (define (resolve-scalar text)
      "Turn plain scalar TEXT into a string, number, boolean or null."
      (let ((trimmed (string-trim-both text char-set:whitespace)))
        (cond
          ((string-null? trimmed) json-null)
          ((member trimmed '("~" "null" "Null" "NULL")) json-null)
          ((member trimmed '("true" "True" "TRUE"
                             "yes" "Yes" "YES"
                             "on" "On" "ON"))
           #t)
          ((member trimmed '("false" "False" "FALSE"
                             "no" "No" "NO"
                             "off" "Off" "OFF"))
           #f)
          ((string-match "^[-+]?[0-9]+$" trimmed)
           (string->number trimmed))
          ((string-match "^0x[0-9a-fA-F]+$" trimmed)
           (string->number (substring trimmed 2 (string-length trimmed)) 16))
          ((string-match "^0o[0-7]+$" trimmed)
           (string->number (substring trimmed 2 (string-length trimmed)) 8))
          ((string-match
            "^[-+]?([0-9]+\\.[0-9]*|\\.[0-9]+|[0-9]+)([eE][-+]?[0-9]+)?$"
            trimmed)
           (string->number trimmed))
          ((string=? trimmed ".inf") +inf.0)
          ((string=? trimmed "-.inf") -inf.0)
          ((string=? trimmed ".nan") +nan.0)
          (else trimmed))))

    (define (unterminated? text start delimiter)
      (let loop ((index start))
        (cond
          ((>= index (string-length text)) #t)
          ((char=? (string-ref text index) #\\) (loop (+ index 2)))
          ((char=? (string-ref text index) delimiter) #f)
          (else (loop (+ index 1))))))

    (define (parse-escape text index)
      "Return @code{(values character next-index)} for the escape at INDEX."
      (when (>= index (string-length text))
        (error "YAML: dangling escape in a double quoted scalar" text))
      (let ((character (string-ref text index)))
        (cond
          ((char=? character #\n) (values #\newline (+ index 1)))
          ((char=? character #\t) (values #\tab (+ index 1)))
          ((char=? character #\r) (values #\return (+ index 1)))
          ((char=? character #\0) (values (integer->char 0) (+ index 1)))
          ((char=? character #\\) (values #\\ (+ index 1)))
          ((char=? character #\") (values #\" (+ index 1)))
          ((char=? character #\') (values #\' (+ index 1)))
          ((char=? character #\/) (values #\/ (+ index 1)))
          ((char=? character #\space) (values #\space (+ index 1)))
          ((char=? character #\x)
           (let* ((end (+ index 3))
                  (number (string->number (substring text (+ index 1) end) 16)))
             (values (integer->char number) end)))
          ((char=? character #\u)
           (let* ((end (+ index 5))
                  (number (string->number (substring text (+ index 1) end) 16)))
             (values (integer->char number) end)))
          (else (values character (+ index 1))))))

    (define (parse-quoted text start)
      "Return @code{(values string end)} for the scalar quoted at START."
      (let* ((delimiter (string-ref text start))
             (limit (string-length text)))
        (let loop ((index (+ start 1)) (out '()))
          (cond
            ((>= index limit)
             (error "YAML: unterminated quoted scalar" text))
            ((and (char=? delimiter #\')
                  (char=? (string-ref text index) #\')
                  (< (+ index 1) limit)
                  (char=? (string-ref text (+ index 1)) #\'))
             (loop (+ index 2) (cons #\' out)))
            ((char=? (string-ref text index) delimiter)
             (values (list->string (reverse out)) (+ index 1)))
            ((and (char=? delimiter #\")
                  (char=? (string-ref text index) #\\))
             (receive (character next) (parse-escape text (+ index 1))
               (loop next (cons character out))))
            (else
             (loop (+ index 1) (cons (string-ref text index) out)))))))

    (define (fetch-quoted-continuation! indent)
      "Return the next line that may continue the quoted scalar at INDENT.

A quoted scalar may continue at the indentation of its opening line or deeper;
the closing quote, not the indentation, is what ends it."
      (fetch-continuation-at-least! indent))

    (define (quoted-value text indent)
      "Parse a quoted scalar that may continue on the following lines."
      (let loop ((buffer text) (guard 0))
        (let ((start (string-index buffer (lambda (character)
                                            (or (char=? character #\')
                                                (char=? character #\"))))))
          (cond
            ((not start)
             (resolve-scalar buffer))
            ((unterminated? buffer (+ start 1) (string-ref buffer start))
             (let ((continuation (if (> guard 1000)
                                     #f
                                     (fetch-quoted-continuation! indent))))
               (if continuation
                   (loop (string-append buffer " " continuation) (+ guard 1))
                   (error "YAML: unterminated quoted scalar" text))))
            (else
             (receive (value _end) (parse-quoted buffer start)
               value))))))

    (define (plain-value text indent)
      "Parse a plain scalar, folding the continuation lines that belong to it."
      (let loop ((parts (list text)))
        (let ((continuation (peek-continuation-at-least indent)))
          (if (and continuation
                   (not (string-null? continuation))
                   (not (mapping-entry continuation))
                   (not (sequence-entry? continuation)))
              (begin
                (set! position (+ position 1))
                (loop (cons continuation parts)))
              (resolve-scalar (string-join (reverse parts) " "))))))

    ;; ====== Block scalars ======

    (define (block-scalar-header? text)
      (and (not (string-null? text))
           (memv (string-ref text 0) '(#\| #\>))))

    (define (parse-block-scalar header parent-indent)
      "Read a literal or folded block scalar whose header is HEADER."
      (let* ((folded? (char=? #\> (string-ref header 0)))
             (flags (substring header 1 (string-length header)))
             (chomping (cond
                         ((string-contains flags "-") 'strip)
                         ((string-contains flags "+") 'keep)
                         (else 'clip)))
             (explicit (let loop ((index 0))
                         (if (>= index (string-length flags))
                             #f
                             (if (char-numeric? (string-ref flags index))
                                 (- (char->integer (string-ref flags index))
                                    (char->integer #\0))
                                 (loop (+ index 1))))))
             (content-indent (and explicit (+ parent-indent explicit)))
             (collected '()))
        (let loop ()
          (when (< position count)
            (let* ((indent (line-indent position))
                   (raw (line-raw position))
                   (blank? (string-every char-set:whitespace raw)))
              (cond
                (blank?
                 (set! collected (cons "" collected))
                 (set! position (+ position 1))
                 (loop))
                ((and (or (not content-indent) (> indent parent-indent))
                      (or (not content-indent) (>= indent content-indent)))
                 (unless content-indent
                   (set! content-indent indent))
                 (set! collected
                       (cons (if (> (string-length raw) content-indent)
                                 (substring raw content-indent
                                            (string-length raw))
                                 "")
                             collected))
                 (set! position (+ position 1))
                 (loop))
                (else #f)))))
        (let* ((ordered (reverse collected))
               (body (if folded?
                         (fold-lines ordered)
                         (string-join ordered "\n"))))
          (cond
            ((eq? chomping 'strip)
             (string-trim-right body #\newline))
            ((eq? chomping 'keep)
             (string-append body "\n"))
            ((string-null? body)
             "")
            (else
             (string-append (string-trim-right body #\newline) "\n"))))))

    (define (fold-lines lines)
      "Fold the lines of a @code{>} block scalar into one string."
      (let loop ((rest lines) (result "") (first? #t) (blanks 0))
        (cond
          ((null? rest) result)
          ((string-null? (car rest))
           (loop (cdr rest) result first? (+ blanks 1)))
          (else
           (let ((separator (cond
                              ((and first? (zero? blanks)) "")
                              ((zero? blanks) " ")
                              (else (make-string blanks #\newline)))))
             (loop (cdr rest)
                   (string-append result separator (car rest))
                   #f
                   0))))))

    ;; ====== Flow collections ======

    (define (flow-balanced? text)
      (let loop ((index 0) (depth 0) (in-string #f))
        (cond
          ((>= index (string-length text))
           (and (zero? depth) (not in-string)))
          (else
           (let ((character (string-ref text index)))
             (cond
               (in-string
                (cond
                  ((char=? character #\\) (loop (+ index 2) depth in-string))
                  ((char=? character in-string) (loop (+ index 1) depth #f))
                  (else (loop (+ index 1) depth in-string))))
               ((memv character '(#\' #\"))
                (loop (+ index 1) depth character))
               ((memv character '(#\[ #\{))
                (loop (+ index 1) (+ depth 1) #f))
               ((memv character '(#\] #\}))
                (loop (+ index 1) (- depth 1) #f))
               (else (loop (+ index 1) depth #f))))))))

    (define (collect-flow text indent)
      "Gather the rest of a flow collection that spans several lines."
      (let loop ((buffer text) (guard 0))
        (cond
          ((flow-balanced? buffer) buffer)
          ((> guard 500) (error "YAML: unbalanced flow collection" text))
          (else
           (let ((continuation (fetch-continuation! indent)))
             (if continuation
                 (loop (string-append buffer " " continuation) (+ guard 1))
                 (error "YAML: unbalanced flow collection" text)))))))

    (define (parse-flow text)
      "Parse TEXT, a complete flow sequence or mapping, into a value."
      (define length (string-length text))

      (define (skip-space index)
        (let loop ((index index))
          (if (and (< index length)
                   (memv (string-ref text index) '(#\space #\tab #\newline)))
              (loop (+ index 1))
              index)))

      (define (plain-token index terminators)
        (let loop ((index index) (out '()))
          (if (or (>= index length)
                  (memv (string-ref text index) terminators))
              (values (list->string (reverse out)) index)
              (loop (+ index 1) (cons (string-ref text index) out)))))

      (define (value-at index)
        (let* ((index (skip-space index))
               (character (and (< index length) (string-ref text index))))
          (cond
            ((not character) (values json-null index))
            ((char=? character #\[) (sequence-at (+ index 1)))
            ((char=? character #\{) (mapping-at (+ index 1)))
            ((memv character '(#\' #\"))
             (parse-quoted text index))
            (else
             (receive (token end)
                      (plain-token index (list #\, #\] #\} #\newline))
               (values (resolve-scalar token) end))))))

      (define (sequence-at index)
        (let loop ((index (skip-space index)) (items '()))
          (cond
            ((>= index length)
             (error "YAML: unterminated flow sequence" text))
            ((char=? (string-ref text index) #\])
             (values (list->vector (reverse items)) (+ index 1)))
            (else
             (receive (value end) (value-at index)
               (let ((next (skip-space end)))
                 (cond
                   ((and (< next length)
                         (char=? (string-ref text next) #\,))
                    (loop (+ next 1) (cons value items)))
                   ((and (< next length)
                         (char=? (string-ref text next) #\]))
                    (loop next (cons value items)))
                   (else
                    (error "YAML: malformed flow sequence" text)))))))))

      (define (mapping-at index)
        (let loop ((index (skip-space index)) (entries '()))
          (cond
            ((>= index length)
             (error "YAML: unterminated flow mapping" text))
            ((char=? (string-ref text index) #\})
             (values (reverse entries) (+ index 1)))
            (else
             (receive (key after-key)
                      (if (memv (string-ref text index) '(#\' #\"))
                          (parse-quoted text index)
                          (receive (token end)
                                   (plain-token index (list #\: #\, #\}))
                            (values (string-trim-both token) end)))
               (let ((colon (skip-space after-key)))
                 (when (or (>= colon length)
                           (not (char=? (string-ref text colon) #\:)))
                   (error "YAML: expected ':' in a flow mapping" text))
                 (receive (value end) (value-at (+ colon 1))
                   (let ((next (skip-space end)))
                     (cond
                       ((and (< next length)
                             (char=? (string-ref text next) #\,))
                        (loop (+ next 1) (cons (cons key value) entries)))
                       ((and (< next length)
                             (char=? (string-ref text next) #\}))
                        (loop next (cons (cons key value) entries)))
                       (else
                        (error "YAML: malformed flow mapping" text)))))))))))

      (receive (value _next) (value-at 0)
        value))

    ;; ====== Mapping and sequence entries ======

    (define (sequence-entry? content)
      (or (string=? content "-")
          (string-prefix? "- " content)))

    (define (mapping-entry content)
      "Return @code{(key . rest)} when CONTENT is a mapping entry, else #f."
      (cond
        ((string-null? content)
         #f)
        ((memv (string-ref content 0) '(#\' #\"))
         (let ((delimiter (string-ref content 0)))
           ;; A quote that does not close on this line starts a multi-line
           ;; scalar, not a mapping entry; the scalar parser will consume the
           ;; continuation lines.
           (if (unterminated? content 1 delimiter)
               #f
               (receive (key end) (parse-quoted content 0)
                 (let loop ((index end))
                   (cond
                     ((>= index (string-length content)) #f)
                     ((char=? (string-ref content index) #\:)
                      (cons key
                            (string-trim-both (substring content (+ index 1)
                                                    (string-length content)))))
                     ((memv (string-ref content index) '(#\space #\tab))
                      (loop (+ index 1)))
                     (else #f)))))))
        (else
         (let loop ((index 0))
           (cond
             ((>= index (string-length content)) #f)
             ((char=? (string-ref content index) #\:)
              (if (or (= (+ index 1) (string-length content))
                      (memv (string-ref content (+ index 1)) '(#\space #\tab)))
                  (cons (string-trim-both (substring content 0 index))
                        (string-trim-both (substring content (+ index 1)
                                                (string-length content))))
                  (loop (+ index 1))))
             (else (loop (+ index 1))))))))

    ;; ====== Anchors ======

    (define (anchor-lookup name)
      (let ((entry (assoc name anchors)))
        (if entry
            (cdr entry)
            (error "YAML: unknown alias" name))))

    (define (anchor-register! name value)
      (set! anchors (cons (cons name value) anchors))
      value)

    ;; ====== Nodes ======

    (define (parse-child indent)
      "Parse the node owned by a key or sequence item at INDENT."
      (let ((next (current-line)))
        (cond
          ((not next) json-null)
          ((> (car next) indent) (parse-node (+ indent 1)))
          ((and (= (car next) indent) (sequence-entry? (cadr next)))
           (parse-node indent))
          (else json-null))))

    (define (parse-value text indent)
      "Parse the value of an entry whose text after the key is TEXT."
      (cond
        ((string-null? text)
         (parse-child indent))
        ((block-scalar-header? text)
         (parse-block-scalar text indent))
        ((char=? (string-ref text 0) #\*)
         (anchor-lookup (string-trim-both (substring text 1 (string-length text)))))
        ((char=? (string-ref text 0) #\&)
         (let* ((rest (string-trim-both (substring text 1 (string-length text))))
                (space (string-index rest char-set:whitespace))
                (name (if space (substring rest 0 space) rest))
                (tail (if space
                          (string-trim-both (substring rest space (string-length rest)))
                          "")))
           (anchor-register! name
                             (if (string-null? tail)
                                 (parse-child indent)
                                 (parse-value tail indent)))))
        ((char=? (string-ref text 0) #\!)
         (let* ((space (string-index text char-set:whitespace))
                (tail (if space
                          (string-trim-both (substring text space (string-length text)))
                          "")))
           (if (string-null? tail)
               (parse-child indent)
               (parse-value tail indent))))
        ((memv (string-ref text 0) '(#\[ #\{))
         (parse-flow (collect-flow text indent)))
        ((memv (string-ref text 0) '(#\' #\"))
         (quoted-value text indent))
        (else
         (plain-value text indent))))

    (define (parse-sequence indent)
      (let loop ((items '()))
        (let ((line (current-line)))
          (cond
            ((or (not line)
                 (not (= (car line) indent))
                 (not (sequence-entry? (cadr line))))
             (list->vector (reverse items)))
            (else
             (let* ((content (cadr line))
                    (tail (string-drop content 1))
                    (trimmed (string-trim-both tail)))
               (advance!)
               (if (string-null? trimmed)
                   (loop (cons (parse-child indent) items))
                   (let ((spaces (- (string-length tail)
                                    (string-length (string-trim-both tail)))))
                     (set! pending (list (+ indent 1 spaces) trimmed))
                     (loop (cons (parse-node (+ indent 1 spaces)) items))))))))))

    (define (parse-mapping indent)
      (let loop ((entries '()) (merged '()))
        (let ((line (current-line)))
          (cond
            ((or (not line)
                 (not (= (car line) indent))
                 (sequence-entry? (cadr line))
                 (not (mapping-entry (cadr line))))
             (jso-append (reverse merged) (reverse entries)))
            (else
             (let* ((entry (mapping-entry (cadr line)))
                    (key (car entry))
                    (rest (cdr entry)))
               (advance!)
               (let ((value (parse-value rest indent)))
                 (if (string=? key "<<")
                     (loop entries
                           (append (cond
                                     ((jso-object? value) value)
                                     ((vector? value)
                                      (append-map (lambda (element)
                                                    (if (jso-object? element)
                                                        element
                                                        '()))
                                                  (vector->list value)))
                                     (else '()))
                                   merged))
                     (loop (cons (cons key value) entries) merged)))))))))

    (define (inline-value? content)
      "Return #t when CONTENT starts a value that cannot be a mapping key.

A flow collection may contain a colon (@code{{name: id}}), so it has to be
recognised before the entry is split into a key and a value."
      (and (not (string-null? content))
           (let ((first (string-ref content 0)))
             (cond
               ((memv first '(#\{ #\[ #\| #\>)) #t)
               ((memv first '(#\' #\" #\& #\* #\!))
                (not (mapping-entry content)))
               (else #f)))))

    (define (parse-node parent-indent)
      (let ((line (current-line)))
        (cond
          ((not line) json-null)
          ((< (car line) parent-indent) json-null)
          ((sequence-entry? (cadr line)) (parse-sequence (car line)))
          ((inline-value? (cadr line))
           (let ((indent (car line))
                 (content (cadr line)))
             (advance!)
             (parse-value content indent)))
          ((mapping-entry (cadr line)) (parse-mapping (car line)))
          (else
           (let ((indent (car line))
                 (content (cadr line)))
             (advance!)
             (parse-value content indent))))))

    (parse-node 0)))

;;; Commentary:
;;; Line scanning

(define (split-lines text)
  "Split @var{text} into lines, dropping a byte order mark and carriage returns."
  (let* ((body (if (and (> (string-length text) 0)
                        (char=? (string-ref text 0) #\xfeff))
                   (substring text 1 (string-length text))
                   text)))
    (map (lambda (line)
           (if (string-suffix? "\r" line)
               (string-drop-right line 1)
               line))
         (string-split body #\newline))))

(define (make-line raw)
  "Scan one source line into @code{(indent content raw)}."
  (let* ((indent (count-indent raw))
         (text (if (> (string-length raw) indent)
                   (substring raw indent (string-length raw))
                   ""))
         (content (strip-comment text)))
    (vector indent
            (if (structural-marker? content)
                ""
                (string-trim-right content char-set:whitespace))
            raw)))

(define (structural-marker? content)
  "Return @code{#t} for document markers and directives, which carry no data."
  (or (string=? content "---")
      (string=? content "...")
      (string-prefix? "%YAML" content)
      (string-prefix? "%TAG" content)))

(define (count-indent line)
  (let loop ((index 0))
    (cond
      ((>= index (string-length line)) index)
      ((char=? (string-ref line index) #\space) (loop (+ index 1)))
      ((char=? (string-ref line index) #\tab)
       (error "YAML: a tab cannot be used for indentation" line))
      (else index))))

(define (strip-comment text)
  "Return @var{text} without its trailing comment, ignoring # inside quotes."
  (let loop ((index 0) (delimiter #f))
    (cond
      ((>= index (string-length text)) text)
      (delimiter
       (let ((character (string-ref text index)))
         (cond
           ((char=? character #\\) (loop (+ index 2) delimiter))
           ((char=? character delimiter) (loop (+ index 1) #f))
           (else (loop (+ index 1) delimiter)))))
      (else
       (let ((character (string-ref text index)))
         (cond
           ((memv character '(#\' #\")) (loop (+ index 1) character))
           ((and (char=? character #\#)
                 (or (zero? index)
                     (char=? (string-ref text (- index 1)) #\space)))
            (substring text 0 index))
           (else (loop (+ index 1) #f))))))))
