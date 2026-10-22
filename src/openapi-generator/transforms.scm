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
;; Small, opt-in rewrites of a specification before it is parsed.  They are kept
;; separate from the readers so that a transform can be tested on a document
;; without generating anything, and so that the spelling an author chose stays
;; visible in the source they wrote.
;;
;; The first transform is the extensible enumeration.  A schema can say
;; @code{x-extensible-enum: [a, b]} to mean "these are the values we know about,
;; but a server may send another one".  Treating it as a closed enumeration is
;; only correct when the caller asks for it, which is why it is a transform and
;; not a reader rule.

(define-module (openapi-generator transforms)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (openapi-generator jso)
  #:export (enable-extensible-enums))

(define (extensible-values node)
  "Return the values the extensible-enum extension of @var{node} declares, or #f."
  (let ((value (or (jso-ref node "x-extensible-enum")
                   (jso-ref node "x-extensible-enums"))))
    (cond
      ((vector? value) value)
      ((or (string? value) (number? value)) (vector value))
      ((eq? value #f) #f)
      (else #f))))

(define (walk node)
  "Return @var{node} with extensible enumerations spelled as @code{enum}."
  (cond
    ((jso-object? node)
     (let* ((values (and (not (jso-has? node "enum"))
                         (extensible-values node)))
            (node (if values
                      (jso-put node "enum" values)
                      node)))
       (map (lambda (entry)
              (cons (car entry) (walk (cdr entry))))
            node)))
    ((vector? node)
     (list->vector (map walk (vector->list node))))
    (else node)))

(define (enable-extensible-enums document)
  "Return @var{document} with @code{x-extensible-enum} treated as @code{enum}.

A schema that already declares an @code{enum} is left alone: an explicit
enumeration is the stronger statement."
  (walk document))
