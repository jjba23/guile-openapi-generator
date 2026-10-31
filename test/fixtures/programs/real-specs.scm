;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Report on a generated tree, so a test can check that a real-world
;; specification survives generation: the route table must hold one route per
;; operation, the modules must export something usable, and the dispatcher must
;; be a procedure.  The tree is imported by the harness, so reaching this file at
;; all means the generated modules compiled and loaded.

(define (export-count module)
  "Return how many bindings MODULE exports."
  (let ((total 0))
    (module-for-each (lambda (name variable)
                       (set! total (+ total 1)))
                     module)
    total))

(define (exports-of suffix)
  "Return the number of exports of the imported module named `... SUFFIX`."
  (let loop ((modules (module-uses (current-module))))
    (cond
      ((null? modules) 0)
      ((eq? (car (reverse (module-name (car modules)))) suffix)
       (export-count (car modules)))
      (else (loop (cdr modules))))))

(format #t "routes=~a~%" (length routes))
(format #t "dispatch=~a~%" (procedure? dispatch))
(format #t "models=~a~%" (exports-of 'models))
(format #t "controllers=~a~%" (exports-of 'controllers))
(format #t "client=~a~%" (exports-of 'client))
