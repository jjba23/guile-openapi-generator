;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; JSON support, on top of guile-json.  The only work done here is to normalise
;; what guile-json returns (symbol keys, the symbol @code{null}) into the value
;; representation shared with the YAML reader, described in
;; @code{(openapi-generator jso)}.

(define-module (openapi-generator json)
  #:declarative? #t
  #:use-module (json)
  #:use-module (ice-9 textual-ports)
  #:use-module (openapi-generator jso)
  #:export (json-string->jso
            json-file->jso
            jso->json-string
            jso->json-file))

(define (json-string->jso text)
  "Parse the JSON document in @var{text} into a jso value.

@example
(json-string->jso \"{\\\"name\\\": \\\"Pet\\\"}\")
;; => ((\"name\" . \"Pet\"))
@end example"
  (jso-normalize (json-string->scm text)))

(define (json-file->jso path)
  "Parse the JSON document in @var{path} into a jso value."
  (json-string->jso (call-with-input-file path get-string-all)))

(define* (jso->json-string value #:key (pretty? #f))
  "Serialise the jso @var{value} as a JSON string.

@example
(jso->json-string '((\"name\" . \"Pet\")))
;; => \"{\\\"name\\\":\\\"Pet\\\"}\"
@end example"
  (scm->json-string (jso-normalize value) #:pretty pretty?))

(define (jso->json-file value path)
  "Write the jso @var{value} to @var{path} as JSON, and return @var{path}."
  (call-with-output-file path
    (lambda (port)
      (display (jso->json-string value) port)))
  path)
