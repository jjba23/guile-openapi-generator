;; guile-openapi

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; guile-openapi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; guile-openapi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with guile-openapi.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (guile-api parser)
             (guile-api ast)
             (srfi srfi-1))

;; Parse JSON from string input
(define json-spec
  "
{
  \"openapi\": \"3.0.0\",
  \"info\": { \"title\": \"PetStore JSON API\", \"version\": \"1.0.0\" },
  \"paths\": {
    \"/pets\": {
      \"get\": {
        \"operationId\": \"listPets\",
        \"summary\": \"List all pets\"
      }
    }
  },
  \"components\": { \"schemas\": {} }
}")

;; Parse YAML from string input
(define yaml-spec
  "
openapi: 3.0.3
info:
  title: Symbanta YAML API
  version: 1.0.0
paths:
  /auth/login:
    post:
      operationId: login
      summary: User login
components:
  schemas: {}
")

(display "=== 1. Parsing Inline Documents ===\n")

(define json-ast
  (parse-openapi-json json-spec))
(format #t "JSON Spec : ~a (v~a) - ~a operation(s)\n"
        (openapi-spec-title json-ast)
        (openapi-spec-version json-ast)
        (length (openapi-spec-paths json-ast)))

(define yaml-ast
  (parse-openapi-yaml yaml-spec))
(format #t "YAML Spec : ~a (v~a) - ~a operation(s)\n\n"
        (openapi-spec-title yaml-ast)
        (openapi-spec-version yaml-ast)
        (length (openapi-spec-paths yaml-ast)))

(display "=== 2. Loading Spec Files ===\n")

(when (file-exists? "openapi.json")
  (let ((ast (parse-openapi-file "openapi.json")))
    (format #t
            "Loaded 'openapi.json': ~a (~a) [~a schemas, ~a operations]
"
            (openapi-spec-title ast)
            (openapi-spec-version ast)
            (length (openapi-spec-schemas ast))
            (length (openapi-spec-paths ast)))))

(when (file-exists? "openapi.yaml")
  (let ((ast (parse-openapi-file "openapi.yaml")))
    (format #t
            "Loaded 'openapi.yaml': ~a (~a) [~a schemas, ~a operations]
"
            (openapi-spec-title ast)
            (openapi-spec-version ast)
            (length (openapi-spec-schemas ast))
            (length (openapi-spec-paths ast)))))
