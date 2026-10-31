;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The Kubernetes pod patch operation declares three JSON media types with three
;; different schemas: a JSON Patch array, a merge patch and a strategic merge
;; patch, the last two both being a Pod.  The dispatcher has to pick the decoder
;; from the actual Content-Type, which is why this program sends one request of
;; each kind and prints what the handler received.

(define patch-path "/api/v1/namespaces/default/pods/web")

(define (dispatch-with content-type body arguments)
  "Dispatch a PATCH of BODY with CONTENT-TYPE and return the handler's REPORT."
  (parameterize ((api-handlers
                  (list (cons 'patch-core-v1namespaced-pod
                              (lambda (request received)
                                (ok (arguments received)))))))
    (http-response-body
     (dispatch (make-http-request 'PATCH patch-path '()
                                  (list (cons "Content-Type" content-type))
                                  body)))))

(define (patch-report arguments)
  (let ((body (cdr (assq 'body arguments))))
    (format #f "json-patch list=~a items=~a"
            (list? body) (length body))))

(define (merge-report arguments)
  (let* ((body (cdr (assq 'body arguments)))
         (pod? (io-k8s-api-core-v1-pod? body))
         (policy (and pod?
                      (let ((spec (io-k8s-api-core-v1-pod-spec body)))
                        (and spec
                             (io-k8s-api-core-v1-pod-spec-restart-policy spec))))))
    (format #f "merge pod=~a policy=~a" pod? policy)))

(format #t "~a~%"
        (dispatch-with
         "application/json-patch+json"
         "[{\"op\":\"replace\",\"path\":\"/spec/restartPolicy\",\"value\":\"Never\"},{\"op\":\"remove\",\"path\":\"/metadata/labels\"}]"
         patch-report))

;; The patched object is a Pod, so the decoder validates it as one: `containers'
;; is required by the schema and is therefore part of the body.
(format #t "~a~%"
        (dispatch-with
         "application/merge-patch+json; charset=utf-8"
         "{\"spec\":{\"restartPolicy\":\"Never\",\"containers\":[{\"name\":\"web\",\"image\":\"nginx\"}]}}"
         merge-report))

(format #t "~a~%"
        (dispatch-with
         "application/strategic-merge-patch+json"
         "{\"spec\":{\"restartPolicy\":\"OnFailure\",\"containers\":[{\"name\":\"web\",\"image\":\"nginx\"}]}}"
         merge-report))
