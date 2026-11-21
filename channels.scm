;; guile-openapi-generator - OpenAPI 3 to Guile Scheme code generator
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The channel file pins the exact Guix revision every task in `maak.scm'
;; runs against, so a local run and a CI run resolve the same packages.

(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (commit "5cea8de04500ba7904e02ee7b53178fb06856e13")
        (introduction
         (make-channel-introduction
          "9edb3f66fd807b096b48283debdcddccfea34bad"
          (openpgp-fingerprint
           "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
       (channel
        (name 'jointhefreeworld)
        (url "https://github.com/jjba23/guix-jointhefreeworld")
        (branch "trunk")
        ;; `guile-dokgen', which `manifest.scm' names, comes from this
        ;; channel, so the Guix revision and the collection are pinned
        ;; together.
        (commit "24db897c6c9064ebf2018582c6dca2229dda0e62")
        (introduction
         (make-channel-introduction
          "b0ac3f05075d7959860fc9e1db5871acddea3986"
          (openpgp-fingerprint
           "83BC 6E1C 8726 B8C2 97F8  D16E 24F4 6738 CE11 4AF6")))))
