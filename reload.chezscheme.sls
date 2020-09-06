;; Chez Scheme R6RS Library reload.
;;
;; Define a reload as inspired by post:
;; https://groups.google.com/d/msg/chez-scheme/8kQhVTkb7wQ/hPB29reOBwAJ
;;
;; BEWARE: this code uses (eval)! Use at own risk.
;;
;; Possible future work:
;; - Automatically handle reverse dependancies
;; - Automatically reload all changed libraries, possibly by tracking file mod timestamps.
;;
;; These would use things like (library-list), (library-requirements), (file-change-time).
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

(library (reload)
  (export reload)
  (import (chezscheme))

;; [syntax] reload import-spec
;; Turn on (import-notify) to see reload messages.
;;
;; Notes:
;; - this must be written as a macro as we do not want to evaluate import-spec.
;; - (import) can only be used where (define) is legal, ie at the beginning of a block.
;;   Using (eval) gets us around that requirement.
(define-syntax reload
  (syntax-rules ()
    [(_ import-spec)
     (let-values ([(source-file object-file libobj-exists?)
                   (default-library-search-handler 'reload 'import-spec (library-directories) (library-extensions))])
       (cond
         [source-file
           (when (import-notify)
             (format #t "reload: reloading ~a~n" source-file))
           ;; Update definitions.
           (load source-file)
           ;; Update library bindings.
           (eval '(import import-spec) (interaction-environment))]
         [else
           (error 'reload "library source not found" 'import-spec (library-directories) (library-extensions))]))]))
)
