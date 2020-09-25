;; Chez Scheme R6RS Library dynamism.
;;
;; BEWARE: reimport uses (eval)! Use at own risk.
;;
;; Possible future work:
;; - Automatically handle reverse dependancies
;; - Automatically reimport all changed libraries, possibly by tracking file mod timestamps.
;;
;; These would use things like (library-list), (library-requirements), (file-change-time).
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

(library (dynamic)
  (export reimport)
  (import (chezscheme))

;; [syntax] reimport import-spec
;; Turn on (import-notify) to see reimport messages.
;;
;; Notes:
;; - this must be written as a macro as we do not want to evaluate import-spec.
;; - (import) can only be used where (define) is legal, ie at the beginning of a block.
;;   Using (eval) gets us around that requirement.
(define-syntax reimport
  (syntax-rules ()
    [(_ import-spec)
     (let-values ([(source-file object-file libobj-exists?)
                   (default-library-search-handler 'reimport 'import-spec (library-directories) (library-extensions))])
       (cond
         [source-file
           (when (import-notify)
             (format #t "reimport: reimporting ~a~n" source-file))
           ;; Update definitions.
           (load source-file)
           ;; Update library bindings.
           (eval '(import import-spec) (interaction-environment))]
         [else
           (error 'reimport "library source not found" 'import-spec (library-directories) (library-extensions))]))]))
)
