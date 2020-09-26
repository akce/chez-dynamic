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
  (export
    case-import
    reimport)
  (import (chezscheme))

(define has-import
  (lambda (who library-tag)
    (let-values ([(source-file object-file obj-exists?)
                  ((library-search-handler) who library-tag (library-directories) (library-extensions))])
      (or source-file obj-exists?))))

;; [syntax] case-import: conditionally import a library.
;; case-import has a similar syntax to case-lambda except its clauses attempt to import a library.
;;
;; Limitations:
;; - many
;; - import-spec can only contain the library path. Specifiers such as only, prefix etc are not supported.
;; - the existance of the library source or object file implies success. No checks are made to see whether the actual import succeeds.
;;
;; TODO: add support for an optional sentinal (ala syntax-case)?
(define-syntax case-import
  (syntax-rules ()
    [(_ (import-spec body ...) ...)
     (meta-cond
       [(has-import 'case-import 'import-spec)
        (import import-spec)
        body ...] ...)]))

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
                   ((library-search-handler) 'reimport 'import-spec (library-directories) (library-extensions))])
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
