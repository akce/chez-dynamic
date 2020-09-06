;; Chez Scheme R6RS Library reload.
;;
;; Define a reload as inspired by post:
;; https://groups.google.com/d/msg/chez-scheme/8kQhVTkb7wQ/hPB29reOBwAJ

;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

(library (reload)
  (export reload)
  (import (chezscheme))

;; [syntax] reload import-spec
;; Turn on (import-notify) to see reload messages.
(define-syntax reload
  (syntax-rules ()
    [(_ import-spec)
     (let-values ([(source-file object-file libobj-exists?)
                   (default-library-search-handler 'reload 'import-spec (library-directories) (library-extensions))])
       (cond
         [source-file
           (when (import-notify)
             (format #t "reload: reloading ~a~n" source-file))
           (load source-file)
           (eval '(import import-spec) (interaction-environment))]
         [else
           (error 'reload "library source not found" 'import-spec (library-directories) (library-extensions))]))]))
)
