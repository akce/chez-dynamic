;; Chez Scheme R6RS Library reload.
;;
;; Define a reload as inspired by post:
;; https://groups.google.com/d/msg/chez-scheme/8kQhVTkb7wQ/hPB29reOBwAJ

;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

;; (import-notify) parameter controlling whether Chez prints import search messages.

;; (invoke-library ...) to explicitly exec library definitions.

;; (library-directories) list of (source-dir . object-dir) pairs.

;; (library-extensions) list of (source-ext . object-ext) pairs.

(library (reload)
  (export reload)
  (import (chezscheme))

;; [syntax] reload import-spec
;; [syntax] reload import-spec search-directories
;; [syntax] reload import-spec search-directories file-extensions
;; search-directories defaults to (library-directories)
;; file-extensions defaults to (library-extensions)
;; Turn on (import-notify) to see reload messages.
(define-syntax reload
  (syntax-rules ()
    [(_ import-spec)
     (reload import-spec (library-directories) (library-extensions))]
    [(_ import-spec dirs)
     (reload import-spec dirs (library-extensions))]
    [(_ import-spec dirs extensions)
     (let-values ([(source-file object-file libobj-exists?)
                   (default-library-search-handler 'reload 'import-spec dirs extensions)])
       (cond
         [source-file
           (when (import-notify)
             (format #t "reload: reloading ~a~n" source-file))
           (load source-file)
           (eval '(import import-spec) (interaction-environment))]
         [else
           (error 'reload "library source not found" 'import-spec dirs extensions)]))]))
)
