#lang typed/racket/base

(require
  require-typed-check
  "../base/command-types.rkt"
  "forth-interface.rkt")
(require/typed/check "commands.rkt"
  (CMD* (Listof (Instance Command%))))
(require (only-in racket/file file->lines))

;; =============================================================================

(define LOOPS 10)

(: main (-> (Listof String) Void))
(define (main lines)
  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines CMD*))
    (void)))

(define lines (file->lines "../base/history-100.txt"))

(time (main lines))
