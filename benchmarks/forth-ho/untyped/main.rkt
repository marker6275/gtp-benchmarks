#lang racket/base

(require "forth-interface.rkt"
         "commands.rkt")
(require (only-in racket/file file->lines))

;; =============================================================================

(define LOOPS 10)

(define (main lines)
  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines CMD*))
    (void)))

(define lines (file->lines "../base/history-100.txt"))

(time (main lines))
