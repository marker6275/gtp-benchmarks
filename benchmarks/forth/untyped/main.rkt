#lang racket
;;(provide;; /configurable-contract
 ;; [LOOPS ([max 1]
 ;;   [types natural?])]
 ;; [main ([max (-> (listof string?) void?)]
 ;;   [types (-> (listof string?) void?)])]
 ;; [lines ([max (listof string?)]
 ;;   [type
    ;; s (listof string?)])])

(require (only-in "eval.rkt"
  forth-eval*
))
(require (only-in racket/file file->lines)
         "../../../ctcs/precision-config.rkt"
         ;; racket/contract
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in racket/math natural?))

;; =============================================================================

(define LOOPS
  1)

(define (main lines)
  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines))
    (void)))

(define lines
  (file->lines "../base/history-100.txt"))

(time (main lines))
