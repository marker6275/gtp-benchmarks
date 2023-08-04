#lang racket

(require "data.rkt"
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")
;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

(provide/configurable-contract
 [cut-tail ([max (->i ([segs ne-segs?])
                      [result (segs)
                              (snake-segs=?/c (drop-right segs 1))])]
            [types (ne-segs? . -> . snake-segs?)])])

(define/ctc-helper ne-segs? (and/c snake-segs? cons?))

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

;; (provide
;;  cut-tail)
