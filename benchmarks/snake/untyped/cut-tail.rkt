#lang racket

(require (except-in "data.rkt" posn=?)
         "../../../ctcs/configurable.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         modalc
         "../../curr-mode.rkt")
(require/configurable-contract "data.rkt" posn=? )
;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

(provide/configurable-contract
 [cut-tail ([max (modal->i curr-mode ([segs ne-segs?])
                           [result (segs)
                                   (snake-segs=?/c (drop-right segs 1))])]
            [types (curr-mode ne-segs? . modal-> . snake-segs?)])])

(define/ctc-helper ne-segs? (and/c snake-segs? cons?))

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define (cut-tail segs)
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

;; (provide
;;  cut-tail)
