#lang racket/base

(require
 racket/list
 racket/contract
  "structs.rkt"
  "benv.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
)

;; ---

(provide
  time-zero
  k
  tick
  alloc
)

;; =============================================================================

(define/ctc-helper natural? exact-nonnegative-integer?)

;; ---
;(define-type Value Closure)

(define/ctc-helper ((length-is/c compare) n)
  ;; ll: we only implemented up to one level of currying :(
  (λ (l) (compare (length l) n)))
(define/ctc-helper length<=/c (length-is/c <=))
(define/ctc-helper length=/c (length-is/c =))
(define/ctc-helper ((prefix-of/c l) pref)
  (list-prefix? pref l))

;(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define/contract (take* l n)
  (configurable-ctc
   [max (->i ([l (listof any/c)]
              [n natural?])
             [result (l n)
                     (and/c (listof any/c)
                            (length<=/c n)
                            (prefix-of/c l))])]
   [types ((listof any/c) natural? . -> . (listof any/c))])
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

;(: time-zero Time)
(define/contract time-zero
  (listof Time?)
  '())

;(: k (Parameterof Natural))
(define/contract k
  (configurable-ctc
   [max (parameter/c (and/c natural?
                            (=/c 1)))]
   [types (parameter/c natural?)])
  (make-parameter 1))

;(: tick (-> Stx Time Time))
(define/contract (tick call time)
  (configurable-ctc
   [max (->i ([call Stx-type?]
              [time Time?])
             [result (call time)
                     (and/c Time?
                            (length=/c (k))
                            (prefix-of/c (cons (Stx-label call) time)))])]
   [types (Stx-type? Time? . -> . Time?)])

  (define label (Stx-label call))
  (take* (cons label time) (k)))

;(: alloc (-> Time (-> Var Addr)))
(define/contract ((alloc time) var)
  (configurable-ctc
   [max (->i ([time Time?])
             [result (time)
                     (->i ([var Var?])
                          [result (var)
                                  (and/c Binding-type?
                                         (Binding/c (equal?/c var)
                                                    (equal?/c time)))])])]
   [types (Time? . -> . (Var? . -> . Binding-type?))])
  (Binding var time))

