#lang racket

(require #;racket/contract
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         modalc
         "../../curr-mode.rkt")



;; Simple streams library.
;; For building and using infinite lists.

(provide/configurable-contract
 [make-simple-stream {[max (modal->i curr-mode ([hd any/c]
                                                [thunk (-> simple-stream?)])
                                     [result (hd thunk)
                                             (begin (displayln 'using-max!)
                                                    (simple-stream/c (equal?/c hd) (equal?/c thunk)))])]
                      [types (modal-> curr-mode any/c (-> simple-stream?) simple-stream?)]}]
 [simple-stream-unfold {[max (modal->i curr-mode ([st simple-stream?])
                                       (values [r1 (st) (equal?/c (simple-stream-first st))]
                                               [r2 simple-stream?]))]
                        [types (modal-> curr-mode simple-stream? (values any/c simple-stream?))]}]
 [simple-stream-get {[max (modal->i curr-mode ([st simple-stream?]
                                               [i exact-nonnegative-integer?])
                                    [result (st i)
                                            (equal?/c (for/fold ([current-st st]
                                                                 #:result (simple-stream-first current-st))
                                                                ([_ (in-range i)])
                                                        ((simple-stream-rest current-st))))])]
                     [types (modal-> curr-mode simple-stream? exact-nonnegative-integer? any/c)]}]
 [simple-stream-take {[max (modal->i curr-mode ([st simple-stream?]
                                                [n exact-nonnegative-integer?])
                                     [result (st n)
                                             (and/c list?
                                                    (equal?/c
                                                     (for/fold ([lst '()]
                                                                [current-st st]
                                                                #:result (reverse lst))
                                                               ([_ (in-range n)])
                                                       (values (cons (simple-stream-first current-st) lst)
                                                               ((simple-stream-rest current-st))))))])]
                      [types (modal-> curr-mode simple-stream? exact-nonnegative-integer? (listof any/c))]}])

(provide (struct-out simple-stream)

         simple-stream/c
         simple-streamof
         simple-stream/dc
         simple-stream/dc*)

;; A simple-stream is a cons of a value and a thunk that computes the next value when applied
(struct simple-stream (first rest) #:transparent)


(define/ctc-helper (simple-stream/c first/c rest/c)
  (struct/c simple-stream first/c rest/c))

(define/ctc-helper (simple-streamof el/c)
  (letrec ([this-ctc (simple-stream/c el/c (modal-> curr-mode (recursive-contract this-ctc #:chaperone)))])
    this-ctc))

;; elem-contract? (elem -> elem-contract?) -> simple-stream-contract?
(define/ctc-helper (simple-stream/dc* first/c next/c-maker)
  (simple-stream/dc first/c
                    (λ (first)
                      (modal-> curr-mode (simple-stream/dc* (next/c-maker first)
                                             next/c-maker)))))

;; Custom projections aren't supported
#;(define/ctc-helper (simple-stream/dc first/c make-rest/c)
  (define first/c-proj (get/build-late-neg-projection first/c))
  (make-contract
   #:name 'simple-stream/dc
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (unless (simple-stream? val)
         (raise-blame-error
          blame #:missing-party neg-party
          val
          '(expected "a simple-stream" given: "~e")
          val))

       (define first/checked ((first/c-proj blame) (simple-stream-first val) neg-party))
       (define rest/c (make-rest/c first/checked))
       (define rest/c-proj (get/build-late-neg-projection rest/c))
       (simple-stream first/checked
                      ((rest/c-proj blame) (simple-stream-rest val)
                                           neg-party))))))

;; elem-contract? (elem -> (-> simple-stream-contract?)) -> simple-stream-contract?
(define/ctc-helper (simple-stream/dc first/c make-rest/c)
  (struct/dc simple-stream
             [first first/c]
             [rest (first) (make-rest/c first)]))

;;--------------------------------------------------------------------------------------------------

(define (make-simple-stream hd thunk)
  (simple-stream hd thunk))

;; `simple-stream-unfold st` Destruct a simple-stream `st` into its first value and the new simple-stream produced by de-thunking the tail
(define (simple-stream-unfold st)
  (values (simple-stream-first st) ((simple-stream-rest st))))

;; `simple-stream-get st i` Get the `i`-th element from the simple-stream `st`
(define (simple-stream-get st i)
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= i 0) hd]
        [else    (simple-stream-get tl (sub1 i))]))

;; `simple-stream-take st n` Collect the first `n` elements of the simple-stream `st`.
(define (simple-stream-take st n)
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (simple-stream-unfold st))
              (cons hd (simple-stream-take tl (sub1 n)))]))
