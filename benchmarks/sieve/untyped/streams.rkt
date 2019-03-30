#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config-dummy.rkt"
         "../../../ctcs/common.rkt")



;; Simple streams library.
;; For building and using infinite lists.

(provide (struct-out stream)
         make-stream
         stream-unfold
         stream-get
         stream-take

         stream/c
         streamof
         stream/dc
         stream/dc*)

;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct stream (first rest) #:transparent)


(define/ctc-helper (stream/c first/c rest/c)
  (struct/c stream first/c rest/c))

(define/ctc-helper (streamof el/c)
  (stream/c el/c (-> (recursive-contract (streamof el/c) #:chaperone))))

;; elem-contract? (elem -> elem-contract?) -> stream-contract?
(define/ctc-helper (stream/dc* first/c next/c-maker)
  (stream/dc first/c
             (λ (first) (-> (stream/dc* (next/c-maker first) next/c-maker)))))

;; elem-contract? (elem -> stream-contract?) -> stream-contract?
(define/ctc-helper (stream/dc first/c make-rest/c)
  (define first/c-proj (get/build-late-neg-projection first/c))
  (make-contract
   #:name 'stream/dc
   #:late-neg-projection
   (λ (blame)
     (λ (val neg-party)
       (unless (stream? val)
         (raise-blame-error
            blame #:missing-party neg-party
            val
            '(expected "a stream" given: "~e")
            val))

       (define first/checked ((first/c-proj blame) (stream-first val) neg-party))
       (define rest/c (make-rest/c first/checked))
       (define rest/c-proj (get/build-late-neg-projection rest/c))
       (stream first/checked
               ((rest/c-proj blame) (stream-rest val) neg-party))))))

;;--------------------------------------------------------------------------------------------------

(define/contract (make-stream hd thunk)
  (configurable-ctc
   [max (->i ([hd any/c]
              [thunk (-> stream?)])
             [result (hd thunk)
                     (stream/c (equal?/c hd) (equal?/c thunk))])]
   [types (-> any/c (-> stream?) stream?)])
  (stream hd thunk))

;; `stream-unfold st` Destruct a stream `st` into its first value and the new stream produced by de-thunking the tail
(define/contract (stream-unfold st)
  (configurable-ctc
   [max (->i ([st stream?])
             (values [r1 (st) (equal?/c (stream-first st))]
                     [r2 stream?]))]
   [types (-> stream? (values any/c stream?))])
  (values (stream-first st) ((stream-rest st))))

;; `stream-get st i` Get the `i`-th element from the stream `st`
(define/contract (stream-get st i)
  (configurable-ctc
   [max (->i ([st stream?]
              [i exact-nonnegative-integer?])
             [result (st i)
                     (equal?/c (for/fold ([current-st st]
                                          #:result (stream-first current-st))
                                         ([_ (in-range i)])
                                 ((stream-rest current-st))))])]
   [types (-> stream? exact-nonnegative-integer? any/c)])
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; `stream-take st n` Collect the first `n` elements of the stream `st`.
(define/contract (stream-take st n)
  (configurable-ctc
   [max (->i ([st stream?]
              [n exact-nonnegative-integer?])
             [result (st n)
                     (and/c list?
                            (equal?/c
                             (for/fold ([lst '()]
                                        [current-st st]
                                        #:result (reverse lst))
                                       ([_ (in-range n)])
                               (values (cons (stream-first current-st) lst)
                                       ((stream-rest current-st))))))])]
   [types (-> stream? exact-nonnegative-integer? (listof any/c))])
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
