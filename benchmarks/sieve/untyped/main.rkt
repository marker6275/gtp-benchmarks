#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config-dummy.rkt"
         "../../../ctcs/common.rkt"
         "streams.rkt"
         math/number-theory)

;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define/contract (count-from n)
  (configurable-ctc
   [max (->i ([n number?])
             [result (n)
                     (stream/dc* (and/c number? (=/c n))
                                 (λ (last)
                                   (and/c number? (=/c (add1 last)))))])]
   [types (-> number? (streamof number?))])
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define/contract (sift n st)
  (configurable-ctc
   [max (->i ([n integer?]
              [st (streamof number?)])
             [result (n)
                     (streamof (and/c number?
                                      (not/c (=/c n))))])]
   [types (-> integer? (streamof number?) (streamof number?))])
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define/contract (sieve st)
  (configurable-ctc
   [max (->i ([st (streamof integer?)])
             [result (streamof (and/c integer? prime?))])]
   [types (-> (streamof integer?) (streamof integer?))])
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(define primes (sieve (count-from 2)))

(define N-1 100)

(define (main)
  (void (stream-get primes N-1)))

(time (main))
