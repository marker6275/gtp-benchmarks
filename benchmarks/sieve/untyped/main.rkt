#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt"
         "streams.rkt")

;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define/contract (count-from n)
  (configurable-ctc
   [types (-> number? stream?)]
   [max (-> number? stream?)])
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define/contract (sift n st)
  (configurable-ctc
   [types (-> integer? stream? stream?)]
   [max (-> integer? stream? stream?)])
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define/contract (sieve st)
  (configurable-ctc
   [types (-> stream? stream?)]
   [max (-> stream? stream?)])
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(define primes (sieve (count-from 2)))

(define N-1 6666)

(define (main)
  (void (stream-get primes N-1)))

(time (main))
