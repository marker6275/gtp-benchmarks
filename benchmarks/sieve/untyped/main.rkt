#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config-dummy.rkt"
         "../../../ctcs/common.rkt"
         "streams.rkt")

;;--------------------------------------------------------------------------------------------------

(define (make-stepping-stream/c init/c next)
  (stream/dc init/c
             (λ (init)
               (-> (make-stepping-stream/c (next init) next)))))

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define/contract (count-from n)
  any/c
  #;(configurable-ctc
   [max (->i ([n number?])
             [result (n)
                     (and/c (streamof number?)
                            (make-stepping-stream/c (=/c n) add1))])]
   [types (-> number? stream?)])
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define/contract (sift n st)
  (configurable-ctc
   [max (-> integer? stream? stream?)]
   [types (-> integer? stream? stream?)])
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define/contract (sieve st)
  (configurable-ctc
   [max (-> stream? stream?)]
   [types (-> stream? stream?)])
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
;; (define primes (sieve (count-from 2)))

;; (define N-1 6666)

;; (define (main)
;;   (void (stream-get primes N-1)))

;; (time (main))
