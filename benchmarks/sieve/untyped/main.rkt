#lang racket

(require #;racket/contract
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         (only-in "streams.rkt"
                  simple-stream
                  simple-stream/c
                  simple-streamof
                  simple-stream/dc
                  simple-stream/dc*))
(require/configurable-contract "streams.rkt" simple-stream-take simple-stream-get simple-stream-unfold make-simple-stream )

;;--------------------------------------------------------------------------------------------------

;; ll: All these configurable-ctc forms are commented out (unnecessary atm) because
;; main provides nothing!

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define (count-from n)
  #;(configurable-ctc
   [max (->i ([n number?])
             [result (n)
                     (simple-stream/dc* (and/c number? (=/c n))
                                        (λ (last)
                                          (and/c number? (=/c (add1 last)))))])]
   [types (-> number? (simple-streamof number?))])
  (make-simple-stream n (lambda () (count-from (add1 n)))))

(define/ctc-helper ((divisible-by/c divisor) x)
  (zero? (modulo x divisor)))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new simple-stream.
(define (sift n st)
  #;(configurable-ctc
   [max (->i ([n integer?]
              [st (simple-streamof number?)])
             [result (n)
                     (simple-streamof (and/c number?
                                             (not/c (divisible-by/c n))))])]
   [types (-> integer? (simple-streamof number?) (simple-streamof number?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-simple-stream hd (lambda () (sift n tl)))]))

(define/ctc-helper prime? (let () (local-require math/number-theory) prime?))

(define/ctc-helper (sieved-simple-stream-following/c sieved-n)
  (and/c (simple-streamof (and/c integer? (not/c (divisible-by/c sieved-n))))
         (simple-stream/dc any/c
                           (λ (first)
                             (-> (sieved-simple-stream-following/c first))))))

;; `sieve st` Sieve of Eratosthenes
(define (sieve st)
  #;(configurable-ctc
   [max (->i ([st (simple-streamof integer?)])
             [result (st)
                     (let ([first (simple-stream-first st)])
                       (simple-stream/c (and/c integer? (=/c first))
                                        (-> (sieved-simple-stream-following/c first))))])]
   [types (-> (simple-streamof integer?) (simple-streamof integer?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (make-simple-stream hd (lambda () (sieve (sift hd tl)))))

;; simple-stream of prime numbers
(define primes
  #;(configurable-ctc
   [max (simple-streamof (and/c integer? prime?))]
   [types (simple-streamof integer?)])
  (sieve (count-from 2)))

(define N-1
  #;(configurable-ctc
   [max (and/c natural? (=/c 20))]
   [types natural?])
  20)

(define (main)
  #;(-> void?)
  (void (simple-stream-get primes N-1)))

(time (main))
