#lang flow-trace

(require racket/contract
         "../../../ctcs/precision-config-dummy.rkt"
         "../../../ctcs/common.rkt"
         "streams.rkt")

;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define/contract (count-from n)
  (configurable-ctc
   [max (->i ([n number?])
             [result (n)
                     (simple-stream/dc* (and/c number? (=/c n))
                                        (λ (last)
                                          (and/c number? (=/c (add1 last)))))])]
   [types (-> number? (simple-streamof number?))])
  (make-simple-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new simple-stream.
(define/contract (sift n st)
  (configurable-ctc
   [max (->i ([n integer?]
              [st (simple-streamof number?)])
             [result (n)
                     (simple-streamof (and/c number?
                                             (not/c (=/c n))))])]
   [types (-> integer? (simple-streamof number?) (simple-streamof number?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-simple-stream hd (lambda () (sift n tl)))]))

(define/ctc-helper prime? (let () (local-require math/number-theory) prime?))

;; `sieve st` Sieve of Eratosthenes
(define/contract (sieve st)
  (configurable-ctc
   [max (->i ([st (simple-streamof integer?)])
             [result (simple-streamof (and/c integer? prime?))])]
   [types (-> (simple-streamof integer?) (simple-streamof integer?))])
  (define-values (hd tl) (simple-stream-unfold st))
  (make-simple-stream hd (lambda () (sieve (sift hd tl)))))

;; simple-stream of prime numbers
(define primes (sieve (count-from 2)))

(define N-1 20)

(define (main)
  (void (simple-stream-get primes N-1)))

(time (main))
