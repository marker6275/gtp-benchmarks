#lang racket

(provide curr-mode
         evens
         flip-coin)

(require modalc)

(define curr-mode
  (mode:once-every 3))

(define (evens n)
  #;(printf "n: ~a, ~a\n" (first n) (string? (first n)))
  (define val (first n))
  (cond [(string? val) #f]
        [(even? val) #t]
        [else #f])
  #;(if (string? val)
      mode:never
      (if (even? val)
          mode:always
          mode:never)))

(define coin
  (random 100))

(define (flip-coin n)
  (define coin (random 101))
  (printf "coin: ~a, check: ~a\n" coin (< coin 50))
  (< coin 50))

#;(println flip-coin)

#;(define flip-coin
  (= coin 0))

#;(define curr-mode
  (mode:first 3))

 