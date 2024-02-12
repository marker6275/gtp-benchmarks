#lang racket

(require
  (only-in racket/list first permutations)
  (only-in racket/file file->value)
  racket/contract
  (only-in "../../../ctcs/common.rkt"
           memberof/c
           permutationof/c)
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/configurable.rkt"
  modalc
  "../../curr-mode.rkt"
)

(provide/configurable-contract
 [orig ([max (modal/c curr-mode any/c)]
        [types (modal/c curr-mode any/c)])]
 [r* ([max (modal/c curr-mode any/c)]
      [types (modal/c curr-mode any/c)])]
 [reset! ([max (modal/c curr-mode (modal->* curr-mode () ;; add modal here!
                    void?
                    #:post (equal? (unbox r*) orig)))]
          [types (modal-> curr-mode void?)])]
 [random ([max (modal->i curr-mode ([n exact-nonnegative-integer?])
                    [result (n) (and/c exact-nonnegative-integer?
                                       (</c n))])]
          [types (curr-mode any/c . modal-> . exact-nonnegative-integer?)])]
 [article ([max (->* (boolean? boolean?)
                     [#:an? boolean?]
                     (apply or/c (list+titlecases "the" "an" "a")))]
           [types (->* (boolean? boolean?)
                       [#:an? boolean?]
                       string?)])]
 [random-between ([max (modal->i curr-mode ([min exact-nonnegative-integer?]
                             [max (min) (and/c exact-nonnegative-integer?
                                               (>/c min))])
                            [result (min max) (random-result-between/c min max)])]
                  [types (curr-mode exact-nonnegative-integer? exact-nonnegative-integer?
                                                     . modal-> . exact-nonnegative-integer?)])]
 [d6 ([max (-> (random-result-between/c 1 7))]
      [types (-> exact-nonnegative-integer?)])]
 [d20 ([max (-> (random-result-between/c 1 21))]
       [types (-> exact-nonnegative-integer?)])]
 [random-from ([max (modal->i curr-mode ([l (listof any/c)])
                         [result (l) (memberof/c l)])]
               [types (curr-mode (listof any/c) . modal-> . any/c)])]
 [shuffle ([max (modal->i curr-mode ([l (listof any/c)])
                          [result (l) (permutationof/c l)])]
           [types (curr-mode (listof any/c) . modal-> . (listof any/c))])])

(provide
;;   article
;;   random-between
;;   d6
;;   d20
;;   random-from
;;   random
;;   reset!
  random-result-between/c
)

;; =============================================================================

(define orig
  '(2 10 24 3 0 2 10 45 2 2 2 2 49 3 1 5 1 0 0 2 1 0 2 1 0 0 2 2 5 0 0 0 3 0 1 2
      0 3 0 0 2 2 0 2 2 0 0 3 0 0 2 0 3 1 0 2 0 0 1 1 0 2 0 0 3 0 0 1 2 0 3 1 0
      2 0 0 0 1 3 1 1 0 1 2 0 3 2 0 1 2 0 1 1 0 2 2 0 1 1 0 2 2 0 0 0 2 1 0 0 0 
      0 3 4 0 0 2 1 0 2 1 0 3 1 0 1 0 0 1 0 0 1 2 0 1 0 0 2 2 0 2 2 0 3 1 0 1 0 
      0 1 1 0 2 1 0 3 2 0 3 0 0 2 2 0 0 0 3 4 2 0 3 0 0 3 1 0 0 3 0 4 0 0 2 0 0 
      2 2 0 2 1 0 0 0 3 6 1 0 3 0 0 0 2 1 3 0 0 3 1 0 1 1 0 2 0 0 3 2 0 2 1 0 1
      2 0 0 3 0 2 2 0 2 2 0 2 2 0 1 1 0 3 1 0 2 1 0 1 2 0 0 2 0 3 1 0 1 1 0 2 2 
      0 2 2 0 1 5 3 3 2 1))
(define r* (box orig))

(define (reset!)
  (set-box! r* orig))

;; Non-specific ctc because this random stuff is rigged to be deterministic
(define (random n)
  (begin0 (car (unbox r*)) (set-box! r* (cdr (unbox r*)))))

(define/ctc-helper (list+titlecases . los)
  (append los
          (map string-titlecase los)))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))


(define/ctc-helper (random-result-between/c min max)
  (and/c exact-nonnegative-integer?
         (>=/c min)
         (<=/c max)))

(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))

(define (d6)
  (random-between 1 7))

(define (d20)
  (random-between 1 21))

(define (random-from l)
  (first (shuffle l)))

(define (shuffle l)
  (reverse l))
