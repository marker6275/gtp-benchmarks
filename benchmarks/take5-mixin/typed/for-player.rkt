#lang typed/racket/base

(provide for-player)

(require racket/list
         require-typed-check
         typed/racket/class
         "deck-types.rkt"
         "stack-types.rkt")

(require/typed/check "for-dealer.rkt"
                     [for-dealer (-> BaseDeck% DealerDeck%)])

(require/typed/check "stack.rkt"
  (bulls  (-> Stack Natural)))

;; Class[my-stacks field] -> Class[my-stacks field and fewest-bulls method]
(: for-player (-> DealerDeck% Deck%))
(define (for-player deck%)
  (class deck%
    (inherit-field my-stacks)
    (super-new)

    (define/public (fewest-bulls)
      (define stacks-with-bulls : (Listof (List Stack Natural))
        (for/list : (Listof (List Stack Natural))
                  ((s my-stacks))
          (list s (bulls s))))
      (first (argmin (lambda ([l : (List Stack Natural)]) (second l)) stacks-with-bulls)))))

