(module deck typed/racket/shallow
  (#%module-begin
   (provide create-deck)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            typed/racket/class
            "basics-types.rkt"
            "card-adapted.rkt"
            "card-pool-types.rkt"
            "deck-types.rkt"
            "stack-types.rkt"
            racket/list)
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (require/typed/check "basics.rkt" (FACE Natural) (STACKS Natural))
   (require/typed/check "stack.rkt" (bulls (-> Stack Natural)))
   (: create-deck (-> CardPool Deck))
   (define (create-deck card-pool) (define deck% (for-player (for-dealer base-deck%))) (define cards (build-list STACKS (lambda (_) (send card-pool draw-card)))) (new deck% (cards0 cards)))
   (: for-player (-> DealerDeck% Deck%))
   (define (for-player deck%)
     (class deck%
       (inherit-field my-stacks)
       (super-new)
       (define/public
        (fewest-bulls)
        (define stacks-with-bulls : (Listof (List Stack Natural)) (for/list : (Listof (List Stack Natural)) ((s my-stacks)) (list s (bulls s))))
        (first (argmin (lambda ((l : (List Stack Natural))) (second l)) stacks-with-bulls)))))
   (: for-dealer (-> BaseDeck% DealerDeck%))
   (define (for-dealer deck%)
     (class deck%
       (inherit-field cards0)
       (inherit-field my-stacks)
       (super-new)
       (send this set-field:my-stacks (map (lambda ((c : Card)) (list c)) cards0))
       (define/public (fit c) (: distance (-> (Listof Card) Real)) (define (distance stack) (define d (first stack)) (if (>-face c d) (--face c d) (+ FACE 1))) (argmin distance my-stacks))
       (define/public (push c) (define s0 (fit c)) (void (replace-stack (first s0) c)))
       (define/public (replace s c) (replace-stack (first s) (list c)))
       (define/public
        (replace-stack top0 c)
        (define result : Natural 0)
        (set! my-stacks
          (for/list
           :
           (Listof Stack)
           ((s : Stack my-stacks))
           (cond ((equal? (first s) top0) (set! result (bulls s)) (if (cons? c) c (if (null? c) (error 'invalid-input) (cons c s)))) (else s))))
        result)
       (define/public (larger-than-some-top-of-stacks? c) (for/or ((s my-stacks)) (>-face c (first s))))))
   (: base-deck% BaseDeck%)
   (define base-deck%
     (class object%
       (init-field cards0)
       (field (my-stacks '()))
       (define/public (get-field:cards0) cards0)
       (define/public (get-field:my-stacks) my-stacks)
       (define/public (set-field:cards0 v) (set! cards0 v))
       (define/public (set-field:my-stacks v) (set! my-stacks v))
       (super-new)))))
