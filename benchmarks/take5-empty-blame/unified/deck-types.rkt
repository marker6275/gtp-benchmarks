(module deck-types typed/racket/shallow
  (#%module-begin
   (provide BaseDeck% BaseDeck PlayerDeck% PlayerDeck DealerDeck% DealerDeck Deck% Deck)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            typed/racket/class
            "basics-types.rkt"
            "card-adapted.rkt"
            "stack-types.rkt")
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (define-type
    BaseDeck%
    (Class
     (init-field (cards0 (Listof Card)))
     (field (my-stacks (Listof Stack)))
     (get-field:cards0 (-> (Listof Card)))
     (get-field:my-stacks (-> (Listof Stack)))
     (set-field:cards0 (-> (Listof Card) Void))
     (set-field:my-stacks (-> (Listof Stack) Void))))
   (define-type PlayerDeck% (Class #:implements/inits BaseDeck% (fewest-bulls (-> Stack))))
   (define-type
    DealerDeck%
    (Class
     #:implements/inits
     BaseDeck%
     (fit (-> Card Stack))
     (push (-> Card Void))
     (replace (-> Stack Card Natural))
     (replace-stack (-> Card (U Card (Listof Card)) Natural))
     (larger-than-some-top-of-stacks? (-> Card Boolean))))
   (define-type
    Deck%
    (Class
     #:implements/inits
     BaseDeck%
     (fewest-bulls (-> Stack))
     (fit (-> Card Stack))
     (push (-> Card Void))
     (replace (-> Stack Card Natural))
     (replace-stack (-> Card (U Card (Listof Card)) Natural))
     (larger-than-some-top-of-stacks? (-> Card Boolean))))
   (define-type BaseDeck (Instance BaseDeck%))
   (define-type PlayerDeck (Instance PlayerDeck%))
   (define-type DealerDeck (Instance DealerDeck%))
   (define-type Deck (Instance Deck%))))
