(module player typed/racket/shallow
  (#%module-begin
   (provide create-player)
   (require "../../../utilities/require-typed-check-provide-transient.rkt"
            "type-interface.rkt"
            typed/racket/class)
   (: create-player (->* (Name) ((-> (Listof Card) (Listof Card))) Player))
   (define (create-player i (order order:sort-by-face)) (new player% (n i) (order order)))
   (: order:sort-by-face (-> (Listof Card) (Listof Card)))
   (define (order:sort-by-face loc) ((inst sort Card Natural) loc > #:key card-face))))
