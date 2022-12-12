(module player racket/base
  (#%module-begin
   (provide create-player)
   (require "type-interface.rkt" racket/class (only-in racket/list first rest))
   (define (create-player i (order order:sort-by-face)) (new player% (n i) (order order)))
   (define (order:sort-by-face loc) (sort loc > #:key card-face))))
