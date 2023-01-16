(module base-player racket/base
  (#%module-begin
   (provide default-order player%)
   (require racket/class (only-in racket/list first rest))
   (define (default-order loc) loc)
   (define player%
     (class object%
       (init-field n (order default-order))
       (field (my-cards '()))
       (define/public (get-field:n) n)
       (define/public (get-field:order) order)
       (define/public (get-field:my-cards) my-cards)
       (define/public (set-field:n v) (set! n v))
       (define/public (set-field:order v) (set! order v))
       (define/public (set-field:my-cards v) (set! my-cards v))
       (define/public (name) n)
       (define/public (start-round loc) (set! my-cards (order loc)))
       (define/public
        (start-turn _d)
        (begin0 (first my-cards) (set! my-cards (rest my-cards))))
       (define/public (choose d) (define fewest-bulls (send d fewest-bulls)) fewest-bulls)
       (super-new)))))
