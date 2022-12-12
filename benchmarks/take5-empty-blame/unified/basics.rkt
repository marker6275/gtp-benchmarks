(module basics typed/racket/shallow
  (#%module-begin
   (provide FACE HAND SIXTYSIX STACKS FIVE MAX-BULL MIN-BULL configuration)
   (require "../../../utilities/require-typed-check-provide-transient.rkt" "basics-types.rkt")
   (define FACE : Natural 104)
   (define HAND : Natural 10)
   (define SIXTYSIX : Natural 66)
   (define STACKS : Natural 4)
   (define FIVE : Natural 5)
   (define MAX-BULL : Bulls 7)
   (define MIN-BULL : Bulls 2)
   (: configuration (-> (Listof (List Symbol Natural))))
   (define (configuration) `((FACE ,FACE) (HAND ,HAND) (SIXTSIX ,SIXTYSIX) (STACKS ,STACKS) (FIVE ,FIVE) (MAX_BULL ,MAX-BULL) (MIN_BULL ,MIN-BULL)))))
