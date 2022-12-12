(module typed-data typed/racket/shallow
  (#%module-begin
   (provide Indexes In-Indexes Weighted-Signal Drum-Symbol Pattern)
   (struct Array ((shape : (Vectorof Integer)) (size : Integer) (strict? : (Boxof Boolean)) (strict! : (-> Void)) (unsafe-proc : (-> (Vectorof Integer) Float))) #:prefab)
   (struct Settable-Array Array ((set-proc : ((Vectorof Integer) Float -> Void))) #:prefab)
   (struct Mutable-Array Settable-Array ((data : (Vectorof Float))) #:prefab)
   (define-type Indexes (Vectorof Integer))
   (define-type In-Indexes Indexes)
   (define-type Weighted-Signal (List Array Real))
   (define-type Drum-Symbol (U 'O 'X #f))
   (define-type Pattern (Listof Drum-Symbol))))
