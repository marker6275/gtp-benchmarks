(module type-interface typed/racket/shallow
  (#%module-begin
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (reprovide
    (except-in "typed-data.rkt" Indexes In-Indexes Weighted-Signal Drum-Symbol Pattern))
   (provide Indexes In-Indexes Weighted-Signal Drum-Symbol Pattern)
   (define-type Indexes (Vectorof Integer))
   (define-type In-Indexes Indexes)
   (define-type Weighted-Signal (List Array Real))
   (define-type Drum-Symbol (U 'O 'X #f))
   (define-type Pattern (Listof Drum-Symbol))
   (struct
    Array
    ((shape : (Vectorof Integer))
     (size : Integer)
     (strict? : (Boxof Boolean))
     (strict! : (-> Void))
     (unsafe-proc : (-> (Vectorof Integer) Float)))
    #:prefab)
   (struct Settable-Array Array ((set-proc : (-> (Vectorof Integer) Float Void))) #:prefab)
   (struct Mutable-Array Settable-Array ((data : (Vectorof Float))) #:prefab)
   (provide (struct-out Array) (struct-out Settable-Array) (struct-out Mutable-Array))
   (require/typed/check/provide
    "array-broadcast.rkt"
    (array-broadcasting (Parameterof (U #f #t 'permissive)))
    (array-broadcast (-> Array Indexes Array))
    (array-shape-broadcast
     (case->
      (-> (Listof Indexes) Indexes)
      (-> (Listof Indexes) (U #f #t 'permissive) Indexes))))
   (require/typed/check/provide
    "array-struct.rkt"
    (array? (-> Any Boolean))
    (array-default-strict! (-> Array Void))
    (array-shape (-> Array Indexes))
    (array-size (-> Array Integer))
    (unsafe-array-proc (-> Array (-> Indexes Float)))
    (unsafe-build-array (-> Indexes (-> Indexes Float) Array))
    (array-strictness (Parameterof (U #f #t)))
    (build-array (-> (Vectorof Integer) (-> (Vectorof Integer) Float) Array))
    (make-array (-> (Vectorof Integer) Float Array))
    (unsafe-vector->array (-> (Vectorof Float) Indexes Mutable-Array)))
   (require/typed/check/provide
    "array-transform.rkt"
    (array-append* (-> (Listof Array) Array)))
   (require/typed/check/provide
    "array-utils.rkt"
    (array-shape-size (-> Indexes Integer))
    (check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes))
    (next-indexes! (-> Indexes Integer Indexes Void)))))
