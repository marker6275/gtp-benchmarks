(module mutation-adapter typed/racket/shallow
  (#%module-begin
   (module contracted racket
     (require racket/require
              (path-up "blame-evaluation-gt/mutation-adapter/mutation-adapter.rkt"))
     (require "original-type-interface.rkt")
     (provide (except-out (all-from-out "original-type-interface.rkt") build-array))
     (provide (contract-out
               (build-array (delegating-> 2 (list (cons 1 (delegating-> 1 (list (cons 0 (delegating-vectorof (sealing-adapter)))) (any/c-adapter) (list)))) (any/c-adapter) (list))))))
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (reprovide (except-in "typed-data.rkt" Indexes In-Indexes Weighted-Signal Drum-Symbol Pattern))
   (provide Indexes In-Indexes Weighted-Signal Drum-Symbol Pattern)
   (provide (struct-out Array) (struct-out Settable-Array) (struct-out Mutable-Array))
   (define-type Indexes (Vectorof Integer))
   (define-type In-Indexes Indexes)
   (define-type Weighted-Signal (List Array Real))
   (define-type Drum-Symbol (U 'O 'X #f))
   (define-type Pattern (Listof Drum-Symbol))
   (struct Array ((shape : (Vectorof Integer)) (size : Integer) (strict? : (Boxof Boolean)) (strict! : (-> Void)) (unsafe-proc : (-> (Vectorof Integer) Float))) #:prefab)
   (struct Settable-Array Array ((set-proc : (-> (Vectorof Integer) Float Void))) #:prefab)
   (struct Mutable-Array Settable-Array ((data : (Vectorof Float))) #:prefab)
   (require/typed/check/provide
    'contracted
    (array-broadcasting (Parameterof (U #f #t 'permissive)))
    (array-broadcast (-> Array Indexes Array))
    (array-shape-broadcast (case-> (-> (Listof Indexes) Indexes) (-> (Listof Indexes) (U #f #t 'permissive) Indexes))))
   (require/typed/check/provide
    'contracted
    (array? (-> Any Boolean))
    (array-default-strict! (-> Array Void))
    (array-shape (-> Array Indexes))
    (array-size (-> Array Integer))
    (unsafe-array-proc (-> Array (-> Indexes Float)))
    (unsafe-build-array (-> Indexes (-> Indexes Float) Array))
    (array-strictness (Parameterof (U #f #t)))
    (build-array (-> (Vectorof Integer) (-> (Vectorof Integer) Float) Array))
    (make-array (-> (Vectorof Integer) Float Array))
    (unsafe-vector->array (-> Indexes (Vectorof Float) Mutable-Array)))
   (require/typed/check/provide 'contracted (array-append* (-> (Listof Array) Array)))
   (require/typed/check/provide
    'contracted
    (array-shape-size (-> Indexes Integer))
    (check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes))
    (next-indexes! (-> Indexes Integer Indexes Void)))))
