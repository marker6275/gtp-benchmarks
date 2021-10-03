#lang typed/racket

(require "../../../utilities/require-typed-check-provide.rkt")

(define-type Indexes (Vectorof Integer))
(define-type In-Indexes Indexes)

(require "typed-data.rkt")
(provide (all-from-out "typed-data.rkt"))
#;(require/typed/check/provide
 "data.rkt"
 [#:struct Array ([shape : Indexes]
                  [size : Integer]
                  [strict? : (Boxof Boolean)]
                  [strict! : (-> Void)]
                  [unsafe-proc : (-> Indexes Float)])]
 [#:struct (Settable-Array Array) ([set-proc : (Indexes Float -> Void)])]
 [#:struct (Mutable-Array Settable-Array) ([data : (Vectorof Float)])])


(require/typed/check/provide
 "array-broadcast.rkt"
 [array-broadcasting (Parameterof (U #f #t 'permissive))]
 [array-broadcast (-> Array Indexes Array)]
 [array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes))])

(require/typed/check/provide
 "array-struct.rkt"
 [array? (-> Any Boolean)]
 [array-strict? (-> Array Boolean)]
 [array-default-strict! (-> Array Void)]
 [array-shape (-> Array Indexes)]
 [array-size (-> Array Integer)]
 [unsafe-array-proc (-> Array (-> Indexes Float))]
 [unsafe-build-array (-> Indexes (-> Indexes Float) Array)]
 [array-strictness (Parameterof (U #f #t))]
 [build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)]
 [make-array ((Vectorof Integer) Float -> Array)]
 [unsafe-build-simple-array (Indexes (Indexes -> Float) -> Array)]
 [unsafe-vector->array (Indexes (Vectorof Float) -> Mutable-Array)])

(require/typed/check/provide
 "array-transform.rkt"
 [array-append* (-> (Listof Array) Array)])

(require/typed/check/provide
 "array-utils.rkt"
 [array-shape-size (Indexes -> Integer)]
 [check-array-shape ((Vectorof Integer) (-> Nothing) -> Indexes)]
 [check-array-shape-size (Symbol Indexes -> Integer)]
 [make-thread-local-indexes (-> Integer (-> Indexes))]
 [next-indexes! (Indexes Integer Indexes -> Void)]
 [unsafe-array-index->value-index (Indexes Indexes -> Integer)]
 [unsafe-vector-insert (-> Indexes Integer Integer Indexes)]
 [unsafe-vector-remove (-> Indexes Integer Indexes)]
 [vector-copy-all (-> Indexes Indexes)])
