#lang racket/base

;; Denotable values and stores to hold them.
;; A denotable is a set of values
;; (A value is a closure)

(require
  require-typed-check
  racket/set
  racket/contract
  racket/list
  "structs.rkt"
  "benv.rkt"
  "time.rkt"
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
)

;; -----------------------------------------------------------------------------

(provide
  (struct-out State)
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
)

;; =============================================================================

;; -- private
;(define-type Denotable (Setof Value))
;(define-type Store (HashTable Addr Denotable))

(define/ctc-helper Denotable? (set/c Closure-type? #:kind 'immutable))
(define/ctc-helper Store? (hash/c Addr? Denotable? #:immutable #t))

;; -- structs

(struct State
 (call ;: Exp]
  benv ;: BEnv]
  store ;: Store]
  time ;: Time]))
  )
  #:mutable)

(define/ctc-helper (State/c call/c benv/c store/c time/c)
  (struct/c State call/c benv/c store/c time/c))
(define/ctc-helper State-type? (State/c Stx-type? BEnv? Store? Time?))

;; -- public

;(: d-bot Denotable)
(define/contract d-bot
  Denotable?
  (set))

;(: d-join (-> Denotable Denotable Denotable))
(define/contract d-join
  (configurable-ctc
   [max (->i ([a Denotable?]
              [b Denotable?])
             [result Denotable?]
             #:post (a b result)
             (for/and ([el (in-sequences (in-set a) (in-set b))])
               (set-member? result el)))]
   [types (Denotable? Denotable? . -> . Denotable?)])
  set-union)

;(: empty-store Store)
(define/contract empty-store
  Store?
  (make-immutable-hasheq '()))

;(: store-lookup (-> Store Addr Denotable))
(define/contract (store-lookup s a)
  (configurable-ctc
   [max (->i ([s Store?]
              [a Addr?])
             [result (s a)
                     (equal?/c (if (hash-has-key? s a)
                                   (hash-ref s a)
                                   d-bot))])]
   [types (Store? Addr? . -> . Denotable?)])
  (hash-ref s a (lambda () d-bot)))

;; any/c flat-contract? (hash/c any/c any/c) -> flat-contract?
(define/ctc-helper ((hash-with/c key val-ok?) h)
  (and (hash? h)
       (hash-has-key? h key)
       (val-ok? (hash-ref h key))))

;(: store-update (-> Store Addr Denotable Store))
(define/contract (store-update store addr value)
  (configurable-ctc
   [max (->i ([s Store?]
              [addr Addr?]
              [value Denotable?])
             [result (s addr value)
                     (and/c
                      Store?
                      (hash-with/c addr
                                   (equal?/c
                                    (set-union value
                                               (hash-ref s addr
                                                         (λ _ (set)))))))])]
   [types (Store? Addr? Denotable? . -> . Store?)])
  ;(: update-lam (-> Denotable Denotable))
  (define (update-lam d) (d-join d value))
  (hash-update store addr update-lam (lambda () d-bot)))

;(: store-update* (-> Store (Listof Addr) (Listof Denotable) Store))
(define/contract (store-update* s as vs)
  (configurable-ctc
   [max (->i ([s Store?]
              [as (listof Addr?)]
              [vs (listof Denotable?)])
             [result Store?]
             #:post (s as vs result)
             (for/and ([a (in-list (remove-duplicates as))])
               (define vs<-a (indexes-where as (equal?/c a)))
               (define vs/unioned (apply set-union vs<-a))
               (define v/expected (hash-ref s a (λ _ (set))))
               (and (hash-has-key? result a)
                    (equal? (hash-ref result a) v/expected))))]
   [types (Store? (listof Addr?) (listof Denotable?) . -> . Store?)])
  (for/fold ([store s])
    ([a (in-list as)]
     [v (in-list vs)])
    (store-update store a v)))

;(: store-join (-> Store Store Store))
(define/contract (store-join s1 s2)
  (configurable-ctc
   [max (->i ([s1 Store?]
              [s2 Store?])
             [result Store?]
             #:post (s1 s2 result)
             (for/and ([(k v) (in-hash result)])
               (equal? v (set-union (hash-ref s1 k (λ _ (set)))
                                    (hash-ref s2 k (λ _ (set)))))))])
  (for/fold ([new-store s1])
    ([(k v) (in-hash s2)])
    (store-update new-store k v)))

