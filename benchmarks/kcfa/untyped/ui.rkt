#lang racket

;; User Interface to `ai.rkt`

(require
  require-typed-check
  racket/set
  "structs.rkt"
  "benv.rkt"
  "denotable.rkt"
  "time.rkt"
  (only-in racket/string string-join)
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
)

(require "ai.rkt")
;(require/typed/check "ai.rkt"
;  (atom-eval (-> BEnv Store (-> Exp Denotable)))
;  (next (-> State (Setof State)))
;  (explore (-> (Setof State) (Listof State) (Setof State)))
;)
;; ---

(provide
  summarize
  empty-mono-store
  monovariant-value
  monovariant-store
  analyze
  format-mono-store
)

;; =============================================================================


;; -- ui.rkt
;(define-type MonoStore (HashTable Var (Setof Exp)))
(define/ctc-helper MonoStore/c (hash/c Var?
                                       (set/c Exp-type/c #:kind 'immutable)))

;(: summarize (-> (Setof State) Store))
(define/contract (summarize states)
  (configurable-ctc
   [max (->i ([states (set/c State-type? #:kind 'immutable)])
             [result (states)
                     (equal?/c
                      (foldl store-join
                             empty-store
                             (set-map states State-store)))])]
   [types ((set/c State-type? #:kind 'immutable) . -> . Store/c)])
  (for/fold ([store empty-store])
    ([state (in-set states)])
    (store-join (State-store state) store)))

;(: empty-mono-store MonoStore)
(define/contract empty-mono-store
  MonoStore/c
  (hash))

;(: monovariant-value (-> Value Lam))
(define/contract (monovariant-value v)
  (configurable-ctc
   [max (->i ([v Closure-type/c])
             [result (v) (equal?/c (Closure-lam v))])]
   [types (Closure-type/c . -> . Lam-type/c)])
  (Closure-lam v))

;(: monovariant-store (-> Store MonoStore))
(define/contract (monovariant-store store)
  (configurable-ctc
   [max (->i ([store Store/c])
             [result MonoStore/c]
             #:post (store result)
             (for/and ([(b vs) (in-hash store)])
               (define result-vs (hash-ref result (Binding-var b) set))
               (define mono-vs (list->set (set-map vs monovariant-value)))
               (subset? mono-vs result-vs)))]
   [types (Store/c . -> . MonoStore/c)])
  ;(: update-lam (-> (Setof Value) (-> (Setof Exp) (Setof Exp))))
  (define (update-lam vs)
    ;(: v-vs (Setof Lam))
    (λ (b-vs)
      (define v-vs (list->set (set-map vs monovariant-value)))
      (set-union b-vs v-vs)))
  ;(: default-lam (-> (Setof Exp)))
  (define (default-lam) (set))
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (Binding-var b)
                 (update-lam vs)
                 default-lam)))

;(: analyze (-> Exp MonoStore))
(define/contract (analyze exp)
  (configurable-ctc
   [max (->i ([exp (and/c Exp-type/c closed-term?)])
             ;; lltodo: can be stronger?
             [result MonoStore/c])]
   [types (Exp-type/c . -> . MonoStore/c)])
  (define init-state (State exp empty-benv empty-store time-zero))
  (define states (explore (set) (list init-state)))
  (define summary (summarize states))
  (define mono-store (monovariant-store summary))
  mono-store)

;(: format-mono-store (-> MonoStore String))
(define/contract (format-mono-store ms)
  (MonoStore/c . -> . string?)
  ;(: res (Listof String))
  (define res
    (for/list ([(i vs) (in-hash ms)])
      (format "~a:\n~a"
              i
              (string-join
                (for/list ([v (in-set vs)])
                  (format "\t~S" v))
                "\n"))))
  (string-join res "\n"))

