#lang racket/base

;; Abstract Interpretation

(require
  "structs.rkt"
  "benv.rkt"
  "time.rkt"
  "denotable.rkt"
  racket/set
  racket/match
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  racket/contract
)

;; ---

(provide
  atom-eval
  next
  explore
)

;; =============================================================================

;(: atom-eval (-> BEnv Store (-> Exp Denotable)))
(define/contract ((atom-eval benv store) id)
  (configurable-ctc
   [max (->i ([benv BEnv?]
              [store Store/c])
             [result (benv store)
                     (->i ([id Exp-type/c])
                          [result Denotable/c]
                          #:post (id result)
                          (match id
                            [(Ref _ var)
                             (=> fail)
                             (match benv
                               [(hash-table ((== var) benv-var) _ ...)
                                (=> fail)
                                (match store
                                  [(hash-table ((== benv-var) res) _ ...)
                                   (equal? result res)]
                                  [_ (fail)])]
                               [_ (fail)])
                             ]
                            [(Ref _ var)
                             (set-empty? result)]
                            [(? Lam?)
                             (equal? result (set (Closure id benv)))]
                            [_ #f]))])]
   [types (BEnv? Store/c . -> . (Exp-type/c . -> . Denotable/c))])
  (cond
    [(Ref? id)
     (store-lookup store (benv-lookup benv (Ref-var id)))]
    [(Lam? id)
     (set (Closure id benv))]
    [else
     (error "atom-eval got a plain Exp")]))

;(: next (-> State (Setof State)))
(define/contract (next st)
  ;; lltodo: I don't think there's any reason to be more specific
  ;; here. It just calls other functions.
  (State-type? . -> . (set/c State-type?))
  (match-define (State c benv store time) st)
  (cond
    [(Call? c)
     (define time* (tick c time))
     (match-define (Call _ f args) c)
     ;(: procs Denotable)
     (define procs ((atom-eval benv store) f))
     ;(: params (Listof Denotable))
     (define params (map (atom-eval benv store) args))
     ;(: new-states (Listof State))
     (define new-states
       (for/list ([proc (in-set procs)])
         (match-define (Closure lam benv*) proc)
         (match-define (Lam _ formals call*) lam)
         (define bindings (map (alloc time*) formals))
         (define benv** (benv-extend* benv* formals bindings))
         (define store* (store-update* store bindings params))
         (State call* benv** store* time*)))
     (list->set new-states)]
    [else (set)]))

;; -- state space exploration

(define/ctc-helper ((subset?/c sub) s)
  (subset? sub s))

;(: explore (-> (Setof State) (Listof State) (Setof State)))
(define/contract (explore seen todo)
  (configurable-ctc
   [max (->i ([seen (set/c State-type?)]
              [todo (listof State-type?)])
             [result (seen todo)
                     (and/c (set/c State-type?)
                            (subset?/c seen)
                            (subset?/c todo))])]
   [types ((set/c State-type?)
           (listof State-type?)
           . -> .
           (set/c State-type?))])
  (cond
    [(eq? '() todo)
     ;; Nothing left to do
     seen]
    [(set-member? seen (car todo))
     ;; Already seen current todo, move along
     (explore seen (cdr todo))]
    [else
      (define st0 (car todo))
      ;(: succs (Setof State))
      (define succs (next st0))
      (explore (set-add seen st0)
               (append (set->list succs) (cdr todo)))]))

