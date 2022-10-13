#lang typed/racket/base

;; Abstract Interpretation

(require
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
  "denotable-adapted.rkt"
  racket/set
  (only-in racket/match match-define)
)

(struct Stx
  ([label : Symbol])
  #:prefab)

(struct exp Stx ()
  #:prefab)

(struct Ref exp
 ([var : Symbol])
  #:prefab)

(struct Lam exp
 ([formals : (Listof Symbol)]
  [call : (U exp Ref Lam Call)])
  #:prefab)

(struct Call Stx
 ([fun : (U exp Ref Lam Call)]
  [args : (Listof (U exp Ref Lam Call))])
  #:prefab)

(struct Closure
 ([lam : Lam]
  [benv : BEnv])
  #:prefab)

(struct Binding
 ([var : Var]
  [time : Time])
  #:prefab)

(struct State
 ([call : Exp]
  [benv : BEnv]
  [store : Store]
  [time : Time])
  #:prefab)

;; ---

(provide
  atom-eval
  next
  explore
)

;; =============================================================================

(: atom-eval (-> BEnv Store (-> Exp Denotable)))
(define ((atom-eval benv store) id)
  (cond
    [(Ref? id)
     (store-lookup store (benv-lookup benv (Ref-var (cast id Ref))))]
    [(Lam? id)
     (set (Closure (cast id Lam) benv))]
    [else
     (error "atom-eval got a plain Exp")]))

(: next (-> State (Setof State)))
(define (next st)
  (match-define (State c benv store time) st)
  (cond
    [(Call? c)
     (define time* (tick c time))
     (match-define (Call _ f args) c)
     (: procs Denotable)
     (define procs ((atom-eval benv store) f))
     (: params (Listof Denotable))
     (define params (map (atom-eval benv store) args))
     (: new-states (Listof State))
     (define new-states
       (for/list ([proc : Value (in-set procs)])
         (match-define (Closure lam benv*) proc)
         (match-define (Lam _ formals call*) lam)
         (define bindings (map (alloc time*) formals))
         (define benv** (benv-extend* benv* formals bindings))
         (define store* (store-update* store bindings params))
         (State call* benv** store* time*)))
     (list->set new-states)]
    [else (set)]))

;; -- state space exploration

(: explore (-> (Setof State) (Listof State) (Setof State)))
(define (explore seen todo)
  (cond
    [(eq? '() todo)
     ;; Nothing left to do
     seen]
    [(set-member? seen (car todo))
     ;; Already seen current todo, move along
     (explore seen (cdr todo))]
    [else
      (define st0 (car todo))
      (: succs (Setof State))
      (define succs (next st0))
      (explore (set-add seen st0)
               (append (set->list succs) (cdr todo)))]))

