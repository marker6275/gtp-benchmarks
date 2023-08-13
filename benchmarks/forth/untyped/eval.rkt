#lang racket

;; (provide forth-eval*)

;; -----------------------------------------------------------------------------

(require
  racket/match
  racket/class
  (only-in racket/port with-input-from-string)
  ;; racket/contract
  "../../../ctcs/configurable.rkt"
  "../../../ctcs/precision-config.rkt"
  (only-in "../../../ctcs/common.rkt"
           command%?-with-exec
           command%?
           stack?
           env?
           equal?/c
           or-#f/c)
  (only-in racket/function
           curry)
  (only-in racket/list first)
)
;; (require (only-in "command.rkt"
;;   CMD*
;;   command%
;; ))
(require/configurable-contract "command.rkt" command% CMD* )
;; (require (only-in "stack.rkt"
;;   stack-init
;; ))
(require/configurable-contract "stack.rkt" stack-init )

(provide/configurable-contract
 [assert ([max #;(parametric->/c [A] (A (A . -> . boolean?) . -> . A))
        (any/c (any/c . -> . boolean?) . -> . any/c)]
   [types (any/c (any/c . -> . boolean?) . -> . any/c)])]
 [defn-command ([max (command%?-with-exec
         (args E S v)
         [result (match v
                   [(cons (or ': 'define)
                          (cons w defn*-any))
                    (cons/c (cons/c command%? (equal?/c E))
                            (equal?/c S))]
                   [_ #f])])]
   [types command%?])]
 [forth-eval* (;; lltodo: not sure if this can (reasonably) be more precise?
   [max ((listof string?) . -> . (values env? stack?))]
   [types ((listof string?) . -> . (values env? stack?))])]
 [forth-eval ([max (->i ([E env?]
              [S stack?]
              [token* token*?])
             ;; ll: as precise as it can get without copying the body exactly
             (values
              [env-result (E)
                          (or/c (or/c #f (equal?/c E))
                                env?)]
              [stack-result (S)
                            (or/c (equal?/c S)
                                  stack?)]))]
   [types (env? stack? token*? . -> . (values (or-#f/c env?) stack?))])]
 [forth-tokenize ([max (->i ([str string?])
             [result (str) (equal?/c
                            (de-nest
                             (read
                              (open-input-string
                               (string-append "(" str ")")))))])]
   [types (string? . -> . token*?)])]
 [de-nest ([max (->i ([v* (listof/any-depth/c token*?)])
             [result (not/c nested-singleton-list?)])]
   [types ((or/c list? symbol?) . -> . (or/c list? symbol?))])])


(define (assert v p)
  (unless (p v) (error 'assert))
  v)

;; =============================================================================

(define defn-command
  (new command%
    (id 'define)
    (descr "Define a new command as a sequence of existing commands")
    (exec (lambda (E S v)
      (match v
       [(cons (or ': 'define) (cons w defn*-any))
        (define defn* (assert defn*-any list?))
        (define cmd
          (new command%
            (id (assert w symbol?))
            (descr (format "~a" defn*))
            (exec (lambda (E S v)
              (if (equal? v (list w))
                  (let-values ([(e+ s+)
                                (for/fold
                                    ([e E] [s S])
                                    ([d (in-list defn*)])
                                  (if e
                                    (forth-eval e s (list d))
                                    (values e s)))])
                    (if e+
                      (cons e+ s+)
                      e+))
                  #f)))))
        (cons (cons cmd E) S)]
       [_ #f])))))

(define (forth-eval* lines)
  (for/fold
            ([e (cons defn-command CMD*)]
             [s (stack-init)])
      ([ln (in-list lines)])
    (define token* (forth-tokenize ln))
    (cond
     [(or (null? token*)
          (not (list? e))) ;; Cheap way to detect EXIT
      (values '() s)]
     [else
      (forth-eval e s token*)])))


(define/ctc-helper ((listof/any-depth/c ctc) v)
  (if (list? v)
      (andmap (listof/any-depth/c ctc) v)
      (ctc v)))

(define/ctc-helper token*? (listof/any-depth/c (or/c symbol? number?)))

(define (forth-eval E S token*)
  ;; Iterates over every cmd in the env trying each one by one until
  ;; one returns a truthy value
  ;; Thus why cmds return #f for invalid input
  (match (for/or
             ([c (in-list E)])
           ((get-field exec c) E S token*))
    ['EXIT
     (values #f S)]
    [#f
     (printf "Unrecognized command '~a'.\n" token*)
     (values E S)]
    [(? pair? E+S)
     (values (car E+S) (cdr E+S))]))

(define (forth-tokenize str)
  (parameterize ([read-case-sensitive #f]) ;; Converts symbols to lowercase
    (with-input-from-string str
      (lambda ()
        (de-nest
         (let loop ()
           (match (read)
             [(? eof-object?) '()]
             [val (cons val (loop))])))))))


(define/ctc-helper (nested-singleton-list? v)
  (and (list? v)
       (= (length v) 1)
       (list? (first v))))

;; Remove all parentheses around a singleton list
(define (de-nest v*)
  (if (and (list? v*)
           (not (null? v*))
           (list? (car v*))
           (null? (cdr v*)))
      (de-nest (car v*))
      v*))
