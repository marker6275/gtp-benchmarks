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
  modalc
  "../../curr-mode.rkt"
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
        (curr-mode any/c (any/c . -> . boolean?) . modal-> . any/c)]
   [types (curr-mode any/c (any/c . -> . boolean?) . modal-> . any/c)])]
 [defn-command ([max (modal/c curr-mode (command%?-with-exec
                                         (args E S v)
                                         [result (match v
                                                   [(cons (or ': 'define)
                                                          (cons w defn*-any))
                                                    (cons/c (cons/c command%? (equal?/c E))
                                                            (equal?/c S))]
                                                   [_ #f])]))]
                [types (modal/c curr-mode command%?)])]
 [forth-eval* (;; lltodo: not sure if this can (reasonably) be more precise?
   [max (curr-mode (listof string?) . modal-> . (values env? stack?))]
   [types (curr-mode (listof string?) . modal-> . (values env? stack?))])]
 [forth-eval ([max (modal->i curr-mode ([E env?]
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
              [types (curr-mode env? stack? token*? . modal-> . (values (or-#f/c env?) stack?))])]
 [forth-tokenize ([max (modal->i curr-mode ([str string?])
             [result (str) (equal?/c
                            (de-nest
                             (read
                              (open-input-string
                               (string-append "(" str ")")))))])]
   [types (curr-mode string? . modal-> . token*?)])]
 [de-nest ([max (modal->i curr-mode ([v* (listof/any-depth/c token*?)])
             [result (not/c nested-singleton-list?)])]
   [types (curr-mode (or/c list? symbol?) . modal-> . (or/c list? symbol?))])])


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
