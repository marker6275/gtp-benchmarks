#lang racket
;; implements the model for the T path finder 

;; (provide 
;;  ;; type MBTA% = 
;;  ;; (Class mbta% 
;;  ;;        [find-path (-> Station Station [Listof Path])] 
;;  ;;        [render (-> [Setof Station] String)
;;  ;;        [station?  (-> String Boolean)]
;;  ;;        [station   (-> String (U Station [Listof Station])])
;;  ;; type Path  = [Listof [List Station [Setof Line]]]
;;  ;; interpretation: take the specified lines to the next station from here 
;;  ;; type Station = String
;;  ;; type Line is one of: 
;;  ;; -- E
;;  ;; -- D 
;;  ;; -- C
;;  ;; -- B 
;;  ;; -- Mattapan
;;  ;; -- Braintree
;;  ;; -- orange
;;  ;; -- blue 
;;  ;; as Strings
 
;;  ;; ->* [instance-of MBTA%]
;;  ;; read the specification of the T map from file and construct an object that can
;;  ;; -- convert a string to a (list of) station(s) 
;;  ;; -- find a path from one station to another
;;  read-t-graph
;;  mbta%)

;; ===================================================================================================
(require "../base/my-graph.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "../../../ctcs/configurable.rkt"
         "data.rkt"
         "helpers.rkt"
         racket/contract
         modalc
         "../../curr-mode.rkt")

(provide/configurable-contract
 [unweighted-graph/directed* ([max (modal/c curr-mode ((listof (list/c any/c any/c)) any/c))]
                              [types (curr-mode (listof (list/c any/c any/c)) . modal-> . any)])]
 [attach-edge-property* ([max (curr-mode
                               [graph?]
                               [#:init any/c
                                #:for-each any/c]
                               . modal->* .
                               any)]
                         [types (curr-mode
                                 [graph?]
                                 [#:init any/c
                                  #:for-each any/c]
                                 . modal->* .
                                 any)])]
 [in-neighbors* ([max (curr-mode graph? any/c . modal-> . any)]
                 [types (curr-mode graph? any/c . modal-> . any)])]
 [SOURCE-DIRECTORY ([max (modal/c curr-mode (λ (res)
                           (string=? "../base/~a.dat" res)))]
                    #;[max/sub1 (and/c string?
                                       (λ (s)
                                         (let ([split (string-split s ".")])
                                           (string=? "dat"
                                                     (list-ref split (- (length split) 1))))))]                                           
                    [types (modal/c curr-mode string?)])]
 [COLORS ([max (modal/c curr-mode (and/c (listof color?)
                      (λ (lst)
                        (andmap (λ (color-file)
                                  (file-exists? (format SOURCE-DIRECTORY color-file)))
                                lst))))]
          #;[max/sub1 (listof color?)]
          [types (modal/c curr-mode (listof string?))])]
 [line-specification? ([max (modal->i curr-mode ([s string?])
                                 [result (s)
                                         (λ (res)
                                           (if res
                                               (and (andmap line? res)
                                                    (andmap (λ (l) (substring? l s)) res))
                                               (not (substring? "--" s))))])]
                       #;[max/sub1 (->i ([s string?])
                                        [result (s)
                                                (λ (res)
                                                  (if res
                                                      (< (length res) (string-length s))
                                                      (not (substring? "-- " s))))])]                      
                       [types (modal-> curr-mode string? (or/c boolean? (listof string?)))])]
 [read-t-graph (;; not the strongest contract I can think of here
                ;; maybe you could check if find-path returns all valid paths
                [max (modal/c curr-mode (object/c
                          (render (->m (set/c string?) string?))
                          (station? (->m string? boolean?))
                          (station (->m string? (or/c station? (listof station?))))
                          (find-path (->m station? station?
                                          (listof (listof (list/c station? (set/c line?))))))))]
                #;[max/sub1 (-> (object/c
                                 (render (->m (set/c string?) string?))
                                 (station? (->m string? boolean?))
                                 (station (->m string? (or/c station? (listof station?))))
                                 (find-path (->m station? station?
                                                 (listof (listof (list/c station? (set/c line?))))))))]
                [types (modal/c curr-mode (object/c
                            (render (->m (set/c string?) string?))
                            (station? (->m string? boolean?))
                            (station (->m string? (or/c string? (listof string?))))
                            (find-path (->m string? string?
                                            (listof (listof (list/c string? (set/c string?))))))))])]
 [read-t-line-from-file ([max (modal->i curr-mode ([lf (λ (lf) (color? lf))])
                                   [result (lf)
                                           (λ (res)
                                             (andmap (λ (pair)
                                                       (and (line? (first pair))
                                                            (= (remainder (length (second pair)) 2) 0)
                                                            (check-station-pairs? (second pair))))
                                                     res))])]
                         #;[max/sub1 (->i ([lf (λ (lf) (color? lf))])
                                          [result (lf)
                                                  (λ (res)
                                                    (andmap (λ (pair)
                                                              (and (line? (first pair))
                                                                   (= (remainder (length (second pair)) 2) 0)))
                                                            res))])]                 
                         [types (modal-> curr-mode string?
                                    (listof (list/c string?
                                                    (listof (list/c string? string?)))))])]
 [lines->hash ([max (modal->i curr-mode ([lines (listof string?)])
                         [result (lines)
                                 ;; ll: checked 4x per unique line
                                 (λ (h)
                                   (and (andmap line? (hash-keys h))
                                        (andmap (cons/c string? (listof (list/c station? station?)))
                                                (hash-values h))
                                        (= (remainder (length (rest (first (hash-values h)))) 2) 0)
                                        (check-station-pairs? (rest (first (hash-values h))))))])]
               #;[max/sub1 (->i ([lines (listof string?)])
                                [result (lines)
                                        (λ (h)
                                          (and (andmap line? (hash-keys h))
                                               (andmap (cons/c string? (listof (list/c station? station?)))
                                                       (hash-values h))
                                               (= (remainder (length (rest (first (hash-values h)))) 2) 0)))])]

               [types (modal-> curr-mode (listof string?)
                          (hash/c string?
                                  (cons/c string?
                                          (listof (list/c string? string?)))))])]
 [mbta% ([max
          (class/c
           (render (modal/c curr-mode (->m (set/c string?) string?)))
           (station? (modal/c curr-mode (->m string? boolean?)))
           (station (modal/c curr-mode (->m string? (or/c station? (listof station?)))))
           (find-path (modal/c curr-mode (->m station? station?
                           (listof (listof (list/c station? (set/c line?)))))))
           (field [G (modal/c curr-mode graph?)]
                  [stations (modal/c curr-mode (listof station?))]
                  [connection-on (modal-> curr-mode station? station? (set/c line?))]
                  [bundles (modal/c curr-mode (and/c (listof (list/c color? (set/c line?)))
                                  (λ (res)
                                    (andmap (λ (lst)
                                              (lines-in-color-file?
                                               (set->list(second lst))
                                               (file->lines (format SOURCE-DIRECTORY (first lst)))))
                                            res))))]))]
         #;[max/sub1
            (class/c
             (render (->m (set/c string?) string?))
             (station? (->m string? boolean?))
             (station (->m string? (or/c station? (listof station?))))
             (find-path (->m station? station?
                             (listof (listof (list/c station? (set/c line?))))))
             (field [G graph?]
                    [stations (listof station?)]
                    [connection-on (-> station? station? (set/c line?))]
                    [bundles (listof (list/c color? (set/c line?)))]))]
         [types
          (class/c
           (render (modal/c curr-mode (->m (set/c string?) string?)))
           (station? (modal/c curr-mode (->m string? boolean?)))
           (station (modal/c curr-mode (->m string? (or/c string? (listof string?)))))
           (find-path (modal/c curr-mode (->m string? string?
                                              (listof (listof (list/c string? (set/c string?)))))))
           (field [G (modal/c curr-mode graph?)]
                  [stations (modal/c curr-mode (listof station?))]
                  [connection-on (modal-> curr-mode string? string? (set/c string?))]
                  [bundles (modal/c curr-mode (listof (list/c string? (set/c string?))))]))])])

(define unweighted-graph/directed*
  unweighted-graph/directed)
(define attach-edge-property*
  attach-edge-property)
(define in-neighbors*
  in-neighbors)




;; type Lines       = [Listof [List String Connections]]
;; type Connections = [Listof Connection]
;; type Connection  = [List Station Station]

(define SOURCE-DIRECTORY
  "../base/~a.dat")

(define COLORS
  '("blue" "orange" "green" "red"))

;; ---------------------------------------------------------------------------------------------------
;; String -> [Maybe [Listof String]]
(define (line-specification? line)
  (define r (regexp-match #px"--* (.*)" line))
  (and r (string-split (second r))))

#| ASSUMPTIONS about source files:
   A data file has the following format: 
   LineSpecification 
   [Station
    | 
    LineSpecification
    ]* 
   A LineSpecification consists of dashes followed by the name of lines, separated by blank spaces. 
   A Station is the string consisting of an entire line, minus surrounding blank spaces. 
|#

;; ---------------------------------------------------------------------------------------------------
(define (read-t-graph)
  (define-values (all-lines bundles)
    (for/fold ((all-lines '()) (all-bundles '())) ((color COLORS))
      (define next (read-t-line-from-file color))
      (values (append next all-lines) (cons (list color (apply set (map first next))) all-bundles))))
  
  (define connections (apply append (map second all-lines)))
  (define stations (set-map (for/fold ((s* (set))) ((c connections)) (set-add s* (first c))) values))
  
  (define graph (unweighted-graph/directed* connections))
  (define-values (connection-on _  connection-on-set!) (attach-edge-property* graph #:init (set)))
  (for ((line (in-list all-lines)))
    (define name (first line))
    (define connections* (second line))
    (for ((c connections*))
      (define from (first c))
      (define to (second c))
      (connection-on-set! from to (set-add (connection-on from to) name))))
  
  (new mbta% [G graph][stations stations][bundles bundles][connection-on connection-on]))

;; ---------------------------------------------------------------------------------------------------
;; String[name of line ~ stem of filename] -> Lines
(define (read-t-line-from-file line-file)
  (define full-path (format SOURCE-DIRECTORY line-file))
  (for/list ([(name line) (in-hash (lines->hash (file->lines full-path)))])
    (list name (rest line))))

;; ---------------------------------------------------------------------------------------------------
;; [Listof String] -> [Hashof String [Cons String [Listof Connections]]]
(define (lines->hash lines0)
  (define names0 (line-specification? (first lines0)))
  (define pred0  (second lines0))
  (define Hlines0 (make-immutable-hash (for/list ([name names0]) (cons name (cons pred0 '())))))
  (let read-t-line ([lines (cddr lines0)][names names0][Hlines Hlines0])
    (cond
      [(empty? lines) Hlines]
      [else 
       (define current-stop (string-trim (first lines)))
       (cond
         [(line-specification? current-stop) 
          => 
          (lambda (names) (read-t-line (rest lines) names Hlines))]
         [else 
          (define new-connections
            (for/fold ([Hlines1 Hlines]) ([name (in-list names)])
              (define line (hash-ref Hlines1 name))
              (define predecessor (first line))
              (define connections 
                (list* (list predecessor current-stop)
                       (list current-stop predecessor)
                       (rest line)))
              (hash-set  Hlines1 name (cons current-stop connections))))
          (read-t-line (rest lines) names new-connections)])])))

;; ---------------------------------------------------------------------------------------------------
(define mbta%
  (class object% 
    (init-field
     ;; Graph 
     G
     ;; [Listof Station]
     stations
     ;; [Station Station -> Line]
     connection-on 
     ;; [Listof [List String [Setof Line]]]
     bundles)
    
    (super-new)
    
    (define/public (render b)
      (define r (memf (lambda (c) (subset? (second c) b)) bundles))
      (if r (first (first r)) (string-join (set-map b values) " ")))

    (define/public (station word)
      (define word# (regexp-quote word))
      (define candidates
        (for/list ([s stations] #:when (regexp-match word# s))
          s))
      (if (and (cons? candidates) (empty? (rest candidates)))
          (first candidates)
          candidates))
    
    (define/public (station? s)
      (cons? (member s stations)))
    
    (define/public (find-path from0 to)
      (define paths* (find-path/aux from0 to))
      (for/list ((path paths*))
        (define start (first path))
        (cond
          [(empty? (rest path)) (list start (set))]
          [else             
           (define next (connection-on start (second path)))
           (define-values (_ result)
             (for/fold ([predecessor start][r (list (list start next))]) ((station (rest path)))
               (values station (cons (list station (connection-on station predecessor)) r))))
           (reverse result)])))
    
    ;; Node Node -> [Listof Path]
    (define/private (find-path/aux from0 to)
      (let search ([from from0][visited '()])
        (cond
          [(equal? from to) (list (list from))]
          [(member from visited) (list)]
          [else
           (define visited* (cons from visited))
           (for/fold ((all-paths '())) ([n (in-neighbors* G from)])
             (define paths-from-from-to-to
               (map (lambda (p) (cons from p)) (search n visited*)))
             (append all-paths paths-from-from-to-to))])))))
