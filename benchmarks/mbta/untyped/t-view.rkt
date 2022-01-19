#lang racket

;; implement the view (renderer) for the T path finder

(provide 
 ;; type Manage = 
 ;; (Class 
 ;;  ;; disable the given station: #f for sucecss, String for failure
 ;;  [add-to-disabled (-> String [Maybe String]]
 ;;  ;; enable the given station: #f for sucecss, String for failure
 ;;  [remove-from-disabled (-> String [Maybe String]]
 ;;  ;; turn the inquiry strings into stations and find a path from the first to the second  
 ;;  [find (-> String String String)])
 manage%)

;; ===================================================================================================
(require "t-graph.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         "helpers.rkt")

(define/ctc-helper t-graph
  (read-t-graph))

;; ===================================================================================================
;; [X -> Real] [Listof X] -> X
;; argmax also okay 
;; select an [Listof X] that satisfies certain length criteria
(define/contract (selector l)
  (configurable-ctc
   [max (->i ([inp-lst (listof (listof any/c))])
             [result (inp-lst)
                     (λ (out-lst)
                       (and (list? out-lst)
                            (cons? (member out-lst inp-lst))
                            (andmap (lambda (l)
                                      (<= (count-strs-in-lst out-lst)
                                          (count-strs-in-lst l)))
                                    inp-lst)))])]
   [max/sub1 (->i ([inp-lst (listof (listof any/c))])
                  [result (inp-lst)
                          (λ (out-lst)
                            (and (list? out-lst)
                                 (cons? (member out-lst inp-lst))))])]
   [types (-> (listof (listof any/c))
              (listof any/c))])
  ((curry argmin (lambda (p) (length (filter string? p)))) l)) 

;; ---------------------------------------------------------------------------------------------------

(define/contract INTERNAL
  (configurable-ctc
   [max "find path: it is impossible to get from ~a to ~a [internal error]"]
   [types string?])
  "find path: it is impossible to get from ~a to ~a [internal error]")

(define/contract CURRENT-LOCATION
  (configurable-ctc
   [max "disambiguate your current location: ~a"]
   [types string?])
  "disambiguate your current location: ~a")

(define/contract CURRENT-LOCATION-0
  (configurable-ctc
   [max "no such station: ~a"]
   [types string?])
  "no such station: ~a") 

(define/contract DESTINATION
  (configurable-ctc
   [max "disambiguate your destination: ~a"]
   [types string?])
  "disambiguate your destination: ~a")

(define/contract DESTINATION-0
  (configurable-ctc
   [max "no such destination: ~a"]
   [types string?])
  "no such destination: ~a")

(define/contract NO-PATH
  (configurable-ctc
   [max "it is currently impossible to reach ~a from ~a via subways"]
   [types string?])
  "it is currently impossible to reach ~a from ~a via subways")

(define/contract DISABLED
  (configurable-ctc
   [max "clarify station to be disabled: ~a"]
   [types string?])
  "clarify station to be disabled: ~a")

(define/contract ENABLED
  (configurable-ctc
   [max "clarify station to be enabled: ~a"]
   [types string?])
  "clarify station to be enabled: ~a")

(define/contract DISABLED-0
  (configurable-ctc
   [max "no such station to disable: ~a"]
   [types string?])
  "no such station to disable: ~a")

(define/contract ENABLED-0
  (configurable-ctc
   [max "no such station to enable: ~a"]
   [types string?])
  "no such station to enable: ~a")

(define/contract ENSURE
  (configurable-ctc
   [max "---ensure you are on ~a"]
   [types string?])
  "---ensure you are on ~a")

(define/contract SWITCH
  (configurable-ctc
   [max "---switch from ~a to ~a"]
   [types string?])
  "---switch from ~a to ~a")

;; ---------------------------------------------------------------------------------------------------

(define manage%
  (class object% 
    (super-new)
    
    (field 
     ;; [instance-of MBTA%]
     [mbta-subways (read-t-graph)]
     ;; [Listof Station]
     [disabled '()])
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (add-to-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (cons station disabled)) #f]
        [(empty? station) (format DISABLED-0 s)]
        [else (format DISABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (remove-from-disabled s)
      (define station (send mbta-subways station s))
      (cond
        [(string? station) (set! disabled (remove* (list station) disabled)) #f]
        [(empty? station) (format ENABLED-0 s)]
        [else (format ENABLED (string-join station))]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (find from to)
      (define from-station (send mbta-subways station from))
      (define to-station   (send mbta-subways station to))
      (cond
        [(string=? from to) (format "Close your eyes and tap your heels three times. Open your eyes. You will be at ~a." to)]
        [(cons? from-station) (format CURRENT-LOCATION (string-join from-station))]
        [(cons? to-station)   (format DESTINATION (string-join to-station))]
        [(empty? from-station) (format CURRENT-LOCATION-0 from)]
        [(empty? to-station)   (format DESTINATION-0 to)]
        [else 
         (define paths (send mbta-subways find-path from-station to-station))
         (define path* (removed-paths-with-disabled-stations paths))
         (cond
           [(empty? paths) (format INTERNAL from-station to-station)]
           [(empty? path*) (format NO-PATH to-station from-station)]
           [else 
            (define paths-with-switch (for/list ([p path*]) (insert-switch p)))
            (define best-path-as-string*
              (for/list ([station-or-comment (pick-best-path paths-with-switch)])
               (match station-or-comment
                 [`(,name ,line) (string-append name ", take " (send mbta-subways render line))]
                 [(? string? comment) comment])))
            (string-join best-path-as-string* "\n")])]))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/private (removed-paths-with-disabled-stations paths*)
      (for/list ([p paths*] 
                 #:unless ;; any of the disabled stations is on the path 
                 (let ([stations (map first p)]) 
                   (for/or ((s stations)) (member s disabled))))
        p))
    
    ;; type Path* ~~ Path with "switch from Line to Line" strings in the middle 
    
    ;; -----------------------------------------------------------------------------------------------
    ;; [Listof Path*] -> Path*
    (define/private (pick-best-path paths*)
      (selector paths*))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Path -> Path* 
    (define/private (insert-switch path0)  
      (define start (first path0))
      (define pred-lines0 (second start))
      (define pred-string0 (send mbta-subways render pred-lines0))
      (cons start
            (let loop ([pred-lines pred-lines0][pred-string pred-string0][path (rest path0)])
              (cond
                [(empty? path) '()]
                [else 
                 (define stop (first path))
                 (define name (first stop))
                 (define stop-lines (second stop))
                 (define stop-string (send mbta-subways render stop-lines))
                 (define remainder (loop stop-lines stop-string (rest path)))
                 (cond
                   [(proper-subset? stop-lines pred-lines)
                    (list* (format ENSURE stop-string) stop remainder)]
                   [(set-empty? (set-intersect stop-lines pred-lines)) 
                    (list* (format SWITCH pred-string stop-string) stop remainder)]
                   [else (cons stop remainder)])]))))))

(define find-method-ctc
  (->i ([this any/c]
        [from string?]
        [to string?])
       [result (from to)
               (λ (res)
                 (let ([from-station (send (read-t-graph) station from)]
                       [to-station (send (read-t-graph) station to)])
                   (cond
                     [(string=? from to) (substring? to res)]
                     [(cons? from-station) (substring? (string-join from-station) res)]
                     [(cons? to-station) (substring? (string-join to-station) res)]
                     [(empty? from-station) (substring? from res)]
                     [(empty? to-station) (substring? to res)]
                     [else (and (substring? from res)
                                (substring? to res))])))]))

(define/ctc-helper manage-c/max-ctc
  (class/c
   (add-to-disabled
    (let ([stash-len (box #f)])
      (->i ([this any/c]
            [s string?])
           #:pre (this)
           (set-box! stash-len (length (get-field disabled this)))
           [result (s)
                   (let ([station (send t-graph station s)])
                     (cond
                       [(string? station) #f]
                       [(empty? station) (λ (res) (substring? res s))]
                       [else (λ (res) (substring? res (string-join station)))]))]
           #:post (this s)
           (let ([station (send t-graph station s)])
             (and (> (length (get-field disabled this))
                     (unbox stash-len))
                  (when (string? station)
                    (member station (get-field disabled this))))))))
   (remove-from-disabled
    (let ([stash-len (box #f)])
      (->i ([this any/c]
            [s string?])
           #:pre (this)
           (set-box! stash-len (length (get-field disabled this)))
           [result (s)
                   (let ([station (send t-graph station s)])
                     (cond
                       [(string? station) #f]
                       [(empty? station) (λ (res) (substring? res s))]
                       [else (λ (res) (substring? res (string-join station)))]))]
           #:post (this s)
           (let ([station (send t-graph station s)])
             (and (<= (length (get-field disabled this))
                      (unbox stash-len))
                  (when (string? station)
                    (not (member station (get-field disabled this)))))))))
   (find (->i ([this any/c]
               [from string?]
               [to string?])
              [result (from to)
                      (λ (res)
                        (let ([from-station (send t-graph station from)]
                              [to-station (send t-graph station to)])
                          (cond
                            [(string=? from to) (substring? to res)]
                            [(cons? from-station) (substring? (string-join from-station) res)]
                            [(cons? to-station) (substring? (string-join to-station) res)]
                            [(empty? from-station) (substring? from res)]
                            [(empty? to-station) (substring? to res)]
                            [else (and (substring? from res)
                                       (substring? to res))])))]))
   (field [mbta-subways (is-a?/c mbta+c%)]
          [disabled list?])))

(define/ctc-helper manage-c/max/sub1-ctc
  (class/c
   (add-to-disabled
    (->i ([this any/c]
          [s string?])
         [result (s)
                 (let ([station (send t-graph station s)])
                   (cond
                     [(string? station) #f]
                     [(empty? station) (λ (res) (substring? res s))]
                     [else (λ (res) (substring? res (string-join station)))]))]
         #:post (this)
         (not (empty? (get-field disabled this)))))
   (remove-from-disabled
    (let ([stash-len (box #f)])
      (->i ([this any/c]
            [s string?])
           #:pre (this)
           (set-box! stash-len (length (get-field disabled this)))
           [result (s)
                   (let ([station (send t-graph station s)])
                     (cond
                       [(string? station) #f]
                       [(empty? station) (λ (res) (substring? s res))]
                       [else (λ (res) (substring? (string-join station) res))]))]

           #:post (this)
           (let ([disabled-len (length (get-field disabled this))])
             (<= disabled-len (unbox stash-len))))))
   (find
    (->i ([this any/c]
          [from string?]
          [to string?])
         [result (from to)
                 (λ (res)
                   (let ([from-station (send t-graph station from)]
                         [to-station (send t-graph station to)])
                     (cond
                       [(string=? from to) (substring? to res)]
                       [(cons? from-station) (substring? (string-join from-station) res)]
                       [(cons? to-station) (substring? (string-join to-station) res)]
                       [(empty? from-station) (substring? from res)]
                       [(empty? to-station) (substring? to res)]
                       [else (and (substring? from res)
                                  (substring? to res))])))]))))

(define manage-c/types-ctc
  (class/c
     (add-to-disabled (->m string? (or/c string? #f)))
     (remove-from-disabled (->m string? (or/c string? #f)))
     (find (->m string? string? string?))
     (field [mbta-subways (is-a?/c mbta+c%)]
            [disabled list?])))

(define/contract manage+c%
  (configurable-ctc
   [max manage-c/max-ctc]
   [max/sub1 manage-c/max/sub1-ctc]
   [types manage-c/types-ctc])
   manage%)

(provide manage-c/max-ctc
         manage-c/max/sub1-ctc
         manage-c/types-ctc
         t-graph)