(module admin racket
  (#%module-begin
   (provide administrator% turn%)
   (require "../base/untyped.rkt" "board.rkt" "state.rkt" "tree.rkt")
   (require (only-in racket/sandbox call-with-limits exn:fail:resource?))
   (require (only-in "basics.rkt" ALL-HOTELS hotel? shares-available? shares? shares-order?))
   (define (in-sandbox producer consumer failure #:time (sec-limit 1) #:memory (mb-limit 30))
     ((let/ec
       fail
       (let ((a
              (with-handlers
               ((exn:fail:resource? (lambda (x) (fail (lambda () (failure 'R))))) (exn:fail:out-of-memory? (lambda (x) (fail (lambda () (failure 'R))))))
               (call-with-limits sec-limit mb-limit producer))))
         (lambda () (consumer a))))))
   (define (log status msg) (void))
   (define administrator%
     (class object%
       (init-field next-tile)
       (super-new)
       (define *count 10)
       (define *named-players '())
       (define/public
        (sign-up name player)
        (define pre (if (< (random 100) 50) "player" "spieler"))
        (set! *count (+ *count 1 (random 10)))
        (set! name (format "~a~a:~a" pre (number->string *count) name))
        (set! *named-players (cons (list name player) *named-players))
        name)
       (define/public (show-players) (for/list ((n+p (in-list *named-players))) (car n+p)))
       (define/public
        (run turns# #:show (show void))
        (define players (player->internals))
        (when (empty? players) (error 'run "players failed to sign up properly"))
        (define tree0 (generate-tree (setup-all players (apply state0 players))))
        (let loop ((tree tree0) (n turns#) (states (list (tree-state tree0))))
          (define state (tree-state tree))
          (cond
           ((= n 0) (list DONE (state-score state) (reverse states)))
           ((empty? (state-players state)) (list EXHAUSTED (state-score state) (reverse states)))
           ((and (is-a? tree lplaced%) (send tree nothing-to-place?)) (finish state) (list SCORE (state-score state) (reverse states)))
           (else
            (define external (or (player-external (state-current-player state)) (error 'external/fail)))
            (define turn (new turn% (current-state state)))
            (in-sandbox
             #:time
             (* (+ (length (state-players state)) 1) 3)
             (lambda () (let-values (((a b c) (send external take-turn turn))) (list a b c)))
             (lambda (arg)
               (define-values (tile hotel-involved buy-shares) (values (car arg) (cadr arg) (caddr arg)))
               (cond
                ((boolean? tile) (finish state) (list IMPOSSIBLE (state-score state) (reverse (cons state states))))
                (else
                 (define merger? (eq? (what-kind-of-spot (state-board state) tile) MERGING))
                 (cond
                  ((and merger? (not (send turn place-called))) (loop (generate-tree (state-remove-current-player state)) (- n 1) states))
                  (else
                   (define-values (t1 h1 d*) (if merger? (send turn decisions) (values #f #f '())))
                   (and (equal? tile t1) (equal? hotel-involved h1))
                   (define eliminate (send turn eliminated))
                   (cond
                    ((member (state-current-player state) eliminate) ((failure state states (lambda (s) (loop s (- n 1) states))) `(S "current player failed on keep")))
                    (else
                     (define tree/eliminate (if (empty? eliminate) tree (generate-tree (state-eliminate state eliminate))))
                     (exec
                      external
                      tree/eliminate
                      tile
                      hotel-involved
                      d*
                      buy-shares
                      (lambda (next-tree state)
                        (inform-all
                         next-tree
                         state
                         (lambda (next-tree state)
                           (cond ((empty? (state-tiles state)) (finish state) (list EXHAUSTED (state-score state) (reverse states))) (else (loop next-tree (- n 1) (cons state states)))))))
                      (failure state states (lambda (s) (loop s (- n 1) states)))))))))))
             (failure state states (lambda (s) (loop s (- n 1) states))))))))
       (define/private
        ((failure state states continue) status)
        (log status `(turn failure ,(player-name (state-current-player state))))
        (define state/eliminate (state-remove-current-player state))
        (if (empty? (state-players state/eliminate)) (list EXHAUSTED '(all players failed) (reverse states)) (continue (generate-tree state/eliminate))))
       (define/private
        (exec external tree0 placement hotel decisions shares-to-buy succ fail)
        (define-values (tile tree) (tree-next tree0 placement hotel decisions shares-to-buy next-tile))
        (unless tile (error 'no-tile))
        (in-sandbox (lambda () (send external receive-tile tile)) (lambda (_void) (succ tree (tree-state tree))) fail))
       (define/private
        (inform-all tree state k)
        (define eliminate
          (for/fold
           ((throw-out '()))
           ((p (state-players state)))
           (in-sandbox
            (lambda arg* (let ((pe (or (player-external p) (error 'external-fail)))) (send pe inform state)))
            (lambda (_void) throw-out)
            (lambda arg* (log (car arg*) `(inform ,(player-name p))) (cons p throw-out)))))
        (cond ((empty? eliminate) (k tree state)) (else (define state/eliminate (state-eliminate state eliminate)) (k (generate-tree state/eliminate) state/eliminate))))
       (define/private
        (player->internals)
        (define-values
         (internals _)
         (for/fold
          ((internals '()) (tile* ALL-TILES))
          ((name+eplayer *named-players))
          (define name (first name+eplayer))
          (define tiles (take tile* STARTER-TILES#))
          (define player
            (let ((t1 (first tiles)) (t2 (second tiles)) (t3 (third tiles)) (t4 (fourth tiles)) (t5 (fifth tiles)) (t6 (sixth tiles))) (player0 name t1 t2 t3 t4 t5 t6 (second name+eplayer))))
          (values (cons player internals) (drop tile* STARTER-TILES#))))
        internals)
       (define/private
        (setup-all players state)
        (define misbehaving
          (for/fold
           ((misbehaving '()))
           ((p players))
           (in-sandbox
            (lambda arg* (let ((pe (or (player-external p) (error 'external-failed)))) (send pe setup state)))
            (lambda (_void) misbehaving)
            (lambda status (log status `(setup ,(player-name p))) (cons p misbehaving)))))
        (if (empty? misbehaving) state (state-eliminate state misbehaving)))
       (define/private
        (finish state)
        (define score (state-score state))
        (for
         ((e (state-players state)))
         (in-sandbox (lambda () (send (or (player-external e) (error 'noexternal)) the-end state score)) void (lambda (status) (log status `(end game ,(player-name e)))))))))
   (define DONE 'done)
   (define EXHAUSTED 'exhausted)
   (define SCORE 'score)
   (define turn%
     (class object%
       (init-field current-state)
       (field
        (board (state-board current-state))
        (current (state-current-player current-state))
        (cash (player-money current))
        (tiles (player-tiles current))
        (shares (state-shares current-state))
        (hotels (state-hotels current-state))
        (players (state-players current-state)))
       (super-new)
       (define/public (reconcile-shares t) t)
       (define/public (eliminated) *eliminated)
       (define/public (decisions) (values *tile *hotel *decisions))
       (define/public (place-called) *called)
       (define *tile #f)
       (define *hotel #f)
       (define *decisions '())
       (define *eliminated '())
       (define *called #f)
       (define/public
        (place tile hotel)
        (unless *called
          (set! *called #t)
          (set! *tile tile)
          (set! *hotel hotel)
          (define-values (acquirers acquired) (merging-which board tile))
          (define acquired-hotels (append (remove hotel acquirers) acquired))
          (let keep-to-all ((players players))
            (unless (empty? players)
              (define p (first players))
              (in-sandbox
               (lambda () (define ex (player-external p)) (if (boolean? ex) (map (lambda (_) #t) acquired-hotels) (send ex keep acquired-hotels)))
               (lambda (p-s-decisions)
                 (set! *decisions (cons (list p (for/list ((h (in-list acquired-hotels)) (d (in-list p-s-decisions))) (list h (if d #t #f)))) *decisions))
                 (keep-to-all (rest players)))
               (lambda (status) (log status `(keep failure ,(player-name p))) (set! *eliminated (cons p *eliminated)) (keep-to-all (rest players))))))
          (define state/eliminated (state-eliminate current-state *eliminated))
          (define state/returns (state-return-shares state/eliminated *decisions))
          (set! current-state state/returns)
          (state-players state/returns)))))))
