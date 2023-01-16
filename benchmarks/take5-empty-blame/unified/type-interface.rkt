(module mutation-adapter typed/racket/shallow
  (#%module-begin
   (module contracted racket
     (require (file
               "/home/lukas/github_sync/grad/projects/blame-gradual-typing/src/blame-evaluation-gt/mutation-adapter/mutation-adapter.rkt"))
     (require "original-type-interface.rkt")
     (provide (except-out (all-from-out "original-type-interface.rkt") create-dealer player%))
     (begin
       (define create-dealer1402045
         (contract
          (delegating->
           1
           (list
            (cons
             0
             (delegating-listof
              (delegating-instanceof
               (delegating-class/c
                (list)
                (list)
                (list
                 (cons
                  'start-turn
                  (delegating->
                   1
                   (list
                    (cons
                     0
                     (delegating-instanceof
                      (delegating-and/c
                       (delegating-class/c
                        (list)
                        (list)
                        (list
                         (cons
                          'fewest-bulls
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter)))))
                         (cons
                          'fit
                          (delegating->
                           1
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter)))))
                         (cons
                          'replace
                          (delegating->
                           2
                           (list (cons 0 (sealing-adapter)))
                           (any/c-adapter)
                           (list)))))
                       (delegating-class/c
                        (list)
                        (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                        (list
                         (cons
                          'get-field:my-stacks
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (delegating-listof (sealing-adapter))))))
                         (cons
                          'set-field:my-stacks
                          (delegating->
                           1
                           (list (cons 0 (delegating-listof (sealing-adapter))))
                           (any/c-adapter)
                           (list)))))))))
                   (any/c-adapter)
                   (list)))
                 (cons
                  'choose
                  (delegating->
                   1
                   (list
                    (cons
                     0
                     (delegating-instanceof
                      (delegating-and/c
                       (delegating-class/c
                        (list)
                        (list)
                        (list
                         (cons
                          'fewest-bulls
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter)))))
                         (cons
                          'fit
                          (delegating->
                           1
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter)))))
                         (cons
                          'replace
                          (delegating->
                           2
                           (list (cons 0 (sealing-adapter)))
                           (any/c-adapter)
                           (list)))))
                       (delegating-class/c
                        (list)
                        (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                        (list
                         (cons
                          'get-field:my-stacks
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list (cons 0 (delegating-listof (sealing-adapter))))))
                         (cons
                          'set-field:my-stacks
                          (delegating->
                           1
                           (list (cons 0 (delegating-listof (sealing-adapter))))
                           (any/c-adapter)
                           (list)))))))))
                   (any/c-adapter)
                   (list (cons 0 (sealing-adapter)))))))))))
           (any/c-adapter)
           (list
            (cons
             0
             (delegating-instanceof
              (delegating-class/c
               (list
                (cons
                 'players
                 (delegating-listof
                  (delegating-instanceof
                   (delegating-class/c
                    (list)
                    (list)
                    (list
                     (cons
                      'start-turn
                      (delegating->
                       1
                       (list
                        (cons
                         0
                         (delegating-instanceof
                          (delegating-and/c
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'fewest-bulls
                              (delegating->
                               0
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter)))))
                             (cons
                              'fit
                              (delegating->
                               1
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter)))))
                             (cons
                              'replace
                              (delegating->
                               2
                               (list (cons 0 (sealing-adapter)))
                               (any/c-adapter)
                               (list)))))
                           (delegating-class/c
                            (list)
                            (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                            (list
                             (cons
                              'get-field:my-stacks
                              (delegating->
                               0
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (delegating-listof (sealing-adapter))))))
                             (cons
                              'set-field:my-stacks
                              (delegating->
                               1
                               (list (cons 0 (delegating-listof (sealing-adapter))))
                               (any/c-adapter)
                               (list)))))))))
                       (any/c-adapter)
                       (list)))
                     (cons
                      'choose
                      (delegating->
                       1
                       (list
                        (cons
                         0
                         (delegating-instanceof
                          (delegating-and/c
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'fewest-bulls
                              (delegating->
                               0
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter)))))
                             (cons
                              'fit
                              (delegating->
                               1
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter)))))
                             (cons
                              'replace
                              (delegating->
                               2
                               (list (cons 0 (sealing-adapter)))
                               (any/c-adapter)
                               (list)))))
                           (delegating-class/c
                            (list)
                            (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                            (list
                             (cons
                              'get-field:my-stacks
                              (delegating->
                               0
                               (list)
                               (any/c-adapter)
                               (list (cons 0 (delegating-listof (sealing-adapter))))))
                             (cons
                              'set-field:my-stacks
                              (delegating->
                               1
                               (list (cons 0 (delegating-listof (sealing-adapter))))
                               (any/c-adapter)
                               (list)))))))))
                       (any/c-adapter)
                       (list (cons 0 (sealing-adapter)))))))))))
               (list
                (cons
                 'internal%
                 (delegating-and/c
                  (delegating-class/c
                   (list
                    (cons
                     'player
                     (delegating-instanceof
                      (delegating-class/c
                       (list)
                       (list)
                       (list
                        (cons
                         'start-turn
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list)))
                        (cons
                         'choose
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list (cons 0 (sealing-adapter))))))))))
                   (list)
                   (list
                    (cons
                     'get-field:player
                     (delegating->
                      0
                      (list)
                      (any/c-adapter)
                      (list
                       (cons
                        0
                        (delegating-instanceof
                         (delegating-class/c
                          (list)
                          (list)
                          (list
                           (cons
                            'start-turn
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list)))
                           (cons
                            'choose
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list (cons 0 (sealing-adapter))))))))))))
                    (cons
                     'set-field:player
                     (delegating->
                      1
                      (list
                       (cons
                        0
                        (delegating-instanceof
                         (delegating-class/c
                          (list)
                          (list)
                          (list
                           (cons
                            'start-turn
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list)))
                           (cons
                            'choose
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list (cons 0 (sealing-adapter))))))))))
                      (any/c-adapter)
                      (list)))))
                  (delegating-class/c
                   (list)
                   (list)
                   (list
                    (cons
                     'start-turn
                     (delegating->
                      1
                      (list
                       (cons
                        0
                        (delegating-instanceof
                         (delegating-and/c
                          (delegating-class/c
                           (list)
                           (list)
                           (list
                            (cons
                             'fewest-bulls
                             (delegating->
                              0
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (sealing-adapter)))))
                            (cons
                             'fit
                             (delegating->
                              1
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (sealing-adapter)))))
                            (cons
                             'replace
                             (delegating->
                              2
                              (list (cons 0 (sealing-adapter)))
                              (any/c-adapter)
                              (list)))))
                          (delegating-class/c
                           (list)
                           (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                           (list
                            (cons
                             'get-field:my-stacks
                             (delegating->
                              0
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (delegating-listof (sealing-adapter))))))
                            (cons
                             'set-field:my-stacks
                             (delegating->
                              1
                              (list (cons 0 (delegating-listof (sealing-adapter))))
                              (any/c-adapter)
                              (list)))))))))
                      (any/c-adapter)
                      (list)))
                    (cons
                     'choose
                     (delegating->
                      1
                      (list
                       (cons
                        0
                        (delegating-instanceof
                         (delegating-and/c
                          (delegating-class/c
                           (list)
                           (list)
                           (list
                            (cons
                             'fewest-bulls
                             (delegating->
                              0
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (sealing-adapter)))))
                            (cons
                             'fit
                             (delegating->
                              1
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (sealing-adapter)))))
                            (cons
                             'replace
                             (delegating->
                              2
                              (list (cons 0 (sealing-adapter)))
                              (any/c-adapter)
                              (list)))))
                          (delegating-class/c
                           (list)
                           (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                           (list
                            (cons
                             'get-field:my-stacks
                             (delegating->
                              0
                              (list)
                              (any/c-adapter)
                              (list (cons 0 (delegating-listof (sealing-adapter))))))
                            (cons
                             'set-field:my-stacks
                             (delegating->
                              1
                              (list (cons 0 (delegating-listof (sealing-adapter))))
                              (any/c-adapter)
                              (list)))))))))
                      (any/c-adapter)
                      (list (cons 0 (sealing-adapter)))))))))
                (cons
                 'internals
                 (delegating-listof
                  (delegating-instanceof
                   (delegating-and/c
                    (delegating-class/c
                     (list
                      (cons
                       'player
                       (delegating-instanceof
                        (delegating-class/c
                         (list)
                         (list)
                         (list
                          (cons
                           'start-turn
                           (delegating->
                            1
                            (list
                             (cons
                              0
                              (delegating-instanceof
                               (delegating-and/c
                                (delegating-class/c
                                 (list)
                                 (list)
                                 (list
                                  (cons
                                   'fewest-bulls
                                   (delegating->
                                    0
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (sealing-adapter)))))
                                  (cons
                                   'fit
                                   (delegating->
                                    1
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (sealing-adapter)))))
                                  (cons
                                   'replace
                                   (delegating->
                                    2
                                    (list (cons 0 (sealing-adapter)))
                                    (any/c-adapter)
                                    (list)))))
                                (delegating-class/c
                                 (list)
                                 (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                 (list
                                  (cons
                                   'get-field:my-stacks
                                   (delegating->
                                    0
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (delegating-listof (sealing-adapter))))))
                                  (cons
                                   'set-field:my-stacks
                                   (delegating->
                                    1
                                    (list (cons 0 (delegating-listof (sealing-adapter))))
                                    (any/c-adapter)
                                    (list)))))))))
                            (any/c-adapter)
                            (list)))
                          (cons
                           'choose
                           (delegating->
                            1
                            (list
                             (cons
                              0
                              (delegating-instanceof
                               (delegating-and/c
                                (delegating-class/c
                                 (list)
                                 (list)
                                 (list
                                  (cons
                                   'fewest-bulls
                                   (delegating->
                                    0
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (sealing-adapter)))))
                                  (cons
                                   'fit
                                   (delegating->
                                    1
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (sealing-adapter)))))
                                  (cons
                                   'replace
                                   (delegating->
                                    2
                                    (list (cons 0 (sealing-adapter)))
                                    (any/c-adapter)
                                    (list)))))
                                (delegating-class/c
                                 (list)
                                 (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                 (list
                                  (cons
                                   'get-field:my-stacks
                                   (delegating->
                                    0
                                    (list)
                                    (any/c-adapter)
                                    (list (cons 0 (delegating-listof (sealing-adapter))))))
                                  (cons
                                   'set-field:my-stacks
                                   (delegating->
                                    1
                                    (list (cons 0 (delegating-listof (sealing-adapter))))
                                    (any/c-adapter)
                                    (list)))))))))
                            (any/c-adapter)
                            (list (cons 0 (sealing-adapter))))))))))
                     (list)
                     (list
                      (cons
                       'get-field:player
                       (delegating->
                        0
                        (list)
                        (any/c-adapter)
                        (list
                         (cons
                          0
                          (delegating-instanceof
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'start-turn
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list)))
                             (cons
                              'choose
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter))))))))))))
                      (cons
                       'set-field:player
                       (delegating->
                        1
                        (list
                         (cons
                          0
                          (delegating-instanceof
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'start-turn
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list)))
                             (cons
                              'choose
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter))))))))))
                        (any/c-adapter)
                        (list)))))
                    (delegating-class/c
                     (list)
                     (list)
                     (list
                      (cons
                       'start-turn
                       (delegating->
                        1
                        (list
                         (cons
                          0
                          (delegating-instanceof
                           (delegating-and/c
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'fewest-bulls
                               (delegating->
                                0
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter)))))
                              (cons
                               'fit
                               (delegating->
                                1
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter)))))
                              (cons
                               'replace
                               (delegating->
                                2
                                (list (cons 0 (sealing-adapter)))
                                (any/c-adapter)
                                (list)))))
                            (delegating-class/c
                             (list)
                             (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                             (list
                              (cons
                               'get-field:my-stacks
                               (delegating->
                                0
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (delegating-listof (sealing-adapter))))))
                              (cons
                               'set-field:my-stacks
                               (delegating->
                                1
                                (list (cons 0 (delegating-listof (sealing-adapter))))
                                (any/c-adapter)
                                (list)))))))))
                        (any/c-adapter)
                        (list)))
                      (cons
                       'choose
                       (delegating->
                        1
                        (list
                         (cons
                          0
                          (delegating-instanceof
                           (delegating-and/c
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'fewest-bulls
                               (delegating->
                                0
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter)))))
                              (cons
                               'fit
                               (delegating->
                                1
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter)))))
                              (cons
                               'replace
                               (delegating->
                                2
                                (list (cons 0 (sealing-adapter)))
                                (any/c-adapter)
                                (list)))))
                            (delegating-class/c
                             (list)
                             (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                             (list
                              (cons
                               'get-field:my-stacks
                               (delegating->
                                0
                                (list)
                                (any/c-adapter)
                                (list (cons 0 (delegating-listof (sealing-adapter))))))
                              (cons
                               'set-field:my-stacks
                               (delegating->
                                1
                                (list (cons 0 (delegating-listof (sealing-adapter))))
                                (any/c-adapter)
                                (list)))))))))
                        (any/c-adapter)
                        (list (cons 0 (sealing-adapter))))))))))))
               (list
                (cons
                 'get-field:players
                 (delegating->
                  0
                  (list)
                  (any/c-adapter)
                  (list
                   (cons
                    0
                    (delegating-listof
                     (delegating-instanceof
                      (delegating-class/c
                       (list)
                       (list)
                       (list
                        (cons
                         'start-turn
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list)))
                        (cons
                         'choose
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list (cons 0 (sealing-adapter)))))))))))))
                (cons
                 'get-field:internal%
                 (delegating->
                  0
                  (list)
                  (any/c-adapter)
                  (list
                   (cons
                    0
                    (delegating-and/c
                     (delegating-class/c
                      (list
                       (cons
                        'player
                        (delegating-instanceof
                         (delegating-class/c
                          (list)
                          (list)
                          (list
                           (cons
                            'start-turn
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list)))
                           (cons
                            'choose
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list (cons 0 (sealing-adapter))))))))))
                      (list)
                      (list
                       (cons
                        'get-field:player
                        (delegating->
                         0
                         (list)
                         (any/c-adapter)
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'start-turn
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list)))
                              (cons
                               'choose
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter))))))))))))
                       (cons
                        'set-field:player
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'start-turn
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list)))
                              (cons
                               'choose
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter))))))))))
                         (any/c-adapter)
                         (list)))))
                     (delegating-class/c
                      (list)
                      (list)
                      (list
                       (cons
                        'start-turn
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-and/c
                             (delegating-class/c
                              (list)
                              (list)
                              (list
                               (cons
                                'fewest-bulls
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'fit
                                (delegating->
                                 1
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'replace
                                (delegating->
                                 2
                                 (list (cons 0 (sealing-adapter)))
                                 (any/c-adapter)
                                 (list)))))
                             (delegating-class/c
                              (list)
                              (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                              (list
                               (cons
                                'get-field:my-stacks
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (delegating-listof (sealing-adapter))))))
                               (cons
                                'set-field:my-stacks
                                (delegating->
                                 1
                                 (list (cons 0 (delegating-listof (sealing-adapter))))
                                 (any/c-adapter)
                                 (list)))))))))
                         (any/c-adapter)
                         (list)))
                       (cons
                        'choose
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-and/c
                             (delegating-class/c
                              (list)
                              (list)
                              (list
                               (cons
                                'fewest-bulls
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'fit
                                (delegating->
                                 1
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'replace
                                (delegating->
                                 2
                                 (list (cons 0 (sealing-adapter)))
                                 (any/c-adapter)
                                 (list)))))
                             (delegating-class/c
                              (list)
                              (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                              (list
                               (cons
                                'get-field:my-stacks
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (delegating-listof (sealing-adapter))))))
                               (cons
                                'set-field:my-stacks
                                (delegating->
                                 1
                                 (list (cons 0 (delegating-listof (sealing-adapter))))
                                 (any/c-adapter)
                                 (list)))))))))
                         (any/c-adapter)
                         (list (cons 0 (sealing-adapter))))))))))))
                (cons
                 'get-field:internals
                 (delegating->
                  0
                  (list)
                  (any/c-adapter)
                  (list
                   (cons
                    0
                    (delegating-listof
                     (delegating-instanceof
                      (delegating-and/c
                       (delegating-class/c
                        (list
                         (cons
                          'player
                          (delegating-instanceof
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'start-turn
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list)))
                             (cons
                              'choose
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter))))))))))
                        (list)
                        (list
                         (cons
                          'get-field:player
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'start-turn
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list)))
                                (cons
                                 'choose
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter))))))))))))
                         (cons
                          'set-field:player
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'start-turn
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list)))
                                (cons
                                 'choose
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter))))))))))
                           (any/c-adapter)
                           (list)))))
                       (delegating-class/c
                        (list)
                        (list)
                        (list
                         (cons
                          'start-turn
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-and/c
                               (delegating-class/c
                                (list)
                                (list)
                                (list
                                 (cons
                                  'fewest-bulls
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'fit
                                  (delegating->
                                   1
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'replace
                                  (delegating->
                                   2
                                   (list (cons 0 (sealing-adapter)))
                                   (any/c-adapter)
                                   (list)))))
                               (delegating-class/c
                                (list)
                                (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                (list
                                 (cons
                                  'get-field:my-stacks
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (delegating-listof (sealing-adapter))))))
                                 (cons
                                  'set-field:my-stacks
                                  (delegating->
                                   1
                                   (list (cons 0 (delegating-listof (sealing-adapter))))
                                   (any/c-adapter)
                                   (list)))))))))
                           (any/c-adapter)
                           (list)))
                         (cons
                          'choose
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-and/c
                               (delegating-class/c
                                (list)
                                (list)
                                (list
                                 (cons
                                  'fewest-bulls
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'fit
                                  (delegating->
                                   1
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'replace
                                  (delegating->
                                   2
                                   (list (cons 0 (sealing-adapter)))
                                   (any/c-adapter)
                                   (list)))))
                               (delegating-class/c
                                (list)
                                (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                (list
                                 (cons
                                  'get-field:my-stacks
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (delegating-listof (sealing-adapter))))))
                                 (cons
                                  'set-field:my-stacks
                                  (delegating->
                                   1
                                   (list (cons 0 (delegating-listof (sealing-adapter))))
                                   (any/c-adapter)
                                   (list)))))))))
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter))))))))))))))
                (cons
                 'set-field:players
                 (delegating->
                  1
                  (list
                   (cons
                    0
                    (delegating-listof
                     (delegating-instanceof
                      (delegating-class/c
                       (list)
                       (list)
                       (list
                        (cons
                         'start-turn
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list)))
                        (cons
                         'choose
                         (delegating->
                          1
                          (list
                           (cons
                            0
                            (delegating-instanceof
                             (delegating-and/c
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'fewest-bulls
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'fit
                                 (delegating->
                                  1
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter)))))
                                (cons
                                 'replace
                                 (delegating->
                                  2
                                  (list (cons 0 (sealing-adapter)))
                                  (any/c-adapter)
                                  (list)))))
                              (delegating-class/c
                               (list)
                               (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                               (list
                                (cons
                                 'get-field:my-stacks
                                 (delegating->
                                  0
                                  (list)
                                  (any/c-adapter)
                                  (list (cons 0 (delegating-listof (sealing-adapter))))))
                                (cons
                                 'set-field:my-stacks
                                 (delegating->
                                  1
                                  (list (cons 0 (delegating-listof (sealing-adapter))))
                                  (any/c-adapter)
                                  (list)))))))))
                          (any/c-adapter)
                          (list (cons 0 (sealing-adapter)))))))))))
                  (any/c-adapter)
                  (list)))
                (cons
                 'set-field:internal%
                 (delegating->
                  1
                  (list
                   (cons
                    0
                    (delegating-and/c
                     (delegating-class/c
                      (list
                       (cons
                        'player
                        (delegating-instanceof
                         (delegating-class/c
                          (list)
                          (list)
                          (list
                           (cons
                            'start-turn
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list)))
                           (cons
                            'choose
                            (delegating->
                             1
                             (list
                              (cons
                               0
                               (delegating-instanceof
                                (delegating-and/c
                                 (delegating-class/c
                                  (list)
                                  (list)
                                  (list
                                   (cons
                                    'fewest-bulls
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'fit
                                    (delegating->
                                     1
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (sealing-adapter)))))
                                   (cons
                                    'replace
                                    (delegating->
                                     2
                                     (list (cons 0 (sealing-adapter)))
                                     (any/c-adapter)
                                     (list)))))
                                 (delegating-class/c
                                  (list)
                                  (list
                                   (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                  (list
                                   (cons
                                    'get-field:my-stacks
                                    (delegating->
                                     0
                                     (list)
                                     (any/c-adapter)
                                     (list (cons 0 (delegating-listof (sealing-adapter))))))
                                   (cons
                                    'set-field:my-stacks
                                    (delegating->
                                     1
                                     (list (cons 0 (delegating-listof (sealing-adapter))))
                                     (any/c-adapter)
                                     (list)))))))))
                             (any/c-adapter)
                             (list (cons 0 (sealing-adapter))))))))))
                      (list)
                      (list
                       (cons
                        'get-field:player
                        (delegating->
                         0
                         (list)
                         (any/c-adapter)
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'start-turn
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list)))
                              (cons
                               'choose
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter))))))))))))
                       (cons
                        'set-field:player
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-class/c
                             (list)
                             (list)
                             (list
                              (cons
                               'start-turn
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list)))
                              (cons
                               'choose
                               (delegating->
                                1
                                (list
                                 (cons
                                  0
                                  (delegating-instanceof
                                   (delegating-and/c
                                    (delegating-class/c
                                     (list)
                                     (list)
                                     (list
                                      (cons
                                       'fewest-bulls
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'fit
                                       (delegating->
                                        1
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (sealing-adapter)))))
                                      (cons
                                       'replace
                                       (delegating->
                                        2
                                        (list (cons 0 (sealing-adapter)))
                                        (any/c-adapter)
                                        (list)))))
                                    (delegating-class/c
                                     (list)
                                     (list
                                      (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                     (list
                                      (cons
                                       'get-field:my-stacks
                                       (delegating->
                                        0
                                        (list)
                                        (any/c-adapter)
                                        (list (cons 0 (delegating-listof (sealing-adapter))))))
                                      (cons
                                       'set-field:my-stacks
                                       (delegating->
                                        1
                                        (list (cons 0 (delegating-listof (sealing-adapter))))
                                        (any/c-adapter)
                                        (list)))))))))
                                (any/c-adapter)
                                (list (cons 0 (sealing-adapter))))))))))
                         (any/c-adapter)
                         (list)))))
                     (delegating-class/c
                      (list)
                      (list)
                      (list
                       (cons
                        'start-turn
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-and/c
                             (delegating-class/c
                              (list)
                              (list)
                              (list
                               (cons
                                'fewest-bulls
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'fit
                                (delegating->
                                 1
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'replace
                                (delegating->
                                 2
                                 (list (cons 0 (sealing-adapter)))
                                 (any/c-adapter)
                                 (list)))))
                             (delegating-class/c
                              (list)
                              (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                              (list
                               (cons
                                'get-field:my-stacks
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (delegating-listof (sealing-adapter))))))
                               (cons
                                'set-field:my-stacks
                                (delegating->
                                 1
                                 (list (cons 0 (delegating-listof (sealing-adapter))))
                                 (any/c-adapter)
                                 (list)))))))))
                         (any/c-adapter)
                         (list)))
                       (cons
                        'choose
                        (delegating->
                         1
                         (list
                          (cons
                           0
                           (delegating-instanceof
                            (delegating-and/c
                             (delegating-class/c
                              (list)
                              (list)
                              (list
                               (cons
                                'fewest-bulls
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'fit
                                (delegating->
                                 1
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (sealing-adapter)))))
                               (cons
                                'replace
                                (delegating->
                                 2
                                 (list (cons 0 (sealing-adapter)))
                                 (any/c-adapter)
                                 (list)))))
                             (delegating-class/c
                              (list)
                              (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                              (list
                               (cons
                                'get-field:my-stacks
                                (delegating->
                                 0
                                 (list)
                                 (any/c-adapter)
                                 (list (cons 0 (delegating-listof (sealing-adapter))))))
                               (cons
                                'set-field:my-stacks
                                (delegating->
                                 1
                                 (list (cons 0 (delegating-listof (sealing-adapter))))
                                 (any/c-adapter)
                                 (list)))))))))
                         (any/c-adapter)
                         (list (cons 0 (sealing-adapter))))))))))
                  (any/c-adapter)
                  (list)))
                (cons
                 'set-field:internals
                 (delegating->
                  1
                  (list
                   (cons
                    0
                    (delegating-listof
                     (delegating-instanceof
                      (delegating-and/c
                       (delegating-class/c
                        (list
                         (cons
                          'player
                          (delegating-instanceof
                           (delegating-class/c
                            (list)
                            (list)
                            (list
                             (cons
                              'start-turn
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list)))
                             (cons
                              'choose
                              (delegating->
                               1
                               (list
                                (cons
                                 0
                                 (delegating-instanceof
                                  (delegating-and/c
                                   (delegating-class/c
                                    (list)
                                    (list)
                                    (list
                                     (cons
                                      'fewest-bulls
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'fit
                                      (delegating->
                                       1
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (sealing-adapter)))))
                                     (cons
                                      'replace
                                      (delegating->
                                       2
                                       (list (cons 0 (sealing-adapter)))
                                       (any/c-adapter)
                                       (list)))))
                                   (delegating-class/c
                                    (list)
                                    (list
                                     (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                    (list
                                     (cons
                                      'get-field:my-stacks
                                      (delegating->
                                       0
                                       (list)
                                       (any/c-adapter)
                                       (list (cons 0 (delegating-listof (sealing-adapter))))))
                                     (cons
                                      'set-field:my-stacks
                                      (delegating->
                                       1
                                       (list (cons 0 (delegating-listof (sealing-adapter))))
                                       (any/c-adapter)
                                       (list)))))))))
                               (any/c-adapter)
                               (list (cons 0 (sealing-adapter))))))))))
                        (list)
                        (list
                         (cons
                          'get-field:player
                          (delegating->
                           0
                           (list)
                           (any/c-adapter)
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'start-turn
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list)))
                                (cons
                                 'choose
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter))))))))))))
                         (cons
                          'set-field:player
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-class/c
                               (list)
                               (list)
                               (list
                                (cons
                                 'start-turn
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list)))
                                (cons
                                 'choose
                                 (delegating->
                                  1
                                  (list
                                   (cons
                                    0
                                    (delegating-instanceof
                                     (delegating-and/c
                                      (delegating-class/c
                                       (list)
                                       (list)
                                       (list
                                        (cons
                                         'fewest-bulls
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'fit
                                         (delegating->
                                          1
                                          (list)
                                          (any/c-adapter)
                                          (list (cons 0 (sealing-adapter)))))
                                        (cons
                                         'replace
                                         (delegating->
                                          2
                                          (list (cons 0 (sealing-adapter)))
                                          (any/c-adapter)
                                          (list)))))
                                      (delegating-class/c
                                       (list)
                                       (list
                                        (cons
                                         'my-stacks
                                         (delegating-listof (sealing-adapter))))
                                       (list
                                        (cons
                                         'get-field:my-stacks
                                         (delegating->
                                          0
                                          (list)
                                          (any/c-adapter)
                                          (list
                                           (cons 0 (delegating-listof (sealing-adapter))))))
                                        (cons
                                         'set-field:my-stacks
                                         (delegating->
                                          1
                                          (list (cons 0 (delegating-listof (sealing-adapter))))
                                          (any/c-adapter)
                                          (list)))))))))
                                  (any/c-adapter)
                                  (list (cons 0 (sealing-adapter))))))))))
                           (any/c-adapter)
                           (list)))))
                       (delegating-class/c
                        (list)
                        (list)
                        (list
                         (cons
                          'start-turn
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-and/c
                               (delegating-class/c
                                (list)
                                (list)
                                (list
                                 (cons
                                  'fewest-bulls
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'fit
                                  (delegating->
                                   1
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'replace
                                  (delegating->
                                   2
                                   (list (cons 0 (sealing-adapter)))
                                   (any/c-adapter)
                                   (list)))))
                               (delegating-class/c
                                (list)
                                (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                (list
                                 (cons
                                  'get-field:my-stacks
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (delegating-listof (sealing-adapter))))))
                                 (cons
                                  'set-field:my-stacks
                                  (delegating->
                                   1
                                   (list (cons 0 (delegating-listof (sealing-adapter))))
                                   (any/c-adapter)
                                   (list)))))))))
                           (any/c-adapter)
                           (list)))
                         (cons
                          'choose
                          (delegating->
                           1
                           (list
                            (cons
                             0
                             (delegating-instanceof
                              (delegating-and/c
                               (delegating-class/c
                                (list)
                                (list)
                                (list
                                 (cons
                                  'fewest-bulls
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'fit
                                  (delegating->
                                   1
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (sealing-adapter)))))
                                 (cons
                                  'replace
                                  (delegating->
                                   2
                                   (list (cons 0 (sealing-adapter)))
                                   (any/c-adapter)
                                   (list)))))
                               (delegating-class/c
                                (list)
                                (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                                (list
                                 (cons
                                  'get-field:my-stacks
                                  (delegating->
                                   0
                                   (list)
                                   (any/c-adapter)
                                   (list (cons 0 (delegating-listof (sealing-adapter))))))
                                 (cons
                                  'set-field:my-stacks
                                  (delegating->
                                   1
                                   (list (cons 0 (delegating-listof (sealing-adapter))))
                                   (any/c-adapter)
                                   (list)))))))))
                           (any/c-adapter)
                           (list (cons 0 (sealing-adapter))))))))))))
                  (any/c-adapter)
                  (list)))))))))
          create-dealer
          #f
          #f))
       (provide (rename-out (create-dealer1402045 create-dealer))))
     (begin
       (define player%1402046
         (contract
          (delegating-class/c
           (list)
           (list)
           (list
            (cons
             'start-turn
             (delegating->
              1
              (list
               (cons
                0
                (delegating-instanceof
                 (delegating-and/c
                  (delegating-class/c
                   (list)
                   (list)
                   (list
                    (cons
                     'fewest-bulls
                     (delegating-> 0 (list) (any/c-adapter) (list (cons 0 (sealing-adapter)))))
                    (cons
                     'fit
                     (delegating-> 1 (list) (any/c-adapter) (list (cons 0 (sealing-adapter)))))
                    (cons
                     'replace
                     (delegating->
                      2
                      (list (cons 0 (sealing-adapter)))
                      (any/c-adapter)
                      (list)))))
                  (delegating-class/c
                   (list)
                   (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                   (list
                    (cons
                     'get-field:my-stacks
                     (delegating->
                      0
                      (list)
                      (any/c-adapter)
                      (list (cons 0 (delegating-listof (sealing-adapter))))))
                    (cons
                     'set-field:my-stacks
                     (delegating->
                      1
                      (list (cons 0 (delegating-listof (sealing-adapter))))
                      (any/c-adapter)
                      (list)))))))))
              (any/c-adapter)
              (list)))
            (cons
             'choose
             (delegating->
              1
              (list
               (cons
                0
                (delegating-instanceof
                 (delegating-and/c
                  (delegating-class/c
                   (list)
                   (list)
                   (list
                    (cons
                     'fewest-bulls
                     (delegating-> 0 (list) (any/c-adapter) (list (cons 0 (sealing-adapter)))))
                    (cons
                     'fit
                     (delegating-> 1 (list) (any/c-adapter) (list (cons 0 (sealing-adapter)))))
                    (cons
                     'replace
                     (delegating->
                      2
                      (list (cons 0 (sealing-adapter)))
                      (any/c-adapter)
                      (list)))))
                  (delegating-class/c
                   (list)
                   (list (cons 'my-stacks (delegating-listof (sealing-adapter))))
                   (list
                    (cons
                     'get-field:my-stacks
                     (delegating->
                      0
                      (list)
                      (any/c-adapter)
                      (list (cons 0 (delegating-listof (sealing-adapter))))))
                    (cons
                     'set-field:my-stacks
                     (delegating->
                      1
                      (list (cons 0 (delegating-listof (sealing-adapter))))
                      (any/c-adapter)
                      (list)))))))))
              (any/c-adapter)
              (list (cons 0 (sealing-adapter)))))))
          player%
          #f
          #f))
       (provide (rename-out (player%1402046 player%)))))
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (require "../../../utilities/require-typed-check-provide-transient.rkt")
   (reprovide "card-adapted.rkt")
   (provide Name Face Bulls)
   (provide CardPool% CardPool Hand)
   (provide Stack)
   (provide BaseDeck% PlayerDeck% DealerDeck% Deck% BaseDeck PlayerDeck DealerDeck Deck)
   (provide Result Internal% Internal Dealer% Dealer)
   (provide Player% Player)
   (provide (struct-out card) Card)
   (define-type Name Natural)
   (define-type Face Natural)
   (define-type Bulls Natural)
   (define-type
    CardPool%
    (Class
     (init-field
      (shuffle (-> (Listof Card) (Listof Card)) #:optional)
      (random-bulls (-> Bulls) #:optional))
     (get-field:shuffle (-> (-> (Listof Card) (Listof Card))))
     (get-field:random-bulls (-> (-> Bulls)))
     (set-field:shuffle (-> (-> (Listof Card) (Listof Card)) Void))
     (set-field:random-bulls (-> (-> Bulls) Void))
     (draw-card (-> Card))
     (draw-hand (-> Hand))))
   (define-type CardPool (Instance CardPool%))
   (define-type Hand (Listof Card))
   (define-type Stack (Listof Card))
   (define-type
    BaseDeck%
    (Class
     (init-field (cards0 (Listof Card)))
     (field (my-stacks (Listof Stack)))
     (get-field:cards0 (-> (Listof Card)))
     (get-field:my-stacks (-> (Listof Stack)))
     (set-field:cards0 (-> (Listof Card) Void))
     (set-field:my-stacks (-> (Listof Stack) Void))))
   (define-type PlayerDeck% (Class #:implements/inits BaseDeck% (fewest-bulls (-> Stack))))
   (define-type
    DealerDeck%
    (Class
     #:implements/inits
     BaseDeck%
     (fit (-> Card Stack))
     (push (-> Card Void))
     (replace (-> Stack Card Natural))
     (replace-stack (-> Card (U Card (Listof Card)) Natural))
     (larger-than-some-top-of-stacks? (-> Card Boolean))))
   (define-type
    Deck%
    (Class
     #:implements/inits
     BaseDeck%
     (fewest-bulls (-> Stack))
     (fit (-> Card Stack))
     (push (-> Card Void))
     (replace (-> Stack Card Natural))
     (replace-stack (-> Card (U Card (Listof Card)) Natural))
     (larger-than-some-top-of-stacks? (-> Card Boolean))))
   (define-type BaseDeck (Instance BaseDeck%))
   (define-type PlayerDeck (Instance PlayerDeck%))
   (define-type DealerDeck (Instance DealerDeck%))
   (define-type Deck (Instance Deck%))
   (define-type Result (List (List Symbol Natural) (Listof (List Name Natural))))
   (define-type
    Internal%
    (Class
     #:implements
     Player%
     (init-field (player Player))
     (field (my-bulls Natural))
     (get-field:player (-> Player))
     (get-field:my-bulls (-> Natural))
     (set-field:player (-> Player Void))
     (set-field:my-bulls (-> Natural Void))
     (bulls (-> Natural))
     (add-score (-> Natural Void))))
   (define-type Internal (Instance Internal%))
   (define-type
    Dealer%
    (Class
     (init-field (players (Listof Player)))
     (field (internal% Internal%) (internals (Listof Internal)))
     (get-field:players (-> (Listof Player)))
     (get-field:internal% (-> Internal%))
     (get-field:internals (-> (Listof Internal)))
     (set-field:players (-> (Listof Player) Void))
     (set-field:internal% (-> Internal% Void))
     (set-field:internals (-> (Listof Internal) Void))
     (present-results (-> Natural Result))
     (any-player-done? (-> Boolean))
     (play-round (-> (-> (Listof Card) (Listof Card)) (-> Bulls) Void))
     (play-game (->* () ((-> (Listof Card) (Listof Card)) (-> Bulls)) Result))))
   (define-type Dealer (Instance Dealer%))
   (define-type
    Player%
    (Class
     (init-field (n Name) (order (-> (Listof Card) (Listof Card)) #:optional))
     (field (my-cards (Listof Card)))
     (get-field:n (-> Name))
     (get-field:order (-> (-> (Listof Card) (Listof Card))))
     (get-field:my-cards (-> (Listof Card)))
     (set-field:n (-> Name Void))
     (set-field:order (-> (-> (Listof Card) (Listof Card)) Void))
     (set-field:my-cards (-> (Listof Card) Void))
     (name (-> Name))
     (start-round (-> (Listof Card) Void))
     (start-turn (-> Deck Card))
     (choose (-> Deck Stack))))
   (define-type Player (Instance Player%))
   (struct card ((face : Face) (bulls : Bulls)) #:prefab #:type-name Card)
   (require/typed/check/provide 'contracted (create-dealer (-> (Listof Player) Dealer)))
   (require/typed/check/provide 'contracted (player% Player%))))
