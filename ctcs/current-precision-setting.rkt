#lang racket

(provide current-precision-config
         precision-configs)

(define precision-configs
  '(none types max))

;; Each precision must be a member of ^; modify to change all
;; configurable-ctc precision levels inside a given module.
;; Paths must be relative to the benchmarks base directory.
(define current-precision-config
  (hash "forth/untyped/main.rkt" 'none
        "forth/untyped/command.rkt" 'none
        "forth/untyped/eval.rkt" 'none
        "forth/untyped/stack.rkt" 'none

        "dungeon/untyped/main.rkt" 'none
        "dungeon/untyped/cell.rkt" 'none
        "dungeon/untyped/grid.rkt" 'none
        "dungeon/untyped/message-queue.rkt" 'none
        "dungeon/untyped/utils.rkt" 'none
        "dungeon/base/un-types.rkt" 'none

        "snake/untyped/main.rkt" 'none
        "snake/untyped/collide.rkt" 'none
        "snake/untyped/const.rkt" 'none
        "snake/untyped/cut-tail.rkt" 'none
        "snake/untyped/data.rkt" 'none
        "snake/untyped/handlers.rkt" 'none
        "snake/untyped/motion-help.rkt" 'none
        "snake/untyped/motion.rkt" 'none

        "kcfa/untyped/ai.rkt" 'max
        "kcfa/untyped/benv.rkt" 'max
        "kcfa/untyped/denotable.rkt" 'max
        "kcfa/untyped/main.rkt" 'max
        "kcfa/untyped/structs.rkt" 'max
        "kcfa/untyped/time.rkt" 'max
        "kcfa/untyped/ui.rkt" 'max

        "sieve/untyped/main.rkt" 'max
        "sieve/untyped/streams.rkt" 'max))
