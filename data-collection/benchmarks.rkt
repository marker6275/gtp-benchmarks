#lang racket/base

(provide benchmarks
         (struct-out benchmark))

(struct benchmark (main others))

(define benchmarks
  (hash "snake"
        (benchmark "snake/untyped/main.rkt"
                   '("snake/untyped/collide.rkt"
                     "snake/untyped/const.rkt"
                     "snake/untyped/cut-tail.rkt"
                     "snake/untyped/data.rkt"
                     "snake/untyped/handlers.rkt"
                     "snake/untyped/motion-help.rkt"
                     "snake/untyped/motion.rkt"))

        "dungeon"
        (benchmark "dungeon/untyped/main.rkt"
                   '("dungeon/untyped/cell.rkt"
                     "dungeon/untyped/grid.rkt"
                     "dungeon/untyped/message-queue.rkt"
                     "dungeon/untyped/utils.rkt"))

        "forth"
        (benchmark "forth/untyped/main.rkt"
                   '("forth/untyped/command.rkt"
                     "forth/untyped/eval.rkt"
                     "forth/untyped/stack.rkt"))))
