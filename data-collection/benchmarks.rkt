#lang racket/base

(provide benchmarks
         (struct-out benchmark)
         resolve-bench-path)

(require racket/runtime-path
         racket/path)

(struct benchmark (main others))

(define benchmarks
  (hash
   ;; for testing
   "mutant-test"
   (benchmark "mutant-test/d.rkt"
              '("mutant-test/e.rkt"))

   "snake"
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
                "forth/untyped/stack.rkt"))

   "sieve"
   (benchmark "sieve/untyped/main.rkt"
              '("sieve/untyped/streams.rkt"))

   "kcfa"
   (benchmark "kcfa/untyped/main.rkt"
              '("kcfa/untyped/ai.rkt"
                "kcfa/untyped/benv.rkt"
                "kcfa/untyped/denotable.rkt"
                "kcfa/untyped/main.rkt"
                "kcfa/untyped/structs.rkt"
                "kcfa/untyped/time.rkt"
                "kcfa/untyped/ui.rkt"))))

(define-runtime-path benchmarks-dir-path "../benchmarks/")
(define (resolve-bench-path p)
  (simple-form-path (build-path benchmarks-dir-path p)))
