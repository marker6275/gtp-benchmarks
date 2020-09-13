take5-mixin
===


History
---

This is a variant of the take5 benchmark.

Original code: <https://github.com/mfelleisen/take5>

Original benchmark: <https://github.com/nuprl/gradual-typing-performance>

Differences from original:

- relaxed some types (e.g. from `(U 2 3 4 ...)` to `Natural`)
- removed as many casts as possible
- more LOOPS in `main.rkt`
- seperated the two mixins originally in `deck.rkt` into their own modules (`for-player.rkt` and `for-dealer.rkt`)
