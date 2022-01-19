#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  require-typed-check
  (only-in racket/file file->value)
  "../../../ctcs/precision-config.rkt"
  "../../../ctcs/common.rkt"
  racket/contract
  racket/random)

(require (only-in "morse-code-strings.rkt"
  string->morse))

(require (only-in "levenshtein.rkt"
               string-levenshtein))

(define/contract word-frequency-list
  string?
  "./../base/frequency.rktd")
(define/contract word-frequency-list-small
  string?
  "./../base/frequency-small.rktd")

(define/contract (file->words filename)
  (-> path-string? (listof string?))
  (define words+freqs (file->value (string->path filename)))
  (for/list ([word+freq  words+freqs])
    (car word+freq)))

(define/contract allwords (listof string?) (file->words word-frequency-list))

(define/contract words-small
  (listof string?)
  (file->words word-frequency-list-small))

;; ll: `words-small` far too large
(define/contract words-smaller
  (listof string?)
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 42)
    (random-sample words-small 10
                   #:replacement? #f)))

(define/contract (main words)
  (-> (listof string?) void?)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    ;; ll: this gets done by the loop anyway: no need to double it
    ;; (string-levenshtein w2 w1)
    (void)))

(time (main words-smaller))
