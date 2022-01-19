#lang racket/base

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; this file contains functions to convert text into sounds

;;bg
;; Original file would make a SOUND from the sequence of dots and dashes.
;; We just make the . and -

(provide string->morse)

(require "morse-code-table.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         racket/contract
         racket/match
         racket/string)

(define/ctc-helper ((morse-decodes-to? str/char) morse-str)
  (define target-str (if (char? str/char)
                         (string str/char)
                         str/char))
  (define encoded-target
    (apply string-append
           (map (λ (c) (hash-ref char-table (char-downcase c)))
                (string->list target-str))))
  (string=? encoded-target morse-str))

;; map a character to a dit-dah string
(define/contract (char->dit-dah-string letter)
  (configurable-ctc
   [max (->i ([letter char?])
             #:pre (letter) (hash-has-key? char-table (char-downcase letter))
             [result (letter)
                     (and/c morse-string?
                            (morse-decodes-to? letter))])]
   [types (-> char? string?)])
  (define res (hash-ref char-table (char-downcase letter) #f))
  (if (eq? #f res)
    (raise-argument-error 'letter-map "character in map"
                              0 letter)
    res))

(define/contract (string->morse str)
  (configurable-ctc
   [max (->i ([str string?])
             [result (str)
                     (and/c morse-string? (morse-decodes-to? str))]
             #:post (str result)
             (if (non-empty-string? str)
                 (non-empty-string? result)
                 (string=? result "")))]
   [types (-> string? string?)])

  (define morse-list (for/list ([c str])
      (char->dit-dah-string c)))
  (apply string-append morse-list))
