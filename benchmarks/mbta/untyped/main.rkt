#lang racket

;; ===================================================================================================
(require "run-t.rkt"
         "data.rkt"
         "helpers.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         (only-in racket/string string-join))

;; ===================================================================================================
(define/contract (dat->station-names fname)
  (configurable-ctc
   [max (->i ([fname (and/c string? (λ (f) (file-exists? f)))])
             [result (fname)
                     (and/c (listof station?)
                            (λ (lst)
                              (sublist? lst (file->lines fname))))])]            
   #;[max/sub1 (-> (and/c string? (λ (f) (file-exists? f)))
                 (listof station?))]             
   [types (-> string? (listof string?))])
  (for/list ([line (in-list (file->lines fname))]
             #:when (and (< 0 (string-length line))
                         (not (eq? #\- (string-ref line 0)))))
    (string-trim line)))

(define/contract BLUE-STATIONS
  (configurable-ctc
   [max (and/c (listof station?)
               (λ (lst)
                 (sublist? lst (file->lines "../base/blue.dat"))))]
   #;[max/sub1 (listof station?)]
   [types (listof string?)])
  (dat->station-names "../base/blue.dat"))

(define/contract ORANGE-STATIONS
  (configurable-ctc
   [max (and/c (listof station?)
               (λ (lst)
                 (sublist? lst (file->lines "../base/orange.dat"))))]
   #;[max/sub1 (listof station?)]
   [types (listof string?)])
  (dat->station-names "../base/orange.dat"))

;; String String -> String
(define/contract (path from to)
  (configurable-ctc
   [max (->i ([from string?]
              [to string?])
             [result (from to)
                     (λ (res)
                       (ordered-substrings? (list "from" from "to" to) res))])]
   #;[max/sub1 (->i ([from string?]
                   [to string?])
                  [result (from to)
                          (λ (res)
                            (and (substring? from res)
                                 (substring? to res)))])]
   [types (-> string? string? string?)])
  (format "from ~a to ~a" from to))

;; String -> String
(define/contract (enable s)
  (configurable-ctc
   [max (->i ([s string?])
             [result (s)
                     (λ (res)
                       (ordered-substrings? (list "enable" s) res))])]
   #;[max/sub1 (->i ([s string?])
                  [result (s)
                          (λ (res)
                            (substring? s res))])]
   [types (-> string? string?)])
  (format "enable ~a" s))

(define/contract (disable s)
  (configurable-ctc
   [max (->i ([s string?])
             [result (s)
                     (λ (res)
                       (ordered-substrings? (list "disable" s) res))])]
   #;[max/sub1 (->i ([s string?])
                  [result (s)
                          (λ (res)
                            (substring? s res))])]
   [types (-> string? string?)])
  (format "disable ~a" s))

;; ===================================================================================================

(define/contract (assert result expected-length)
  (string? natural? . -> . void?)
  (define num-result (length (string-split result "\n")))
  (unless (= num-result expected-length)
    (error (format "Expected ~a results, got ~a\nFull list:~a"
                   expected-length
                   num-result
                   result))))

(define/contract (main)
  any/c
  (define (run-query str)
    (define r (run-t str))
    (if r
        r
        (error 'main (format "run-t failed to respond to query ~e\n" str))))
  (assert (run-query (path "Airport" "Northeastern")) 14)
  (assert (run-query (disable "Government")) 1)
  (assert (run-query (path "Airport" "Northeastern")) 16)
  (assert (run-query (enable "Government")) 1)
  (assert (run-query (path "Airport" "Harvard Square")) 12)
  (assert (run-query (disable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 1) ;;impossible path
  (assert (run-query (enable "Park Street")) 1)
  (assert (run-query (path "Northeastern" "Harvard Square")) 12)
  ;; --
  (for* ([s1 (in-list ORANGE-STATIONS)] [s2 (in-list BLUE-STATIONS)])
    (run-query (path s1 s2))))

(time (main))
