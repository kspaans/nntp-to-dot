;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; Count the number of posts per day on the newsgroup

#lang scheme

(require srfi/19)
(require "common.ss")

(provide posts-per-day)

(define dates (make-hash))
(define date-list '())

;; dates-to-count: (listof date) -> void
;; Uses the global "dates" hash table to store counts (values) related to unique
;; days (keys) that are built up using posts-per-day and stored in date-list
(define (dates-to-count dlst)
  (cond
    [(empty? dlst) (hash-for-each dates (lambda (d c) (printf "~a ~a~n" d c)))]
    [else
     (letrec [(date (date->string (car dlst) "~D"))
              (count (hash-ref dates date #f))]
       (cond
         [(boolean? count) (hash-set! dates date 1) (dates-to-count (cdr dlst))]
         [else (hash-set! dates date (+ 1 count)) (dates-to-count (cdr dlst))]))]))

;; posts-per-day: int int newsgroup -> void
;; Print out the number of posts per day on the newsgroup, in a format ready for
;; gnuplot to plot.
;; Date headers seem to be either 32,36,37 or 43 chars long
;; --> Parse date, save to a list, For-each save $DAY into hash table with count
(define (posts-per-day first last newsd)
  (cond
    [(= first last) (dates-to-count date-list)]
    [else (let [(message (message-getter newsd first (list date-regexp)))]
            (cond
              [(boolean? message) (void)]
              [else (set! date-list (cons (get-date (car message)) date-list))]))
          (posts-per-day (+ first 1) last newsd)]))
