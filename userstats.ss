;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; Calculate a bunch of user statistics:
;; - Number of users
;; - Post counts of users
;; - Average number of posts
;; - posts per day...
;; - new users versus time...

#lang scheme

(require "common.ss")
(require srfi/19)

(provide count-users)

;; User Statistics Struct
;; String - usename in the form of an email address, possibility of duplicates
;; int    - total number of posts in newsgroup
;; date   - SRFI-19 date struct, date of first post to group
;; date   - "     "    ", date of last post to struct
;; int    - number of days in range from first post day to past post day
;; real   - number of posts / number of days
(define-struct ustats (user nump firstp lastp days avgppd))

;; Collect stats in the form of '(USERNAME ustats)
(define users (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print-stats: void -> void
;; Pretty print the contents of the user statistics table to stdout
(define (print-stats)
  (printf "Total number of users: ~a~n~n" (hash-count users))
  (hash-for-each
    users
    (lambda (k v)
      (printf "User: ~a~nCount: ~a~nFirst: ~a~nLast:  ~a~nAvgPPD: ~a~n~n"
              (ustats-user v)
              (ustats-nump v)
              (ustats-firstp v)
              (ustats-lastp v)
              (ustats-avgppd v)))))

;; count-users: int int newsgroup -> void
;; build a hash table with each user from the group collecting stats:
;; - number of posts
;; - first/last post
;; So if "last" post is empty, then user has only 1 post
(define (count-users first last newsd)
  (cond
    [(= first last) (print-stats)]
    [else (letrec [(message (message-getter newsd first (list from-regexp date-regexp)))
                   (mesg-from (if (boolean? message) message (car message)))
                   (mesg-date (if (boolean? message) message (cadr message)))]
            (cond
              [(boolean? message) (void)]
              [else
               (let [(result (hash-ref users mesg-from #f))]
                 (cond
                   [(boolean? result)
                    ;(printf "First post ~a ::@:: ~a~n" mesg-from mesg-date)
                    (hash-set! users mesg-from (make-ustats mesg-from 1 (get-date mesg-date) '() 1 1))]
                   [else
                                     ;; Calculate the ordinal day of the year (1-365) of the given date
                                     ;; then subtract. Alternatively date->time and time-difference
                                     ;; functions can be used to calculate this
                    (let [(pdays (if (empty? (ustats-lastp result))
                                     1
                                     (+ 1 (- (date-year-day (ustats-lastp result))
                                             (date-year-day (ustats-firstp result))))))
                          (new-nump (+ 1 (ustats-nump result)))]
                    (hash-set! users mesg-from (make-ustats
                                                 mesg-from
                                                 new-nump
                                                 (ustats-firstp result)
                                                 (get-date mesg-date)
                                                 pdays
                                                 (/ new-nump pdays))))]))]))
          (count-users (+ first 1) last newsd)]))
