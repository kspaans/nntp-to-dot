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

(provide count-users new-u-vs-time posts)

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

;; Collect some other, extra stats, store in ngposts struct
(define pcounts (make-hash))
;; NewsGroup Posts Struct
;; string - username ("From:" header)
;; int    - Total posts by user
;; int    - "post" posts by user
;; int    - "reply" posts by user
(define-struct ngposts (user tp np nr))

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
    [else (let* [(message (message-getter newsd first (list from-regexp date-regexp)))
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

;; new-u-vs-time: void -> void
;; Prints out info (similar to PPD) representing the number of new users showing up
;; on the newsgroup (e.g. first posts) plotted versus time. This will be plottable with GNUPLOT.
;; First map all first post instances to a new hash table that will count the number of first
;; posts on each day. Then map over that hash table to print them out appropriately.
(define (new-u-vs-time)
  (let [(new-u-hash (make-hash))]
    (hash-for-each
      users
      (lambda (k v)
        (let* [(ndate (date->string (ustats-firstp v) "~D"))
               (result (hash-ref new-u-hash ndate #f))]
          (cond
            [(boolean? result) (hash-set! new-u-hash ndate 1)]
            [else (hash-set! new-u-hash ndate (+ 1 result))]))))
    (hash-for-each new-u-hash
                   (lambda (k v) (printf "~a ~a~n" k v)))))

;; posts: int int usergroup
;; Counts the number of posts of each user, mapping email to post count.
(define (posts first last newsd)
  (cond
    [(= first last)
     (hash-for-each
       pcounts
       (lambda (k v)
         (printf "~a\t~a~n" k (ngposts-tp v))))]
    [else
     (let* [(message (message-getter newsd first (list from-regexp ref-regexp)))
            (mesg-from (if (boolean? message) #f (car message)))
            (mesg-refs (if (and (not (boolean? message)) (= 2 (length message)))
                           #t;(get-refs (cadr message)) ; only need to know, right?
                           #f))]
       (cond
         [(boolean? message) (posts (add1 first) last newsd)]
         [else
          (let [(result (hash-ref pcounts mesg-from #f))]
            (cond
              [(boolean? result)
               (if mesg-refs
                   (hash-set! pcounts mesg-from (make-ngposts mesg-from 1 0 1))
                   (hash-set! pcounts mesg-from (make-ngposts mesg-from 1 1 0)))
               (posts (add1 first) last newsd)]
              [else
               (if mesg-refs
                   (hash-set! pcounts mesg-from (make-ngposts
                                                  mesg-from
                                                  (add1 (ngposts-tp result))
                                                  (ngposts-np result)
                                                  (add1 (ngposts-nr result))))
                   (hash-set! pcounts mesg-from (make-ngposts
                                                  mesg-from
                                                  (add1 (ngposts-tp result))
                                                  (add1 (ngposts-np result))
                                                  (ngposts-nr result))))
               (posts (add1 first) last newsd)]))]))]))
