;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; NNTP-TO-DOT - perform various analyses of a newsgroup, generate some
;;               DOT code for plotting with graphviz
;;  FEATURES:
;; - Generate a graph depicting all interactions between users on the group
;; - Count the number of posts made per day

#lang scheme

(require net/nntp)
(require "common.ss")
(require "userrel.ss")
(require "thread.ss")
(require "ppd.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data structure to capture interactions on the newsgroup.
;;  Want to have FROM and TO fields. Ideally TO should be a list of
;;   people who the from has interacted with. Or perhaps use a hash table?
;;   Yes, want constant insert time for new users (read: node), and then
;;   value can be a list of other users that this user has interacted with.
;;  Should I differentiate between interactions in FROM and TO?
;;  ^^^^^^^^^ I don't think I can. Especailly since I can make it an
;;   undirected graph.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dotfile (open-output-file "data/cs136-trial.dot" #:exists 'truncate))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uwnews (connect-to-server "news.uwaterloo.ca"))
(define-values (total first last) (open-news-group uwnews "uw.cs.cs136"))

;; read-all: int int newsgroup -> void
;; recurse over all possible message "numbers" from the newsgroup
(define (read-all first last newsd)
  (cond
    [(= first last) (printf "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")]
    [else (let [(message (message-getter uwnews first (list from-regexp subj-regexp mid-regexp ref-regexp)))]
            (cond
              [(boolean? message) (void)]
              [else (for-each (lambda (header) (printf "~a~n" header))
                              message)])
            (newline))
          (read-all (+ first 1) last newsd)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nntp-map: operation newsgroup (listof header-regexp) -> void
;; maps some operation across each newsgroup post
;;;(define (nntp-map op newsd hregexps)
;;;  (let-values ([(total first-id last-id) (open-news-group newsd "uw.cs.cs136")])
;;;    (op newsd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(read-all first (+ 100 first) uwnews)
;;(userrel first (+ 800 first) uwnews dotfile)

;;(begin
;;  (hash-map users (lambda (x y) (printf "~a]]] ~a~n~n" x y)))
;;  "_ _ _")
;;(begin
;;  (hash-map mids (lambda (x y) (printf "~a]]] ~a~n~n" x y)))
;;  "_ _ _")

;;(read-all first last uwnews)
;;(read-all first (+ first 1000) uwnews)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE TO EXECUTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(posts-per-day first last uwnews)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(close-output-port dotfile)
(disconnect-from-server uwnews)
