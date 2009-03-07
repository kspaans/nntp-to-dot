#lang scheme

(require net/nntp)

;; A first try with connecting to the newsgroup and downloading some posts

(define uwnews (connect-to-server "news.uwaterloo.ca"))

(define-values (total first last) (open-news-group uwnews "uw.cs.cs136"))

(printf "~a : ~a : ~a~n~n" total first last)

;(for-each (lambda (x) (printf "~a~n" x)) (head-of-message uwnews 6038))

(define from-regexp (make-desired-header "From"))
(define mid-regexp (make-desired-header "Message-ID"))
(define from-and-mid (extract-desired-headers
                       (head-of-message uwnews 6038)
                       (list from-regexp mid-regexp)))

(printf "From:  ~a~nMID:   ~a~n~n" (car from-and-mid) (cadr from-and-mid))

(disconnect-from-server uwnews)
(display "Disconnected.\n")
