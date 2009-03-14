#lang scheme

(require net/nntp)

;; A first try with connecting to the newsgroup and downloading some posts

(define uwnews (connect-to-server "news.uwaterloo.ca"))

(define-values (total first last) (open-news-group uwnews "uw.cs.cs136"))

(printf "~a : ~a : ~a~n~n" total first last)

;; read-all: int int -> void
;; recurse over all possible message "numbers" from the newsgroup
;;   I wonder what will happen with the messages that slrn doesn't let
;;   me read?
(define (read-all first last newsd)
  (cond
    [(= first last) (printf "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")]
    ;;    ^^^^ Screw it, I'll just increment first as I recurse.
    [else (printf "MID: ~a~n~s\n\n"
            first
            (with-handlers ([article-not-in-group?
                             (lambda (x) (format "Post ~a probably deleted.~n"
                                         (article-not-in-group-article x)))])
              (body-of-message newsd first)))
          (read-all (+ first 1) last newsd)]))

;(for-each (lambda (x) (printf "~a~n" x)) (head-of-message uwnews 6038))

;(define from-regexp (make-desired-header "From"))
;(define mid-regexp (make-desired-header "Message-ID"))
;(define from-and-mid (extract-desired-headers
;                       (head-of-message uwnews 6038)
;                       (list from-regexp mid-regexp)))

;(printf "From:  ~a~nMID:   ~a~n~n" (car from-and-mid) (cadr from-and-mid))

;(printf "-------------------------------------------------------~n~n~n~n")

(read-all first last uwnews)

(disconnect-from-server uwnews)
(display "Disconnected.\n")
