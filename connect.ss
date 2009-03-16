#lang scheme

(require net/nntp)

;; A first try with connecting to the newsgroup and downloading some posts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data structure to capture interactions on the newsgroup.
;;  Want to have FROM and TO fields. Ideally TO should be a list of
;;   people who the from has interacted with. Or perhaps use a hash table?
;;   Yes, want constant insert time for new users (read: node), and then
;;   value can be a list of other users that this user has interacted with.
;;  Should I differentiate between interactions in FROM and TO?
;;  ^^^^^^^^^ I don't think I can. Especailly since I can make it an
;;   undirected graph.

(define users (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hash-table-put! users 'kspaans@csclub.uwaterloo.ca '())
(hash-table-put! users 'kspaans@csclub.uwaterloo.ca
                       (cons 'nosane@user.com
		             (hash-table-get users 'kspaans@csclub.uwaterloo.ca)))
;; ^^^^ This is kind of tricky? Because I'll have to search the hashtable for each
;;   value _while_ inserting it and adding to it... This seems indeal.
;; Since hastables are mutable, maybe I can integrate getting they key into the
;; whole process? I.E. search to make sure it's already there, and meanwhile
;; save the value. If it's not already there, revert to adding the user.


(hash-table-for-each users (lambda (x y) (printf "~a~n" y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
