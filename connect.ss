#lang scheme

(require net/nntp)
(require "ref-helper.ss")

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

;; Need to use the new version of hash tables.
(define users (make-hash))

(define httest (make-hash))
(define refers (make-hash))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hash-set! users 'kspaans@csclub.uwaterloo.ca '())
(hash-set! users 'kspaans@csclub.uwaterloo.ca
                       (cons 'nosane@user.com
		             (hash-ref users 'kspaans@csclub.uwaterloo.ca)))
;; ^^^^ This is kind of tricky? Because I'll have to search the hashtable for each
;;   value _while_ inserting it and adding to it... This seems indeal.
;; Since hastables are mutable, maybe I can integrate getting they key into the
;; whole process? I.E. search to make sure it's already there, and meanwhile
;; save the value. If it's not already there, revert to adding the user.


;(hash-for-each users (lambda (x y) (printf "~a~n" y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uwnews (connect-to-server "news.uwaterloo.ca"))

(define-values (total first last) (open-news-group uwnews "uw.cs.cs136"))

(printf "~a : ~a : ~a~n~n" total first last)


;(for-each (lambda (x) (printf "~a~n" x)) (head-of-message uwnews 6038))

(define from-regexp (make-desired-header "From"))
(define mid-regexp (make-desired-header "Message-ID"))
(define ref-regexp (make-desired-header "References"))
(define subj-regexp (make-desired-header "Subject"))
;(define from-and-mid (extract-desired-headers
;                       (head-of-message uwnews 6038)
;                       (list from-regexp mid-regexp)))

;(printf "From:  ~a~nMID:   ~a~n~n" (car from-and-mid) (cadr from-and-mid))

;; message-getter: newsgroup_connector number regexp -> (union string false)
;; Do the dirty work of reading the message header info from the newsgroup.
;; Returns false if the article cannot be retreived, and a string otherwise.
(define (message-getter group article headers)
  (with-handlers ([article-not-in-group? (lambda (x) #f)])
    (extract-desired-headers (head-of-message group article)
                             headers)))

;; read-all: int int newsgroup -> void
;; recurse over all possible message "numbers" from the newsgroup
;;   I wonder what will happen with the messages that slrn doesn't let
;;   me read?
(define (read-all first last newsd)
  (cond
    [(= first last) (printf "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")]
    ;;    ^^^^ Screw it, I'll just increment first as I recurse.
    [else (local ((define mesg-from
                          (message-getter uwnews first (list from-regexp))))
          (cond [(not (boolean? mesg-from))
                 (let ((in-table (hash-ref httest mesg-from #f)))
                   (cond [(false? in-table) (hash-set! httest mesg-from 1)]
                         [else (hash-set! httest mesg-from (+ 1 in-table))]))]))
          (read-all (+ first 1) last newsd)]))

;(printf "-------------------------------------------------------~n~n~n~n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nntp-map: operation newsgroup -> void
;; maps some operation across each newsgroup post
;;;(define (nntp-map op newsd)
;;;  (let-values ([(total first-id last-id) (open-news-group newsd "uw.cs.cs136")])
;;;    (op newsd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; thread-depth: newsgroup article -> void
;; Given a newsgroup article, prints the depth of the thread that it's in.
;;   OH GAWD this sounds inefficient, but should be a good prototype.
;(define thread-dpeth

;; look at head of the first few messages
(define (thread-all first last newsd)
  (cond
    [(= first last) (printf "****************\n")]
    [else (local ((define mesg-from (message-getter uwnews first
			                            (list from-regexp
						          mid-regexp
							  ref-regexp
							  subj-regexp))))
            (cond
	      [(and (not (boolean? mesg-from)) (> (length mesg-from) 3))
               (printf "From: ~a~nSubj: ~a~nMID:  ~a~nRefs: ~a~n~n"
		       (car mesg-from) (cadr mesg-from) (caddr mesg-from) (get-refs (cadddr mesg-from)))]
              [(not (boolean? mesg-from))
	       (printf "From: ~a~nSubj: ~a~nMID:  ~a~n~n"
        	       (car mesg-from) (cadr mesg-from) (caddr mesg-from))]))
          (thread-all (+ first 1) last newsd)]))

(thread-all first (+ first 20) uwnews)

;(read-all first last uwnews)
;(read-all first (+ first 1000) uwnews)

;; Now let's see what's in the hash table
;(printf "########~n")
;(printf "Size of Table: ~a~n" (hash
;(hash-for-each httest (lambda (x y) (printf "-->~a\t\t~a~n" x y)))
;(printf "########~n")

(disconnect-from-server uwnews)
;(display "Disconnected.\n")
