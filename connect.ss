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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define users (make-hash))
(define httest (make-hash))
(define refers (make-hash))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dotfile (open-output-file "cs136-trial.dot" #:exists 'truncate))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hash-set! users 'kspaans@csclub.uwaterloo.ca '())
(hash-set! users 'kspaans@csclub.uwaterloo.ca
                       (cons 'nosane@user.com
		             (hash-ref users 'kspaans@csclub.uwaterloo.ca)))

(fprintf dotfile "digraph cs136 {\n")
(fprintf dotfile "// Trial run starting at: ~a\n" (current-seconds))
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


;-------------------------
;-------------------------
;;; Want to put the message ID into the hash table with some kind of other unique ID
;;; and then match references against the message ID to discover threads.
;;; Directed edges will lead _away_ from the original post (towards follow-ups)
;-------------------------
;-------------------------

;; look at head of the first few messages
(define (thread-print first last newsd)
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
          (thread-print (+ first 1) last newsd)]))

(define (thread-hash first last newsd)
  (cond
    [(= first last) (printf "@@@@@@@@@@@@@@@@\n")]
    [else (local [(define mesg-from (message-getter uwnews first
			                            (list mid-regexp  ; Ugh, the order coming out of this function
						          from-regexp ; depends on what's in the headers, not the
							  ref-regexp  ; order I have here. Usually From, Subj, MID, Refs
							  subj-regexp)))
                  (define node-id (make-dot-id))]
            (cond
	      [(and (not (boolean? mesg-from)) (> (length mesg-from) 2))
               (let [(result (map (lambda (x) (hash-ref refers x #f))
                                  (get-refs (caddr mesg-from))))
                     (mesg-ID (get-refs (caddr mesg-from)))]
                 (cond
                   [(boolean? (car result))
                    (printf "----~nInserting MID(~a) into hash table.~n" (car mesg-ID))
                      (fprintf dotfile "// Node ~a\n    ~a;\n" mesg-ID node-id)
                      (hash-set! refers (car mesg-ID) node-id)]
                   [else (printf "MIDs already in hash table?~n    >>~a<<~n" (caddr mesg-from))]))
                 ;(if (> (length (car mesg-ID)) 1)
                 ;    (printf "Exciting, more than one reference!~n")
                 ;    (void)))
               (cond [(> (length mesg-from) 3)
                      (printf "Checking References to find threading...~n")
                      (let* [(Refs (get-refs (cadddr mesg-from)))
                             (hRef (hash-ref refers (car Refs) #f))]
                        (printf "Refs:     ~a~n" Refs)
                        (printf "          Is it in the table? ~a~n~n" hRef)
                        (if (boolean? hRef) (void) ; (printf "          Nope.~n~n")
                            (fprintf dotfile "    ~a -> ~a;\n" hRef node-id)))]
                     [else ;(printf "Headers:\t ~a~n~n" mesg-from)])]
                       (for-each (lambda (z) (printf "\t~a~n" z)) mesg-from)
                       (newline)])]
              [(not (boolean? mesg-from))
               (printf "Pooppoop!~n")
               (printf "From: ~a~nSubj: ~a~nMID:  ~a~n~n"
        	       (car mesg-from) (cadr mesg-from) (caddr mesg-from))]))
          (thread-hash (+ first 1) last newsd)]))

;(thread-print first (+ first 20) uwnews)
(thread-hash first (+ first 100) uwnews)
;; Want better threading now. Use the second pair in the references line


;(read-all first last uwnews)
;(read-all first (+ first 1000) uwnews)
(fprintf dotfile "// Trial run finished at: ~a\n}\n" (current-seconds))

(close-output-port dotfile)
(disconnect-from-server uwnews)
;(display "Disconnected.\n")
