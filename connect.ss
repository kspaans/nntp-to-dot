;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

#lang scheme

(require net/nntp)
(require srfi/19)
(require "ref-helper.ss")

(provide uwnews total first last read-all posts-per-day thread-print
         thread-hash userrel)
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
(define mids (make-hash))
(define httest (make-hash))
(define refers (make-hash))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dotfile (open-output-file "cs136-trial.dot" #:exists 'truncate))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fprintf dotfile "digraph cs136 {\n")
(fprintf dotfile "// Trial run starting at: ~a\n" (current-seconds))
(fprintf dotfile "ranksep = 3\n")
;(fprintf dotfile "nodesep = 1.0\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uwnews (connect-to-server "news.uwaterloo.ca"))
(define-values (total first last) (open-news-group uwnews "uw.cs.cs136"))
(printf "~a : ~a : ~a~n~n" total first last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define from-regexp (make-desired-header "From"))
(define mid-regexp (make-desired-header "Message-ID"))
(define ref-regexp (make-desired-header "References"))
(define subj-regexp (make-desired-header "Subject"))
(define date-regexp (make-desired-header "Date"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; posts-per-day: int int newsgroup -> void
;; Print the date of each message in a format appropriate for gnuplot
;;  "date number"
;; Date headers seem to be either 32,36,37 or 43 chars long
(define (posts-per-day first last newsd)
  (cond
    [(= first last) (void)]
    [else (let [(message (message-getter uwnews first (list date-regexp)))]
            (cond
              [(boolean? message) (void)]
              [else (printf "~a~n~a~n" (string-length (car message)) (car message))
               (let [(mlen (string-length (car message)))]
               (cond
                 [(= 32 mlen) (printf "~a~n" (string->date (substring (car message) 5) "~d ~b ~Y ~H:~M:~S ~z"))]
                 [(= 36 mlen) (printf "~a~n" (string->date (substring (car message) 5) "~a, ~d ~b ~Y ~H:~M:~S ~z"))]
                 [(= 37 mlen) (printf "~a~n" (string->date (substring (car message) 5) "~a, ~d ~b ~Y ~H:~M:~S ~z"))]
                 [(= 43 mlen) (printf "~a~n" (string->date (substring (car message) 5) "~a, ~d ~b ~Y ~H:~M:~S ~z"))]))]))
          (posts-per-day (+ first 1) last newsd)]))

#|    [else (let [(message (message-getter uwnews first (list date-regexp)))]
            (cond
              [(boolean? message) (void)]
              [else (if (> (string-length (car message)) 32)
                        (printf "~a~n"
                            (substring (car message)
                                       11
                                       ;; Assume all date headers are nice and uniform
                                       ;; use that date SRFI?
                                       (- (string-length (car message)) 15)))
                            ;first)
                        (printf "~a~n"
                            (substring (car message)
                                       6
                                       ;; Assume all date headers are nice and uniform
                                       ;; use that date SRFI?
                                       (- (string-length (car message)) 15))))]))
                            ;first)
          (posts-per-day (+ first 1) last newsd)]))
|#

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
;(thread-hash first (+ first 100) uwnews)
;; Want better threading now. Use the second pair in the references line


;; userrel: User relations: who talked to whom on the newsgroup?
;; Store usernames (email addressses) and their message-ids in a hash table
;; for retreival and matching later.
(define (userrel first last newsd)
  (cond
    [(= first last) (void)]
    [else
     (local [(define mesg-from
                     (message-getter uwnews first
                                     (list from-regexp  ; Ugh, the order coming out of this function
		                           mid-regexp ; depends on what's in the headers, not the
					   ref-regexp)))  ; order I have here. Usually From, Subj, MID, Refs
             (define node-id (make-dot-id))]
       (cond
         [(and (not (boolean? mesg-from)) (= (length mesg-from) 3))
          (printf "--> ~a~n" (caddr mesg-from))
          (printf " `-> Using ~a~n" (car (get-refs (caddr mesg-from))))
                ;; Supposedly I want to use the _last_ reference from the list in that header line.
                ;; since any reply has all references of message it replies to, plus MID.
                ;;  ---> helper function that traverses list and prints out threading as it goes by
                ;;  searching the hash table
          (let [(exists (hash-ref mids (car (get-refs (caddr mesg-from))) #f))
                (poster (hash-ref users (car mesg-from) #f))]
            (cond
              [(boolean? exists) (printf " `-> Uhhh, ref to post that DNE?~n")
                        ;; consider this just like the case where there is a new post:
                        ;;  check to see if user already exists
                        ;;  create node but not edge
                                 (printf "  `-> ~a :: ~a~n~n" (car mesg-from) node-id)]
              [(boolean? poster) ;(printf " `-> Uhhh, ref to user that DNE?~n")
                                 (printf "  |`-> ~a :: ~a~n" (car mesg-from) node-id)
                                 (printf "  `-> ~a~n~n" exists)
                                 (ins-mid-u mids (car (get-refs (cadr mesg-from)))
                                                 (list (car mesg-from) node-id))
                                 (hash-set! users (car mesg-from) node-id)
                                 (fprintf dotfile "~a //[label=\"~a\"];\n" node-id (car mesg-from))
                                 (fprintf dotfile "~a -> ~a //[arrowhead=\"none\", style=\"invis\"];\n" node-id (cadr exists))]
              [else (printf "  |`-> ~a :: ~a~n" (car mesg-from) poster)
                    (printf "  `-> ~a~n~n" exists)
                    (fprintf dotfile "~a -> ~a //[arrowhead=\"none\", style=\"invis\"];\n" poster (cadr exists))]))
          (userrel (+ 1 first) last newsd)]
         [(and (not (boolean? mesg-from)) (= (length mesg-from) 2))
          ;; Only From and MID? It's a first post.
          (printf "--> New Post:~n")
          (printf " `-> ~a :: ~a~n" (car mesg-from) node-id)
          (printf " `-> MID:  ~a~n~n" (car (get-refs (cadr mesg-from))))
          (let [(uresult (hash-ref users (car mesg-from) #f))]
            (cond
              ;; If user does not already exist:
              [(boolean? uresult)
               ;; Save a key->val: MID -> '(From node-ID)
               (ins-mid-u mids (car (get-refs (cadr mesg-from)))
                               (list (car mesg-from) node-id))
               ;; Save a key->val: From -> node-ID
               (hash-set! users (car mesg-from) node-id)
               (fprintf dotfile "~a //[label=\"~a\"];\n" node-id (car mesg-from))]
              ;; Else only add the MID and existing node-ID to the hash table
              [else (ins-mid-u mids (car (get-refs (cadr mesg-from)))
                                    (list (car mesg-from) uresult))]))
          (userrel (+ 1 first) last newsd)]
         [else (userrel (+ 1 first) last newsd)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(read-all first (+ 100 first) uwnews)
;;(userrel first (+ 800 first) uwnews)

;;(begin
;;  (hash-map users (lambda (x y) (printf "~a]]] ~a~n~n" x y)))
;;  "_ _ _")
;;(begin
;;  (hash-map mids (lambda (x y) (printf "~a]]] ~a~n~n" x y)))
;;  "_ _ _")

(posts-per-day first (+ 200 first) uwnews)
;;(read-all first last uwnews)
;;(read-all first (+ first 1000) uwnews)


(fprintf dotfile "// Trial run finished at: ~a\n}\n" (current-seconds))

(close-output-port dotfile)
(disconnect-from-server uwnews)
