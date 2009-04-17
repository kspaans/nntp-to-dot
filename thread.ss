;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; Detect and work with threads of messages in the newsgroup.

#lang scheme

(require "common.ss")

(provide thread-print thread-hash)

(define refers (make-hash))

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
    [else (local ((define mesg-from (message-getter newsd first
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

(define (thread-hash first last newsd dotfile)
  (cond
    [(= first last) (printf "@@@@@@@@@@@@@@@@\n")]
    [else (local [(define mesg-from (message-getter newsd first
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
