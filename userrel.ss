;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; Graph the relationships between all users in the newsgroup, generate DOT
;; code to represent them. Each unique user gets a node, and edges are drawn
;; whenever a user replies to another user's message.

#lang scheme

(require "common.ss")

(provide userrel)

(define users (make-hash))
(define mids (make-hash))

;; userrel: int int newsgroup ioport -> void
;; User relations: who talked to whom on the newsgroup?
;; Create a DOT file to graph the user interactions.
;; Store usernames (email addressses) and their message-ids in a hash table
;; for retreival and matching later.
(define (userrel first last newsd dotfile)
  (fprintf dotfile "digraph cs136-userrel {\n")
  (fprintf dotfile "// Run starting at: ~a\n" (current-seconds))
  (fprintf dotfile "ranksep = 3\n")
  ;(fprintf dotfile "nodesep = 1.0\n")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cond
    [(= first last) (void)]
    [else
     (local [(define mesg-from
                     (message-getter newsd first
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
         [else (userrel (+ 1 first) last newsd)]))])
  (fprintf dotfile "// Run finished at: ~a\n}\n" (current-seconds)))
