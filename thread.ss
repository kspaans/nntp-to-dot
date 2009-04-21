;; Copyright (C) 2009  Kyle Spaans
;;  this program is distributed under the GPL v2 or (at your option)
;;  any later version.

;; Detect and work with threads of messages in the newsgroup.
;; Want to see:
;; - number of unanswered messages
;; - number of original posts versus replies
;; - thread depths, average
;; -

#lang scheme

(require "common.ss")
(require srfi/19)

(provide thread-print thread-hash)

(define refers (make-hash)) ;; map Message-IDs to node-IDs
(define threads '())        ;; list of structures representing each thread
(define post-count 0)       ;; total number of first-posts
(define reply-count 0)      ;; total number of reply messages

;; Message Node Struct
;;  Store newsgroup messages and reporoduce their threaded hierarchy in a
;;  tree structure.
;; string     - the user (From: header)
;; string     - the Message-ID: of the message
;; date       - date of the message
;; string     - subject
;; string     - node ID, used for dot ((make-node-id))
;; list/mesgn - pointer to parent of this node, empty if no parent.
;; list       - of mesgn, the children of this node
(define-struct mesgn (from id date subject node-id parent children) #:mutable #:transparent)

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
    [else
     (local
       [(define mesg-from (message-getter
                            newsd
                            first
                            (list from-regexp mid-regexp ref-regexp subj-regexp)))]
       (if (boolean? mesg-from)
           (void)
           (map (lambda (x) (printf "~a~n" x)) mesg-from))
       (newline))
     (thread-print (+ first 1) last newsd)]))

;; thread-post-process: void -> void
;; do some work with the data collected by thread-hash, print to stdout:
;; - number of "posts" number of "replies"
;; - number of unanswered posts
(define (thread-post-process)
  ;(for-each (lambda (n) (printf "~a~n" n)) threads)
  (printf "Posts:   ~a~nReplies: ~a~n" post-count reply-count)
  (printf "Size: ~a~n" (hash-count refers))
  (printf "Threads: ~a~n" (length threads))
  (printf "Heights: ~a~n" (map height threads))
  (printf "TSizes:  ~a~n" (map num-children threads))
  (newline)
  ;(thread-pprint))
  (thread-dot (cadddr (cddr threads))))

;; thread-refs: (listof string) mesgn -> void
;; Takes a list of references and a message node and creates the threading
;; tree appropriately by looking up refs in the hash table. The last ref in
;; the list should always be the appropriate one to use.
(define (thread-refs aloref mesgnode)
   (let [(node-pair (hash-ref refers (last aloref) #f))]
     (cond
       [(boolean? node-pair) (printf "$$  Uh oh! Reference does not exist!~n")
                             (printf "$$  ~a~n" (last aloref))
                             (printf "$$  ~a~n~n" (mesgn-subject mesgnode))]
        ;; Add to children of node it refers, and set mesg's parent
       [else (printf "|--~a (~a) is a reply to ~a (~a)~n~n"
                     (mesgn-node-id mesgnode)
                     (mesgn-subject mesgnode)
                     (mesgn-node-id (cadr node-pair))
                     (mesgn-subject (cadr node-pair)))
             (set-mesgn-children! (cadr node-pair)
                            (cons mesgnode
                                  (mesgn-children (cadr node-pair))))
             (set-mesgn-parent! mesgnode (cadr node-pair))])))

;; thread-pprint-internal: int mesgn -> void
;; Does the work of thread-pprint, takes an argument for the threading
;; depth.
(define (thread-pprint-internal depth mesgnode)
  (cond
    [(and (empty? (mesgn-children mesgnode)) (= 0 depth))
     (printf "~a~n" (mesgn-subject mesgnode))]
    [(empty? (mesgn-children mesgnode))
     (printf "~a`--> ~a~n"
             (make-string depth #\space)
             (substring (mesgn-subject mesgnode) 9))]
    [(= 0 depth) (printf "~a~n" (mesgn-subject mesgnode))
                 (for-each
                   (lambda (node)
                     (thread-pprint-internal (add1 depth) node))
                   (mesgn-children mesgnode))]
    [else (printf "~a`--> ~a~n"
                  (make-string depth #\space)
                  (substring (mesgn-subject mesgnode) 9))
          (for-each
            (lambda (node)
              (thread-pprint-internal (add1 depth) node))
            (mesgn-children mesgnode))]))

;; thread-pprint: void -> void
;; Traverses the list "thread" and pretty prints threading.
(define (thread-pprint)
  (for-each
    (lambda (node)
      (thread-pprint-internal 0 node))
    threads))

;; listmax: (listof nat) -> nat
;; Finds the max in a list
(define (listmax lst)
  (cond
    [(empty? lst) 0]
    [else (max (car lst) (listmax (cdr lst)))]))

;; height: mesgn -> int
;; Calculates the height of the thread
(define (height n)
  (cond
    [(empty? (mesgn-children n)) 1]
    [else (add1 (listmax (map height (mesgn-children n))))]))

;; num-children: mesgn -> int
;; Calculates number of messages in thread
(define (num-children n)
  (cond
    [(empty? (mesgn-children n)) 1]
    [else (foldr + 0 (map num-children (mesgn-children n)))]))

;; thread-dot: mesgn -> void
;; Takes a Message Node Struct and recursively prints out DOT code representing
;; that tree. Prints to the given ioport.
;; Also want easily-changable node-label options:
;;   node-id, from, subj, date
(define node-label-format "~a [label=\"~a\\n~a\\n~a\"];")
(define (thread-dot mesgnode)
  (local
    [(define tdot (open-output-file "data/thread.dot" #:exists 'truncate))
     (define (thread-dot-rec mesgnode)
       (fprintf tdot
                node-label-format
                (mesgn-node-id mesgnode)
                ;; Remove '"' chars from the header
                (list->string
                  (filter (lambda (c) (not (char=? #\" c)))
                          (string->list (mesgn-from mesgnode))))
                (mesgn-subject mesgnode)
                (date->string (mesgn-date mesgnode) "~D"))
       (for-each (lambda (n)
                   (fprintf tdot
                            "~a -> ~a\n"
                            (mesgn-node-id mesgnode)
                            (mesgn-node-id n)))
                 (mesgn-children mesgnode))
       (for-each thread-dot-rec (mesgn-children mesgnode)))]
  (fprintf tdot "digraph cs136_threads {\n")
  (thread-dot-rec mesgnode)
  (fprintf tdot "// Run finished at: ~a\n}\n" (current-seconds))
  (close-output-port tdot)))


;; thread-hash: int int newsgroup ioport
;; Reads messages from first to last in newsgroup newsd, printing
;; info to stdout and to the file dotfile.
;; Examines threading information, mostly in the "References:" header
;; of each message, using a hash table "refers" to keep track of things.
;; HASH TABLE:
;;  Each message is given a unique node ID which is entered as a value in
;;  the hash table, with the message-ID as the key. Paired with the node ID
;;  is the Message Node Structure created for that message. This is so that
;;  when references are found they can be added to the threading tree.
;;  References are in the form of message-IDs, so the node ID that a
;;  message refers to can be looked up.
(define (thread-hash first last newsd dotfile)
  (cond
    [(= first last) (thread-post-process)]
    [else
             ;; Order is: From, Subject, Date, MID, Refs
     (local [(define message
               (message-getter
                  newsd
                  first
                  (list mid-regexp from-regexp date-regexp ref-regexp subj-regexp)))
             (define mesg-from
               (if (boolean? message) #f (car message)))
             (define mesg-subj
               (if (boolean? message) #f (cadr message)))
             (define mesg-date
               (if (boolean? message) #f (get-date (caddr message))))
             (define mesg-mid
               (if (boolean? message) #f (car (get-refs (cadddr message)))))
             (define mesg-refs
               (if (and (not (boolean? message)) (= 5 (length message)))
                   (get-refs (cadr (cdddr message)))
                   #f))
             (define node-id (make-dot-id))]
       (cond
          ;; Message exists
         [(not (boolean? message))
          (let [(result (hash-ref refers mesg-mid #f))
                (mesgn-struct
                  (make-mesgn mesg-from mesg-mid mesg-date mesg-subj node-id '() '()))]
            (cond
               ;; MID collision, should not happen.
              [(string? result)
               ;(printf "MIDs already in hash table?~n    >>~a<<~n" (caddr mesg-from))
               (error `mid-collision)]
               ;; No MID collision
              [(boolean? result)
               (fprintf dotfile "// Node ~a\n    ~a;\n" mesg-mid node-id)
               (hash-set! refers mesg-mid (list node-id mesgn-struct))
               (cond
                  ;; Message has references
                 [(= 5 (length message))
                  (set! reply-count (add1 reply-count))
                  (printf "|-Checking References to find threading...~n")
                  (thread-refs mesg-refs mesgn-struct)]
                  ;; Message does not have references
                 [(= 4 (length message))
                  (set! post-count (add1 post-count))
                  (set! threads
                        (cons mesgn-struct
                              threads))])]))]
         [else (printf "&&&&&&&& Whoops! Couldn't read message number ~a~n~n" first)]))
     (thread-hash (+ first 1) last newsd dotfile)]))
