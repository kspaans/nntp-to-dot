#lang scheme
(provide get-refs make-dot-id ins-user-id ins-mid-u)
;; get-refs: string -> (listof string)
;; Given an NNTP "References:" header line, will extract all Message-IDs in it
(define references-regexp #rx"<[^>]*>")
(define (get-refs refline)
  (let [(ref-match (regexp-match references-regexp refline))]
    (cond
      [(boolean? ref-match) empty]
      [else (cons (car ref-match)
                  (get-refs (substring refline
		                       ;; Need to only go to the last matching
				       ;; index so that we don't go out of range
                                       (cdar (regexp-match-positions
				               references-regexp
					       refline)))))])))

;; make-dot-id: void -> string
;; Generates a unique dot ID every time it is called, side effect of
;; incrementing an internal counter is used.
;; Turns an integer (internal counter) into a string of uppercase
;; letters
(define counter -1)
;;;;
(define (num2list num first)
  (cond [(and (not first) (zero? num)) '()]
        [else (cons (+ 65 (modulo num 26))
                    (num2list (quotient num 26) false))]))
(define (num2letters num first)
  (list->string (reverse
    (map integer->char (num2list num first)))))
(define make-dot-id
  (lambda ()
      (begin
        (set! counter (+ 1 counter))
        (num2letters counter true))))

;; ins-user-id: hash-table string string -> void
;; Inserts user information into a hash table. Takes the table, the username
;; (email address) and a message-id. user is used as the key and MID the
;; value. Each user can post multiple messages, therefore MIDs are stored
;; in another hash table along with the (date/time/order/count?) of the post
(define (ins-user-id htable user mid)
  (let [(uresult (hash-ref htable user #f))]
    (if (boolean? uresult)
        ;; New user, create hash with MID
        (local [(define new-user-htable (make-hash))]
          (hash-set! new-user-htable mid 0) ; user date instead?
          (hash-set! htable user new-user-htable))
        ;; Add MID to hash table (MIDs are assumed to be unique
	(hash-set! uresult mid (hash-count uresult)))))

;; ins-mid-u: hash-table string string -> void
;; Inserts message-ID information into a hash table. Takes the table, the message-id
;; and a username (email address). MID is used as the key and user the
;; value. MIDs are unique, but multiple MIDS may have the same user value
(define (ins-mid-u htable mid user)
  (let [(mresult (hash-ref htable mid #f))]
    (if (boolean? mresult)
        ;; New MID, associate with user
        (hash-set! htable mid user)
        ;; Mid already exists?
	(error 'mid-collision))))
