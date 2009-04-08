#lang scheme
(provide get-refs make-dot-id)
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
