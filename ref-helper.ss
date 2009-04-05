#lang scheme
(provide get-refs)

;; get-refs: string -> (listof string)
;; Given an NNTP "References:" header line, will extract all Message-IDs in it
(define references-regexp #rx"<[^>]*>")
(define (get-refs refline)
  (let ([ref-match (regexp-match references-regexp refline)])
    (cond
      [(boolean? ref-match) empty]
      [else (cons (car ref-match)
                  (get-refs (substring refline
		                       ;; Need to only go to the last matching
				       ;; index so that we don't go out of range
                                       (cdar (regexp-match-positions
				               references-regexp
					       refline)))))])))
