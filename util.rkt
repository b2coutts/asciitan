;; contains miscellaneous helper functions that don't belong anywhere
#lang racket

(require racket/contract)

(provide member? user? user user-color debug)

;; true iff x is an element of lst
(define/contract (member? x lst)
  (-> any/c list? boolean?)
  (if (member x lst) #t #f))

;; debugging function; prints and produces its argument
(define (debug x)
  (printf "DEBUG: ~s\n" x)
  x)

;; TODO: temporary user struct while I figure out state
(struct user (color))
