;; contains miscellaneous helper functions that don't belong anywhere
#lang racket

(require racket/contract)

(provide member?)

;; true iff x is an element of lst
(define/contract (member? x lst)
  (-> any/c list? boolean?)
  (if (member x lst) #t #f))
