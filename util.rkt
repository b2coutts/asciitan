;; contains miscellaneous helper functions that don't belong anywhere
#lang racket

(require racket/contract)

(provide member? prompt)

;; true iff x is an element of lst
(define/contract (member? x lst)
  (-> any/c list? boolean?)
  (if (member x lst) #t #f))

;; prompt the user for input
(define/contract (prompt msg validate [in (current-input-port)]
                                      [out (current-output-port)])
  (->* (string? (-> any/c (or/c string? #f)))
       (input-port? output-port?)
       boolean?)
  (fprintf "~a\n" msg)
  (define input (read in))
  (define err (validate input))
  (cond
    [err (fprintf "~a\n" err)
         (prompt msg validate in out)]
    [else input]))
