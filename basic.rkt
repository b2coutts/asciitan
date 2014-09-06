;; contains miscellaneous helper functions that do not fundamentally have
;; anything to do with this project
#lang racket

(require racket/contract)

(provide member? mysplitf-at mydropf prompt in notin logf)

;; true iff x is an element of lst
(define/contract (member? x lst)
  (-> any/c list? boolean?)
  (if (member x lst) #t #f))

;; given a list, produces a function which checks if its argument is in the list
(define/contract (in lst)
  (-> list? (-> any/c boolean?))
  (curryr member? lst))

;; negation of the above
(define/contract (notin lst)
  (-> list? (-> any/c boolean?))
  (compose not (in lst)))

;; prompt the user for input
(define/contract (prompt msg validate [in (current-input-port)]
                                      [out (current-output-port)])
  (->* (string? (-> any/c (or/c string? boolean?)))
       (input-port? output-port?)
       any/c)
  (fprintf out "~a\n" msg)
  (define input (read in))
  (define err (validate input))
  (cond
    [err (when (string? err) (fprintf "~a\n" err))
         (prompt msg validate in out)]
    [else input]))

;; Function in Racket 6 I'm implementing here for better portability
(define (mysplitf-at lst pred)
  (match lst
    ['() (values '() '())]
    [(cons x xs) (cond
      [(pred x) (define-values (ys zs) (mysplitf-at (rest lst) pred))
                (values (cons x ys) zs)]
      [else (values '() lst)])]))

;; same as above
(define (mydropf lst pred)
  (match lst
    ['() '()]
    [(cons x xs) (cond
      [(pred x) (mydropf xs pred)]
      [else lst])]))

;; logging function
;; TODO: timestamp
(define printable '(info))
(define logf-mutex (make-semaphore 1))
(define/contract (logf type fstr . args)
  (->* ((or/c 'debug 'info) string?) () #:rest (listof any/c) void?)
  (when (member? type printable)
    (call-with-semaphore logf-mutex (thunk
      (define tstr (match type ['debug "DEBUG: "] ['info "INFO:  "]))
      (apply (curry printf (string-append tstr fstr)) args)))))
