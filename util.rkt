;; contains miscellaneous helper functions that don't belong anywhere
#lang racket

(require racket/contract)

(provide member? prompt in notin resource->color style->string logf)

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

;; produce the color code of the given resource
(define/contract (resource->color res)
  (-> symbol? integer?)
  (match res ['sheep 92]
             ['clay 31]
             ['grain 33]
             ['wood 32]
             ['ore 90]
             ['desert 37]))

;; given a style, produce the ANSI escape string for inducing that style
(define/contract (style->string sty)
  (-> (list/c integer? integer? boolean? boolean?) string?)
  (match-define (list bg fg bold underline) sty)
  (format "~a[~a;~a~a~am" (integer->char #x1b) bg fg (if bold ";1" "")
                          (if underline ";4" ""))) 

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
