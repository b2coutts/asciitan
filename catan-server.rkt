#lang racket

(define mutex (make-semaphore 1))

(define response? string?)

;; begins interacting with a given user on the given input/output ports
;; TODO: add a channel for when the game is won
(define/contract (run-listener usr in out)
  (-> input-port? output-port? void?)
  (let loop []
    (sync (read-line-evt in 'any))
    (write (call-with-semaphore mutex (thunk (handle-action! usr evt))) out)
    (loop)))

;; TODO: replace these hardcoded connection details with something else
;; TODO: actually hardcode these connection details
(define state (create-state usr1 usr2))

;; run listener threads
(thread (thunk (run-listener usr1 in1 out1)))
(thread (thunk (run-listener usr2 in2 out2)))

(define/contract (handle-action! usr act)
  (-> user? action? response?)
  (match act
    [`(buy ,item) (buy-item! usr item)]
    [`(use ,card-num) (use-card! usr card-num)]
    [`(bank ,res-list ,target) (bank! usr res-list target)])

(define/contract (buy-item! usr item)
  (-> user? item? response?)
  

(define/contract (use-card! usr card-num)
  (-> user? integer? response?)
  (void))

(define/contract (bank! usr res-list target)
  (-> user? (listof resource?) resource? response?)
  (void))

