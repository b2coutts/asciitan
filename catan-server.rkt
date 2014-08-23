#lang racket

(define state (void))

(define (resource? x) (if (member x '(wood clay ore grain sheep)) #t #f))
(define (item? x) (if (member x '(settlement city dev-card road)) #t #f))

(define/contract (handle-action! usr act)
  (-> user? action? response?)
  (match act
    ;; TODO: trade
    [`(buy ,item) (buy-item! usr item)]
    [`(use ,card-num) (use-card! usr card-num)]
    [`(bank ,res-list ,target) (bank! usr res-list target)])

(define/contract (buy-item! usr item)
  (-> user? item? response?)
  (void))

(define/contract (use-card! usr card-num)
  (-> user? integer? response?)
  (void))

(define/contract (bank! usr res-list target)
  (-> user? (listof resource?) resource? response?)
  (void))

