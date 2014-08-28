;; various data type declarations
#lang racket

(provide
  board
  user
  state

  resource?
  building?
  roll-num?
  dev-card?
  item?
  stock?
  response?
)

(define-struct/contract board (
  [cells (hash/c cell? (cons/c roll-num? resource?))]
  [edges (hash/c edge? (or/c user? #f))]
  [verts (hash/c vertex? (or/c (cons/c user? building?) #f))]
  [thief cell?]) #:mutable #:transparent)

(define-struct/contract user
  [name string?]                      ;; name of the user
  [cards (listof dev-card?)]          ;; list of the user's held dev cards
  [res (hash/c resource? integer?)]   ;; list of the user's held resources
  [color color?]                      ;; the user's color
  [io (list/c input-port? output-port? semaphore?)] ;; i/o handles/mutex
)

(define-struct/contract state
  [users (listof user?)]      ;; list of the users in the game
  [turnu user?]               ;; user whose turn it is
  [board board?]              ;; the game board
  [cards (listof dev-card?)]  ;; the stack of dev cards
)

(define (resource? res) (member? res '(wood grain sheep ore clay desert)))
(define (building? bu) (member? bu '(settlement city)))
(define (roll-num? n) (member? n (build-list 11 (curry + 2))))
(define (dev-card? dc) (member? dc dev-cards))
(define (item? item) (member? item items))

;; represents an amount of resources, i.e., a price
(define stock? (hash/c resource? integer?))

;; represents a response sent back to the user
(define response? (or/c string? void?))
