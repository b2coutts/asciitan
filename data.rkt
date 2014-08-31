;; various data type declarations
#lang racket

(require "basic.rkt" "cell.rkt" "constants.rkt")

(provide
  (struct-out board)
  (struct-out user)
  (struct-out state)

  resource?
  building?
  roll-num?
  dev-card?
  item?
  command?
  showable?
  style?
  stock?
  response?
)

(define resource? (in resources))
(define building? (in '(settlement city)))
(define roll-num? (in (build-list 11 (curry + 2))))
(define dev-card? (in dev-cards))
(define item?     (in items))
(define command?  (in commands))
(define showable? (in showables))

;; represents the current ANSI terminal style; (bg fg bold underline)
(define style? (list/c integer? integer? boolean? boolean?))

;; represents an amount of resources, i.e., a price
(define stock? (hash/c resource? integer?))

;; represents a response sent back to the user
(define response? (cons/c (in responses) list?))

(define-struct/contract user (
  [name string?]                      ;; name of the user
  [cards (listof dev-card?)]          ;; list of the user's held dev cards
  [res (hash/c resource? integer?)]   ;; list of the user's held resources
  [color integer?]                    ;; the user's color
  [io (list/c input-port? output-port? semaphore?)] ;; i/o handles/mutex
) #:mutable #:transparent)

(define-struct/contract board (
  [cells (hash/c cell? (cons/c (or/c roll-num? 'nil) (or/c resource? 'desert)))]
  [edges (hash/c edge? (or/c user? #f))]
  [verts (hash/c vertex? (or/c (cons/c user? building?) #f))]
  [thief cell?]
) #:mutable #:transparent)

(define-struct/contract state (
  [users (listof user?)]      ;; list of the users in the game
  [turnu user?]               ;; user whose turn it is
  [board board?]              ;; the game board
  [cards (listof dev-card?)]  ;; the stack of dev cards
) #:mutable #:transparent)
