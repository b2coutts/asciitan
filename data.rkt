;; various data type declarations
#lang racket

(require "basic.rkt" "cell.rkt" "constants.rkt")

(provide
  (struct-out board)
  (struct-out user)
  (struct-out rlock)
  (struct-out state)

  resource?
  building?
  roll-num?
  dev-card?
  item?
  command?
  showable?
  prompt?
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
(define prompt?   (in prompts))

;; represents a trading post
(define trading-post? (list/c symbol? vertex? vertex?))

;; represents the current ANSI terminal style; (bg fg bold underline)
(define style? (list/c integer? integer? boolean? boolean?))

;; represents an amount of resources, i.e., a price
(define stock? (hash/c resource? integer?))

;; represents a response sent back to the user
(define response? (cons/c (in responses) list?))

(define-struct/contract user (
  [name string?]                      ;; name of the user
  [veeps integer?]                    ;; number of victory points
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

(define-struct/contract rlock (
  [holder user?]    ;; user being waited for
  [action string?]  ;; description of what action is being waited for
  [type prompt?]    ;; which kind of response is being waited for
  [var any/c]       ;; allows rlock to have some situation-dependent state
  ;; lock function; takes the state (any/c because state? isn't defined yet)
  ;; and a response, and produces a server response for the user. If the user's
  ;; response is valid, func will unlock the state itself
  [func (-> any/c any/c (or/c response? void?))]
) #:mutable #:transparent)

(define-struct/contract state (
  [users (listof user?)]      ;; list of the users in the game
  [turnu user?]               ;; user whose turn it is
  [board board?]              ;; the game board
  [cards (listof dev-card?)]  ;; the stack of dev cards
  [lock (or/c rlock? #f)]     ;; lock used to wait for a user response
) #:mutable #:transparent)
