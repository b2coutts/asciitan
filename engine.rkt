;; contains code for running the game
#lang racket

(require "board.rkt")
(require "cell.rkt")

(provide
  user
  state

  init-state
  handle-action!
)

;; ---------------------------------- STRUCTS ---------------------------------
(define-struct/contract user (
  [name string?]                      ;; name of the user
  [cards (listof dev-card?)]          ;; list of the user's held dev cards
  [res (hash/c resource? integer?)]   ;; list of the user's held resources
  [color color?]                      ;; the user's color
  [io (list/c input-port? output-port? semaphore?)] ;; i/o handles/mutex
)

(define-struct/contract state (
  [users (listof user?)]      ;; list of the users in the game
  [turnu user?]               ;; user whose turn it is
  [board board?]              ;; the game board
  [cards (listof dev-card?)]  ;; the stack of dev cards
)

;; ------------------------- CONSTANTS/LISTS/CONTRACTS -------------------------
;; ordered list of all dev cards
(define dev-cards
  (append (build-list 14 (const 'knight))
          (build-list 5  (const 'veep)) ;; TODO: separate these?
          (build-list 2  (const 'year-of-plenty))
          (build-list 2  (const 'road-building))))

;; list of all buyable items
(define items '(city settlement dev-card road))

;; hash mapping buyable things to prices
(define item-prices `#hash(
  [city . #hash([ore . 3] [grain . 2] [clay . 0] [wood . 0] [sheep . 0])]
  [settlement . #hash([ore . 0] [grain . 1] [clay . 1] [wood . 1] [sheep . 1])]
  [dev-card . #hash([ore . 1] [grain . 1] [clay . 0] [wood . 0] [sheep . 1])]
  [road . #hash([ore . 0] [grain . 0] [clay . 1] [wood . 1] [sheep . 0])]))

(define (dev-card? dc) (member? dc dev-cards))
(define (item? item) (member? item items))

;; represents an amount of resources, i.e., a price
(define stock? (hash/c resource? integer?))

;; represents a response sent back to the user
(define response? (or/c string? void?))

;; -------------------------- SMALL HELPER FUNCTIONS --------------------------
;; broadcast a server message to everyone
(define/contract (broadcast st fstr . args)
  (->* state? (string?) () #:rest (listof any/c))
  (map (lambda (usr)
        (match-define (list _ out mutex) (user-io usr))
        (call-with-semaphore mutex (thunk
          (apply (curry fprintf out fstr) args))))
       (state-users st)))

;; give a specified (possibly negative) amount of a resource to a user
(define/contract (give-res! usr res [amt 1])
  (->* (user? resource?) (integer?) void?)
  (hash-set! (user-res usr) res (+ (hash-ref (user-res usr) res) amt)))

;; give a stock of resources to a user
(define/contract (give-stock! usr stock)
  (-> user? stock? void?)
  (hash-map (lambda (res amt) (give-res! usr res amt)) stock))

;; convert a list of resources into a stock
(define/contract (list->stock lst)
  (-> (listof res?) stock?)
  (define stock '#hash())
  (map (lambda (res) (hash-set! stock res (+ (hash-ref stock res 0) 1))) lst)
  stock)

;; generates a properly-weighted random roll (from 2-12)
(define/contract (random-roll)
  (-> roll-num?)
  (+ (random 6) (random 6) 2))

;; determines whether or not the user can afford a given price (hash)
(define/contract (can-afford? usr price)
  (-> user? item? boolean?)
  (apply and
    (hash-map (lambda (res amt) (>= amt (hash-ref price res))) (user-res usr))))

;; removes a dev card from the top of the stack
(define/contract (pop-dev-card! st)
  (-> state? dev-card?)
  (match-define (cons x xs) (state-cards st))
  (set-state-cards! st xs)
  x)

;; TODO: implement this fully
;; produce the number of veeps a given user has
(define/contract (user-veeps st usr)
  (-> state? user? integer?)
  (foldr + 0 (map (lambda (vtx) (match (board-vertex-pair vtx)
                                  [(cons (quote usr) 'settlement) 1]
                                  [(cons (quote usr) 'city) 2]
                                  [_ 0]))
                  board-vertex-list)))

;; if the game is over, returns the winner; otherwise, returns #f
(define/contract (game-over? st)
  (-> state? (or/c user? #f))
  (define veeps (map user-veeps (state-users st)))
  (define leader (sort (map (lambda (usr) (user-veeps st usr)) (state-users st))
                       (lambda (x y) (< (user-veeps x) (user-veeps y)))))
  (if (>= (user-veeps leader) 10) leader #f))

;; --------------------------- BIG HELPER FUNCTIONS ---------------------------

;; given a roll number, modify the game state to add resources
(define/contract (apply-roll! st roll)
  (-> state? roll-num? void?)
  (define b (state-board st))
  (map (lambda (usr)
    (define stock-gain (list->stock
      (hash-map (lambda (cell num-res)
        (define res (cdr num-res))
        (apply append
          (filter (lambda (vtx) (match (board-vertex-pair b vtx)
                    [(cons (quote usr) 'city) (list res res)]
                    [(cons (quote usr) 'settlement) (list res)]
                    [#f '()]))
                  (adj-vertices cell))))
       (board-edges b))))
    ;; TODO: better broadcast message
    (broadcast st "~a gets ~a." (user-name usr) stock-gain)
    (give-stock! usr stock-gain))
   (state-users st))
  (void))

;; TODO: handle endgame conditions
;; TODO: handle roll #7
(define/contract (change-turn! st)
  (-> state? void?)
  (define winner (game-over? st))
  (cond
    ;; TODO: send some sort of endgame signal
    [winner (broadcast st "~a won the game!" (user-name winner))]
    [else
      (define usrs (state-users st))
      (define oldind (- (length usrs) (length (member (state-turnu st) usrs))))
      (define newind (modulo (add1 oldind) (length usrs)))
      (set-state-turnu! st (list-ref usrs newind))
      (broadcast st "It's ~a's turn." (user-name (state-turnu st)))
      (define roll (random-roll))
      (broadcast st "~a rolls a ~a." (user-name (state-turnu st)) roll)
      (apply-roll! st roll)]))

;; -------------------------- MAJOR HELPER FUNCTIONS --------------------------
(define/contract (buy-item! st usr item args)
  (-> state? user? item? (or/c vertex? edge? void?) response?)
  (define b (state-board st))
  (cond
    [(not (equal? usr (state-turnu st))) "It's not your turn!"]
    [(not (can-afford? usr item)) (format "You can't afford ~a!" item)]
    [(and (building? item) (board-vertex-pair b args))
      "That space is already occupied!"]
    [(and (equal? item 'road) (board-road-owner b args))
      "That space is already occupied!"]
    [(and (equal? item 'dev-card) (empty? (state-cards st)))
      "There are no more dev cards left to draw!"]
    [else (hash-map (lambda (res amt)
                      (hash-set! (user-res usr) res
                                 (- (hash-ref (user-res usr) res) amt))))
          (match item
            [(or 'city 'settlement)
              (set-board-vertex-pair! b args usr item)
              (broadcast st "~a has built a ~a at ~a." (user-name usr) item
                (vertex->string args))]
            ['road
              (set-board-road-owner! b args usr)
              (broadcast st "~a has built a road at ~a." (user-name usr)
                (edge->string args))]
            ['dev-card
              (define draw (pop-dev-card!))
              (set-user-cards! usr (cons draw (user-cards usr)))
              (broadcast st "~a has built a dev card." (user-name usr))
              (format "You draw a ~a." draw)])]))

;; TODO
(define/contract (use-card! st usr card-num)
  (-> state? user? integer? response?)
  (void))

;; TODO
(define/contract (bank! st usr res-list target)
  (-> state? user? (listof resource?) resource? response?)
  (void))

;; ------------------------------- API FUNCTIONS -------------------------------
;; TODO: description
(define/contract (handle-action! st usr act)
  (-> state? user? action? response?)
  (match act
    [`(buy ,item ,args) (buy-item! st usr item)]
    [`(use ,card-num) (use-card! st usr card-num)] ;; TODO: use card name instead?
    [`(bank ,res-list ,target) (bank! st usr res-list target)]
    [`(end) (change-turn!)]))

;; creates a new state, given a non-empty list of users
(define/contract (init-state usrs)
  (-> (listof user?) state?)
  (state usrs (first usrs) (create-board) (shuffle dev-cards)))
