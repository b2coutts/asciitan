;; contains code for running the game
#lang racket

(require "board.rkt" "cell.rkt" "basic.rkt" "data.rkt" "constants.rkt" "adv.rkt")

(provide
  ;; TODO: remove these 2?
  user
  state

  init-state
  handle-action!
)

;; -------------------------- SMALL HELPER FUNCTIONS --------------------------
;; broadcast a server message to everyone
;; TODO: remove SERVER
(define/contract (broadcast st fstr . args)
  (->* (state? string?) #:rest (listof any/c) void?)
  (map (lambda (usr)
        (match-define (list _ out mutex) (user-io usr))
        (call-with-semaphore mutex (thunk
          (apply (curry fprintf out (string-append "* " fstr "\n")) args))))
       (state-users st))
  (void))

;; give a specified (possibly negative) amount of a resource to a user
(define/contract (give-res! usr res [amt 1])
  (->* (user? resource?) (integer?) void?)
  (hash-set! (user-res usr) res (+ (hash-ref (user-res usr) res) amt))
  (void))

;; give a stock of resources to a user
(define/contract (give-stock! usr stock)
  (-> user? stock? void?)
  (hash-map stock (lambda (res amt) (give-res! usr res amt)))
  (void))

;; convert a list of resources into a stock
(define/contract (list->stock lst)
  (-> (listof resource?) stock?)
  (define stock (make-hash))
  (map (lambda (res) (hash-set! stock res (+ (hash-ref stock res 0) 1))) lst)
  stock)

;; convert a stock to a pretty-printed string
(define/contract (stock->string stock)
  (-> stock? string?)
  (string-append
    (if (hash-empty? stock) "nothing" (apply string-append (add-between
      (map (lambda (res) (format "~a[~am~a ~a" col-esc (resource->color res)
                                               (hash-ref stock res) res))
           (filter (lambda (res) (not (= (hash-ref stock res 0) 0))) resources))
     (format "~a, " (style->string '(40 37 #f #f))))))
    (style->string '(40 37 #f #f))))

;; generates a properly-weighted random roll (from 2-12)
(define/contract (random-roll)
  (-> roll-num?)
  (+ (random 6) (random 6) 2))

;; determines whether or not the user can afford a given price (hash)
(define/contract (can-afford? usr item)
  (-> user? item? boolean?)
  (define price (hash-ref item-prices item))
  (foldr (lambda (x y) (and x y)) #t
    (hash-map (user-res usr) (lambda (res amt) (>= amt (hash-ref price res))))))

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
  (foldr + 0 (map (lambda (vtx) (match (board-vertex-pair (state-board st) vtx)
                                  [(cons (== usr) 'settlement) 1]
                                  [(cons (== usr) 'city) 2]
                                  [_ 0]))
                  board-vertex-list)))

;; if the game is over, returns the winner; otherwise, returns #f
(define/contract (game-over? st)
  (-> state? (or/c user? #f))
  (define veeps (map (curry user-veeps st) (state-users st)))
  (define leader (first (sort (state-users st)
                       (lambda (x y) (< (user-veeps st x) (user-veeps st y))))))
  (if (>= (user-veeps st leader) 10) leader #f))

;; --------------------------- BIG HELPER FUNCTIONS ---------------------------
;; given a user, produce a string of its user name with correct color-coding
(define/contract (uname usr)
  (-> user? string?)
  (format "~a[~am~a~a[37m" col-esc (user-color usr) (user-name usr) col-esc))

;; given a roll number, modify the game state to add resources
(define/contract (apply-roll! st roll)
  (-> state? roll-num? void?)
  (define b (state-board st))
  (map (lambda (usr)
    (define stock-gain (list->stock
      (apply append
        (hash-map (board-cells b) (lambda (cell num-res)
          (define res (cdr num-res))
          (if (or (equal? res 'desert) (not (= (car num-res) roll)))  '()
            (apply append
              (filter-map (lambda (vtx) (match (board-vertex-pair b vtx)
                            [(cons (== usr) 'city) (list res res)]
                            [(cons (== usr) 'settlement) (list res)]
                            [_ #f]))
                      (adj-vertices cell)))))))))
    ;; TODO: better broadcast message
    (broadcast st "~a gets ~a." (uname usr) (stock->string stock-gain))
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
    [winner (broadcast st "~a won the game!" (uname winner))]
    [else
      (define usrs (state-users st))
      (define oldind (- (length usrs) (length (member (state-turnu st) usrs))))
      (define newind (modulo (add1 oldind) (length usrs)))
      (set-state-turnu! st (list-ref usrs newind))
      (broadcast st "It's ~a's turn." (uname (state-turnu st)))
      (define roll (random-roll))
      (broadcast st "~a rolls a ~a." (uname (state-turnu st)) roll)
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
    [else (hash-map (hash-ref item-prices item) (lambda (res amt)
                      (hash-set! (user-res usr) res
                                 (- (hash-ref (user-res usr) res) amt))))
          (match item
            [(or 'city 'settlement)
              (set-board-vertex-pair! b args usr item)
              (broadcast st "~a has built a ~a at ~a." (uname usr) item
                (vertex->string args))]
            ['road
              (set-board-road-owner! b args usr)
              (broadcast st "~a has built a road at ~a." (uname usr)
                (edge->string args))]
            ['dev-card
              (define draw (pop-dev-card!))
              (set-user-cards! usr (cons draw (user-cards usr)))
              (broadcast st "~a has built a dev card." (uname usr))
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
  (-> (or/c state? #f) user? (cons/c command? list?) response?)
  (logf 'debug "handle-action!: usr=~a, act=~s\n" (user-name usr) act)
  (match act
    [`(buy ,item ,args) (buy-item! st usr item args)]
    [`(use ,card-num) (use-card! st usr card-num)] ;; TODO: use card name instead?
    [`(bank ,res-list ,target) (bank! st usr res-list target)]
    [`(end) (change-turn! st)]
    [`(show board) (board->string (state-board st))]
    [`(ping ,str) (format "pong ~a" str)]))

;; creates a new state, given a non-empty list of users
(define/contract (init-state usrs)
  (-> (listof user?) state?)
  (state usrs (first usrs) (create-board) (shuffle dev-cards)))
