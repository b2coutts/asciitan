#lang racket

(define mutex (make-semaphore 1))

(define response? (or/c string? void?))

(define (item? x) (member? x '(city settlement dev-card road)))

;; begins interacting with a given user on the given input/output ports
;; TODO: add a channel for when the game is won
(define/contract (run-listener usr in out)
  (-> input-port? output-port? void?)
  (let loop []
    (sync (read-line-evt in 'any))
    (define response (call-with-semaphore mutex (thunk (handle-action! usr evt))))
    (unless (void? response) (write response out))
    (loop)))

;; TODO: replace these hardcoded connection details with something else
;; TODO: actually hardcode these connection details
(define st (create-state usr1 usr2))

;; run listener threads
(thread (thunk (run-listener usr1 in1 out1)))
(thread (thunk (run-listener usr2 in2 out2)))

(define/contract (handle-action! usr act)
  (-> user? action? response?)
  (match act
    [`(buy ,item ,args) (buy-item! usr item)]
    [`(use ,card-num) (use-card! usr card-num)] ;; TODO: use card name instead?
    [`(bank ,res-list ,target) (bank! usr res-list target)]
    [`(end) (change-turn!)]))

;; TODO: implement this properly
;; if the game is over, returns the winner; otherwise, returns #f
(define/contract (game-over? st)
  (-> state? (or/c user? #f))
  #f)

(define/contract (random-roll)
  (-> roll-num?)
  (+ (random 6) (random 6) 2))

;; TODO: handle endgame conditions
;; TODO: handle roll #7
(define/contract (change-turn!)
  (-> void?)
  (define winner (game-over? st))
  (cond
    ;; TODO: send some sort of endgame signal
    [winner (broadcast "~a won the game!" (user-name winner))]
    [else
      (define usrs (state-users st))
      (define oldind (- (length usrs) (length (member (state-turnu st) usrs))))
      (define newind (modulo (add1 oldind) (length usrs)))
      (set-state-turnu! st (list-ref usrs newind))
      (broadcast "It's ~a's turn." (user-name (state-turnu st)))
      (define roll (random-roll))
      (broadcast "~a rolls a ~a." (user-name (state-turnu st)) roll)
      (apply-roll! st roll)]))

;; hash mapping buyable things to prices
(define item-prices `#hash(
  [city . #hash([ore . 3] [grain . 2] [clay . 0] [wood . 0] [sheep . 0])]
  [settlement . #hash([ore . 0] [grain . 1] [clay . 1] [wood . 1] [sheep . 1])]
  [dev-card . #hash([ore . 1] [grain . 1] [clay . 0] [wood . 0] [sheep . 1])]
  [road . #hash([ore . 0] [grain . 0] [clay . 1] [wood . 1] [sheep . 0])]))

;; determines whether or not the user can afford a given price (hash)
(define/contract (can-afford? usr price)
  (-> user? item? boolean?)
  (apply and
    (hash-map (lambda (res amt) (>= amt (hash-ref price res))) (user-res usr))))

;; removes a dev card from the top of the stack
(define/contract (pop-dev-card!)
  (-> dev-card?)
  (match-define (cons x xs) (state-cards st))
  (set-state-cards! st xs)
  x)

(define/contract (buy-item! usr item args)
  (-> user? item? (or/c vertex? edge? void?) response?)
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
              (broadcast "~a has built a ~a at ~a." (user-name usr) item
                (vertex->string args))]
            ['road
              (set-board-road-owner! b args usr)
              (broadcast "~a has built a road at ~a." (user-name usr)
                (edge->string args))]
            ['dev-card
              (define draw (pop-dev-card!))
              (set-user-cards! usr (cons draw (user-cards usr)))
              (broadcast "~a has built a dev card." (user-name usr))
              (format "You draw a ~a." draw)])]))

;; TODO
(define/contract (use-card! usr card-num)
  (-> user? integer? response?)
  (void))

;; TODO
(define/contract (bank! usr res-list target)
  (-> user? (listof resource?) resource? response?)
  (void))
