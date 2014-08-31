;; contains code for running the game
#lang racket

(require "board.rkt" "cell.rkt" "basic.rkt" "data.rkt" "constants.rkt" "adv.rkt")

(provide init-state handle-action!)

;; -------------------------- SMALL HELPER FUNCTIONS --------------------------
;; broadcast a server message to everyone
(define/contract (broadcast st fstr . args)
  (->* (state? string?) #:rest (listof any/c) void?)
  (map (lambda (usr)
        (match-define (list _ out mutex) (user-io usr))
        (define str (apply (curry format fstr) args))
        (call-with-semaphore mutex (thunk
          (fprintf out "~s\n" (list 'broadcast str)))))
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
    (if (zero? (hash-count stock)) "nothing" (apply string-append (add-between
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

;; given a user and a vertex, #t iff the user is allowed to build at the vertex
(define/contract (can-build? b usr vtx)
  (-> board? user? vertex? boolean?)
  ;; list of all neighbours of vtx, and vtx itself
  (define nbrs
    (filter (lambda (v) (> (length (filter (curryr member? vtx) v)) 1))
            board-vertex-list))

  ;; list of all edges adjacent to vtx
  (define edgs (filter (lambda (e) (match-define (cons a b) e)
                        (= (length (filter (curryr member? vtx) (list a b))) 2))
                       board-edge-list))

  (and (andmap (lambda (v) (not (board-vertex-pair b v))) nbrs)
       (ormap (lambda (e) (equal? (board-road-owner b e) usr)) edgs)))

;; given a user and an edge, #t iff the user is allowed to build a road there
(define/contract (can-road? b usr edg)
  (-> board? user? edge? boolean?)
  (match-define (cons x y) edg)

  ;; list of the 4 surrounding cells
  (define surr (append (list x y)
    (filter (lambda (cell) (or (member? (list cell x y) board-vertex-list)
                               (member? (list x cell y) board-vertex-list)
                               (member? (list x y cell) board-vertex-list)))
            board-cell-list)))

  ;; list of the 4 adjacent edges to edg
  (define edgs
    (filter (lambda (e) (= 2 (length (filter (curryr member? surr)
                                             (list (car e) (cdr e))))))
            board-edge-list))

  (and (not (board-road-owner b edg))
       (ormap (lambda (e) (equal? (board-road-owner b e) usr)) edgs)))

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
  (-> state? user? item? (or/c vertex? edge? void?) (or/c response? void?))
  (define b (state-board st))
  (cond
    [(not (equal? usr (state-turnu st))) (list 'message "It's not your turn!")]
    [(not (can-afford? usr item))
      (list 'message (format "You can't afford ~a!" item))]
    [(and (building? item) (not (can-build? b usr args)))
      (list 'message "You can't build a building there!")]
    [(and (equal? item 'road) (not (can-road? b usr args)))
      (list 'message "You can't build a road there!")]
    [(and (equal? item 'dev-card) (empty? (state-cards st)))
      (list 'message "There are no more dev cards left to draw!")]
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
              (define draw (pop-dev-card! st))
              (set-user-cards! usr (cons draw (user-cards usr)))
              (broadcast st "~a has built a dev card." (uname usr))
              (list 'message (format "You draw a ~a." draw))])]))

;; TODO
(define/contract (use-card! st usr card-num)
  (-> state? user? integer? (or/c response? void?))
  (void))

;; TODO
(define/contract (bank! st usr res-list target)
  (-> state? user? (listof resource?) resource? (or/c response? void?))
  (void))

;; say a message to a user
(define/contract (say st sender msg usr)
  (-> state? user? string? user? void?)
  (match-define (list _ out mutex) (user-io usr))
  (call-with-semaphore mutex (thunk
    (fprintf out "~s\n" (list 'say (uname sender) msg)))))

;; ------------------------------- API FUNCTIONS -------------------------------
;; handle a request from the user
;; TODO: replace any/c (3rd arg to ->) with (cons/c command? list?)
(define/contract (handle-action! st usr act)
  (-> (or/c state? #f) user? any/c (or/c response? void?))
  (logf 'debug "handle-action!: usr=~a, act=~s\n" (user-name usr) act)
  (match act
    [`(buy ,item ,args) (buy-item! st usr item args)]
    [`(use ,card-num) (use-card! st usr card-num)] ;; TODO: use card name instead?
    [`(bank ,res-list ,target) (bank! st usr res-list target)]
    [`(end) (change-turn! st)]
    [`(show board) (list 'raw (board->string (state-board st)))]
    [`(show resources)
      (list 'message (format "You have ~a." (stock->string (user-res usr))))]
    [`(ping ,str) (list 'message (format "pong ~a" str))]
    [`(say ,msg) (void (map (curry say st usr msg) (state-users st)))]
    [_ (list 'message (format "Invalid command: ~s" act))]))

;; creates a new state, given a non-empty list of users
(define/contract (init-state usrs)
  (-> (listof user?) state?)
  (state usrs (first usrs) (create-board) (shuffle dev-cards)))
