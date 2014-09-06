;; various useful constant values
#lang racket

(provide item-prices dev-cards items commands client-commands icommands
         resources responses showables prompts

         col-esc reset

         trading-posts board-cell-list cell-labels board-edge-list
         board-vertex-list)

;; list of resources
(define resources '(wood grain sheep ore clay))

;; hash mapping buyable things to prices
(define item-prices `#hash(
  [city . #hash([ore . 3] [grain . 2] [clay . 0] [wood . 0] [sheep . 0])]
  [settlement . #hash([ore . 0] [grain . 1] [clay . 1] [wood . 1] [sheep . 1])]
  [dev-card . #hash([ore . 1] [grain . 1] [clay . 0] [wood . 0] [sheep . 1])]
  [road . #hash([ore . 0] [grain . 0] [clay . 1] [wood . 1] [sheep . 0])]))

;; ordered list of all dev cards
(define dev-cards
  (append (build-list 14 (const 'knight))
          (build-list 5  (const 'veep)) ;; TODO: separate these?
          (build-list 2  (const 'monopoly))
          (build-list 2  (const 'year-of-plenty))
          (build-list 2  (const 'road-building))))

;; list of all buyable items
(define items '(city settlement dev-card road))

;; list of valid commands to the server
(define commands '(buy use bank end show respond say offer request-update))

;; list of client-only commands (i.e., ones which get translated to respond)
(define client-commands '(move discard steal help take choose build place accept
                          decline))

;; list of commands which a player can use when it is not their turn
(define icommands '(show say respond))

;; list of showable things (i.e., valid arguments to show)
(define showables '(turn veeps board resources users dev-cards))

;; list of valid response types (from the server to the client)
(define responses '(broadcast message update say prompt game-over))

;; list of requests with which the user can be prompted
(define prompts '(move-thief discard-resources pick-target monopoly
                  year-of-plenty road-building init-settlement init-road trade))

;; ANSI terminal color code escape character
(define col-esc (integer->char #x1b))

;; string to reset ANSI terminal mode to default
(define reset (format "~a[37;40;22;24m" col-esc))

;; list of all 9 trading posts, ordered starting from top-left, going clockwise
(define trading-posts
  '((sheep ((-2 . 4) (-1 . 3) (-1 . 5)) ((-1 . 3) (-1 . 5) (0 . 4)))
    (any ((0 . 4) (1 . 3) (1 . 5)) ((1 . 3) (1 . 5) (2 . 4)))
    (any ((2 . 2) (2 . 4) (3 . 3)) ((2 . 2) (3 . 1) (3 . 3)))
    (clay ((2 . 0) (3 . -1) (3 . 1)) ((2 . -2) (2 . 0) (3 . -1)))
    (wood ((1 . -3) (2 . -4) (2 . -2)) ((1 . -5) (1 . -3) (2 . -4)))
    (any ((0 . -6) (0 . -4) (1 . -5)) ((-1 . -5) (0 . -6) (0 . -4)))
    (grain ((-2 . -4) (-1 . -5) (-1 . -3)) ((-2 . -4) (-2 . -2) (-1 . -3)))
    (ore ((-3 . -1) (-2 . -2) (-2 . 0)) ((-3 . -1) (-3 . 1) (-2 . 0)))
    (any ((-3 . 1) (-3 . 3) (-2 . 2)) ((-3 . 3) (-2 . 2) (-2 . 4)))))

;; list of every cell on the board
(define board-cell-list '(
  (-2 . -2) (-2 . 0) (-2 . 2) (-1 . -3) (-1 . -1) (-1 . 1) (-1 . 3) (0 . -4)
  (0 . -2) (0 . 0) (0 . 2) (0 . 4) (1 . -3) (1 . -1) (1 . 1) (1 . 3) (2 . -2)
  (2 . 0) (2 . 2)))

;; association list bijecting cells with labels
(define cell-labels
  (map cons '(A B C D E F G H I J K L M N O P Q R S) board-cell-list))

;; normalized list of every edge on the board
(define board-edge-list '(
  ((0 . 2) 0 . 4) ((-1 . 5) 0 . 4) ((0 . 4) 1 . 5) ((-1 . 3) -1 . 5)
  ((1 . 3) 1 . 5) ((-2 . 4) -1 . 3) ((-1 . 3) 0 . 4) ((0 . 4) 1 . 3)
  ((1 . 3) 2 . 4) ((-2 . 2) -2 . 4) ((2 . 2) 2 . 4) ((-3 . 3) -2 . 2)
  ((-2 . 2) -1 . 3) ((-1 . 3) 0 . 2) ((0 . 2) 1 . 3) ((1 . 3) 2 . 2)
  ((2 . 2) 3 . 3) ((-1 . 1) -1 . 3) ((1 . 1) 1 . 3) ((-3 . 1) -2 . 2)
  ((-2 . 2) -1 . 1) ((-1 . 1) 0 . 2) ((0 . 2) 1 . 1) ((1 . 1) 2 . 2)
  ((2 . 2) 3 . 1) ((-2 . 0) -2 . 2) ((0 . 0) 0 . 2) ((2 . 0) 2 . 2)
  ((-3 . 1) -2 . 0) ((-2 . 0) -1 . 1) ((-1 . 1) 0 . 0) ((0 . 0) 1 . 1)
  ((1 . 1) 2 . 0) ((2 . 0) 3 . 1) ((-1 . -1) -1 . 1) ((1 . -1) 1 . 1)
  ((-3 . -1) -2 . 0) ((-2 . 0) -1 . -1) ((-1 . -1) 0 . 0) ((0 . 0) 1 . -1)
  ((1 . -1) 2 . 0) ((2 . 0) 3 . -1) ((-2 . -2) -2 . 0) ((0 . -2) 0 . 0)
  ((2 . -2) 2 . 0) ((-3 . -1) -2 . -2) ((-2 . -2) -1 . -1) ((-1 . -1) 0 . -2)
  ((0 . -2) 1 . -1) ((1 . -1) 2 . -2) ((2 . -2) 3 . -1) ((-1 . -3) -1 . -1)
  ((1 . -3) 1 . -1) ((-3 . -3) -2 . -2) ((-2 . -2) -1 . -3) ((-1 . -3) 0 . -2)
  ((0 . -2) 1 . -3) ((1 . -3) 2 . -2) ((2 . -2) 3 . -3) ((-2 . -4) -2 . -2)
  ((0 . -4) 0 . -2) ((2 . -4) 2 . -2) ((-2 . -4) -1 . -3) ((-1 . -3) 0 . -4)
  ((0 . -4) 1 . -3) ((1 . -3) 2 . -4) ((-1 . -5) -1 . -3) ((1 . -5) 1 . -3)
  ((-1 . -5) 0 . -4) ((0 . -4) 1 . -5) ((0 . -6) 0 . -4) ((0 . 4) 0 . 6)))

;; normalized list of every vertex on the board
(define board-vertex-list '(
  ((-1 . 5) (0 . 4) (0 . 6)) ((0 . 4) (0 . 6) (1 . 5))
  ((-2 . 4) (-1 . 3) (-1 . 5)) ((-1 . 3) (-1 . 5) (0 . 4))
  ((0 . 4) (1 . 3) (1 . 5)) ((1 . 3) (1 . 5) (2 . 4))
  ((-3 . 3) (-2 . 2) (-2 . 4)) ((-2 . 2) (-2 . 4) (-1 . 3))
  ((-1 . 3) (0 . 2) (0 . 4)) ((0 . 2) (0 . 4) (1 . 3))
  ((1 . 3) (2 . 2) (2 . 4)) ((2 . 2) (2 . 4) (3 . 3))
  ((-3 . 1) (-3 . 3) (-2 . 2)) ((-2 . 2) (-1 . 1) (-1 . 3))
  ((-1 . 1) (-1 . 3) (0 . 2)) ((0 . 2) (1 . 1) (1 . 3))
  ((1 . 1) (1 . 3) (2 . 2)) ((2 . 2) (3 . 1) (3 . 3))
  ((-3 . 1) (-2 . 0) (-2 . 2)) ((-2 . 0) (-2 . 2) (-1 . 1))
  ((-1 . 1) (0 . 0) (0 . 2)) ((0 . 0) (0 . 2) (1 . 1))
  ((1 . 1) (2 . 0) (2 . 2)) ((2 . 0) (2 . 2) (3 . 1))
  ((-3 . -1) (-3 . 1) (-2 . 0)) ((-2 . 0) (-1 . -1) (-1 . 1))
  ((-1 . -1) (-1 . 1) (0 . 0)) ((0 . 0) (1 . -1) (1 . 1))
  ((1 . -1) (1 . 1) (2 . 0)) ((2 . 0) (3 . -1) (3 . 1))
  ((-3 . -1) (-2 . -2) (-2 . 0)) ((-2 . -2) (-2 . 0) (-1 . -1))
  ((-1 . -1) (0 . -2) (0 . 0)) ((0 . -2) (0 . 0) (1 . -1))
  ((1 . -1) (2 . -2) (2 . 0)) ((2 . -2) (2 . 0) (3 . -1))
  ((-3 . -3) (-3 . -1) (-2 . -2)) ((-2 . -2) (-1 . -3) (-1 . -1))
  ((-1 . -3) (-1 . -1) (0 . -2)) ((1 . -3) (1 . -1) (2 . -2))
  ((2 . -2) (3 . -3) (3 . -1)) ((-3 . -3) (-2 . -4) (-2 . -2))
  ((-2 . -4) (-2 . -2) (-1 . -3)) ((0 . -4) (0 . -2) (1 . -3))
  ((1 . -3) (2 . -4) (2 . -2)) ((2 . -4) (2 . -2) (3 . -3))
  ((-2 . -4) (-1 . -5) (-1 . -3)) ((-1 . -5) (-1 . -3) (0 . -4))
  ((0 . -4) (1 . -5) (1 . -3)) ((1 . -5) (1 . -3) (2 . -4))
  ((-1 . -5) (0 . -6) (0 . -4)) ((0 . -6) (0 . -4) (1 . -5))
  ((0 . -2) (1 . -3) (1 . -1)) ((-1 . -3) (0 . -4) (0 . -2))))
