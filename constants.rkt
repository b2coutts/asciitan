;; various useful constant values
#lang racket

(provide item-prices dev-cards items
         board-cell-list board-edge-list board-vertex-list)

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
          (build-list 2  (const 'year-of-plenty))
          (build-list 2  (const 'road-building))))

;; list of all buyable items
(define items '(city settlement dev-card road))


;; list of every cell on the board
(define board-cell-list '(
  (-2 . -2) (-2 . 0) (-2 . 2) (-1 . -3) (-1 . -1) (-1 . 1) (-1 . 3) (0 . -4)
  (0 . -2) (0 . 0) (0 . 2) (0 . 4) (1 . -3) (1 . -1) (1 . 1) (1 . 3) (2 . -2)
  (2 . 0) (2 . 2)))

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