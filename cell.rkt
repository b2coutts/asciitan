;; contains various functions for dealing with cells/coordinates
#lang racket

(require
  racket/contract
)

(provide
  board-cell-list
  board-edge-list
  board-vertex-list

  cell?
  cell-valid?
  edge?
  vertex?

  cell<
  edge-normalize
  vertex-normalize
  adj-cells
)

;; --------------------------------- CONSTANTS ---------------------------------
;; list of every cell on the board
(define board-cell-list '((-2 . -2) (-2 . 0) (-2 . 2)
                          (-1 . -3) (-1 . -1) (-1 . 1) (-1 . 3)
                          (0 . -4) (0 . -2) (0 . 0) (0 . 2) (0 . 4)
                          (1 . -3) (1 . -1) (1 . 1) (1 . 3)
                          (2 . -2) (2 . 0) (2 . 2)))

;; normalized list of every edge on the board
(define board-edge-list
  (map edge-normalize
    (filter (lambda (edge) (member? (car x) (adj-cells (cdr x))))
      (apply append
        (map (lambda (c1) (map (lambda (c2) (cons c1 c2)) board-cell-list))
             board-cell-list))))

;; normalized list of every vertex on the board
(define board-vertex-list
  (map vertex-normalize
    (filter (lambda (vertex) (match-define (list a b c) vertex)
              (and (member? a (adj-cells b))
                   (member? b (adj-cells c))
                   (member? c (adj-cells a))))
      (apply append
        (map (lambda (c1)
                (map (lambda (c2)
                        (map (lambda (c3) (list c1 c2 c3)) board-cell-list))
                     board-cell-list))
             board-cell-list)))))


;; --------------------------- PREDICATES/CONTRACTS ---------------------------
;; true iff cell is a valid cell coordinate (which may refer to a cell beyond
;; the boundary of the board
(define/contract (cell? cell)
  (-> any? boolean?)
  (match cell
    [(cons (? integer? x) (? integer? y)) (even? (+ x y))]
    [_ #f]))

;; true iff c refers to a cell on the board
(define/contract (cell-valid? cell)
  (-> cell? boolean?)
  (match-define (cons x y) cell)
  (match x
    [(or -2 2) (<= -2 y 2)]
    [(or -1 1) (<= -3 y 3)]
    [0 (<= -4 y 4)]
    [_ #f]))

;; true iff r is a valid edge on the board (pair of cells)
(define/contract (edge? edge)
  (-> any? boolean?)
  (match edge
    [(cons (? cell? a) (? cell? b))
      (and (member? a (adj-cells b)) ;; cells are adjacent (they form an edge)
           (or (cell-valid? a) (cell-valid? b)))] ;; edge is on the board
    [_ #f]))

;; true iff v is a valid vertex on the board (trio of cells)
(define/contract (vertex? vertex)
  (-> any? boolean?)
  (match vertex
    [(list (? cell? a) (? cell? b) (? cell? c))
      (and (member? a (adj-cells b))
           (member? b (adj-cells c))
           (member? c (adj-cells a))
           (ormap cell-valid? vertex))]
    [_ #f]))

;; ------------------------------- MISCELLANEOUS -------------------------------
;; comparison function, for sorting cells
(define/contract (cell< c1 c2)
  (-> cell? cell? boolean?)
  (cond
    [(< (car c1) (car c2)) #t]
    [(> (car c1) (car c2)) #f]
    [else (< (cdr c1) (cdr c2))]))

;; normalizes an edge, for equality testing
(define/contract (edge-normalize edge)
  (-> edge? edge?)
  (match-define (cons c1 c2) edge)
  (if (cell< c1 c2) edge (cons c2 c1)))

;; normalizes a vertex, for equality testing
(define/contract (vertex-normalize vertex)
  (-> vertex? vertex?)
  (sort vertex cell<))

;; produces a list of the cells adjacent to cell
(define/contract (adj-cells cell)
  (-> cell? (list cell? cell? cell? cell? cell? cell?))
  (match-define (cons x y) cell)
  (list (cons (- x 1) (- y 1)) (cons (- x 1) (+ y 1))
        (cons x (- y 2)) (cons x (+ y 2))
        (cons (+ x 1) (- y 1)) (cons (+ x 1) (+ y 1))))
