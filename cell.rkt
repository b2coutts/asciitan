;; contains various functions for dealing with cells/coordinates
#lang racket

(require
  racket/contract

  "util.rkt"
)

(provide
  cell?
  cell-valid?
  edge?
  vertex?

  cell<
  edge-normalize
  vertex-normalize
  adj-cells
  adj-vertices
  edge->string
  vertex->string
)

;; --------------------------- PREDICATES/CONTRACTS ---------------------------
;; true iff cell is a valid cell coordinate (which may refer to a cell beyond
;; the boundary of the board
(define/contract (cell? cell)
  (-> any/c boolean?)
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
  (-> any/c boolean?)
  (match edge
    [(cons (? cell? a) (? cell? b))
      (and (member? a (adj-cells b)) ;; cells are adjacent (they form an edge)
           (or (cell-valid? a) (cell-valid? b)))] ;; edge is on the board
    [_ #f]))

;; true iff v is a valid vertex on the board (trio of cells)
(define/contract (vertex? vertex)
  (-> any/c boolean?)
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
  (-> cell? (list/c cell? cell? cell? cell? cell? cell?))
  (match-define (cons x y) cell)
  (list (cons (- x 1) (- y 1)) (cons (- x 1) (+ y 1))
        (cons x (- y 2)) (cons x (+ y 2))
        (cons (+ x 1) (- y 1)) (cons (+ x 1) (+ y 1))))

;; TODO: edges adjacent to a cell?

;; produces a list of the vertices adjacent to a cell
(define/contract (adj-vertices cell)
  (-> cell? (list/c cell? cell? cell? cell? cell? cell?))
  (match-define (list c1 c2 c3 c4 c5 c6) (adj-cells cell))
  (list (list cell c1 c2) (list cell c2 c4) (list cell c4 c6)
        (list cell c6 c5) (list cell c5 c3) (list cell c3 c1)))

;; TODO
;; convert an edge to a user-friendly string
(define/contract (edge->string edge)
  (-> edge? string?)
  (~a edge))

;; TODO
;; convert vertex to a user-friendly string
(define/contract (vertex->string vtx)
  (-> vertex? string?)
  (~a vtx))
