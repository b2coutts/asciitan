;; contains helper functions specifically related to this project, which are
;; used in multiple modules
#lang racket

(require racket/contract "basic.rkt" "cell.rkt" "data.rkt" "constants.rkt")

(provide
  resource->color style->string

  cell->label label->cell

  edge->string vertex->string
)

;; produce the color code of the given resource
(define/contract (resource->color res)
  (-> (or/c resource? 'desert) integer?)
  (match res ['sheep 92]
             ['clay 31]
             ['grain 33]
             ['wood 32]
             ['ore 90]
             ['desert 37]))

;; given a style, produce the ANSI escape string for inducing that style
(define/contract (style->string sty)
  (-> style? string?)
  (match-define (list bg fg bold underline) sty)
  (format "~a[~a;~a~a~am" (integer->char #x1b) bg fg (if bold ";1" ";22")
                          (if underline ";4" ";24"))) 

;; produce the label of a given cell
(define/contract (cell->label cell)
  (-> cell-valid? symbol?)
  (cdr (assoc cell (map (lambda (x) (cons (cdr x) (car x))) cell-labels))))

;; produce the cell associated with the given label
(define/contract (label->cell lbl)
  (-> symbol? cell-valid?)
  (cdr (assoc lbl cell-labels)))

;; pretty-print an edge
(define/contract (edge->string edg)
  (-> edge? string?)
  (match-define (cons cell othr) (cond
    [(cell-valid? (car edg)) edg]
    [else (cons (cdr edg) (car edg))]))

  (define side (match (cons (- (car othr) (car cell)) (- (cdr othr) (cdr cell)))
    [(cons 0 2)   1]
    [(cons 1 1)   2]
    [(cons 1 -1)  3]
    [(cons 0 -2)  4]
    [(cons -1 -1) 5]
    [(cons -1 1)  6]))

  (format "~a~a-~a~a" (style->string '(40 37 #t #f)) (cell->label cell) side
                      (style->string '(40 37 #f #f))))

;; pretty-print a vertex
(define/contract (vertex->string vtx)
  (-> vertex? string?)
  (define cell (first (filter (curryr member? board-cell-list) vtx)))
  (define least (first (remove cell vtx)))

  (define pt (match (cons (- (car least) (car cell)) (- (cdr least) (cdr cell)))
    [(cons -1 -1) (match (last (remove cell vtx))
                    [(cons -1 1) 6]
                    [(cons 0 -2) 5])]
    [(cons -1 1)  1]
    [(cons 0 2)   2]
    [(cons 1 -1)  3]
    [(cons 0 -2)  4]))

  (format "~a~a.~a~a" (style->string '(40 37 #t #f)) (cell->label cell) pt
                      (style->string '(40 37 #f #f))))
