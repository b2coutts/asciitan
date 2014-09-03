;; contains helper functions specifically related to this project, which are
;; used in multiple modules
#lang racket

(require racket/contract "basic.rkt" "cell.rkt" "data.rkt" "constants.rkt")

(provide
  resource->color show-res style->string uname

  cell->label label->cell

  user=?

  edge->string vertex->string string->edge string->vertex
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

;; produces a color-coded amount of resources
(define/contract (show-res amt res)
  (-> integer? resource? string?)
  (format "~a~a ~a" (style->string `(,(resource->color res) 40 #f #f)) amt res))

;; given a style, produce the ANSI escape string for inducing that style
(define/contract (style->string sty)
  (-> style? string?)
  (match-define (list bg fg bold underline) sty)
  (format "~a[~a;~a~a~am" (integer->char #x1b) bg fg (if bold ";1" ";22")
                          (if underline ";4" ";24"))) 

;; given a user, produce a string of its user name with correct color-coding
(define/contract (uname usr)
  (-> user? string?)
  (format "~a~a~a" (style->string `(,(user-color usr) 40 #t #f)) (user-name usr)
                   reset))

;; produce the label of a given cell
(define/contract (cell->label cell)
  (-> cell-valid? symbol?)
  (cdr (assoc cell (map (lambda (x) (cons (cdr x) (car x))) cell-labels))))

;; produce the cell associated with the given label; #f if the label is invalid
(define/contract (label->cell lbl)
  (-> symbol? (or/c cell-valid? #f))
  (define result (assoc lbl cell-labels))
  (if result (cdr result) #f))

;; #t iff two users have the same name
(define/contract (user=? usr1 usr2)
  (-> user? user? boolean?)
  (string=? (user-name usr1) (user-name usr2)))

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
  (match-define (list least most) (remove cell vtx))

  (define pt (match (cons (- (car least) (car cell)) (- (cdr least) (cdr cell)))
    [(cons -1 -1) (match (cons (- (car most) (car cell))
                               (- (cdr most) (cdr cell)))
                    [(cons -1 1) 6]
                    [(cons 0 -2) 5])]
    [(cons -1 1)  1]
    [(cons 0 2)   2]
    [(cons 1 -1)  3]
    [(cons 0 -2)  4]))

  (format "~a~a.~a~a" (style->string '(40 37 #t #f)) (cell->label cell) pt
                      (style->string '(40 37 #f #f))))

;; helper function; "adds" two cells as vectors
(define/contract (cell-add c1 c2)
  (-> cell? cell? cell?)
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

;; read an edge from a string; #f if the string is invalid
(define/contract (string->edge str)
  (-> string? (or/c edge? #f))
  (match (string->list str)
    [`(,label #\- ,num)
      (define cell (label->cell (string->symbol (list->string (list label)))))
      (cond
        [cell (edge-normalize (cons cell (cell-add cell (match num
          [#\1 (cons 0 2)]
          [#\2 (cons 1 1)]
          [#\3 (cons 1 -1)]
          [#\4 (cons 0 -2)]
          [#\5 (cons -1 -1)]
          [#\6 (cons -1 1)]))))]
        [else #f])]
    [_ #f]))

;; read a vertex from a string; #f if the string is invalid
(define/contract (string->vertex str)
  (-> string? (or/c vertex? #f))
  (match (string->list str)
    [`(,label #\. ,num)
      (define c (label->cell (string->symbol (list->string (list label)))))
      (cond
        [c (vertex-normalize (match num
          [#\1 (list c (cell-add c (cons -1 1)) (cell-add c (cons 0 2)))]
          [#\2 (list c (cell-add c (cons 0 2)) (cell-add c (cons 1 1)))]
          [#\3 (list c (cell-add c (cons 1 1)) (cell-add c (cons 1 -1)))]
          [#\4 (list c (cell-add c (cons 1 -1)) (cell-add c (cons 0 -2)))]
          [#\5 (list c (cell-add c (cons 0 -2)) (cell-add c (cons -1 -1)))]
          [#\6 (list c (cell-add c (cons -1 -1)) (cell-add c (cons -1 1)))]))]
        [else #f])]
    [_ #f]))
