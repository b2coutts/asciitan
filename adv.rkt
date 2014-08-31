;; contains helper functions specifically related to this project, which are
;; used in multiple modules
#lang racket

(require racket/contract "basic.rkt" "cell.rkt" "data.rkt" "constants.rkt")

(provide resource->color style->string cell->label label->cell)

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
  (format "~a[~a;~a~a~am" (integer->char #x1b) bg fg (if bold ";1" "")
                          (if underline ";4" ""))) 

;; produce the label of a given cell
(define/contract (cell->label cell)
  (-> cell-valid? symbol?)
  (assoc cell (map (lambda (x) (cons (cdr x) (car x))) cell-labels)))

;; produce the cell associated with the given label
(define/contract (label->cell lbl)
  (-> symbol? cell-valid?)
  (assoc lbl cell-labels))
