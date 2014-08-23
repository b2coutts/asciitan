#lang racket

(require racket/contract)
(require "cell.rkt")

(define-struct/contract board (
  [cells (hash/c cell? (cons/c roll-num? resource?))]
  [edges (hash/c edge? (or/c user? #f))]
  [verts (hash/c vertex? (or/c (cons/c user? building?) #f))]
  [thief cell?]) #:mutable #:transparent)

;; ----------------------------- HELPER FUNCTIONS -----------------------------
;; compute the cartesian product of a list with itself
(define/contract (cart-prod lst)
  (-> list? (listof cons?))
  (append
    (map (lambda (x)
          (map (curry 

;; true iff x is an element of lst
(define/contract (member? x lst)
  (-> any? list? boolean?)
  (if (member x lst) #t #f))

;; true iff bu is a building
(define/contract (building? bu)
  (-> any? boolean?)
  (member? bu '(settlement city)))

;; true iff n is a valid roll number; 'nil is used for desert
(define/contract (roll-num? n) 
  (-> any? boolean?)
  (or (member? n (build-list 12 add1)) (equal? n 'nil)))

;; produce a string which is str repeated amt times
(define/contract (replicate amt str)
  (-> integer? string? string?)
  (if (= amt 0) "" (string-append str (replicate (- amt 1) str))))

;; recurse on the board template to display the board
(define/contract (fill-template b tp)
  (-> board? list? string?)
  (cond
    [(empty? tp) ""]
    [else (define str (match (first tp)
      [(? char? c) (~a c)]
      ['nl (~a #\newline)]
      ['sp (~a #\space)]
      [`(rep ,amt ,code)
        (apply string-append (build-list amt (const (fill-template b tp))))]
      ['(thief ,cell) (if (equal? (board-thief b) cell) "T" " ")]
      [`(num ,cell) (~a (board-cell-number b cell) #:width 2)]
      [`(res ,cell)
        (substring (symbol->string (board-cell-resource b cell)) 0 1)]
      [`(road ,char ,edge)
        (define owner (board-road-owner b edge))
        (format "~a~a" (if owner (ansi-color-str (user-color owner)) "") char)]
      ))
      (string-append str (fill-template b (rest tp)))]))


;; -------------------------- CONSTRUCTING FUNCTIONS --------------------------
;; construct a new random board
(define/contract (create-board)
  (-> board?)
  (define roll-list (shuffle '(nil 2 3 3 4 4 5 5 6 6 8 8 9 9 10 10 11 11 12)))
  (define res-list (shuffle '(ore ore ore wood wood wood wood clay clay clay
                              grain grain grain grain sheep sheep sheep sheep
                              desert)))
  (board (make-hash (map cons board-cell-list (map cons roll-list res-list)))
         (make-hash (map (curryr cons #f) board-edge-list))
         (make-hash (map (curryr cons #f) board-vertex-list))
         (cdr (assoc 'nil (map cons roll-list board-cell-list)))))

;; ---------------------------- QUERYING FUNCTIONS ----------------------------
;; displays a board as a string
(define/contract (board->string b)
  (-> board? string?)
  (format "~a" b))

;; get the roll number of a cell
(define/contract (board-cell-number b cell)
  (-> board? cell-valid? roll-num?)
  (car (hash-ref (board-cells b) cell)))

;; get the resource of a cell
(define/contract (board-cell-resource b cell)
  (-> board? cell-valid? resource?)
  (cdr (hash-ref (board-cells b) cell)))

;; get the owner of the road on an edge (#f if no road is there)
(define/contract (board-road-owner b edge)
  (-> board? edge? (or/c user? #f))
  (hash-ref (board-edges b) edge))

;; get the owner/building pair of a vertex (#f if no building is there)
(define/contract (board-vertex-pair b vertex)
  (-> board? vertex? (or/c (cons/c user? building?) #f))
  (hash-ref (board-verts b) vertex))

;; ---------------------------- MUTATING FUNCTIONS ----------------------------
;; set the owner of the road on an edge
(define/contract (set-board-road-owner! b edge usr)
  (-> board? edge? user? void?)
  (hash-set! (board-edges b) edge usr))

;; set the owner/building pair of a vertex
(define/contract (set-board-vertex-pair! b vertex usr bldg)
  (-> board? vertex? user? building? void?)
  (hash-set! (board-verts b) vertex (cons usr bldg)))
