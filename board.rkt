#lang racket

(require
  racket/contract

  "util.rkt"
  "cell.rkt"
  "board-template.rkt"
)

(provide
  create-board
  board?

  board->string
  board-cell-number
  board-cell-resource
  board-road-owner
  board-vertex-pair
  board-thief

  set-board-road-owner!
  set-board-vertex-pair!
)

(define-struct/contract board (
  [cells (hash/c cell? (cons/c roll-num? resource?))]
  [edges (hash/c edge? (or/c user? #f))]
  [verts (hash/c vertex? (or/c (cons/c user? building?) #f))]
  [thief cell?]) #:mutable #:transparent)

;; ----------------------------- HELPER FUNCTIONS -----------------------------
;; true iff res is a resource
(define/contract (resource? res)
  (-> any/c boolean?)
  (member? res '(wood grain sheep ore clay desert)))

;; true iff bu is a building
(define/contract (building? bu)
  (-> any/c boolean?)
  (member? bu '(settlement city)))

;; true iff n is a valid roll number; 'nil is used for desert
(define/contract (roll-num? n) 
  (-> any/c boolean?)
  (or (member? n (build-list 12 add1)) (equal? n 'nil)))

;; produce a string which is str repeated amt times
(define/contract (replicate amt str)
  (-> integer? string? string?)
  (if (= amt 0) "" (string-append str (replicate (- amt 1) str))))

;; convenient macro for dealing with color changes
;; TODO: remove this color code reference from the code
;;   ((black) "30") ((red) "31")
;;   ((green) "32") ((yellow) "33")
;;   ((blue) "34") ((magenta) "35")
;;   ((cyan) "36") ((white) "37")
;;   ((default) "39"))
;; TODO: bg/bold/underline?
(define-syntax-rule (with-color current-color use-color str)
  (cond
    [(= current-color use-color) str]
    [else (set! current-color use-color)
          ;; TODO: is 30; needed after '['?
          (format "~a[~am~a" (integer->char #x1b) use-color str)]))

(printf "Red: ~a~a~a\n"
  (integer->char #x1b)
  (format "[~a;~a~a~am" 40 35 "" "")
  "text!")

;; recurse on the board template to display the board
(define/contract (fill-template b tp col)
  (-> board? list? integer? string?)
  (cond
    [(empty? tp) ""]
    [else (define str (match (first tp)
      [(? char? c) (with-color col 37 (~a c))]
      ['nl (~a #\newline)]
      ['sp (~a #\space)]
      [`(str ,str) (with-color col 37 str)]
      [`(rep ,amt ,code)
        (apply string-append (build-list amt (const (fill-template b tp))))]
      [`(thief ,x ,y)
        (with-color col 31 (if (equal? (board-thief b) (cons x y)) "T" " "))]
      [`(num ,x ,y)
        (with-color col 37 (~a (board-cell-number b (cons x y)) #:width 2))]
      [`(res ,x ,y)
        (with-color col 37
          (substring (symbol->string (board-cell-resource b (cons x y))) 0 1))]
      [`(edge ,char ,a ,b ,x ,y)
        (define edge (cons (cons a b) (cons x y)))
        (define owner (board-road-owner b (edge-normalize edge)))
        (with-color col (if owner (user-color owner) 37) (~a char))]
      [`(vertex ,char ,a ,b ,s ,t ,x ,y)
        (match (board-vertex-pair b (cons (cons a b) (cons s t) (cons x y)))
          [#f (with-color col 37 (~a char))]
          [(cons owner bldg) (with-color col (user-color owner)
                               (substring (symbol->string bldg) 0 1))])]))
      (string-append str (fill-template b (rest tp) col))]))

;; --------------------------------- CONSTANTS ---------------------------------
;; list of every cell on the board
(define board-cell-list '((-2 . -2) (-2 . 0) (-2 . 2)
                          (-1 . -3) (-1 . -1) (-1 . 1) (-1 . 3)
                          (0 . -4) (0 . -2) (0 . 0) (0 . 2) (0 . 4)
                          (1 . -3) (1 . -1) (1 . 1) (1 . 3)
                          (2 . -2) (2 . 0) (2 . 2)))

;; normalized list of every edge on the board
(define board-edge-list
  (remove-duplicates (filter-map (lambda (x) (match x
    [`(edge ,_ ,a ,b ,x ,y) (cons (cons a b) (cons x y))]
    [_ #f])))))

;; normalized list of every vertex on the board
(define board-vertex-list
  (remove-duplicates (filter-map (lambda (x) (match x
    [`(vertex ,_ ,a ,b ,s ,t ,x ,y) (cons (cons a b) (cons s t) (cons x y))]
    [_ #f])))))

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
  (fill-template b board-template -1))

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
  (hash-ref (board-edges b) (edge-normalize edge)))

;; get the owner/building pair of a vertex (#f if no building is there)
(define/contract (board-vertex-pair b vertex)
  (-> board? vertex? (or/c (cons/c user? building?) #f))
  (hash-ref (board-verts b) (vertex-normalize vertex)))

;; ---------------------------- MUTATING FUNCTIONS ----------------------------
;; set the owner of the road on an edge
(define/contract (set-board-road-owner! b edge usr)
  (-> board? edge? user? void?)
  (hash-set! (board-edges b) (edge-normalize edge) usr))

;; set the owner/building pair of a vertex
(define/contract (set-board-vertex-pair! b vertex usr bldg)
  (-> board? vertex? user? building? void?)
  (hash-set! (board-verts b) (vertex-normalize vertex) (cons usr bldg)))
