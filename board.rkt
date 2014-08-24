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

;; various predicates (TODO: move these to a separate module?)
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


(define-struct/contract board (
  [cells (hash/c cell? (cons/c roll-num? resource?))]
  [edges (hash/c edge? (or/c user? #f))]
  [verts (hash/c vertex? (or/c (cons/c user? building?) #f))]
  [thief cell?]) #:mutable #:transparent)

;; ----------------------------- HELPER FUNCTIONS -----------------------------
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

;; parses a vertex out of a term in the board template
(define (parse-vertex x) (match x
  [`(vertex ,_ ,a ,b ,s ,t ,x ,y) (list (cons a b) (cons s t) (cons x y))]
  [`(rep ,_ ,code) (parse-vertex code)]
  [_ #f]))

;; parses an edge out of a term in the board template
(define (parse-edge x) (match x
  [`(edge ,_ ,a ,b ,x ,y) (cons (cons a b) (cons x y))]
  [`(rep ,_ ,code) (parse-edge code)]
  [_ #f]))


;; recurse on the board template to display the board
;; returns a string and its ending colour
(define/contract (fill-template b tp col)
  (-> board? list? integer? (cons/c string? integer?))
  ; (printf "MYDEB: ~a, ~a, ~a\n" b "" col) TODO: remove
  (cond
    [(empty? tp) (cons "" col)]
    [else (define str (match (first tp)
      [(? char? c) (with-color col 37 (~a c))]
      ['nl (~a #\newline)]
      ['sp (~a #\space)]
      [`(str ,str) (with-color col 37 str)]
      [`(rep ,amt ,code)
        (match-define (cons rst newcol) (fill-template b (list code) col))
        (replicate amt rst)]
      [`(thief ,x ,y)
        (with-color col 31 (if (equal? (board-thief b) (cons x y)) "T" " "))]
      [`(num ,x ,y)
        (define num (board-cell-number b (cons x y)))
        (with-color col 37 (~a (match num ['nil ""] [x x]) #:width 2))]
      [`(res ,x ,y)
        (define res (board-cell-resource b (cons x y)))
        (define rcolor (match res
          ['sheep 32] ['clay 35] ['grain 33] ['wood 32] ['ore 36] [_ 37]))
        (with-color col rcolor (substring (symbol->string res) 0 1))]
      [`(edge ,char ,u ,v ,x ,y)
        (define edge (cons (cons u v) (cons x y)))
        (define owner (board-road-owner b (edge-normalize edge)))
        (with-color col (if owner (user-color owner) 37) (~a char))]
      [`(vertex ,char ,s ,t ,u ,v ,x ,y)
        (match (board-vertex-pair b (list (cons s t) (cons u v) (cons x y)))
          [#f (with-color col 37 (~a char))]
          [(cons owner bldg) (with-color col (user-color owner)
                               (substring (symbol->string bldg) 0 1))])]))
      (match-define (cons rst newcol) (fill-template b (rest tp) col))
      (cons (string-append str rst) newcol)]))

;; --------------------------------- CONSTANTS ---------------------------------
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
  ((-1 . -5) 0 . -4) ((0 . -4) 1 . -5) ((0 . -6) 0 . -4)))

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
  ((-1 . -5) (0 . -6) (0 . -4)) ((0 . -6) (0 . -4) (1 . -5))))

;; -------------------------- CONSTRUCTING FUNCTIONS --------------------------
;; construct a new random board
(define/contract (create-board)
  (-> board?)
  (define roll-list (shuffle '(2 3 3 4 4 5 5 6 6 8 8 9 9 10 10 11 11 12)))
  (define res-list (shuffle '(ore ore ore wood wood wood wood clay clay clay
                              grain grain grain grain sheep sheep sheep sheep)))
  (define cell-list (shuffle board-cell-list))
  (board (make-hash (map cons cell-list
                      (cons (cons 'nil 'desert) (map cons roll-list res-list))))
         (make-hash (map (curryr cons #f) board-edge-list))
         (make-hash (map (curryr cons #f) board-vertex-list))
         (first cell-list)))

;; ---------------------------- QUERYING FUNCTIONS ----------------------------
;; displays a board as a string
(define/contract (board->string b)
  (-> board? string?)
  (car (fill-template b board-template -1)))

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
