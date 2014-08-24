#lang racket

(require "board.rkt" "util.rkt")

(define (vtx a b c d e f) (list (cons a b) (cons c d) (cons e f)))
(define (edg a b c d) (cons (cons a b) (cons c d)))

(define yellow (user 93))
(define blue (user 94))
(define magenta (user 95))
(define cyan (user 96))

;; initial board
(define b (create-board))

(set-board-vertex-pair! b (vtx -3 -1 -2 -2 -2 0) yellow 'settlement)
(set-board-road-owner! b (edg -3 -1 -2 0) yellow)
(set-board-vertex-pair! b (vtx -3 -1 -3 1 -2 0) yellow 'settlement)
(set-board-road-owner! b (edg -3 1 -2 0) yellow)

(set-board-vertex-pair! b (vtx 0 0 0 2 1 1) blue 'settlement)
(set-board-road-owner! b (edg 0 0 0 2) blue)
(set-board-vertex-pair! b (vtx 0 -6 0 -4 1 -5) blue 'settlement)
(set-board-road-owner! b (edg 0 -4 1 -5) blue)

(set-board-vertex-pair! b (vtx 1 1 1 3 2 2) magenta 'settlement)
(set-board-road-owner! b (edg 1 3 2 2) magenta)
(set-board-vertex-pair! b (vtx -2 0 -1 -1 -1 1) magenta 'settlement)
(set-board-road-owner! b (edg -2 0 -1 1) magenta)

(set-board-vertex-pair! b (vtx 1 -3 1 -1 2 -2) cyan 'settlement)
(set-board-road-owner! b (edg 1 -3 1 -1) cyan)
(set-board-vertex-pair! b (vtx -2 2 -2 4 -1 3) cyan 'settlement)
(set-board-road-owner! b (edg -2 4 -1 3) cyan)

;; further developments
(set-board-road-owner! b (edg -1 1 0 2) blue)
(set-board-road-owner! b (edg -1 1 -1 3) blue)
(set-board-road-owner! b (edg -1 3 0 2) blue)
(set-board-vertex-pair! b (vtx -1 1 -1 3 0 2) blue 'city)
;; (set-board-vertex-pair! b (vtx -1 1 0 0 0 2) blue 'city)

;; display board
(display (board->string b))
