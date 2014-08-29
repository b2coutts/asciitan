#lang racket

(require "board.rkt" "data.rkt")

(define (vtx a b c d e f) (list (cons a b) (cons c d) (cons e f)))
(define (edg a b c d) (cons (cons a b) (cons c d)))

(define yel (user "yel" '() '#hash() 93
  (list (current-input-port) (current-output-port) (make-semaphore 0))))
(define blu (user "blu" '() '#hash() 94
  (list (current-input-port) (current-output-port) (make-semaphore 0))))
(define mag (user "mag" '() '#hash() 95
  (list (current-input-port) (current-output-port) (make-semaphore 0))))
(define cya (user "cya" '() '#hash() 96
  (list (current-input-port) (current-output-port) (make-semaphore 0))))

;; initial board
(define b (create-board))

(set-board-vertex-pair! b (vtx -3 -1 -2 -2 -2 0) yel 'settlement)
(set-board-road-owner! b (edg -3 -1 -2 0) yel)
(set-board-vertex-pair! b (vtx -3 -1 -3 1 -2 0) yel 'settlement)
(set-board-road-owner! b (edg -3 1 -2 0) yel)

(set-board-vertex-pair! b (vtx 0 0 0 2 1 1) blu 'settlement)
(set-board-road-owner! b (edg 0 0 0 2) blu)
(set-board-vertex-pair! b (vtx 0 -6 0 -4 1 -5) blu 'settlement)
(set-board-road-owner! b (edg 0 -4 1 -5) blu)

(set-board-vertex-pair! b (vtx 1 1 1 3 2 2) mag 'settlement)
(set-board-road-owner! b (edg 1 3 2 2) mag)
(set-board-vertex-pair! b (vtx -2 0 -1 -1 -1 1) mag 'settlement)
(set-board-road-owner! b (edg -2 0 -1 1) mag)

(set-board-vertex-pair! b (vtx 1 -3 1 -1 2 -2) cya 'settlement)
(set-board-road-owner! b (edg 1 -3 1 -1) cya)
(set-board-vertex-pair! b (vtx -2 2 -2 4 -1 3) cya 'settlement)
(set-board-road-owner! b (edg -2 4 -1 3) cya)

;; further developments
(set-board-road-owner! b (edg -1 1 0 2) blu)
(set-board-road-owner! b (edg -1 1 -1 3) blu)
(set-board-road-owner! b (edg -1 3 0 2) blu)
(set-board-vertex-pair! b (vtx -1 1 -1 3 0 2) blu 'city)
;; (set-board-vertex-pair! b (vtx -1 1 0 0 0 2) blu 'city)

;; display board
(display (board->string b))
