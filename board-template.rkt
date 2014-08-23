;; contains a template for a board to be printed out
#lang racket

(provide board-template)

;; TODO: add in cities/settlements
(define board-template `(
  (rep 20 sp) (rep 5 (road #\_ '((0 . 2) . (0 . 4)))) (rep 44 sp) nl
  (rep 13 sp) (rep 5 #\s) sp (road #\/ '((-1 . 5) . (0 . 4))) sp sp
    (thief '(0 4)) sp sp (road #\\ '((0 . 4) . (1 . 5))) sp (rep 5 #\?)
    (rep 38 sp) nl
  (rep 13 sp) (rep 5 (road #\_ '((-1 . 3) . (-1 . 5))))
    (road #\/ '((-1 . 5) . (0 . 4))) sp sp sp (num '(0 . 4)) sp sp
    (road #\\ '((0 . 4) . (1 . 5))) (rep 5 (road #\_ '((1 . 3) . (1 . 5))))
    (rep 38 sp) nl
  (rep 12 sp) (road #\/ '((-2 . 4) . (-1 . 3))) sp sp (thief '(-1 . 3)) sp sp
    (road #\\ '((-1 . 3) . (0 . 4))) sp sp sp (res '(0 . 4)) sp sp
    (road #\/ '((0 . 4) . (1 . 3))) sp sp (thief '(1 . 3)) sp sp
    (road #\\ '((1 . 3) . (2 . 4))) (rep 37 sp) nl
  (rep 6 sp) (rep 5 (road #\_ '((-2 . 2) . (-2 . 4))))
    (road #\/ '((-2 . 4) . (-1 . 3))) sp sp sp (num '(-1 . 3)) sp sp
    (road #\\ '((-1 . 3) . (0 . 4))) (rep 5 (road #\_ '((0 . 2) . (0 . 4))))
    (road #\/ '((0 . 4) . (1 . 3))) sp sp sp (num '(1 . 3)) sp sp
    (road #\\ '((1 . 3) . (2 . 4))) (rep 5 (road #\_ '((2 . 2) . (2 . 4))))
    (rep 10 sp) #\+ (rep 18 #\-) #\+ nl
  
  continue on lines 54 and 30
