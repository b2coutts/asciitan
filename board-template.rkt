;; contains a template for a board to be printed out
#lang racket

(provide board-template)

;; TODO: add in cities/settlements
(define board-template `(
  (rep 20 sp) (vertex #\_ -1 5 0 4 0 6) (rep 3 (edge #\_ 0 2 0 4))
    (vertex #\_ 0 4 0 6 1 5) nl
  (rep 13 sp) (rep 5 #\s) sp (edge #\/ -1 5 0 4) sp sp (thief 0 4) sp sp
    (edge #\\ 0 4 1 5) sp (rep 5 #\?) nl
  (rep 13 sp) (vertex #\_ -2 4 -1 3 -1 5) (rep 3 (edge #\_ -1 3 -1 5))
    (vertex #\_ -1 3 -1 5 0 4) (edge #\/ -1 5 0 4) sp sp sp
    (num 0 4) sp sp (edge #\\ 0 4 1 5) (vertex #\_ 0 4 1 3 1 5)
    (rep 3 (edge #\_ 1 3 1 5)) (vertex #\_ 1 3 1 5 2 4) nl
  (rep 12 sp) (edge #\/ -2 4 -1 3) sp sp (thief -1 3) sp sp
    (edge #\\ -1 3 0 4) sp sp sp (res 0 4) sp sp sp (edge #\/ 0 4 1 3) sp sp
    (thief 1 3) sp sp (edge #\\ 1 3 2 4) nl
  (rep 6 sp) (vertex #\_ -3 3 -2 2 -2 4) (rep 3 (edge #\_ -2 2 -2 4))
    (vertex #\_ -2 2 -2 4 -1 3) (edge #\/ -2 4 -1 3) sp sp sp (num -1 3) sp sp
    (edge #\\ -1 3 0 4) (vertex #\_ -1 3 0 2 0 4) (rep 3 (edge #\_ 0 2 0 4))
    (vertex #\_ 0 2 0 4 1 3) (edge #\/ 0 4 1 3) sp sp sp (num 1 3) sp sp
    (edge #\\ 1 3 2 4) (vertex #\_ 1 3 2 2 2 4) (rep 3 (edge #\_ 2 2 2 4))
    (vertex #\_ 2 2 2 4 3 3) (rep 10 sp) #\+ (rep 18 #\-) #\+ nl
  (rep 3 sp) #\? sp (edge #\/ -3 3 -2 2) sp sp (thief -2 2) sp sp
    (edge #\\ -2 2 -1 3) sp sp sp (res -1 3) sp sp sp (edge #\/ -1 3 0 2) sp sp
    (thief 0 2) sp sp (edge #\\ 0 2 1 3) sp sp sp (res 1 3) sp sp sp
    (edge #\/ 1 3 2 2) sp sp (thief 2 2) sp sp (edge #\\ 2 2 3 3) sp #\?
    (rep 7 sp) (str "| Legend:          |") nl
  (rep 2 sp) #\? (vertex #\space -3 1 -3 3 -2 2) (edge #\/ -3 3 -2 2) sp sp sp
    (num -2 2) sp sp (edge #\\ -2 2 -1 3) (vertex #\_ -2 2 -1 1 -1 3)
    (rep 3 (edge #\_ -1 1 -1 3)) (vertex #\_ -1 1 -1 3 0 2) (edge #\/ -1 3 0 2)
    sp sp sp (num 0 2) sp sp (edge #\\ 0 2 1 3) (vertex #\_ 0 2 1 1 1 3)
    (rep 3 (edge #\_ 1 1 1 3)) (vertex #\_ 1 1 1 3 2 2) (edge #\/ 1 3 2 2) sp
    sp sp (num 2 2) sp sp (edge #\\ 2 2 3 3) (vertex #\space 2 2 3 1 3 3) #\?
    (rep 6 sp) (str "+---+--------------+") nl
  (rep 4 sp) (edge #\\ -3 1 -2 2) sp sp sp (res -2 2) sp sp sp
    (edge #\/ -2 2 -1 1) sp sp (thief -1 1) sp sp (edge #\\ -1 1 0 2) sp sp sp
    (res 0 2) sp sp sp (edge #\/ 0 2 1 1) sp sp (thief 1 1) sp sp
    (edge #\\ 1 1 2 2) sp sp sp (res 2 2) sp sp sp (edge #\/ 2 2 3 1) (rep 8 sp)
    (str "| s | sheep        |") nl
  (rep 5 sp) (edge #\\ -3 1 -2 2) (vertex #\_ -3 1 -2 0 -2 2)
    (rep 3 (edge #\_ -2 0 -2 2)) (vertex #\_ -2 0 -2 2 -1 1)
    (edge #\/ -2 2 -1 1) sp sp sp (num -1 1) sp sp (edge #\\ -1 1 0 2)
    (vertex #\_ -1 1 0 0 0 2) (rep 3 (edge #\_ 0 0 0 2))
    (vertex #\_ 0 0 0 2 1 1) (edge #\/ 0 2 1 1) sp sp sp (num 1 1) sp sp
    (edge #\\ 1 1 2 2) (vertex #\_ 1 1 2 0 2 2) (rep 3 (edge #\_ 2 0 2 2))
    (vertex #\_ 2 0 2 2 3 1) (edge #\/ 2 2 3 1) (rep 9 sp)
    (str "| c | clay         |") nl
(rep 5 sp) (edge #\/ -3 1 -2 0) sp sp (thief -2 0) sp sp (edge #\\ -2 0 -1 1)
    sp sp sp (res -1 1) sp sp sp (edge #\/ -1 1 0 0) sp sp (thief 0 0) sp sp
    (edge #\\ 0 0 1 1) sp sp sp (res 1 1) sp sp sp (edge #\/ 1 1 2 0) sp sp
    (thief 2 0) sp sp (edge #\\ 2 0 3 1) (rep 9 sp) (str "| g | grain        |")
    nl
  (rep 3 sp) (vertex #\space -3 -1 -3 1 -2 0) (edge #\/ -3 1 -2 0) sp sp sp
    (num -2 0) sp sp (edge #\\ -2 0 -1 1) (vertex #\_ -2 0 -1 -1 -1 1)
    (rep 3 (edge #\_ -1 -1 -1 1)) (vertex #\_ -1 -1 -1 1 0 0)
    (edge #\/ -1 1 0 0) sp sp sp (num 0 0) sp sp (edge #\\ 0 0 1 1)
    (vertex #\_ 0 0 0 2 1 1) (rep 3 (edge #\_ 1 -1 1 1))
    (vertex #\_ 1 -1 1 1 2 0) (edge #\/ 1 1 2 0) sp sp sp (num 2 0) sp sp
    (edge #\\ 2 0 3 1) (vertex #\space 2 0 3 -1 3 1) (rep 7 sp)
    (str "| w | wood         |") nl
  (rep 2 sp) #\o sp (edge #\\ -3 -1 -2 0) sp sp sp (res -2 0) sp sp sp
    (edge #\/ -2 0 -1 -1) sp sp (thief -1 -1) sp sp (edge #\\ -1 -1 0 0) sp sp
    sp (res 0 0) sp sp sp (edge #\/ 0 0 1 -1) sp sp (thief 1 -1) sp sp
    (edge #\\ 1 -1 2 0) sp sp sp (res 2 0) sp sp sp (edge #\/ 2 0 3 -1) sp #\c
    (rep 6 sp) (str "| o | ore          |") nl
  (rep 3 sp) #\o sp (edge #\\ -3 -1 -2 0) (vertex #\_ -3 -1 -2 -2 -2 0)
    (rep 3 (edge #\_ -2 -2 -2 0)) (vertex #\_ -2 -2 -2 0 -1 -1)
    (edge #\/ -2 0 -1 -1) sp sp sp (num -1 -1) sp sp (edge #\\ -1 -1 0 0)
    (vertex #\_ -1 -1 0 -2 0 0) (rep 3 (edge #\_ 0 -2 0 0))
    (vertex #\_ 0 -2 0 0 1 -1) (edge #\/ 0 0 1 -1) sp sp sp (num 1 -1) sp sp
    (edge #\\ 1 -1 2 0) (vertex #\_ 1 -1 2 -2 2 0) (rep 3 (edge #\_ 2 -2 2 0))
    (vertex #\_ 2 -2 2 0 3 -1) (edge #\/ 2 0 3 -1) sp #\c (rep 7 sp)
    (str "| d | desert       |") nl
  (rep 5 sp) (edge #\/ -3 -1 -2 -2) sp sp (thief -2 -2) sp sp
    (edge #\\ -2 -2 -1 -1) sp sp sp (res -1 -1) sp sp sp (edge #\/ -1 -1 0 -2)
    sp sp (thief 0 -2) sp sp (edge #\\ 0 -2 1 -1) sp sp sp (res 1 -1) sp sp sp
    (edge #\/ 1 -1 2 -2) sp sp (thief 2 -2) sp sp (edge #\\ 2 -2 3 -1)
    (rep 9 sp) (str "| T | thief        |") nl
  (rep 3 sp) (vertex #\space -3 -3 -3 -1 -2 -2) (edge #\/ -3 -1 -2 -2) sp sp sp
    (num -2 -2) sp sp (edge #\\ -2 -2 -1 -1) (vertex #\_ -2 -2 -1 -3 -1 -1)
    (rep 3 (edge #\_ -1 -3 -1 -1)) (vertex #\_ -1 -3 -1 -1 0 -2)
    (edge #\/ -1 -1 0 -2) sp sp sp (num 0 -2) sp sp (edge #\\ 0 -2 1 -1)
    (vertex #\_ 0 -2 0 0 1 -1) (rep 3 (edge #\_ 1 -3 1 -1))
    (vertex #\_ 1 -3 1 -1 2 -2) (edge #\/ 1 -1 2 -2) sp sp sp (num 2 -2) sp sp
    (edge #\\ 2 -2 3 -1) (vertex #\space 2 -2 3 -3 3 -1) (rep 7 sp)
    (str "| ? | 3:1 post     |") nl
  (rep 4 sp) (edge #\\ -3 -3 -2 -2) sp sp sp (res -2 -2) sp sp sp
    (edge #\/ -2 -2 -1 -3) sp sp (thief -1 -3) sp sp (edge #\\ -1 -3 0 -2)
    sp sp sp (res 0 -2) sp sp sp (edge #\/ 0 -2 1 -3) sp sp (thief 1 -3) sp sp
    (edge #\\ 1 -3 2 -2) sp sp sp (res 2 -2) sp sp sp (edge #\/ 2 -2 3 -3)
    (rep 8 sp) (str "| S | settlement   |") nl
  (rep 5 sp) (edge #\\ -3 -3 -2 -2) (vertex #\_ -3 -3 -2 -4 -2 -2)
    (rep 3 (edge #\_ -2 -4 -2 -2)) (vertex #\_ -2 -4 -2 -2 -1 -3)
    (edge #\/ -2 -2 -1 -3) sp sp sp (num -1 -3) sp sp (edge #\\ -1 -3 0 -2)
    (vertex #\_ -1 -3 -1 -1 0 -2) (rep 3 (edge #\_ 0 -4 0 -2))
    (vertex #\_ 0 -4 0 -2 1 -3) (edge #\/ 0 -2 1 -3) sp sp sp (num 1 -3) sp sp
    (edge #\\ 1 -3 2 -2) (vertex #\_ 1 -3 2 -4 2 -2)
    (rep 3 (edge #\_ 2 -4 2 -2)) (vertex #\_ 2 -4 2 -2 3 -3)
    (edge #\/ 2 -2 3 -3) (rep 9 sp) (str "| C | city         |") nl
  (rep 9 sp) #\g sp (edge #\\ -2 -4 -1 -3) sp sp sp (res -1 -3) sp sp sp
    (edge #\/ -1 -3 0 -4) sp sp (thief 0 -4) sp sp (edge #\\ 0 -4 1 -3) sp sp sp
    (res 1 -3) sp sp sp (edge #\/ 1 -3 2 -4) sp #\w (rep 13 sp)
    (str "+------------------+") nl
  (rep 10 sp) #\g sp (edge #\\ -2 -4 -1 -3) (vertex #\_ -2 -4 -1 -5 -1 -3)
    (rep 3 (edge #\_ -1 -5 -1 -3)) (vertex #\_ -1 -5 -1 -3 0 -4)
    (edge #\/ -1 -3 0 -4) sp sp sp (num 0 -4) sp sp (edge #\\ 0 -4 1 -3)
    (vertex #\_ 0 -4 1 -5 1 -3) (rep 3 (edge #\_ 1 -5 1 -3))
    (vertex #\_ 1 -5 1 -3 2 -4) (edge #\/ 1 -3 2 -4) sp #\w nl
  (rep 18 sp) (edge #\\ -1 -5 0 -4) sp sp sp (res 0 -4) sp sp sp
    (edge #\/ 0 -4 1 -5) nl
  (rep 19 sp) (edge #\\ -1 -5 0 -4) (vertex #\_ -1 -5 0 -6 0 -4)
    (rep 3 (edge #\_ 0 -6 0 -4)) (vertex #\_ 0 -6 0 -4 1 -5) (edge #\/ 0 -4 1 -5)
    nl
  (rep 20 sp) (rep 5 #\?) nl))
