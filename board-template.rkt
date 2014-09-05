;; contains a template for a board to be printed out
#lang racket

(provide board-template)

(define board-template `(
  (rep 18 sp) (hedge -1 5 0 4 0 6 1 5) nl
  (rep 11 sp) (rep 5 (tp 0)) sp (edge #\/ -1 5 0 4) (label 0 4)
    (edge #\\ 0 4 1 5) sp (rep 5 (tp 1)) nl
  (rep 11 sp) (hedge -2 4 -1 3 -1 5 0 4) (edge #\/ -1 5 0 4) sp sp sp (num 0 4)
    sp sp (edge #\\ 0 4 1 5) (hedge 0 4 1 3 1 5 2 4) nl
  (rep 10 sp) (edge #\/ -2 4 -1 3) (label -1 3)
    (edge #\\ -1 3 0 4) sp sp sp (res 0 4) sp sp sp (edge #\/ 0 4 1 3)
    (label 1 3) (edge #\\ 1 3 2 4) nl
  (rep 4 sp) (hedge -3 3 -2 2 -2 4 -1 3) (edge #\/ -2 4 -1 3) sp sp sp
    (num -1 3) sp sp (edge #\\ -1 3 0 4) (hedge -1 3 0 2 0 4 1 3)
    (edge #\/ 0 4 1 3) sp sp sp (num 1 3) sp sp (edge #\\ 1 3 2 4)
    (hedge 1 3 2 2 2 4 3 3) nl
  (rep 1 sp) (tp 8) sp (edge #\/ -3 3 -2 2) (label -2 2)
    (edge #\\ -2 2 -1 3) sp sp sp (res -1 3) sp sp sp (edge #\/ -1 3 0 2)
    (label 0 2) (edge #\\ 0 2 1 3) sp sp sp (res 1 3) sp sp sp
    (edge #\/ 1 3 2 2) (label 2 2) (edge #\\ 2 2 3 3) sp (tp 2) nl
  (tp 8) (vertex #\space -3 1 -3 3 -2 2) (edge #\/ -3 3 -2 2) sp sp sp
    (num -2 2) sp sp (edge #\\ -2 2 -1 3) (hedge -2 2 -1 1 -1 3 0 2)
    (edge #\/ -1 3 0 2) sp sp sp (num 0 2) sp sp (edge #\\ 0 2 1 3)
    (hedge 0 2 1 1 1 3 2 2) (edge #\/ 1 3 2 2) sp sp sp (num 2 2) sp sp
    (edge #\\ 2 2 3 3) (vertex #\space 2 2 3 1 3 3) (tp 2) nl
  (rep 2 sp) (edge #\\ -3 1 -2 2) sp sp sp (res -2 2) sp sp sp
    (edge #\/ -2 2 -1 1) (label -1 1) (edge #\\ -1 1 0 2) sp sp sp
    (res 0 2) sp sp sp (edge #\/ 0 2 1 1) (label 1 1)
    (edge #\\ 1 1 2 2) sp sp sp (res 2 2) sp sp sp (edge #\/ 2 2 3 1) nl
  (rep 3 sp) (edge #\\ -3 1 -2 2) (hedge -3 1 -2 0 -2 2 -1 1)
    (edge #\/ -2 2 -1 1) sp sp sp (num -1 1) sp sp (edge #\\ -1 1 0 2)
    (hedge -1 1 0 0 0 2 1 1) (edge #\/ 0 2 1 1) sp sp sp (num 1 1) sp sp
    (edge #\\ 1 1 2 2) (hedge 1 1 2 0 2 2 3 1) (edge #\/ 2 2 3 1) nl
  (rep 3 sp) (edge #\/ -3 1 -2 0) (label -2 0) (edge #\\ -2 0 -1 1)
    sp sp sp (res -1 1) sp sp sp (edge #\/ -1 1 0 0) (label 0 0)
    (edge #\\ 0 0 1 1) sp sp sp (res 1 1) sp sp sp (edge #\/ 1 1 2 0)
    (label 2 0) (edge #\\ 2 0 3 1) nl
  (rep 1 sp) (vertex #\space -3 -1 -3 1 -2 0) (edge #\/ -3 1 -2 0) sp sp sp
    (num -2 0) sp sp (edge #\\ -2 0 -1 1) (hedge -2 0 -1 -1 -1 1 0 0)
    (edge #\/ -1 1 0 0) sp sp sp (num 0 0) sp sp (edge #\\ 0 0 1 1)
    (hedge 0 0 1 -1 1 1 2 0) (edge #\/ 1 1 2 0) sp sp sp (num 2 0) sp sp
    (edge #\\ 2 0 3 1) (vertex #\space 2 0 3 -1 3 1) nl
  (tp 7) sp (edge #\\ -3 -1 -2 0) sp sp sp (res -2 0) sp sp sp
    (edge #\/ -2 0 -1 -1) (label -1 -1) (edge #\\ -1 -1 0 0) sp sp
    sp (res 0 0) sp sp sp (edge #\/ 0 0 1 -1) (label 1 -1)
    (edge #\\ 1 -1 2 0) sp sp sp (res 2 0) sp sp sp (edge #\/ 2 0 3 -1) sp
    (tp 3) nl
  (rep 1 sp) (tp 7) sp (edge #\\ -3 -1 -2 0) (hedge -3 -1 -2 -2 -2 0 -1 -1)
    (edge #\/ -2 0 -1 -1) sp sp sp (num -1 -1) sp sp (edge #\\ -1 -1 0 0)
    (hedge -1 -1 0 -2 0 0 1 -1) (edge #\/ 0 0 1 -1) sp sp sp (num 1 -1) sp sp
    (edge #\\ 1 -1 2 0) (hedge 1 -1 2 -2 2 0 3 -1) (edge #\/ 2 0 3 -1) sp (tp 3)
    nl
  (rep 3 sp) (edge #\/ -3 -1 -2 -2) (label -2 -2)
    (edge #\\ -2 -2 -1 -1) sp sp sp (res -1 -1) sp sp sp (edge #\/ -1 -1 0 -2)
    (label 0 -2) (edge #\\ 0 -2 1 -1) sp sp sp (res 1 -1) sp sp sp
    (edge #\/ 1 -1 2 -2) (label 2 -2) (edge #\\ 2 -2 3 -1) nl
  (rep 1 sp) (vertex #\space -3 -3 -3 -1 -2 -2) (edge #\/ -3 -1 -2 -2) sp sp sp
    (num -2 -2) sp sp (edge #\\ -2 -2 -1 -1) (hedge -2 -2 -1 -3 -1 -1 0 -2)
    (edge #\/ -1 -1 0 -2) sp sp sp (num 0 -2) sp sp (edge #\\ 0 -2 1 -1)
    (hedge 0 -2 1 -3 1 -1 2 -2) (edge #\/ 1 -1 2 -2) sp sp sp (num 2 -2) sp sp
    (edge #\\ 2 -2 3 -1) (vertex #\space 2 -2 3 -3 3 -1) nl
  (rep 2 sp) (edge #\\ -3 -3 -2 -2) sp sp sp (res -2 -2) sp sp sp
    (edge #\/ -2 -2 -1 -3) (label -1 -3) (edge #\\ -1 -3 0 -2)
    sp sp sp (res 0 -2) sp sp sp (edge #\/ 0 -2 1 -3) (label 1 -3)
    (edge #\\ 1 -3 2 -2) sp sp sp (res 2 -2) sp sp sp (edge #\/ 2 -2 3 -3) nl
  (rep 3 sp) (edge #\\ -3 -3 -2 -2) (hedge -3 -3 -2 -4 -2 -2 -1 -3)
    (edge #\/ -2 -2 -1 -3) sp sp sp (num -1 -3) sp sp (edge #\\ -1 -3 0 -2)
    (hedge -1 -3 0 -4 0 -2 1 -3) (edge #\/ 0 -2 1 -3) sp sp sp (num 1 -3) sp sp
    (edge #\\ 1 -3 2 -2) (hedge 1 -3 2 -4 2 -2 3 -3) (edge #\/ 2 -2 3 -3) nl
  (rep 7 sp) (tp 6) sp (edge #\\ -2 -4 -1 -3) sp sp sp (res -1 -3) sp sp sp
    (edge #\/ -1 -3 0 -4) (label 0 -4) (edge #\\ 0 -4 1 -3) sp sp sp
    (res 1 -3) sp sp sp (edge #\/ 1 -3 2 -4) sp (tp 4) nl
  (rep 8 sp) (tp 6) sp (edge #\\ -2 -4 -1 -3) (hedge -2 -4 -1 -5 -1 -3 0 -4)
    (edge #\/ -1 -3 0 -4) sp sp sp (num 0 -4) sp sp (edge #\\ 0 -4 1 -3)
    (hedge 0 -4 1 -5 1 -3 2 -4) (edge #\/ 1 -3 2 -4) sp (tp 4) nl
  (rep 16 sp) (edge #\\ -1 -5 0 -4) sp sp sp (res 0 -4) sp sp sp
    (edge #\/ 0 -4 1 -5) nl
  (rep 17 sp) (edge #\\ -1 -5 0 -4) (hedge -1 -5 0 -6 0 -4 1 -5)
    (edge #\/ 0 -4 1 -5) nl
  (rep 18 sp) (rep 5 (tp 5)) nl))
