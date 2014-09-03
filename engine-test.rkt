;; module for testing the game engine (without tcp, etc)
#lang racket

(require "data.rkt" "constants.rkt" "board.rkt" "cell.rkt" "engine.rkt"
         "basic.rkt" "adv.rkt")

;; ----------------------------- UTILITY FUNCTIONS -----------------------------
;; produce a human-readable string for the given user
(define/contract (user->string usr)
  (-> user? string?)
  (string-append
    (format "~a[~am~a~a[37m: " col-esc (user-color usr) (user-name usr) col-esc)
    (format "dev-cards: ~a; " (user-cards usr))
    (apply (curry format "wood: ~a, grain: ~a, sheep: ~a, ore: ~a, clay: ~a\n")
           (map (lambda (res) (hash-ref (user-res usr) res))
                '(wood grain sheep ore clay)))))

;; produce a human-readable string for a state
(define/contract (state->string st [show-dev-cards #f])
  (->* (state?) (boolean?) string?)
  (string-append
    "---------------------------------------------------------------------\n"
    (format "~a's turn.\n" (user-name (state-turnu st)))
    (board->string (state-board st))
    (apply string-append (map user->string (state-users st)))
    (if show-dev-cards (format "~a\n" (state-cards st)) "")
    "---------------------------------------------------------------------\n"))

;; --------------------------------- MAIN CODE ---------------------------------
;; create initial state
;; seed random number generator to make the test deterministic
(random-seed 1231)

;; function to avoid eq?
(define (no-res) (make-hash (map (lambda (res) (cons res 0))
                               '(wood grain sheep ore clay))))

(define ron (user "ron" 2 '(knight veep monopoly year-of-plenty road-building)
  (no-res) 95 
  (list (current-input-port) (current-output-port) (make-semaphore 1))))
(define dan (user "dan" 2 '() (no-res) 96
  ;;(list (current-input-port) (open-output-file "/dev/null" #:exists 'truncate)
  (list (current-input-port) (current-output-port)
        (make-semaphore 1))))
(define st (init-state (list ron dan)))
(define b (state-board st))

;; helper function; run a server command and print the response
(define/contract (act! usr act)
  (-> user? (cons/c command? list?) void?)
  (define resp (handle-action! st usr act))
  (printf "~a: ~a\n" (user-name usr) resp))

;; main code of interacting with the server
(printf "~a[37m" col-esc) ;; set color to white
(display (state->string st))

(handle-action! st ron `(respond init-settlement ,(string->vertex "J.3")))
(handle-action! st ron `(respond init-road ,(string->edge "J-2")))

(handle-action! st dan `(respond init-settlement ,(string->vertex "F.1")))
(handle-action! st dan `(respond init-road ,(string->edge "F-1")))

(handle-action! st dan `(respond init-settlement ,(string->vertex "D.2")))
(handle-action! st dan `(respond init-road ,(string->edge "D-1")))

(handle-action! st ron `(respond init-settlement ,(string->vertex "K.2")))
(handle-action! st ron `(respond init-road ,(string->edge "K-2")))

(display (state->string st))
