;; module for testing the game engine (without tcp, etc)
#lang racket

(require "data.rkt" "constants.rkt" "board.rkt" "cell.rkt" "engine.rkt"
         "util.rkt")

;; TODO: move some of these into existing modules?
;; ----------------------------- UTILITY FUNCTIONS -----------------------------
;; produce a human-readable string for the given user
(define/contract (user->string usr)
  (-> user? string?)
  (string-append
    (format "~a[~am~a~a[37m: " (integer->char #x1b) (user-color usr) (user-name usr)
      (integer->char #x1b))
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
(random-seed 1234)
(define io (list (current-input-port) (current-output-port) (make-semaphore 1)))
(define no-res (make-hash (map (lambda (res) (cons res 0))
                               '(wood grain sheep ore clay))))
(define ron (user "ron" '() no-res 95 io))
(define dan (user "dan" '() no-res 96 io))
(define st (state (list ron dan) ron (create-board) (shuffle dev-cards)))
(define b (state-board st))

;; assign initial settlements/roads
(define (init-rs!) ;; wrap in function to avoid polluting namespace
  (define (vtx a b c d e f) (list (cons a b) (cons c d) (cons e f)))
  (define (edg a b c d) (cons (cons a b) (cons c d)))

  (define usr (first (state-users st)))
  (define (f usr vtx) (set-board-vertex-pair! b vtx usr 'settlement))
  (define (g usr edge) (set-board-road-owner! b edge usr))

  (f ron (vtx -1 -1 0 -2 0 0))
  (g ron (edg 0 -2 0 0))
  (f ron (vtx 1 -1 1 1 2 0))
  (g ron (edg 1 -1 1 1))

  (f dan (vtx -2 2 -1 1 -1 3))
  (g dan (edg -2 2 -1 3))
  (f dan (vtx 1 -3 2 -4 2 -2))
  (g dan (edg 1 -3 2 -4)))
(init-rs!)

;; helper function; run a server command and print the response
(define/contract (act! usr act)
  (-> user? (cons/c command? list?) response?)
  (define resp (handle-action! st usr act))
  (printf "~a: ~a\n" (user-name usr) resp))

;; main code of interacting with the server
(printf "~a[37m" (integer->char #x1b)) ;; set color to white
(display (state->string st))

(act! ron '(end))
(act! dan '(end))
(act! ron '(end))
(act! dan '(end))
(act! ron '(end))

(display (state->string st))
