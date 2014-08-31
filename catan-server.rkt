#lang racket

(require "board.rkt" "data.rkt" "engine.rkt" "basic.rkt")

;; TODO: unhardcode this maybe or something
(define MAX-USERS 4)

(define st #f) ;; global state variable (initial value is a place holder)
(define mutex (make-semaphore 0)) ;; mutex for st

(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

;; produce a stock of empty resources for a user
(define/contract (empty-stock)
  (-> stock?)
  ;; TODO: change from 5 to 0
  (make-hash (map (lambda (res) (cons res 5)) '(wood grain sheep ore clay))))

;; begins interacting with a given user on the given input/output ports
;; TODO: code for ending
;; TODO: prompting
(define/contract (run-listener parent usr init-port)
  (-> thread? user? output-port? void?)
  (define tcpl (tcp-listen 0))
  (match-define-values (_ port _ _) (tcp-addresses tcpl #t))
  (send port init-port)
  (logf 'info "Awaiting listener connection from client ~a on port ~a...\n"
    (user-name usr) port)
  (define-values (in out) (tcp-accept tcpl))
  (file-stream-buffer-mode out 'line) ;; TODO: is line-buffering okay?
  (set-user-io! usr (list in out (third (user-io usr))))
  (logf 'info "Client ~a listener connection established.\n" (user-name usr))
  (thread-send parent 'done)
  (let loop []
    (logf 'debug "listener for ~a waiting for request...\n" (user-name usr))
    (define line (sync (read-line-evt in 'any))) ;; TODO: eof
    (define req (with-input-from-string line (thunk (read))))
    (logf 'debug "listener for ~a received request ~s\n" (user-name usr) req)
    (define response
      (call-with-semaphore mutex (thunk (handle-action! st usr req))))
    (logf 'debug "listener responding with ~s\n" response)
    (unless (void? response)
      (call-with-semaphore (third (user-io usr))
        (thunk (send response out))))
    (loop)))

;; dispatch listeners, generate the initial state
;; TODO: close initial tcp connection after establishing listener?
(define/contract (init-server [init-port 0])
  (->* () (integer?) state?)
  (define colors '(93 94 95 96))
  (define listener (tcp-listen init-port))
  (match-define-values (_ port _ _) (tcp-addresses listener #t))
  (logf 'info "Listening for connections on port ~a.\n" port)
  (define (loop usrs)
    (define continue (cond
      [(empty? usrs) #t]
      [(= (length usrs) MAX-USERS) #f]
      [else (equal? (prompt "[c]ontinue awaiting users, or [s]tart the game?"
                            (notin '(c s))) 'c)]))
    (cond
      [continue
        (logf 'info "Awaiting connection...\n")
        (define-values (in out) (tcp-accept listener))
        (file-stream-buffer-mode out 'line) ;; TODO: is line-buffering okay?
        (logf 'debug "connection made, waiting for name...\n")
        (define usr (user (read in) '() (empty-stock)
                          (list-ref colors (length usrs))
                          (list in out (make-semaphore 1))))
        (logf 'info "Connection established; name is '~a'\n" (user-name usr))
        (define parent (current-thread))
        (thread (thunk (run-listener parent usr out)))
        (sync (thread-receive-evt)) ;; TODO: do this better?
        (thread-receive)
        (loop (cons usr usrs))]
      [else usrs]))
  (init-state (loop '())))

;; TODO: remove this section
;; ----------------- UTILITY FUNCTIONS STOLEN FROM ENGINE-TEST -----------------
;; produce a human-readable string for the given user
(define/contract (user->string usr)
  (-> user? string?)
  (string-append
    (format "~a[~am~a~a[37m: " (integer->char #x1b) (user-color usr)
            (user-name usr) (integer->char #x1b))
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

;; ----------------------------- MAIN RUNNING CODE -----------------------------
;; initialize connections to the clients, and the game state
(set! st (init-server))

;; TODO: allow the clients to choose their initial settlements/roads
(logf 'debug "beginning initial settlement/road placement\n")
((thunk
  (define (vtx a b c d e f) (list (cons a b) (cons c d) (cons e f)))
  (define (edg a b c d) (cons (cons a b) (cons c d)))

  (define (f usr vtx) (set-board-vertex-pair! (state-board st) vtx usr 'settlement))
  (define (g usr edge) (set-board-road-owner! (state-board st) edge usr))
  (void)

  (g (first (state-users st)) (edg 0 0 0 2))

#|
  (match-define (list ron dan bob) (state-users st))

  (f ron (vtx -2 2 -1 1 -1 3))
  (g ron (edg -2 2 -1 1))
  (f ron (vtx 1 1 2 0 2 2))
  (g ron (edg 1 1 2 2))

  (f dan (vtx 0 0 0 2 1 1))
  (g dan (edg 0 0 0 2))
  (f dan (vtx -1 -3 -1 -1 0 -2))
  (g dan (edg 0 -4 0 -2))

  (f bob (vtx 1 -3 1 -1 2 -2))
  (g bob (edg 1 -1 2 -2))
  (f bob (vtx -2 -2 -2 0 -1 -1))
  (g bob (edg -1 -3 -1 -1))
|#
))

(semaphore-post mutex)

;(printf "Initial state:\n")
;(display (state->string st))

(logf 'debug "entering loop\n")
(define (loop)
  (define evt (sync (thread-receive-evt)))
  (printf "EVT:   ~a\n" (thread-receive))
  (loop))
(loop)
