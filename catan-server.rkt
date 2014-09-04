#lang racket

(require "board.rkt" "data.rkt" "engine.rkt" "basic.rkt" "adv.rkt")

(define MAX-USERS 4)
(define TESTING #f) ;; flag used to get into a game more quickly

(define st #f) ;; global state variable (initial value is a place holder)
(define mutex (make-semaphore 0)) ;; mutex for st
(define main-thread (current-thread))

(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

;; produce a stock of empty resources for a user
(define/contract (empty-stock)
  (-> stock?)
  (make-hash (map (lambda (res) (cons res (if TESTING 5 0)))
                  '(wood grain sheep ore clay))))

;; begins interacting with a given user on the given input/output ports
(define/contract (run-listener parent usr init-port)
  (-> thread? user? output-port? void?)
  (define tcpl (tcp-listen 0))
  (match-define-values (_ port _ _) (tcp-addresses tcpl #t))
  (send port init-port)
  (logf 'info "Awaiting listener connection from client ~a on port ~a...\n"
    (user-name usr) port)
  (define-values (in out) (tcp-accept tcpl))
  (file-stream-buffer-mode out 'line)
  (set-user-io! usr (list in out (third (user-io usr))))
  (logf 'info "Client ~a listener connection established.\n" (user-name usr))
  (thread-send parent 'done)
  (let loop []
    (logf 'debug "listener for ~a waiting for request...\n" (user-name usr))
    (match (sync (read-line-evt in 'any))
      [(? eof-object?) (printf "Client ~a sent EOF.\n" (user-name usr))]
      [line
        (define req (with-input-from-string line (thunk (read))))
        (logf 'debug "listener ~a received request ~s\n" (user-name usr) req)
        (define response
          (call-with-semaphore mutex (thunk (handle-action! st usr req))))
        (logf 'debug "listener responding with ~s\n" response)
        (cond
          [(equal? response '(game-over)) (thread-send main-thread 'game-over)]
          [else
            (unless (void? response)
              (call-with-semaphore (third (user-io usr))
                (thunk (send response out))))
            (loop)])])))

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
        (file-stream-buffer-mode out 'line)
        (logf 'debug "connection made, waiting for name...\n")
        (define usr (user (read in) 2 '() (empty-stock)
                          (list-ref colors (length usrs))
                          (list in out (make-semaphore 1))))
        (logf 'info "Connection established; name is '~a'\n" (user-name usr))
        (define parent (current-thread))
        (thread (thunk (run-listener parent usr out)))
        (sync (thread-receive-evt))
        (thread-receive)
        (loop (cons usr usrs))]
      [else usrs]))
  (init-state (shuffle (loop '()))))

;; ----------------------------- MAIN RUNNING CODE -----------------------------
;; initialize connections to the clients, and the game state
(set! st (init-server))

;; tell all users that initial settlement/road placement is starting
(logf 'info "beginning initial settlement/road placement\n")

;; TODO: remove the TESTING block of code
(cond
  [TESTING 
    (define (setl usr vtx)
      (set-board-vertex-pair! (state-board st) (string->vertex vtx) usr
                              'settlement))
    (define (road usr edge)
      (set-board-road-owner! (state-board st) (string->edge edge) usr))

    (define usrs (state-users st))
    (setl (first usrs) "C.4")
    (road (first usrs) "C-3")
    (setl (first usrs) "Q.1")
    (road (first usrs) "Q-6")

    (when (>= (length usrs) 2)
      (setl (second usrs) "F.3")
      (road (second usrs) "F-2")
      (setl (second usrs) "L.4")
      (road (second usrs) "L-3"))
    (when (>= (length usrs) 3)
      (setl (third usrs) "A.2")
      (road (third usrs) "A-1")
      (setl (third usrs) "J.3")
      (road (third usrs) "J-3"))
    (when (>= (length usrs) 4)
      (setl (fourth usrs) "I.3")
      (road (fourth usrs) "N-4")
      (setl (fourth usrs) "O.2")
      (road (fourth usrs) "O-2"))
    (semaphore-post mutex)
    (set-state-lock! st #f)]
  [else
    (void (map (lambda (usr)
      (match-define (list _ out mutex) (user-io usr))
      (call-with-semaphore mutex (thunk
        (send (handle-action! st usr '(show board)) out)
        (send '(broadcast "Starting initial settlement/road placement.") out)
        (send (list 'broadcast (format "Order is: ~a."
          (apply string-append (add-between (map uname (state-users st)) ", "))))
          out)
        (send '(broadcast "Type `help` for a list of commands.") out))))
     (state-users st)))

    (call-with-semaphore (third (user-io (first (state-users st)))) (thunk
      (send '(prompt init-settlement "Where will you place your 1st settlement?")
            (second (user-io (first (state-users st)))))))

    (semaphore-post mutex)])


(logf 'info "waiting for game over\n")
(void (sync (thread-receive-evt)))
(match (thread-receive)
  ['game-over (logf 'info "Game is over; exiting.\n")]
  [msg (printf "ERROR: main server thread received message: ~s\n" msg)])
