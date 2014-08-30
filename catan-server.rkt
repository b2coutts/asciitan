#lang racket

(require "board.rkt" "data.rkt" "engine.rkt" "util.rkt")

;; TODO: unhardcode this maybe or something
(define MAX-USERS 4)

(define st #f) ;; global state variable (initial value is a place holder)
(define mutex (make-semaphore 1)) ;; mutex for st

(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

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
        (thunk (send response out)))
    (loop))))

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
      [#t #f] ;; TODO: remove this line
      [(= (length usrs) MAX-USERS) #f]
      [else (equal? (prompt "[c]ontinue awaiting users, or [s]tart the game?"
                            (notin '(c s))) 'c)]))
    (cond
      [continue
        (logf 'info "Awaiting connection...\n")
        (define-values (in out) (tcp-accept listener))
        (file-stream-buffer-mode out 'line) ;; TODO: is line-buffering okay?
        (logf 'debug "connection made, waiting for name...\n")
        (define usr (user (read in) '() '#hash() (list-ref colors (length usrs))
                          (list in out (make-semaphore 1))))
        (logf 'info "Connection established; name is '~a'\n" (user-name usr))
        (define parent (current-thread))
        (thread (thunk (run-listener parent usr out)))
        (sync (thread-receive-evt)) ;; TODO: do this better
        (loop (cons usr usrs))]
      [else usrs]))
  (init-state (loop '())))

;; ----------------------------- MAIN RUNNING CODE -----------------------------
;; initialize connections to the clients, and the game state
(set! st (init-server 38215)) ;; TODO: un-hardcode port

;; TODO: allow the clients to choose their initial settlements/roads
(logf 'debug "beginning initial settlement/road placement\n")
(call-with-semaphore mutex (thunk
  (define (vtx a b c d e f) (list (cons a b) (cons c d) (cons e f)))
  (define (edg a b c d) (cons (cons a b) (cons c d)))

  (define usr (first (state-users st)))
  (define (f vtx) (set-board-vertex-pair! (state-board st) vtx usr 'settlement))
  (define (g edge) (set-board-road-owner! (state-board st) edge usr))

  (f (vtx -1 -1 0 -2 0 0))
  (g (edg 0 -2 0 0))
  (f (vtx 1 -1 1 1 2 0))
  (g (edg 2 -2 2 0))))

(logf 'debug "entering loop\n")
(define (loop)
  (define evt (sync (thread-receive-evt)))
  (printf "EVT:   ~a\n" (thread-receive))
  (loop))
(loop)
