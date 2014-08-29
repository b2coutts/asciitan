#lang racket

(require "data.rkt" "engine.rkt" "util.rkt")

;; TODO: unhardcode this maybe or something
(define MAX-USERS 4)

(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

;; begins interacting with a given user on the given input/output ports
;; TODO: code for ending
;; TODO: prompting
(define/contract (run-listener parent usr init-port)
  (-> thread? user? output-port? void?)
  (define tcpl (tcp-listen 0))
  (match-define-values (_ port _ _) (tcp-addresses tcpl #t))
  (send port init-port)
  (printf "Awaiting listener connection from client ~a on port ~a...\n"
    (user-name usr) port)
  (define-values (in out) (tcp-accept tcpl))
  (set-user-io! usr (list in out (third (user-io usr))))
  (printf "Client ~a listener connection established.\n" (user-name usr))
  (thread-send parent 'done)
  (let loop []
    (define req (sync (read-line-evt in 'any)))
    (define response
      (call-with-semaphore mutex (thunk (handle-action! st usr req))))
    (unless (void? response)
      (call-with-semaphore (third (user-io usr))
        (send response out))
    (loop))))

;; dispatch listeners, generate the initial state
;; TODO: close initial tcp connection after establishing listener?
(define/contract (init-server [init-port 0])
  (->* () (integer?) state?)
  (define colors '(93 94 95 96))
  (define listener (tcp-listen init-port))
  (match-define-values (_ port _ _) (tcp-addresses listener #t))
  (printf "Listening for connections on port ~a.\n" port)
  (define (loop usrs)
    (define continue (cond
      [(empty? usrs) #t]
      [(= (length usrs) MAX-USERS) #f]
      [else (equal? (prompt "[c]ontinue awaiting users, or [s]tart the game?"
                            (notin '(c s))) 'c)]))
    (cond
      [continue
        (printf "Awaiting connection...\n")
        (define-values (in out) (tcp-accept listener))
        (file-stream-buffer-mode out 'line) ;; TODO: is line-buffering okay?
        (printf "TODO: connection made, waiting for name...\n")
        (define usr (user (read in) '() '#hash() (list-ref colors (length usrs))
                          (list in out (make-semaphore 1))))
        (printf "Connection established; name is '~a'\n" (user-name usr))
        (define parent (current-thread))
        (thread (thunk (run-listener parent usr out)))
        (sync (thread-receive-evt)) ;; TODO: do this better
        (loop (cons usr usrs))]
      [else usrs]))
  (init-state (loop '())))

;; ----------------------------- MAIN RUNNING CODE -----------------------------
(define mutex (make-semaphore 0)) ;; mutex for game state

;; initialize connections to the clients, and the game state
(define st (init-server))

;; TODO: allow the clients to choose their initial settlements/roads

(semaphore-post mutex) ;; TODO: don't just block before state is ready
