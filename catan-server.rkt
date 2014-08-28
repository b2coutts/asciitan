#lang racket

(require "engine.rkt")

;; TODO: unhardcode this maybe or something
(define MAX-USERS 4)

;; begins interacting with a given user on the given input/output ports
(define/contract (run-listener usr init-port)
  (-> output-port? void?)
  (define tcpl (tcp-listen 0))
  (match-define (values _ port _ _) (tcp-addresses tcpl #t))
  (write port init-port)
  (printf "Awaiting listener connection from client ~a on port ~a...\n"
    (user-name usr) port)
  (define-values (in out) (tcp-accept tcpl))
  (set-user-io! usr (list in out (third (user-io usr))))
  (printf "Client ~a listener connection established.\n" (user-name usr))
  (let loop []
    (sync (read-line-evt in 'any))
    (define response
      (call-with-semaphore mutex (thunk (handle-action! usr evt))))
    (unless (void? response)
      (call-with-semaphore (third (user-io usr))
        (write response out))
    (loop)))

;; dispatch listeners, generate the initial state
;; TODO: close initial tcp connection after establishing listener?
(define/contract (init-server [init-port 0])
  (->* () (integer-in 0 65535) state?)
  (define colors '(magenta blue yellow cyan))
  (define listener (tcp-listen port))
  (match-define (values _ port _ _) (tcp-addresses listener #t))
  (printf "Listening for connections on port ~a...\n" port)
  (define (loop usrs)
    (define continue (cond
      [(empty? usrs) #t]
      [(= (length usrs) MAX-USERS) #f]
      [else (equal? (prompt "[c]ontinue awaiting users, or [s]tart the game?\n"
                            (or/c 'c 's)) 'c)]))
    (cond
      [continue
        (printf "Awaiting connection...\n")
        (define-values in out (tcp-accept listener))
        (define usr (user (read in) '() '() (list-ref colors (length usrs))
                          (list in out (make-semaphore 1))))
        (printf "Connection established; name is '~a'\n" (user-name usr))
        (thread (thunk (run-listener usr out)))
        ;; TODO: block until listener connection is established
        ]
      [else usrs]))
  (init-state (loop '())))

;; ----------------------------- MAIN RUNNING CODE -----------------------------
(define mutex (make-semaphore 0)) ;; mutex for game state

;; initialize connections to the clients, and the game state
(define st (init-state))

;; TODO: allow the clients to choose their initial settlements/roads

(semaphore-post mutex) ;; TODO: don't just block before state is ready
