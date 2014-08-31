;; client for connecting to the soa server
;; TODO: this is very rough; do it properly!
#lang racket

;; macro for writing something over TCP
(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

(printf "Which port is the server running on?\n")
(define port (read))
(void (read-line)) ;; take trailing newline
(define-values (in out) (tcp-connect "localhost" port))
(file-stream-buffer-mode out 'line)

(printf "Which name should be used?\n")
(define name (read-line))

(send name out)
(define listener-port (read in))

(define-values (nin nout) (tcp-connect "localhost" listener-port))
(file-stream-buffer-mode nout 'line)

(printf "Connection established; attempting to ping\n")

(send '(ping "asdf") nout) (flush-output nout)
(define response (read-line nin))
(when (not (equal? response "\"pong asdf\""))
  (error (format "response was ~s" response)))
(printf "Successfully established connection\n")

(define (interp x) (with-input-from-string x (thunk (read))))

(printf "entering direct user-server REPL\n")
(define (repl)
  (define evt (sync (wrap-evt (read-line-evt (current-input-port) 'any)
                              (curry cons 'user))
                    (wrap-evt (read-line-evt nin 'any) (curry cons 'server))))
  (match evt
    [(cons _ (? eof-object?)) (printf "EOF encountered. Exiting.\n")]
    [(cons 'user msg) (fprintf nout "~a\n" msg) (repl)]
    [(cons 'server msg) (display (interp msg)) (newline) (repl)]))
(repl)
