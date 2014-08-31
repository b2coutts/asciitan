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

(define-values (listen-in listen-out) (tcp-connect "localhost" listener-port))
(file-stream-buffer-mode listen-out 'line)

(define (interp x) (with-input-from-string x (thunk (read))))

(printf "Connection established; entering direct user-server REPL\n")
(define (repl)
  (define evt (sync (wrap-evt (read-line-evt (current-input-port) 'any)
                              (curry cons 'user))
                    (wrap-evt (read-line-evt listen-in 'any) (curry cons 'server))))
  (match evt
    [(cons _ (? eof-object?)) (printf "EOF encountered. Exiting.\n")]
    [(cons 'user msg) (fprintf listen-out "~a\n" msg) (repl)]
    [(cons 'server msg)
      (match (interp msg)
        [`(broadcast ,msg) (printf "* ~a\n" msg)]
        [`(message ,msg) (printf "? ~a\n" msg)]
        [`(raw ,msg) (display msg)]
        [`(say ,name ,msg) (printf "~a: ~a\n" name msg)]
        [x (printf "ERROR: unknown command from server: ~a\n" x)])
      (repl)]))
(repl)
