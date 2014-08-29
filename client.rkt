;; client for connecting to the soa server
#lang racket

;; TODO: this is very rough; do it properly!
(printf "Which port is the server running on?\n")
(define port (read))
(define-values (in out) (tcp-connect "localhost" port))

(printf "Which name should be used?\n")
(define name (read))

(write name out)
(define listener-port (read in))

(define-values (nin nout) (tcp-connect "localhost" listener-port))
(write "b2coutts" nout)
(define response (read nin))
(when (not (equal? response "b2coutts"))
  (error (format "response was ~s" response)))
(printf "Successfully established connection\n")
