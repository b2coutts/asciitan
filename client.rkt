;; client for connecting to the soa server
;; TODO: this is very rough; do it properly!
#lang racket

(require racket/contract "adv.rkt" "data.rkt")

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

;; parse a line of user input
(define/contract (parse str)
  (-> string? (listof string?))
  (define lst (string->list str))
  (cond
    [(empty? lst) '()]
    [else (define-values (x xs) (splitf-at lst (not/c (or/c #\tab #\space))))
          (cons (list->string x)
                (parse (list->string (dropf xs (or/c #\tab #\space)))))]))

(printf "Connection established; entering direct user-server REPL\n")
(define (repl)
  (define evt (sync (wrap-evt (read-line-evt (current-input-port) 'any)
                              (curry cons 'user))
                    (wrap-evt (read-line-evt listen-in 'any) (curry cons 'server))))
  (match evt
    [(cons _ (? eof-object?)) (printf "EOF encountered. Exiting.\n")]
    [(cons 'user msg)
      (define req (match (parse msg)
        [`("buy" "dev-card") `(buy dev-card)]
        [`("buy" "road" ,edg) (cond
          [(not (string->edge edg))
            (printf "! invalid edge: ~a\n" edg)]
          [else `(buy road ,(string->edge edg))])]
        [`("buy" ,item ,vtx) (cond
          [(not (building? (string->symbol item)))
            (printf "! invalid usage of buy\n")]
          [(not (string->vertex vtx))
            (printf "! invalid vertex: ~a\n" vtx)]
          [else `(buy ,(string->symbol item) ,(string->vertex vtx))])]
        [`("use" ,numstr)
          (define num (string->number numstr))
          (cond
            [(and num (integer? num) (>= num 0)) `(use ,num)]
            [else (printf "! invalid card number: ~a\n" numstr)])]
        ;; TODO: bank
        [`("end") '(end)]
        [`("show" ,thing) (cond
          [(showable? (string->symbol thing)) `(show ,(string->symbol thing))]
          [else (printf "! invalid thing to show: ~a\n" thing)])]
        ;; TODO: substring errors on strings smaller than 4
        [(cons "say" _) `(say ,(substring msg 4))]
        [(cons cmd args) (cond
          [(command? (string->symbol cmd))
            ;; TODO: show usage
            (printf "! invalid usage of ~a\n" cmd)]
          [else (printf "! invalid command: ~a\n" cmd)])]
        [_ (printf "! badly formatted request: ~a\n" msg)]))
      (when (not (void? req))
        (fprintf listen-out "~s\n" req))
      (repl)]
    [(cons 'server msg)
      (match (interp msg)
        [`(broadcast ,msg) (printf "* ~a\n" msg)]
        [`(message ,msg) (printf "? ~a\n" msg)]
        [`(raw ,msg) (display msg)]
        [`(say ,name ,msg) (printf "~a: ~a\n" name msg)]
        [x (printf "ERROR: unknown command from server: ~a\n" x)])
      (repl)]))
(repl)
