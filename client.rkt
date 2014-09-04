;; client for connecting to the soa server
#lang racket

(require racket/contract "basic.rkt" "adv.rkt" "data.rkt" "constants.rkt"
         "help.rkt")

;; macro for writing something over TCP
(define-syntax-rule (send msg out) (fprintf out "~s\n" msg))

(display (style->string '(40 37 #f #f)))

(printf "Welcome to asciitan!\n")
(printf "Which port is the server running on?\n")
(define port (read))
(void (read-line)) ;; take trailing newline
(define-values (srv-in srv-out) (tcp-connect "localhost" port))
(file-stream-buffer-mode srv-out 'line)

;; TODO: avoid duplicate names, maybe limit name length
(printf "Which nickname do you want to use?\n")
(define name (read-line))
(printf "Connecting to server...\n")

(send name srv-out)
(define listener-port (read srv-in))

(define-values (listen-in listen-out) (tcp-connect "localhost" listener-port))
(file-stream-buffer-mode listen-out 'line)

(printf "Connection established; waiting for other users to connect.\n")

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

(define ss string->symbol) ;; to make my life easier

(define (repl)
  (define evt (sync (wrap-evt (read-line-evt (current-input-port) 'any)
                              (curry cons 'user))
                    (wrap-evt (read-line-evt listen-in 'any)
                              (curry cons 'server))))
  (match evt
    [(cons _ (? eof-object?)) (printf "EOF encountered. Exiting.\n")]
    [(cons 'user msg)
      (define req (match (parse msg)
        ;; prompt responses
        [`("move" ,cstr) (define cell (label->cell (ss cstr)))
          (cond [cell `(respond move-thief ,cell)]
                [else (printf "! invalid cell: ~a\n" cstr)])]
        [`("take" ,res) (cond
          [(resource? (ss res)) `(respond monopoly ,(ss res))]
          [else (printf "! invalid resource: ~a\n" res)])]
        [`("choose" ,res1 ,res2) (cond
          [(not (resource? (ss res1))) (printf "! invalid resource: ~a\n" res1)]
          [(not (resource? (ss res2))) (printf "! invalid resource: ~a\n" res2)]
          [else `(respond year-of-plenty ,(cons (ss res1) (ss res2)))])]
        [`("build" ,e1 ,e2) (match (cons (string->edge e1) (string->edge e2))
          [(cons #f _) (printf "! invalid edge: ~a\n" e1)]
          [(cons _ #f) (printf "! invalid edge: ~a\n" e2)]
          [edgs `(respond road-building ,edgs)])]
        [(cons "discard" rlist)
          (match (filter-not (compose resource? ss) rlist)
            [(cons str _) (printf "! invalid resource: ~a\n" str)]
            [_ `(respond discard-resources ,(map ss rlist))])]
        ;; TODO: validate the username
        [`("steal" ,usr) `(respond pick-target ,usr)]
        [`("place" "road" ,estr) (match (string->edge estr)
          [#f (printf "! invalid edge: ~a\n" estr)]
          [edg `(respond init-road ,edg)])]
        [`("place" "settlement" ,vstr) (match (string->vertex vstr)
          [#f (printf "! invalid vertex: ~a\n" vstr)]
          [vtx `(respond init-settlement ,vtx)])]
        [(cons "offer" (cons target ress)) (cond
          [(not (member? "for" ress)) (printf "Usage: ~a\n"
                                              (car (help-cmd '(offer))))]
          [else
            (match-define-values (give (cons "for" get))
                                 (splitf-at ress (not/c "for")))
            (match (filter (not/c (compose resource? ss)) (append give get))
              [(cons str _) (printf "! invalid resource: ~a\n" str)]
              ['() `(offer ,target ,(map ss give) ,(map ss get))])])]
        ['("accept") '(respond trade accept)]
        ['("decline") '(respond trade decline)]

        [(cons "help" args) (match (help-cmd (map ss args))
          [(cons usage details) (printf "Usage: ~a\n~a\n" usage details)]
          [info (printf "~a\n" info)])]
        [`("buy" "dev-card") `(buy dev-card)]
        [`("buy" "road" ,edg) (cond
          [(not (string->edge edg))
            (printf "! invalid edge: ~a\n" edg)]
          [else `(buy road ,(string->edge edg))])]
        [`("buy" ,item ,vtx) (cond
          [(not (building? (ss item)))
            (printf "! invalid usage of buy\n")]
          [(not (string->vertex vtx))
            (printf "! invalid vertex: ~a\n" vtx)]
          [else `(buy ,(ss item) ,(string->vertex vtx))])]
        [`("use" ,card) (match (ss card)
          [(? dev-card? dc) `(use ,dc)]
          [_ (printf "! not a dev card: ~a\n" card)])]
        [(cons "bank" (cons target lst))
          (define err (ormap (lambda (res) (cond
                              [(resource? (ss res)) #f]
                              [else (printf "! invalid resource: ~a\n" res)]))
                             (cons target lst)))
          (if err (void)
            `(bank ,(map ss lst) ,(ss target)))]
        [`("end") '(end)]
        [`("show" ,thing) (cond
          [(showable? (ss thing)) `(show ,(ss thing))]
          [else (printf "! invalid thing to show: ~a\n" thing)])]
        [(cons "say" _) `(say ,(if (< (string-length msg) 4)
                                   "" (substring msg 4)))]
        [(cons cmd args) (cond
          [(or (command? (ss cmd)) (member? (ss cmd) client-commands))
            (printf "Usage: ~a\n" (car (help-cmd (list (ss cmd)))))]
          [else (printf "! invalid command: ~a\n" cmd)])]
        [_ (printf "! badly formatted request: ~a\n" msg)]))
      (when (not (void? req))
        (fprintf listen-out "~s\n" req))
      (when (not (equal? req 'game-over))
        (repl))]
    [(cons 'server msg)
      (define continue
        (match (interp msg)
          [`(broadcast ,msg) (printf "* ~a\n" msg)]
          [`(message ,msg) (printf "? ~a\n" msg)]
          [`(raw ,msg) (display msg)]
          [`(say ,name ,msg) (printf "~a: ~a\n" name msg)]
          [`(prompt ,type ,msg) (printf "> ~a\n" msg)]
          [`(game-over) #f]
          [x (printf "ERROR: unknown command from server: ~a\n" x)]))
      (when continue (repl))]))
(repl)
