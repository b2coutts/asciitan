#lang racket

(require
  (planet neil/charterm) racket/string racket/list racket/contract
  
  "basic.rkt" "adv.rkt" "data.rkt" "constants.rkt" "help.rkt"
)

;; list containing the labels for each of the board lines
(define bline-headers '(
  "Resources: "
  "VPs: "
))

(define (interp x) (with-input-from-string x (thunk (read))))

;; ---------------------------- INITIAL SETUP CODE ----------------------------
;; get connection/name info
(printf "Welcome to asciitan!\n")
(printf "What is the hostname or IP of the server?\n")
(define host (read-line))
(printf "Which port is the server running on?\n")
(define port (read))
(void (read-line)) ;; take trailing newline
(define-values (srv-in srv-out) (tcp-connect host port))
(file-stream-buffer-mode srv-out 'line)

;; TODO: avoid duplicate names, maybe limit name length
(printf "Which nickname do you want to use?\n")
(define name (read-line))
(printf "Connecting to server...\n")

;; establish connection to listener
(fprintf srv-out "~s\n" name)
(define listener-port (read srv-in))
(define-values (game-in game-out) (tcp-connect host listener-port))
(file-stream-buffer-mode game-out 'line)
(printf "Connection established; waiting for other users to connect.\n")

(match-define `(update all ,brd ,sstr ,init-blines)
  (interp (sync (read-line-evt game-in 'any))))

;; Setup UI
(open-charterm)
(define-values (width height) (charterm-screen-size))
(when (or (not height) (< height 24)) (set! height 24))
(when (or (not width) (< width 80)) (set! width 80))

;; ---------------------- UI MANIPULATION CODE ----------------------
;; (reversed) bottom line of user's text input
(define input '())

;; lines of console text
(define clines '())

;; lines of info below the board
(define blines (map cons bline-headers init-blines))

;; sets the current charterm style
(define/contract (charterm-style sty)
  (-> style? void?)
  (charterm-display (style->string sty)))

;; get the length of a string, taking formatting escape codes into account
(define (strlen str)
  (define (lstlen lst)
    (match lst
      ['() 0]
      [(cons #\u001B xs)
        (lstlen (rest (mydropf xs (not/c #\m))))]
      [(cons _ xs) (add1 (lstlen xs))]))
  (lstlen (string->list str)))

;; take a substring of a string, while respecting terminal escape codes
(define (substr str start end)
  (define (sublst lst start end)
    (cond
      [(and (zero? start) (zero? end)) '()]
      [(empty? lst) '()]
      [else (match (first lst)
        [#\u001B
          (match-define-values (fst (cons #\m rst))
                               (mysplitf-at lst (not/c #\m)))
          (append fst '(#\m) (sublst rst start end))]
        [ch (cond
          [(zero? start) (cons ch (sublst (rest lst) 0 (sub1 end)))]
          [else (sublst (rest lst) (sub1 start) (sub1 end))])])]))
  (list->string (sublst (string->list str) start end)))

;; get the index of the latest #\space in a string
(define (last-space str [idx (- (strlen str) 1)])
  (cond
    [(< idx 0) #f]
    [(char=? (string-ref str idx) #\space) idx]
    [else (last-space str (sub1 idx))]))

;; wrap a single string to the specified number of characters
(define (wrap-str str len)
  (define slen (strlen str))
  (cond
    [(<= slen len)
      (list (string-append str (make-string (- len slen) #\space)))]
    [else (match (last-space (substr str 0 (add1 len)))
      [#f (cons (substr str 0 len) (wrap-str (substr str len slen) len))]
      [idx (cons (substr str 0 idx)
                 (wrap-str (substr str idx slen) len))])]))

;; wraps a message to fit in the provided number of columns
(define (wrap-msg pad str cols)
  (define len (strlen pad))
  (match-define (cons x xs) (wrap-str str (- cols len)))
  (cons (string-append pad x)
        (map (curry string-append (list->string (make-list len #\space))) xs)))

;; refreshes/updates the console
(define (refresh-console! [ind 0] [line (- height 1)])
  (when (< ind (length clines))
    (define strs (wrap-msg (car (list-ref clines ind))
                           (cdr (list-ref clines ind))
                           (- width 43)))
    (when (>= (- line 1) (length strs))
      (map (lambda (i)
            (charterm-cursor 44 (- line (length strs) (- i) -1))
            (charterm-style '(37 40 #f #f))
            (charterm-clear-line-right)
            (charterm-display (list-ref strs i)))
           (range 0 (min (length strs) (- line 1))))
      (refresh-console! (add1 ind) (- line (length strs))))))

;; add a new line to the console, and refresh it; str is a format string
(define/contract (console! pad str . args)
  (->* (string? string?) #:rest (listof any/c) void?)
  (charterm-style '(37 40 #f #f))
  (set! clines (cons (cons pad (apply (curry format str) args)) clines))
  (refresh-console!)
  (cursor-input))

;; print the lines of info below the board, as long as they fit
(define/contract (print-blines [ind 0] [line 24])
  (-> void?)
  (cond
    [(and (< line height) (< ind (length blines)))
      (define strs (wrap-msg (car (list-ref blines ind))
                             (cdr (list-ref blines ind))
                             41))
      (map (lambda (i)
            (charterm-cursor 41 (+ line i))
            (charterm-style '(37 40 #f #f))
            (charterm-clear-line-left)
            (charterm-cursor 1 (+ line i))
            (safe-display 41 (list-ref strs i)))
           (range 0 (min (length strs) (- height line))))
      (print-blines (add1 ind) (+ line (length strs)))]
    [else (cursor-input)]))

;; update the board state with a new board from the server
(define/contract (update-board! lines)
  (-> (listof string?) void?)
  (charterm-style '(37 40 #f #f))
  (map (lambda (ind)
        (charterm-cursor 1 (+ ind 2))
        (charterm-display (list-ref lines ind)))
       (range 0 (length lines)))
  (cursor-input))

;; sets the text in the status bar
(define/contract (set-status str)
  (-> string? void?)
  (charterm-cursor 1 1)
  (charterm-style '(37 40 #f #f))
  (charterm-clear-line)
  (safe-display width str)
  (cursor-input))

;; draws the middle | separator
(define/contract (draw-separator)
  (-> void?)
  (charterm-style '(37 40 #f #f))
  (map (lambda (row)
        (charterm-cursor 42 row)
        (charterm-display "|"))
       (range 2 height))
  (cursor-input))

;; moves the charterm console to the proper spot in the input bar
(define/contract (cursor-input)
  (-> void?)
  (charterm-cursor (+ (length input) 3) height)
  (charterm-style '(37 44 #f #f)))

;; clears the input prompt line
(define/contract (clear-prompt)
  (-> void?)
  (charterm-cursor 1 height)
  (charterm-style '(37 44 #f #f))
  (charterm-clear-line)
  (charterm-display (format ">~a" (make-string (sub1 width) #\space)))
  (charterm-cursor 3 height))

;; display a string of text at the cursor, cutting it off at n characters
(define/contract (safe-display n str)
  (-> integer? string? void?)
  (charterm-display (substr str 0 n)))

;; places a line of text n lines below the board, if there's room
(define/contract (board-line n str)
  (-> integer? string? void?)
  (when (<= n (- height 24))
    (charterm-style '(37 40 #f #f))
    (charterm-cursor 41 (+ 23 n))
    (charterm-clear-line-left)
    (charterm-cursor 1 (+ 23 n))
    (safe-display 41 str)
    (cursor-input)))

;; reprint the entire screen
(define/contract (reprint-screen)
  (-> void?)
  (charterm-clear-screen)
  (fprintf game-out "~s\n" '(request-update))
  (draw-separator)
  (clear-prompt)
  (refresh-console!)
  (cursor-input))

;; check if the terminal has resized; if so, adjust the UI
(define/contract (handle-resize!)
  (-> void?)
  (define-values (new-width new-height) (charterm-screen-size))
  (when (or (not new-width) (< new-width 80)) (set! new-height 80))
  (when (or (not new-height) (< new-height 24)) (set! new-height 24))
  (unless (and (equal? height new-height) (equal? width new-width))
    (set! height new-height)
    (set! width new-width)
    (reprint-screen)))
    

;; ------------------------ END OF UI MANIPULATION CODE ------------------------
;; parse a line of user input
(define/contract (parse str)
  (-> string? (listof string?))
  (define lst (string->list str))
  (cond
    [(empty? lst) '()]
    [else (define-values (x xs) (mysplitf-at lst (not/c (or/c #\tab #\space))))
          (cons (list->string x)
                (parse (list->string (mydropf xs (or/c #\tab #\space)))))]))

(define ss string->symbol)

;; function for interpreting user input. If a message needs to be sent to the
;; server as a result, returns the message; otherwise, returns (void)
(define/contract (user-cmd str)
  (-> string? any/c)
  (define req (match (parse str)
    ;; prompt responses
    [`("move" ,cstr) (define cell (label->cell (ss cstr)))
      (cond [cell `(respond move-thief ,cell)]
            [else (console! "! " "invalid cell: ~a" cstr)])]
    [`("take" ,res) (cond
      [(resource? (ss res)) `(respond monopoly ,(ss res))]
      [else (console! "! " "invalid resource: ~a" res)])]
    [`("choose" ,r1 ,r2) (cond
      [(not (resource? (ss r1))) (console! "! " "invalid resource: ~a" r1)]
      [(not (resource? (ss r2))) (console! "! " "invalid resource: ~a" r2)]
      [else `(respond year-of-plenty ,(cons (ss r1) (ss r2)))])]
    [`("build" ,e1 ,e2) (match (cons (string->edge e1) (string->edge e2))
      [(cons #f _) (console! "! " "invalid edge: ~a" e1)]
      [(cons _ #f) (console! "! " "invalid edge: ~a" e2)]
      [edgs `(respond road-building ,edgs)])]
    [(cons "discard" rlist)
      (match (filter-not (compose resource? ss) rlist)
        [(cons str _) (console! "! " "invalid resource: ~a" str)]
        [_ `(respond discard-resources ,(map ss rlist))])]
    ;; TODO: validate the username
    [`("steal" ,usr) `(respond pick-target ,usr)]
    [`("place" "road" ,estr) (match (string->edge estr)
      [#f (console! "! " "invalid edge: ~a" estr)]
      [edg `(respond init-road ,edg)])]
    [`("place" "settlement" ,vstr) (match (string->vertex vstr)
      [#f (console! "! " "invalid vertex: ~a" vstr)]
      [vtx `(respond init-settlement ,vtx)])]
    [(cons "offer" (cons target ress)) (cond
      [(not (member? "for" ress))
        (console! "! " "Usage: ~a" (car (help-cmd '(offer))))]
      [else
        (match-define-values (give (cons "for" get))
                             (mysplitf-at ress (not/c "for")))
        (match (filter (not/c (compose resource? ss)) (append give get))
          [(cons str _) (console! "! " "invalid resource: ~a" str)]
          ['() `(offer ,target ,(map ss give) ,(map ss get))])])]
    ['("accept") '(respond trade accept)]
    ['("decline") '(respond trade decline)]

    [(cons "help" args) (match (help-cmd (map ss args))
      [(cons usage details)
        (console! "! " "Usage: ~a" usage)
        (console! "  " details)]
      [info (console! "! " "~a" info)])]
    [`("buy" "dev-card") `(buy dev-card)]
    [`("buy" "road" ,edg) (cond
      [(not (string->edge edg))
        (console! "! " "invalid edge: ~a" edg)]
      [else `(buy road ,(string->edge edg))])]
    [`("buy" ,item ,vtx) (cond
      [(not (building? (ss item)))
        (console! "! " "invalid usage of buy")]
      [(not (string->vertex vtx))
        (console! "! " "invalid vertex: ~a" vtx)]
      [else `(buy ,(ss item) ,(string->vertex vtx))])]
    [`("use" ,card) (match (ss card)
      [(? dev-card? dc) `(use ,dc)]
      [_ (console! "! " "not a dev card: ~a" card)])]
    [(cons "bank" (cons target lst))
      (define err (ormap (lambda (res) (cond
                          [(resource? (ss res)) #f]
                          [else (console! "! " "invalid resource: ~a" res)]))
                         (cons target lst)))
      (if err (void)
        `(bank ,(map ss lst) ,(ss target)))]
    [`("end") '(end)]
    [`("show" ,thing) (cond
      [(showable? (ss thing)) `(show ,(ss thing))]
      [else (console! "! " "invalid thing to show: ~a" thing)])]
    [(cons "say" _) `(say ,(if (< (string-length str) 4)
                               "" (substring str 4)))]
    [(cons cmd args) (cond
      [(or (command? (ss cmd)) (member? (ss cmd) client-commands))
        (console! "! " "Usage: ~a" (car (help-cmd (list (ss cmd)))))]
      [else (console! "! " "invalid command: ~a" cmd)])]
    [_ (console! "! " "badly formatted request: ~a" str)]))
  (when (not (void? req))
    (fprintf game-out "~s\n" req)))
    
(charterm-clear-screen)
(draw-separator)
(update-board! (string-split brd "\n"))
(set-status sstr)
(print-blines)
(clear-prompt)

(define (loop)
  (define evt (sync/timeout 0.5 (current-charterm) (read-line-evt game-in 'any)))
  (handle-resize!)
  (cond
    [(or (charterm? evt) (and (not evt) (charterm-byte-ready?)))
     (match (charterm-read-key)
      ['return
        (unless (empty? input)
          (user-cmd (list->string (reverse input)))
          (set! input '())
          (clear-prompt))]
      ['backspace
        (unless (empty? input)
          (charterm-cursor (+ (length input) 2) height)
          (charterm-display " ")
          (charterm-cursor (+ (length input) 2) height)
          (set! input (rest input)))]
      ['ctrl-w
        (set! input (mydropf (mydropf input (or/c #\space)) (not/c #\space)))
        (cursor-input)
        (charterm-clear-line-right)]
      [(? char? ch) (charterm-display (~a ch))
                    (set! input (cons ch input))]
      [_ (void)])]
    [evt (match (interp evt)
      [`(broadcast ,msg) (console! "* " msg)]
      [`(message ,msg) (console! "? " msg)]
      [`(prompt ,_ ,msg) (console! "> " msg)]
      [`(say ,name ,msg) (console! (format "~a: " name) msg)]
      [`(update all ,brd ,sstr ,new-blines)
        (update-board! (string-split brd "\n"))
        (set-status sstr)
        (set! blines (map cons bline-headers new-blines))
        (print-blines)]
      [`(update board ,brd) (update-board! (string-split brd "\n"))]
      [`(update status ,sstr) (set-status sstr)]
      [`(raw ,_) (error "client received raw")]
      [`(game-over) (close-charterm) (exit)])]
    [else (void)])
  (loop))
(loop)
