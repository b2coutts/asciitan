;; TODO deal with *really* long commands (for some reason)
#lang racket

(require (planet neil/charterm) racket/string racket/list)

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

;; wait for initial state data
(match-define `(raw ,init-board)
  (interp (sync (read-line-evt game-in 'any))))

;; Setup UI
(open-charterm)
(define-values (width height) (charterm-screen-size))

;; ---------------------- UI MANIPULATION CODE ----------------------
;; (reversed) bottom line of user's text input
(define input '())

;; lines of console text
(define clines '())

;; get the index of the latest #\space in a string
(define (last-space str [idx (- (string-length str) 1)])
  (cond
    [(< idx 0) #f]
    [(char=? (string-ref str idx) #\space) idx]
    [else (last-space str (sub1 idx))]))

;; wrap a single string to the specified number of characters
(define (wrap-str str len)
  (cond
    [(<= (string-length str) len)
      (list (string-append str (make-string (- len (string-length str))
                                            #\space)))]
    [else (match (last-space (substring str 0 len))
      [#f (cons (substring str 0 len) (wrap-str (substring str len) len))]
      [idx (cons (substring str 0 idx)
                 (wrap-str (substring str (add1 idx)) len))])]))

;; wraps a console message to fit in the console
(define (wrap-msg pad str)
  (define len (string-length pad))
  (match-define (cons x xs) (wrap-str str (- width len 43)))
  (cons (string-append pad x)
        (map (curry string-append (list->string (make-list len #\space))) xs)))

;; refreshes/updates the console
(define (refresh-console! [ind 0] [line (- height 1)])
  (when (and (> line 1) (< ind (length clines)))
    (define strs (wrap-msg (car (list-ref clines ind))
                           (cdr (list-ref clines ind))))
    (map (lambda (i)
          (charterm-cursor 44 (- line i))
          (charterm-display (list-ref strs (- (length strs) i 1))))
         (range 0 (min (length strs) (- line 1))))
    (refresh-console! (add1 ind) (- line (length strs)))))

;; add a new line to the console, and refresh it
;; TODO: deal with wrapping properly
(define/contract (console! pad str)
  (-> string? string? void?)
  (reset-colors)
  (set! clines (cons (cons pad str) clines))
  (refresh-console!)
  (cursor-input))

;; update the board state with a new board from the server
;; TODO: handle other state updates from server
(define/contract (update-board! lines)
  (-> (listof string?) void?)
  (reset-colors)
  (map (lambda (ind)
        (charterm-cursor 1 (+ ind 2))
        (charterm-display (list-ref lines ind)))
       (range 0 (length lines)))
  (cursor-input))

;; sets the text in the status bar
;; TODO: deal with really long statuses?
(define/contract (set-status str)
  (-> string? void?)
  (charterm-cursor 1 1)
  (charterm-display (format "~a[30;47m~a~a" (integer->char #x1b) str
                      (make-string (- width (string-length str)) #\space)))
  (cursor-input))

;; draws the middle | separator
(define/contract (draw-separator)
  (-> void?)
  (reset-colors)
  (map (lambda (row)
        (charterm-cursor 42 row)
        (charterm-display "|"))
       (range 2 height))
  (cursor-input))

;; moves the charterm console to the proper spot in the input bar
(define/contract (cursor-input)
  (-> void?)
  (charterm-cursor (+ (length input) 3) height)
  (charterm-display (format "~a[37;44m" (integer->char #x1b))))

;; clears the input prompt line
(define/contract (clear-prompt)
  (-> void?)
  (charterm-cursor 1 height)
  (charterm-display (format "~a[37;44m>~a" (integer->char #x1b)
                      (make-string (- width 1) #\space)))
  (charterm-cursor 3 height))

;; sets the terminal colours back to default
(define/contract (reset-colors)
  (-> void?)
  (charterm-display (format "~a[37;40m" (integer->char #x1b))))

;; ------------------------ END OF UI MANIPULATION CODE ------------------------
;; TODO: implement this properly
(define (user-cmd str)
  (when (string=? str "really-quit") (exit))
  (console! "$ " str))

(charterm-clear-screen)
(clear-prompt)
(draw-separator)
(set-status "foo")
(update-board! (string-split init-board "\n"))

(define (loop)
  (match (sync (wrap-evt (current-charterm) (curry cons 'user))
               (wrap-evt (read-line-evt game-in 'any) (curry cons 'server)))
    [(cons 'user _) (match (charterm-read-key)
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
      [ch (charterm-display (~a ch))
          (set! input (cons ch input))])]
    [(cons 'server msg) (match (interp msg)
      [`(broadcast ,msg) (console! "* " msg)]
      [`(message ,msg) (console! "? " msg)]
      [`(prompt ,_ ,msg) (console! "> " msg)]
      [`(say ,name ,msg) (console! (format "~a: " name) msg)]
      [`(raw ,brd) (update-board! (string-split brd "\n"))] ;; TODO: raw => update
      [`(raw ,_) (error "client received raw")]
      ;; TODO: return terminal to normal?
      [`(game-over) (exit)])])
  (loop))
(loop)
