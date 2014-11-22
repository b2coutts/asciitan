;; contains code for running the game
#lang racket

(require "board.rkt" "cell.rkt" "basic.rkt" "data.rkt" "constants.rkt" "adv.rkt")

(provide init-state handle-action! send-updates)

;; -------------------------- SMALL HELPER FUNCTIONS --------------------------
;; give a user an (possibly negative) amount of victory points
(define/contract (give-veeps! usr amt)
  (-> user? integer? void?)
  (set-user-veeps! usr (+ (user-veeps usr) amt)))

;; give a specified (possibly negative) amount of a resource to a user
(define/contract (give-res! usr res [amt 1])
  (->* (user? resource?) (integer?) void?)
  (hash-set! (user-res usr) res (+ (hash-ref (user-res usr) res) amt))
  (void))

;; give a stock of resources to a user
(define/contract (give-stock! usr stock)
  (-> user? stock? void?)
  (hash-map stock (lambda (res amt) (give-res! usr res amt)))
  (void))

;; generates a properly-weighted random roll (from 2-12)
(define/contract (random-roll)
  (-> roll-num?)
  (+ (random 6) (random 6) 2))

;; determines whether or not the user can afford a given price (hash)
(define/contract (can-afford? usr price)
  (-> user? stock? boolean?)
  (foldr (lambda (x y) (and x y)) #t
    (hash-map (user-res usr) (lambda (res amt)
                              (>= amt (hash-ref price res 0))))))

;; removes a dev card from the top of the stack
(define/contract (pop-dev-card! st)
  (-> state? dev-card?)
  (match-define (cons x xs) (state-cards st))
  (set-state-cards! st xs)
  x)

;; if the game is over, returns the winner; otherwise, returns #f
(define/contract (game-over? st)
  (-> state? (or/c user? #f))
  (define leader (first (sort (state-users st) > #:key user-veeps)))
  (if (>= (user-veeps leader) 10) leader #f))

;; send initial info to players (after initial settlement/road placement)
(define/contract (send-start-message st)
  (-> state? void?)
  (map (lambda (usr)
        (send-message usr (list 'broadcast "The game is starting.")))
        ;; TODO: send state updates here
        ;; (send-message usr (handle-action! st usr '(show all))))
       (state-users st))
  (void))

;; send updated state to a list of users (all users if no list is given)
;; TODO: maybe only call this function when it needs to be called
(define/contract (send-updates st [usrs (state-users st)])
  (->* (state?) ((listof user?)) void?)
  (define sstr (status-message st))
  (define brd (board->string (state-board st)))
  (define veeps
    (apply string-append
      (add-between
        (map (lambda (usr) (format "~a (~a)" (uname usr) (user-veeps usr)))
             (sort (state-users st) > #:key user-veeps))
        ", ")))
  (map (lambda (usr)
        (define res (stock->string (user-res usr)))
        (send-message usr `(update all ,brd ,sstr (,res ,veeps))))
       usrs)
  (void))

;; gets a status message to be sent to the clients
(define/contract (status-message st)
  (-> state? string?)
  (define turnstr (format "It's ~a's turn. " (uname (state-turnu st))))
  (match (state-lock st)
    [(rlock usr act (or 'init-settlement 'init-road) _ _)
      (format "Waiting for ~a to ~a." (uname usr) act)]
    [(rlock usr act _ _ _)
      (format "~aWaiting for ~a to ~a." turnstr (uname usr) act)]
    [#f turnstr]))


;; --------------------------- BIG HELPER FUNCTIONS ---------------------------
;; given a roll number, give players their earned resources
(define/contract (apply-roll! st roll)
  (-> state? roll-num? void?)
  (define b (state-board st))
  (map (lambda (usr)
    (define stock-gain (list->stock
      (apply append
        (hash-map (board-cells b) (lambda (cell num-res)
          (define res (cdr num-res))
          (if (or (equal? res 'desert)
                  (not (= (car num-res) roll))
                  (equal? cell (board-thief b)))
              '()
              (apply append
                (filter-map (lambda (vtx) (match (board-vertex-pair b vtx)
                              [(cons (== usr) 'city) (list res res)]
                              [(cons (== usr) 'settlement) (list res)]
                              [_ #f]))
                        (adj-vertices cell)))))))))
    (broadcast st "~a gets ~a." (uname usr) (stock->string stock-gain))
    (give-stock! usr stock-gain))
   (state-users st))
  (void))

;; given a user and a vertex, #t iff the user is allowed to build a settlement
;; at the vertex
(define/contract (can-settle? b usr vtx)
  (-> board? user? vertex? boolean?)
  ;; list of all neighbours of vtx, and vtx itself
  (define nbrs
    (filter (lambda (v) (> (length (filter (curryr member? vtx) v)) 1))
            board-vertex-list))

  ;; list of all edges adjacent to vtx
  (define edgs (filter (lambda (e) (match-define (cons a b) e)
                        (= (length (filter (curryr member? vtx) (list a b))) 2))
                       board-edge-list))

  (and (andmap (lambda (v) (not (board-vertex-pair b v))) nbrs)
       (ormap (lambda (e) (equal? (board-road-owner b e) usr)) edgs)))

;; #t iff the user can bulid a city at the given vertex
(define/contract (can-city? b usr vtx)
  (-> board? user? vertex? boolean?)
  (match (board-vertex-pair b vtx)
    [(cons (app (curry user=? usr) #t) 'settlement) #t]
    [_ #f]))

;; given a user and an edge, #t iff the user is allowed to build a road there
(define/contract (can-road? b usr edg)
  (-> board? user? edge? boolean?)
  (match-define (cons x y) edg)

  ;; list of the 4 surrounding cells
  (define surr (append (list x y)
    (filter (lambda (cell) (or (member? (list cell x y) board-vertex-list)
                               (member? (list x cell y) board-vertex-list)
                               (member? (list x y cell) board-vertex-list)))
            (append (adj-cells x) (adj-cells y)))))

  ;; list of the 4 adjacent edges to edg
  (define edgs
    (filter (lambda (e) (= 2 (length (filter (curryr member? surr)
                                             (list (car e) (cdr e))))))
            board-edge-list))

  (and (not (board-road-owner b edg))
       (ormap (lambda (e) (equal? (board-road-owner b e) usr)) edgs)))

;; third argument is the index of a user (in state-users). attempts to remove
;; resources from this user (if they have >7); failing that, moves on to the
;; next user until the last user has been reached. Finally, prompts the first
;; user to move the thief.
(define/contract (thief-cut! st usri)
  (-> state? integer? void?)
  (cond
    ;; allow the user to place the thief
    [(>= usri (length (state-users st)))
      (set-state-lock! st (rlock (state-turnu st) "move the thief" 'move-thief
                                 #f prompt-move-thief!))
      (send-message (state-turnu st)
        `(prompt move-thief
          "Where will you move the thief? Use the `move` command"))]
    [else
      (define usr (list-ref (state-users st) usri))
      (define numres (length (stock->list (user-res usr))))
      (cond
        [(< numres 8) (thief-cut! st (add1 usri))]
        [else (set-state-lock! st (rlock usr "discard resources"
                                    'discard-resources usri prompt-discard!))
              (broadcast st "~a has ~a resources!" (uname usr) numres)
              (send-message usr `(prompt discard-resources ,(format
                "Select ~a resources to discard. Use the `discard` command."
                (quotient numres 2))))])]))

(define/contract (change-turn! st)
  (-> state? (or/c response? void?))
  (define winner (game-over? st))
  (cond
    [winner (broadcast st "~a wins the game!" (uname winner))
            (map (lambda (usr)
                  (send-message usr (list 'message (show st usr 'veeps))))
                 (state-users st))
            (map (curryr send-message '(game-over)) (state-users st))
            '(game-over)]
    [else
      (define usrs (state-users st))
      (define oldind (- (length usrs) (length (member (state-turnu st) usrs))))
      (define newind (modulo (add1 oldind) (length usrs)))
      (set-state-turnu! st (list-ref usrs newind))
      (broadcast st "It's ~a's turn." (uname (state-turnu st)))
      (define roll (random-roll))
      (broadcast st "~a rolls a ~a." (uname (state-turnu st)) roll)
      (cond
        [(= roll 7) (thief-cut! st 0)]
        [else (apply-roll! st roll)])]))

;; spends a given stock of the user's resources
(define/contract (spend-stock! usr stock)
  (-> user? stock? void?)
  (hash-map stock (lambda (res amt)
    (hash-set! (user-res usr) res (- (hash-ref (user-res usr) res) amt))))
  (void))

;; have usr1 steal a random resource from usr2
(define/contract (steal-resource! st usr1 usr2)
  (-> state? user? user? void?)
  (define reslist (shuffle (stock->list (user-res usr2))))
  (define res (match reslist
    ['() 'nothing]
    [(cons x _) x]))
  (unless (equal? res 'nothing)
    (give-res! usr2 res -1)
    (give-res! usr1 res))
  (broadcast st "~a stole ~a from ~a." (uname usr1)
    (if (equal? res 'nothing) "nothing"
        (format "~a1 ~a~a" (style->string `(,(resource->color res) 40 #f #f))
                           res (style->string '(37 40 #f #f))))
    (uname usr2)))

;; ------------------------- PROMPT HANDLING FUNCTIONS -------------------------
;; discard resources due to thief, move on to next user
(define/contract (prompt-discard! st rlist)
  (-> state? (listof resource?) (or/c response? void?))
  (define stock (list->stock rlist))
  (match-define (rlock usr _ _ usri _) (state-lock st))
  (define needed (quotient (length (stock->list (user-res usr))) 2))
  (cond
    [(not (= (length rlist) needed))
      (list 'message (format "You must discard ~a resources!" needed))]
    [(can-afford? usr stock)
      (spend-stock! usr stock)
      (set-state-lock! st #f)
      (broadcast st "~a discarded ~a." (uname usr) (stock->string stock))
      (thief-cut! st (add1 usri))]
    [else (list 'message (format "You don't have ~a to discard!"
                                 (stock->string stock)))]))

;; move the thief
(define/contract (prompt-move-thief! st cell)
  (-> state? cell-valid? (or/c response? void?))
  (define usr (rlock-holder (state-lock st)))
  (cond
    [(equal? cell (board-thief (state-board st)))
      (list 'message "You can't move the thief to where it already is!")]
    [else (set-board-thief! (state-board st) cell)
          (broadcast st "~a moved the thief to ~a~a~a." (uname usr)
            (style->string '(37 40 #t #f)) (cell->label cell)
            (style->string '(37 40 #f #f)))
          (define usrs (remove usr (remove-duplicates (filter-map
            (lambda (vtx) (match (board-vertex-pair (state-board st) vtx)
              [(cons u _) u]
              [#f #f]))
            (adj-vertices cell)))))
          (match usrs
            ['() (set-state-lock! st #f)]
            [(list usr2) (set-state-lock! st #f) (steal-resource! st usr usr2)]
            [_ (set-state-lock! st (rlock usr "pick a target" 'pick-target usrs
                                          prompt-target!))
               `(prompt pick-target
                ,(format "Will you steal from ~a? Use the `steal` command"
                  (apply string-append (add-between (map uname usrs) ", "
                                        #:before-last ", or "))))])]))

;; choose a target for the thief
(define/contract (prompt-target! st usrname)
  (-> state? string? (or/c void? response?))
  (match (filter (lambda (usr) (equal? (user-name usr) usrname))
                 (state-users st))
    ['() (list 'message (format "~a is not a player in this game!" usrname))]
    [(list usr)
      (define holdr (rlock-holder (state-lock st)))
      (define usrs (rlock-var (state-lock st)))
      (cond
        [(not (member? usr usrs))
          (list 'message (format "You must steal from ~a!"
            (apply string-append
              (add-between (map uname usrs) ", " #:before-last ", or "))))]
        [else (set-state-lock! st #f) (steal-resource! st holdr usr)])]))

;; prompt a target for a resource to take from everyone, for monopoly
(define/contract (prompt-monopoly! st res)
  (-> state? resource? void?)
  (define usr (state-turnu st))
  (define usrs (remove usr (state-users st)))
  (define amts (map (lambda (u) (define usr-amt (hash-ref (user-res u) res))
                                  (hash-set! (user-res u) res 0)
                                  usr-amt)
                    usrs))
  (give-res! usr res (foldr + 0 amts))
  (set-state-lock! st #f)
  (broadcast st "~a stole ~a." (uname usr) (apply string-append (add-between
    (map (lambda (a-u) (format "~a~a from ~a" (show-res (car a-u) res) reset
                                              (uname (cdr a-u))))
         (map cons amts usrs))
    (string-append reset ", ") #:before-last (string-append reset ", and ")))))

(define/contract (prompt-year-of-plenty! st ress)
  (-> state? (cons/c resource? resource?) void?)
  (define usr (state-turnu st))
  (give-res! usr (car ress))
  (give-res! usr (cdr ress))
  (set-state-lock! st #f)
  (broadcast st "~a gained ~a." (uname usr)
                (stock->string (list->stock (list (car ress) (cdr ress))))))

(define/contract (prompt-road-building! st edges)
  (-> state? (cons/c edge? edge?) (or/c response? void?))
  (define usr (state-turnu st))
  (cond
    [(equal? (car edges) (cdr edges))
      (list 'message "Those are the same edge!")]
    [(not (can-road? (state-board st) usr (car edges)))
      (list 'message (format "You can't build a road at ~a!"
                             (edge->string (car edges))))]
    [else (set-board-road-owner! (state-board st) (car edges) usr)
          (cond
            [(can-road? (state-board st) usr (cdr edges))
              (set-board-road-owner! (state-board st) (cdr edges) usr)
              (set-state-lock! st #f)
              (broadcast st "~a built roads at ~a and ~a." (uname usr)
                (edge->string (car edges)) (edge->string (cdr edges)))]
            [else (set-board-road-owner! (state-board st) (car edges) #f)
                  (list 'message (format "You can't build a road at ~a!"
                                         (edge->string (cdr edges))))])]))

(define/contract (prompt-trade! st answer)
  (-> state? (or/c 'accept 'decline) (or/c response? void?))
  (match-define (cons give get) (rlock-var (state-lock st)))
  (define usr (state-turnu st))
  (define target (rlock-holder (state-lock st)))
  (match answer
    ['decline
      (set-state-lock! st #f)
      (broadcast st "~a declined ~a's offer." (uname target) (uname usr))]
    ['accept (cond
      [(can-afford? target get)
        (spend-stock! usr give)
        (give-stock! usr get)
        (spend-stock! target get)
        (give-stock! target give)
        (set-state-lock! st #f)
        (broadcast st "~a accepted ~a's offer." (uname target) (uname usr))]
      [else (list 'message "You can't afford to accept!\n")])]))

;; -------------------------- MAJOR HELPER FUNCTIONS ---------------------------
(define/contract (buy-item! st usr item args)
  (-> state? user? item? (or/c vertex? edge? void?) (or/c response? void?))
  (define b (state-board st))
  (cond
    [(not (can-afford? usr (hash-ref item-prices item)))
      (list 'message (format "You can't afford ~a!" item))]
    [(and (equal? item 'settlement) (not (can-settle? b usr args)))
      (list 'message (format "You can't build a settlement at ~a!"
                             (vertex->string args)))]
    [(and (equal? item 'city) (not (can-city? b usr args)))
      (list 'message (format "You can't build a city at ~a!"
                             (vertex->string args)))]
    [(and (equal? item 'road) (not (can-road? b usr args)))
      (list 'message (format "You can't build a road at ~a!"
                             (edge->string args)))]
    [(and (equal? item 'dev-card) (empty? (state-cards st)))
      (list 'message "There are no more dev cards left to draw!")]
    [else (spend-stock! usr (hash-ref item-prices item))
          (match item
            [(or 'city 'settlement)
              (give-veeps! usr 1)
              (set-board-vertex-pair! b args usr item)
              (broadcast st "~a has built a ~a at ~a." (uname usr) item
                (vertex->string args))]
            ['road
              (set-board-road-owner! b args usr)
              (broadcast st "~a has built a road at ~a." (uname usr)
                (edge->string args))]
            ['dev-card
              (define draw (pop-dev-card! st))
              (set-user-cards! usr (cons draw (user-cards usr)))
              (broadcast st "~a has built a dev card." (uname usr))
              (list 'message (format "You draw a ~a." draw))])]))

(define/contract (use-card! st usr card)
  (-> state? user? dev-card? (or/c response? void?))
  (define usr (state-turnu st))
  (cond
    [(not (member? card (user-cards usr)))
      (list 'message (format "You don't have a ~a!" card))]
    [else
      (set-user-cards! usr (remove card (user-cards usr)))
      (broadcast st "~a uses ~a." (uname usr) card)
      (match card
        ['knight (set-state-lock! st (rlock usr "move the thief"
                                            'move-thief #f prompt-move-thief!))
          `(prompt move-thief
            "Where will you move the thief? Use the `move` command")]
        ['monopoly
          (set-state-lock! st (rlock usr "choose a resource"
                                     'monopoly #f prompt-monopoly!))
          `(prompt monopoly
            "Which resource will you steal? Use the `take` command")]
        ['year-of-plenty
          (set-state-lock! st (rlock usr "choose 2 resources"
                                     'year-of-plenty #f prompt-year-of-plenty!))
          `(prompt year-of-plenty
            "Which resources will you take? Use the `choose` command")]
        ['road-building
          (set-state-lock! st (rlock usr "choose 2 edges"
                                     'road-building #f prompt-road-building!))
          `(prompt road-building
            "Where will you build your 2 roads? Use the `build` command")]
        ['veep (give-veeps! usr 1)
               (broadcast st "~a gains 1 victory point." (uname usr))])]))

;; TODO: trading posts
(define/contract (bank! st usr res-list target)
  (-> state? user? (listof resource?) resource? (or/c response? void?))
  (define cost (list->stock res-list))
  (define posts
    (filter-map (lambda (tp)
              (match-define (list res v1 v2) tp)
              (match (cons (board-vertex-pair (state-board st) v1)
                           (board-vertex-pair (state-board st) v2))
                [(cons (cons (app (curry user=? usr) #t) _) _) res]
                [(cons _ (cons (app (curry user=? usr) #t) _)) res]
                [_ #f]))
            trading-posts))
  (cond
    [(not (can-afford? usr cost))
      (list 'message (format "You don't have ~a!" (stock->string cost)))]
    [(not (member? (length res-list) '(2 3 4)))
      (list 'message (string-append
        "You must trade 2-4 (depending on trading posts) resources to the "
        "bank!"))]
    [(and (= (length res-list) 2)
          (not (equal? (first res-list) (second res-list))))
      (list 'message "You can't bank 2:1 with two different resources!")]
    [(and (= (length res-list) 2) (not (member? (first res-list) posts)))
      (list 'message (format "You need a ~a trading post to bank ~a 2:1!"
                             (first res-list) (first res-list)))]
    [(and (= (length res-list) 3) (not (member? 'any posts)))
      (list 'message "You need a ? trading post to bank 3:1!")]
    [else (spend-stock! usr cost) (give-res! usr target)
      (broadcast st "~a banked ~a for ~a[~am1 ~a~a" (uname usr)
                 (stock->string cost) col-esc (resource->color target)
                 target (style->string '(40 37 #f #f)))]))

;; produce a string of info about a thing
;; TODO: remove all?
(define/contract (show st usr thing)
  (-> state? user? showable? string?)
  (match thing
    ['turn (format "It's ~a's turn." (uname (state-turnu st)))]
    ['board (board->string (state-board st))]
    ['veeps (format "Victory points: ~a."
      (apply string-append
        (add-between
          (map (lambda (usr) (format "~a (~a)" (uname usr) (user-veeps usr)))
               (sort (state-users st) > #:key user-veeps))
          ", ")))]
    ['resources (format "You have ~a." (stock->string (user-res usr)))]
    ['users (string-append "Players are " (list->string (apply append 
      (add-between (map (compose string->list uname) (state-users st))
                   '(#\, #\space)))))]
    ['dev-cards (cond
      [(empty? (user-cards usr)) "You have no dev cards."]
      [else (string-append "Your dev cards: " (list->string (apply append
       (add-between (map (compose string->list symbol->string) (user-cards usr))
                    '(#\, #\space)))))])]
    ['all (string-append
      (show st usr 'turn) "\n"
      (show st usr 'board)
      (show st usr 'resources) "\n"
      (show st usr 'dev-cards) "\n"
      (show st usr 'users) "\n"
      (show st usr 'veeps) "\n")]))

;; send a trade offer to another player
(define/contract (offer! st usr targetname give get)
  (-> state? user? string? stock? stock? (or/c response? void?))
  (define target (match (filter (compose (curry string=? targetname) user-name)
                                (state-users st))
                  [(cons u _) u]
                  [_ #f]))
  (cond
    [(not target) (list 'message (format "~a is not a player in this game!"
                                         targetname))]
    [(user=? usr target) (list 'message "You can't trade with yourself!")]
    [(not (can-afford? usr give)) (list 'message (format "You can't afford ~a!"
                                        (stock->string give)))]
    [else (set-state-lock! st (rlock target "decide on the trade" 'trade
                                     (cons give get) prompt-trade!))
          (broadcast st "~a has offered to give ~a ~a for ~a." (uname usr)
                     (uname target) (stock->string give) (stock->string get))
          (send-message target (list 'prompt 'trade
            (format "Will you accept ~a's offer? Use `accept` or `decline`."
                    (uname usr))))]))

;; ------------------------------- API FUNCTIONS -------------------------------
;; rlock prompt for getting an initial settlement placement
(define/contract (init-settlement! st vtx)
  (-> state? vertex? (or/c response? void?))
  (match-define (rlock usr _ _ (cons usri forward) _) (state-lock st))
  (define b (state-board st))
  (define nbrs
    (filter (lambda (v) (> (length (filter (curryr member? vtx) v)) 1))
            board-vertex-list))
  (cond
    [(andmap (lambda (v) (not (board-vertex-pair b v))) nbrs)
      (set-board-vertex-pair! b vtx usr 'settlement)
      (unless forward
        (give-stock! usr (list->stock
          (remove 'desert (filter-map (curry board-cell-resource b)
                                      (filter cell-valid? vtx))))))
      (set-state-lock! st (rlock usr (format "place their ~a road"
                                             (if forward "1st" "2nd"))
                                 'init-road (list vtx usri forward) init-road!))
      (broadcast st "~a has placed their ~a settlement at ~a." (uname usr)
        (if forward "1st" "2nd") (vertex->string vtx))
      (send-message usr (list 'prompt 'init-road
        (format "Where will you place your ~a road? Use the `place` command"
                (if forward "1st" "2nd"))))]
    [else (list 'message (format "You can't place your settlement at ~a!"
                                 (vertex->string vtx)))]))

;; rlock prompt for getting an initial road placement
(define/contract (init-road! st edg)
  (-> state? edge? (or/c response? void?))
  (match-define (rlock usr _ _ (list vtx usri forward) _) (state-lock st))
  (define b (state-board st))
  (define adj (filter (curryr member? vtx) (list (car edg) (cdr edg))))
  (cond
    [(< (length (filter (curryr member? vtx) (list (car edg) (cdr edg)))) 2)
      (list 'message (format "You must place your road next to ~a!"
                             (vertex->string vtx)))]
    [(board-road-owner b edg)
      (list 'message (format "There is already a road at ~a!"
                             (edge->string edg)))]
    [else
      (set-board-road-owner! b edg usr)
      (broadcast st "~a has placed their ~a road at ~a." (uname usr)
                    (if forward "1st" "2nd") (edge->string edg))
      (match-define (cons new-usri new-forward) (match (cons usri forward)
        [(cons (== (sub1 (length (state-users st)))) #t) (cons usri #f)]
        [(cons (== 0) #f) (cons (void) (void))]
        [(cons _ #t) (cons (add1 usri) #t)]
        [(cons _ #f) (cons (sub1 usri) #f)]))
      (cond
        [(not (void? new-usri))
          (send-message (list-ref (state-users st) new-usri) (list 'prompt
            'settlement (format "Where will you place your ~a settlement? ~a"
                                (if new-forward "1st" "2nd")
                                "Use the `place` command")))
          (set-state-lock! st
            (rlock (list-ref (state-users st) new-usri)
              (format "place their ~a settlement" (if new-forward "1st" "2nd"))
              'init-settlement (cons new-usri new-forward) init-settlement!))]
        [else (set-state-lock! st #f)
              (send-start-message st)])]))

;; handle a request from the user
(define/contract (handle-action! st usr act)
  (-> (or/c state? #f) user? any/c (or/c response? void?))
  (logf 'debug "handle-action!: usr=~a, act=~s\n" (user-name usr) act)
  (cond
    ;; user tries to do something out of turn
    [(and (not (equal? usr (state-turnu st)))
          (cons? act)
          (not (member? (car act) icommands)))
      (list 'message "It is not your turn.")]
    ;; user tries to do something during an active rlock
    [(and (state-lock st)
          (cons? act)
          (not (member? (car act) icommands)))
      (list 'message (format "Waiting for ~a to ~a."
                             (uname (rlock-holder (state-lock st)))
                             (rlock-action (state-lock st))))]
    ;; general case
    [else
      (match act
        [`(buy dev-card) (buy-item! st usr 'dev-card (void))]
        [`(buy ,item ,args) (buy-item! st usr item args)]
        [`(use ,card) (use-card! st usr card)]
        [`(bank ,res-list ,target) (bank! st usr res-list target)]
        [`(offer ,target ,give ,get) (offer! st usr target (list->stock give)
                                                           (list->stock get))]
        [`(end) (change-turn! st)]
        ;; TODO: rework show
        [`(show board) `(update board ,(show st usr 'board))]
        [`(show ,thing) (list 'message (show st usr thing))]
        [`(request-update) (send-updates st (list usr))]
        [`(say ,msg) (void (map (curryr send-message `(say ,(uname usr) ,msg))
                                (state-users st)))]
        [`(respond ,type ,resp) (match (state-lock st)
          [(rlock (app (curry user=? usr) #t) _ (== type) _ fn) (fn st resp)]
          [_ (list 'message "You can't do that right now.")])]
        [_ (list 'message (format "Invalid command: ~s" act))])]))

;; creates a new state, given a non-empty list of users
(define/contract (init-state usrs)
  (-> (listof user?) state?)
  (state usrs (first usrs) (create-board) (shuffle dev-cards)
         (rlock (first usrs) "place their 1st settlement" 'init-settlement
                (cons 0 #t) init-settlement!)))
