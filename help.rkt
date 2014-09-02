;; provides info about available commands
#lang racket

(require racket/contract "adv.rkt" "data.rkt" "constants.rkt")

(provide help-cmd)

;; message which lists the commands available to the user
(define general-help (apply string-append `(
  "Available commands: "
  ,@(add-between (map ~a (remove 'respond (append commands client-commands)))
                 " ")
  "\nType `help command` for detailed info on a single command.\n")))

;; to make my life easier in help-cmd
(define sa string-append)

;; produce an info message for the given command
;; TODO: Available commands: buy use bank end show respond move discard steal help
(define/contract (help-cmd cmd)
  (-> (listof symbol?) string?)
  (match cmd
    ['() general-help]
    ['(buy road) (sa "Usage: buy road edge\n"
      "Build a road at edge. See `help edge` for info on how to input an "
      "edge.\n")]
    ['(buy city) (sa "Usage: buy city vertex\n"
      "Build a city at vertex. See `help vertex` for info on how to input a "
      "vertex.\n")]
    ['(buy settlement) (sa "Usage: buy settlement vertex\n"
      "Build a settlement at vertex. See `help vertex` for info on how to "
      "input a vertex.\n")]
    ['(buy dev-card) (sa "Usage: buy dev-card\n"
      "Buy a development card.\n")]
    [(cons 'buy _) (sa "Usage: buy item [place]\n"
      "Buy an item. item is one of road, city, settlement, dev-card. Type "
       "`help buy item` for item-specific info.\n")]
    [(cons 'use _) (sa "Usage: use dev-card\n"
      "Use a development card. dev-card is the name of one of your development "
      "cards\n")]
    ;; TODO: update this when trading posts finally work
    [(cons 'bank _) (sa "Usage: bank take give1 give2 give3 give4\n"
      "Trade 4 resources (give[1-4]) for one resource (take).")]
    [(cons 'end _) (sa "Usage: end\n"
      "End your turn.\n")]
    [(cons 'show _) (sa "Usage: show thing\n"
      "Provides game info. thing is one of board, resources, users, dev-cards, "
      "all.\n")]
    [(cons 'say _) (sa "Usage: say message\n"
      "Sends a chat message to every user.\n")]
    [(cons 'move _) (sa "Usage: move cell\n"
      "When prompted, moves the thief to cell. See `help cell` for info on "
      "how to input a cell.\n")]
    [(cons 'discard _) (sa "Usage: discard res1 res2 ...\n"
      "When prompted (due to having >7 resources on a 7 roll), discards your "
      "resources.\n")]
    [(cons 'steal _) (sa "Usage: steal user\n"
      "When prompted, selects a user from whom to steal a random resource.\n")]
    [(cons 'help _) (sa "Usage: help [command]\n"
      "Displays info on how to use a command. If no command is specified, "
      "displays a list of commands.\n")]

    [(cons 'cell _) (sa "To specify a cell, use the bold uppercase letter in its "
      "top-left corner on the map.\n")]
    [(cons 'edge _) (sa "An edge is specified with the format Z-n, where Z is an "
      "adjacent cell (see `help cell`), and n (in [1-6]) specifies its edge as "
      "follows: the top edge is 1, go clockwise from there.\n")]
    [(cons 'vertex _) (sa "A vertex is specified with the format Z.n, where Z is an "
      "adjacent cell (see `help cell`), and n (in [1-6]) specifies its vertex "
      "as follows: the top-left vertex is 1, go clockwise from there.\n")]
    [(cons cmd _) (format "Unknown command: ~a\n" cmd)]))
