;; provides info about available commands
#lang racket

(require racket/contract "adv.rkt" "data.rkt" "constants.rkt")

(provide help-cmd)

;; message which lists the commands available to the user
(define general-help (apply string-append `(
  "Available commands: "
  ,@(add-between (map ~a (remove 'respond (append commands client-commands)))
                 " ")
  "\nType `help command` for detailed info on a single command.")))

;; helper function to construct usage and detailed info
(define/contract (mkinfo usage . details)
  (->* (string?) #:rest (listof string?) (cons/c string? string?))
  (cons usage (apply string-append details)))

;; produces info for the given command
(define/contract (help-cmd cmd)
  (-> (listof symbol?) (or/c (cons/c string? string?) string?))
  (match cmd
    ;; no arguments
    ['() general-help]
    
    ;; commands
    ['(buy road) (mkinfo "buy road edge"
      (format "Cost: ~a\n" (stock->string (hash-ref item-prices 'road)))
      "Build a road at edge. See `help edge` for info on how to input an "
      "edge.")]
    ['(buy city) (mkinfo "buy city vertex"
      (format "Cost: ~a\n" (stock->string (hash-ref item-prices 'city)))
      "Build a city at vertex. See `help vertex` for info on how to input a "
      "vertex.")]
    ['(buy settlement) (mkinfo "buy settlement vertex"
      (format "Cost: ~a\n" (stock->string (hash-ref item-prices 'settlement)))
      "Build a settlement at vertex. See `help vertex` for info on how to "
      "input a vertex.")]
    ['(buy dev-card) (mkinfo "buy dev-card"
      (format "Cost: ~a\n" (stock->string (hash-ref item-prices 'dev-card)))
      "Buy a development card. Use `help card-name` for info on a specific "
      "card")]
    [(cons 'buy _) (mkinfo "buy item [place]"
      "Buy an item. item is one of road, city, settlement, dev-card. Type "
       "`help buy item` for item-specific info.")]
    [(cons 'use _) (mkinfo "use dev-card"
      "Use a development card. dev-card is the name of one of your development "
      "cards")]
    ;; TODO: update this when trading posts finally work
    [(cons 'bank _) (mkinfo "bank take give1 give2 give3 give4"
      "Trade 4 resources (give[1-4]) for one resource (take).")]
    [(cons 'end _) (mkinfo "end"
      "End your turn.")]
    [(cons 'show _) (mkinfo "show thing"
      "Provides game info. thing is one of board, resources, users, dev-cards, "
      "all.")]
    [(cons 'say _) (mkinfo "say message"
      "Sends a chat message to every user.")]
    [(cons 'help _) (mkinfo "help [command]"
      "Displays info on how to use a command. If no command is specified, "
      "displays a list of commands.")]

    ;; prompt responses
    [(cons 'move _) (mkinfo "move cell"
      "When prompted, moves the thief to cell. See `help cell` for info on "
      "how to input a cell.")]
    [(cons 'discard _) (mkinfo "discard res1 res2 ..."
      "When prompted (due to having >7 resources on a 7 roll), discards your "
      "resources.")]
    [(cons 'steal _) (mkinfo "steal user"
      "When prompted, selects a user from whom to steal a random resource.")]
    [(cons 'take _) (mkinfo "take resource"
      "When prompted by the monopoly dev card, selects a resource to steal "
      "from everyone.")]
    [(cons 'choose _) (mkinfo "choose res1 res2"
      "When prompted by the year-of-plenty dev card, selects two resources "
      "to gain.")]
    [(cons 'build _) (mkinfo "build edge1 edge2"
      "When prompted by the road-building dev card, selects two places to "
      "build roads. The road at edge1 is built first, then the road at "
      "edge2.")]
    [(cons 'place _ ) (mkinfo "place settlement vertex | place road edge"
      "When prompted before the game, place an initial settlement or road. See "
      "`help edge` and `help vertex` for info on how to input an edge/vertex.")]

    ;; cell/edge/vertex format info
    [(cons 'cell _) "To specify a cell, use the bold uppercase letter in its "
      "top-left corner on the map."]
    [(cons 'edge _) "An edge is specified with the format Z-n, where Z is an "
      "adjacent cell (see `help cell`), and n (in [1-6]) specifies its edge as "
      "follows: the top edge is 1, go clockwise from there."]
    [(cons 'vertex _) "A vertex is specified with the format Z.n, where Z is "
      "an adjacent cell (see `help cell`), and n (in [1-6]) specifies its "
      "vertex as follows: the top-left vertex is 1, go clockwise from there."]

    ;; development card info
    [`(knight) (mkinfo "Development card: knight"
      "Move the thief to a location of your choosing (`move` cell).")]
    [`(veep) (mkinfo "Development card: veep"
      "Gain 1 victory point.")]
    [`(monopoly) (mkinfo "Development card: monopoly"
      "Choose a resource. Every other player gives you all of that resource "
      "that they have (`take resource`).")]
    [`(year-of-plenty) (mkinfo "Development card: year-of-plenty"
      "Gain two resources of your choosing (`choose res1 res2`).")]
    [`(road-building) (mkinfo "Development card: road-building"
      "Build two roads for free (`build edge1 edge2`)")]

    [(cons cmd _) (format "Unknown command: ~a" cmd)]))
