asciitan is still under heavy development. In particular, the following things
do not work:

    - Trading posts
    - Longest road/Largest army
    - Non-local connections

To run asciitan, run `racket catan-server.rkt` to start up the server. Then,
have each player run `racket client.rkt`; when it asks for a port, provide the
one printed by catan-server.rkt. Follow the prompts of catan-server.rkt to
start the game. Once the game is started, users can use the help command to
figure out how to play.
