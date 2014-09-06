asciitan is under development. Recently, the UI is overhauled; some
tweaks/bugfixes to the new UI are still ocurring. The game functionality is
almost entirely implemented, however, longest road and largest army are not yet
implemented.

To run asciitan, run `racket catan-server.rkt` to start up the server. Then,
have each player run `racket client.rkt`; when it asks for a port, provide the
one printed by catan-server.rkt. Follow the prompts of catan-server.rkt to
start the game. Once the game is started, users can use the help command to
figure out how to play.
