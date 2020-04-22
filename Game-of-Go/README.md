# Game-of-Go
Repo containing Go game - implemented in Typed Racket. Running the code will open an interactive game of Go via the command (play 19 (BoardSpec 'moccasin 24 14 7)) at the very end of the code. 

The game supports piece capture, illegal move detection, and players can pass on moving by pressing "p". The game ends if two consecutive moves are passes, and the program calculates a score to determine the winner. The game has a timer and can be saved and loaded using "s" and "l" respectively.

NOTE: This code was written in Typed Racket 7.0, and may be incompatible with later versions.
