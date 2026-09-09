# User documetation
## Build
To build the program run:
```ghc -o AtomicChess Main.hs ChessPieces.hs Moves.hs Display.hs PositionValid.hs IntExtended.hs AlphaBeta.hs ReadMove.hs```
in the root directory of the project.

Alternatively the provided `build.bat` script can be used to build and run the program.

## Using the program
After starting the program, the user is presented by 2 options.

1. starting an atomic chess game with entering `a` to the standard input
2. starting a game from custom position by entering `c` to the standard input followed by the FE notation when requested

To play a move enter it as a string of 4 letters in the extended form without piece identifier character:

For example to play Nf3 as a first move, use:
```
g1f3
```

## Tests
Every time the program is started test are ran automatically before the game is initialized.
