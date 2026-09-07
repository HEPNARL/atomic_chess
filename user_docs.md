# User documetation
## Build
To build the program run:
```ghc -o AtomicChess Main.hs ChessPieces.hs Moves.hs Display.hs PositionValid.hs IntExtended.hs```
in the root directory of the project.

Alternatively the provided `build.bat` script can be used to build and run the program.

## Using the program
By running the program a new game is automatically initiated, human player is automatically assigned white.

To play a move enter it in a form of 4 integers separated by spaces in folloing form: 

\<aphabetical order of starting row letter\> \<number of starting column\> \<aphabetical order of target row letter\> \<number of end column\>

For example to play Nf3 (g1f3) as a first move, use:

7 1 6 3

## Tests
Every time the program is started test are ran automatically before the game is initialized.
