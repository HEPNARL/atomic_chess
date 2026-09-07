# Developer documentation
The source code is spit into following 6 modules:
- Main.hs
- Moves.hs
- PositionValid.hs
- IntExtended.hs
- Display.hs
- ChessPieces.hs

## ChessPieces.hs
Provides core functionality for the game: explosions and definition of data types essential for representation of the game.

```
data Coordinate = Coord Int Int
    deriving (Eq, Ord, Read, Show)

data Color = Black | White
    deriving (Eq, Ord, Read, Show)

data Piece = Pc Int Coordinate
    deriving (Eq, Ord, Read, Show)

data Move = Mv Coordinate Coordinate
    deriving (Eq, Ord, Read, Show)
```

Tests for the module are provided in `explosionTests`

## Moves.hs
This module provides following functions:

- `getPlayerMoves :: Color -> [Piece] -> [Move]`
    - generates list of all moves for selected player from set position
- `executeMove :: [Piece] -> Move -> [Piece]`
    - alters position of the first piece on the list that has the staring position from the Move
- `kingMoves :: Piece -> [Piece] -> [Move]`
    - list of all moves of selected pieces
- `getInaccessibleSquares :: Color -> [Piece] -> [Coordinate]`
    - lists all squares that are under attack from the opponent
- `nullMove`
    - provides instance of empty move
- `moveTests`
    - module tests

# PositionValid.hs
Provides checks if the selected position is one of an ongoing name. With funcions for both one sided and collective game termination checks:

```
validPosition :: [Piece] -> Bool

terminated :: Color -> [Piece] -> Bool
```

Module test are avalable in `terminationTests`

# IntExtended.hs
This module provides implementation if Integer data type with positive and negative infinity:
```
data InfInt = NegInf | PosInf | IntValue Int
    deriving (Eq, Read, Show)
```

The type has implemented only Ord operation and doesn't support any mathematical operations.

The ordering functionality can be tested with `infIntTests`

# Display.hs
This module provides a single function: `showBoard :: [Piece] -> IO()` that displays the board state in terminal.

# Main.hs
The main module contains the game engine that searches the game tree:

```
-- position, variants, low high tree_size move_from_previous_position player_turn
data GameTree = Tree [Piece] [GameTree] Int (Maybe Move) Color
    deriving (Eq, Ord, Read, Show)
```

The game tree is searched through using alpha beta pruning that uses the difference of the ammount of moves for each player at the position as the heuristic evaluation function for non-terminal position.