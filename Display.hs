module Display (showBoard) where

import ChessPieces ( Piece, Coordinate(Coord), idAt )
-- import Moves

-- initiates display of the board from square A8
showBoard :: [Piece] -> IO()
showBoard = showBoardHelper 1 8

-- displays the board
showBoardHelper :: Int -> Int -> [Piece] -> IO()
showBoardHelper x y pieces
    | y == 0 = putStr "\n"
    | x == 9 = do
        putStr "\n"
        showBoardHelper 1 (y-1) pieces
    | otherwise = do
        putStr (getPieceSymbol id : " ")
        showBoardHelper (x+1) y pieces
    where
        id = idAt pieces (Coord x y)

-- converts piece identification nubers to their chracter representation
getPieceSymbol :: Maybe Int -> Char
getPieceSymbol (Just id)
    | id == 1 = 'P'
    | id == 2 = 'p'
    | id == 3 = 'N'
    | id == 4 = 'n'
    | id == 5 = 'B'
    | id == 6 = 'b'
    | id == 7 = 'R'
    | id == 8 = 'r'
    | id == 9 = 'Q'
    | id == 10 = 'q'
    | id == 11 = 'K'
    | id == 12 = 'k'
getPieceSymbol Nothing = '_'
