module Display (showBoard, readFEN) where

import ChessPieces ( Piece (Pc), Coordinate(Coord), idAt, Color(White,Black) )
import Data.Char (ord)


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

-- converts chracter representations to their piece identification nubers 
-- inverse of getPieceSymbol
getSymbolID :: Char -> Maybe Int
getSymbolID letter
    | letter == 'P' = Just 1
    | letter == 'p' =Just 2
    | letter == 'N' =Just 3
    | letter == 'n' =Just 4
    | letter == 'B' =Just 5
    | letter == 'b' =Just 6
    | letter == 'R' =Just 7
    | letter == 'r' =Just 8
    | letter == 'Q' =Just 9
    | letter == 'q' =Just 10
    | letter == 'K' =Just 11
    | letter == 'k' =Just 12
    | otherwise = Nothing

-- converts FEN color identifier to the internal data type
-- non exhaustive by design
getColorFromChar :: Char -> Color
getColorFromChar letter
    | letter == 'w' = White
    | letter == 'b' = Black

-- reads FEN notation and converts it to a pair of position definition and color of player that is to play next move
readFEN :: [Char] -> (Color, [Piece])
readFEN = readFENhelper 8 1 (White, [])
readFENhelper :: Int -> Int -> (Color, [Piece]) -> [Char] -> (Color, [Piece])
readFENhelper row col (_, constructed) (next:notation)
    | next == ' ' = ((getColorFromChar.head) notation, constructed)
    | col == 9 = readFENhelper (row-1) 1 (White, constructed) notation --ignores the forward slash
    | id == Nothing = readFENhelper row new_col (White, constructed) notation
    | otherwise = readFENhelper row (col+1) (White, Pc (unjust id) (Coord col row):constructed) notation
    where
        id = getSymbolID next
        num = ord next - ord '0'
        new_col = col+num

-- removes Just from Maybe types
unjust :: Maybe a -> a
unjust (Just a) = a