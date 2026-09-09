{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Redundant bracket" -}
module ReadMove (getLegalMove) where

import ChessPieces
import Moves
import Data.Char (ord)

-- UI move readed
readMove :: IO (Int, Int, Int, Int)
readMove = do
    putStrLn "Enter move as four integers separated by spaces:"
    line <- getLine
    let components = words line
    case components of
        [x, y, z, w] -> return (read x, read y, read z, read w)
        _ -> do
            putStrLn "Invalid input. Please enter exactly four integers."
            readMove

-- improved UI reading to make move imput easier with extended notation ie. e2e4
betterRead :: IO (Char, Int, Char, Int)
betterRead = do
    putStrLn "Enter move in form of full move syntax without without piece type specifier separated by spaces:"
    line <- getLine
    case line of
        [x, y, z, w] -> return (x, ord y  - ord '0', z, ord w - ord '0')
        _ -> do
            betterRead
-- converts the read input to the older readMove format
betterReadTranslate :: (Char, Int, Char, Int) -> (Int, Int, Int, Int)
betterReadTranslate (col1, row1, col2, row2) = (ord col1 - ord 'a' + 1, row1, ord col2 - ord 'a' + 1, row2)

-- verification of move legality for UI
verifyMove :: Move -> Color -> [Piece] -> Bool
verifyMove move color pieces = move `elem` legal
    where
        legal = getPlayerMoves color pieces

-- move reading for UI
getLegalMove :: Color -> [Piece] -> IO(Move)
getLegalMove color pieces = do
    inputline <- betterRead
    let
        (a, b, c, d) = betterReadTranslate inputline
        move = Mv (Coord a b) (Coord c d)
    if verifyMove move color pieces
        then do
            return move
        else do
            move <- getLegalMove color pieces
            return move
