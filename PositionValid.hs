{- HLINT ignore "Redundant bracket" -}
module PositionValid where

import ChessPieces
import Moves
import Display
import Text.XHtml (black)


isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf (x:xs) ys = x `elem` ys && isSubsetOf xs ys

--check if both kings are present
validPosition :: [Piece] -> Bool
validPosition pieces = 11 `elem` ids && 12 `elem` ids
    where
        ids = [getId x | x <- pieces]

finished :: [Piece] -> Bool
finished x = terminated Black x || terminated White x

-- one sided termination state check
terminated :: Color -> [Piece] -> Bool
terminated color pieces = length kings /= 1
    -- | otherwise = isSubsetOf ((getCoordinate (head kings)):[ end | Mv _ end <- (kingMoves (head kings) pieces)]) (getInaccessibleSquares color pieces)
    where
        kings = filter (\x -> (getColor x) == color && (getId x) >= 11) pieces --assumes 2 king positions don't exist


-- termination tests
terminationTests :: IO()
terminationTests = do
    let
        black_win_position = (executeMove (executeMove (executeMove startingPosition (Mv (Coord 7 8) (Coord 6 6))) (Mv (Coord 6 6) (Coord 7 4))) (Mv (Coord 7 4) (Coord 6 2)))
    test "termination test 1" (not $ terminated White startingPosition)
    test "termination test 2" (not $ terminated Black startingPosition)
    test "termination test 3" (not $ terminated Black black_win_position)
    test "termination test 4" (terminated White black_win_position)