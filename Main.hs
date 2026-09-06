{- HLINT ignore "Redundant bracket" -}
module Main where

import ChessPieces
import Moves
    ( kingMoves, executeMove, moveTests, getInaccessibleSquares, getPlayerMoves , nullMove)
import Display
import PositionValid
import IntExtended ( InfInt (NegInf, PosInf, IntValue), zero )
import Distribution.Utils.Generic (fstOf3, sndOf3, trdOf3)
-- positions will be represented as a list of pieces and their positions
-- explosion deletes all surrounding squares

-- position, variants, low high tree_size move_from_previous_position player_turn
data GameTree = Tree [Piece] [GameTree] Int (Maybe Move) Color
    deriving (Eq, Ord, Read, Show)

startingPositionTree :: GameTree
startingPositionTree = Tree startingPosition [] 1 Nothing White

isLeaf :: GameTree -> Bool
isLeaf (Tree _ children _ _ _) = null children

eval :: GameTree -> InfInt
eval tree
    | terminated Black pcs = PosInf  --PosInf
    | terminated White pcs = NegInf
    | otherwise = IntValue (length (getPlayerMoves White pcs) - length (getPlayerMoves Black pcs))
    where
        pcs = getPieces tree



getChildren :: GameTree -> [GameTree]
getChildren (Tree _ children _ _ _) = children

getMove :: GameTree -> Move
getMove (Tree _ _ _ (Just move) _) = move
getMove (Tree _ _ _ Nothing _) = nullMove -- runtime error prevention, shouldn't be used

getColor :: GameTree -> Color
getColor (Tree _ _ _ _ color) = color

getPieces :: GameTree -> [Piece]
getPieces (Tree pcs _ _ _ _) = pcs


generateChildren :: GameTree -> GameTree
generateChildren (Tree pieces children size move color)
    | not $ null children = (Tree pieces children size move color) -- repeated generation prevention
    | (terminal == PosInf) || (terminal == NegInf) = (Tree pieces [] size move color) -- no expansion for terminal positions
    | (not.null) generated = (Tree pieces generated size move color)
    where
        terminal = eval (Tree pieces children size move color)
        generated = [Tree (executeMove pieces x) [] 1 (Just x) (inverseColor color) | x <- (getPlayerMoves color pieces)]

abStart :: Int -> GameTree -> (InfInt, [Move], GameTree)
abStart depth = alphaBeta depth NegInf PosInf

alphaBranches :: Int -> Color -> InfInt -> InfInt -> [GameTree] -> [(InfInt, [Move], GameTree)]
alphaBranches _ _ _ _ [] = []
alphaBranches n color alpha beta (tree:trees)
    | alpha >= beta = []
    | color == White = (val, best_move, subtree):(alphaBranches n color (max alpha val) beta trees) -- black to move
    | color == Black = (val, best_move, subtree):(alphaBranches n color alpha (min beta val) trees) -- white to move
    where
        (val, best_move, subtree) = alphaBeta (n-1) alpha beta tree

getBest :: Color -> [(InfInt, [Move], GameTree)] -> (InfInt, [Move])
getBest _ [(val, moves, tree)] = (val, best:moves)
    where best = getMove tree
getBest Black ((val, moves, tree):xs)
    | fst other < val = other
    | otherwise = (val, best:moves)
    where
        other = getBest Black xs
        best = getMove tree
getBest White ((val, moves, tree):xs)
    | fst other > val = other
    | otherwise = (val, best:moves)
    where
        other = getBest White xs
        best = getMove tree
getBest Black [] = (PosInf, [nullMove])
getBest White [] = (NegInf, [nullMove])



-- depth max min starting position
alphaBeta :: Int -> InfInt -> InfInt -> GameTree -> (InfInt, [Move], GameTree)
alphaBeta 0 _ _ tree = (eval tree, [], tree)
alphaBeta _ _ _ tree
    | terminated Black (getPieces tree) = (eval tree, [], tree)
    | terminated White (getPieces tree) = (eval tree, [], tree)
alphaBeta n alpha beta (Tree pieces children size move color)
    | not $ null results = (val, best_move, Tree pieces outTrees size move color)
    | otherwise = (val, best_move, Tree pieces outTrees size move color)
    where
        newchildren = generateChildren (Tree pieces children size move color)
        results = alphaBranches n color (if color == White then NegInf else alpha) (if color == Black then beta else PosInf) (getChildren newchildren)
        (_ , _, outTrees) = unzip3 results
        (val, best_move) = getBest color results

playMove :: GameTree -> Move -> GameTree
playMove (Tree pieces children size _ color) move = (Tree (executeMove pieces move) [] size Nothing (inverseColor color))

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


startGame :: IO()
startGame = playGame startingPositionTree

playGame :: GameTree -> IO()
playGame tree = do
    showBoard (getPieces tree)
    if finished $ getPieces tree
    then do 
        if terminated  Black $ getPieces tree then putStrLn "Game over white won." else putStrLn "Game over black won."
    else do
        (a, b, c, d) <- readMove
        let
            moved = playMove tree (Mv (Coord a b) (Coord c d))
        showBoard (getPieces moved)
        if finished $ getPieces moved
            then do 
                (if terminated  Black $ getPieces tree then putStrLn "Game over white won." else putStrLn "Game over black won.")
        else do
            let res = abStart 3 moved
            if PosInf == fstOf3 res || NegInf == fstOf3 res
                then do
                    playGame (playMove moved (head $ sndOf3 res))
            else do
                playGame (playMove moved (head $ sndOf3 (abStart 4 (trdOf3 res))))



main :: IO ()
main = do
    explosionTests
    moveTests
    startGame
    -- example
