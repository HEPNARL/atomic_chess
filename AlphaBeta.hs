module AlphaBeta where

import ChessPieces
import Moves
    ( kingMoves, executeMove, moveTests, getInaccessibleSquares, getPlayerMoves , nullMove)
import Display
import PositionValid
import IntExtended ( InfInt (NegInf, PosInf, IntValue), zero, infIntTests )
import Distribution.Utils.Generic (fstOf3, sndOf3, trdOf3)

-- position, variants, low high tree_size move_from_previous_position player_turn
data GameTree = Tree [Piece] [GameTree] (Maybe Move) Color
    deriving (Eq, Ord, Read, Show)

-- extension of startingPosition for the tree sturcture of the game tree
startingPositionTree :: GameTree
startingPositionTree = Tree startingPosition [] Nothing White

-- static evaluation function
eval :: GameTree -> InfInt
eval tree
    | terminated Black pcs = PosInf  --PosInf
    | terminated White pcs = NegInf
    | otherwise = IntValue (length (getPlayerMoves White pcs) - length (getPlayerMoves Black pcs))
    where
        pcs = getPieces tree


-- function that returns list of all existing children of a game tree node
getChildren :: GameTree -> [GameTree]
getChildren (Tree _ children _ _) = children

-- function that returns the move that was played to reach the selected game tree node
getMove :: GameTree -> Move
getMove (Tree _ _ (Just move) _) = move
getMove (Tree _ _ Nothing _) = nullMove -- runtime error prevention, shouldn't be used

-- returns color of player that is to move next
getColorTree :: GameTree -> Color
getColorTree (Tree _ _ _ color) = color

-- returns state of the board from game tree node
getPieces :: GameTree -> [Piece]
getPieces (Tree pcs _ _ _) = pcs

-- generates children nodes for selected game tree node if they don't already exist
generateChildren :: GameTree -> GameTree
generateChildren (Tree pieces children move color)
    | not $ null children = Tree pieces children move color -- repeated generation prevention
    | (terminal == PosInf) || (terminal == NegInf) = Tree pieces [] move color -- no expansion for terminal positions
    | (not.null) generated = Tree pieces generated move color
    where
        terminal = eval (Tree pieces children move color)
        generated = [Tree (executeMove pieces x) [] (Just x) (inverseColor color) | x <- getPlayerMoves color pieces]

-- initiation function of alpha beta pruning
-- inputs: maximum depth and root node
-- returns triplet of position evaluation, best found line and constructed game tree
abStart :: Int -> GameTree -> (InfInt, [Move], GameTree)
abStart depth = alphaBeta depth NegInf PosInf
-- recursive part of search with alpha beta cutoff
alphaBranches :: Int -> Color -> InfInt -> InfInt -> [GameTree] -> [(InfInt, [Move], GameTree)]
alphaBranches _ _ _ _ [] = []
alphaBranches n color alpha beta (tree:trees)
    | alpha >= beta = []
    | color == White = (val, best_move, subtree):alphaBranches n color (max alpha val) beta trees -- black to move
    | color == Black = (val, best_move, subtree):alphaBranches n color alpha (min beta val) trees -- white to move
    where
        (val, best_move, subtree) = alphaBeta (n-1) alpha beta tree


-- function that checks all provided followup variants and returns the best one for selected color
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
-- node entry function that chechks if node is terminal based on set parameters
alphaBeta :: Int -> InfInt -> InfInt -> GameTree -> (InfInt, [Move], GameTree)
alphaBeta 0 _ _ tree = (eval tree, [], tree)
alphaBeta _ _ _ tree
    | terminated Black (getPieces tree) = (eval tree, [], tree)
    | terminated White (getPieces tree) = (eval tree, [], tree)
alphaBeta n alpha beta (Tree pieces children move color)
    | not $ null results = (val, best_move, Tree pieces outTrees move color)
    | otherwise = (val, best_move, Tree pieces outTrees move color)
    where
        newchildren = generateChildren (Tree pieces children move color)
        results = alphaBranches n color (if color == White then NegInf else alpha) (if color == Black then beta else PosInf) (getChildren newchildren)
        (_ , _, outTrees) = unzip3 results
        (val, best_move) = getBest color results