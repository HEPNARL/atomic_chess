module Main where

import ChessPieces
import Moves
    ( kingMoves, executeMove, moveTests, getInaccessibleSquares, getPlayerMoves , nullMove)
import Display
import PositionValid
import IntExtended ( InfInt (NegInf, PosInf, IntValue), zero, infIntTests )
import Distribution.Utils.Generic (fstOf3, sndOf3, trdOf3)
import ReadMove
import AlphaBeta

-- move execution function
playMove :: GameTree -> Move -> GameTree
playMove (Tree pieces children _ color) move = Tree (executeMove pieces move) [] Nothing (inverseColor color)

-- regular game starting function
startGame :: IO()
startGame = playGame startingPositionTree

-- custom game strting function that reads fen of choice from standard input
customGame :: IO()
customGame = do
    putStrLn "Insert FEN: "
    fen <- getLine
    let
        (color, pieces) = readFEN fen
    playGame (Tree pieces [] Nothing color)


-- UI game mechanics
playGame :: GameTree -> IO()
playGame tree = do
    showBoard (getPieces tree)
    if finished $ getPieces tree
    then do
        putStrLn "Game over black won."
    else do
        move <- getLegalMove (getColorTree tree) (getPieces tree)
        let
            moved = playMove tree move
        showBoard (getPieces moved)
        if finished $ getPieces moved
            then do
                putStrLn "Game over white won."
        else do
            let res = abStart 4 moved
            print (fstOf3 res)
            playGame (playMove moved (head $ sndOf3 res))



main :: IO ()
main = do
    explosionTests
    moveTests
    infIntTests
    terminationTests
    -- customGame
    startGame


