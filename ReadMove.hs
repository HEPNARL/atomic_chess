module ReadMove where
    
import ChessPieces
import Moves


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


-- verification of move legality for UI
verifyMove :: Move -> Color -> [Piece] -> Bool
verifyMove move color pieces = move `elem` legal
    where
        legal = getPlayerMoves color pieces

-- move reading for UI
getLegalMove :: Color -> [Piece] -> IO(Move)
getLegalMove color pieces = do
    (a, b, c, d) <- readMove
    let
        move = Mv (Coord a b) (Coord c d)
    if verifyMove move color pieces
        then do
            return move
        else do
            move <- getLegalMove color pieces
            return move

