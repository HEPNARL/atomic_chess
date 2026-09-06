{- HLINT ignore "Redundant bracket" -}
module Moves (getPlayerMoves, executeMove, moveTests, kingMoves, getInaccessibleSquares, nullMove) where

import ChessPieces
import Data.List (nub)

-- EXPORTED FUNCTIONS

-- returns a move for selected player
getPlayerMoves :: Color -> [Piece] -> [Move]
getPlayerMoves color pieces = concat [getMoves x pieces | x <- colored]
    where
        colored = filterByColor pieces color

-- executes a move including potential explosion
executeMove :: [Piece] -> Move -> [Piece]
executeMove pieces move = explodePieces (movePiece pieces move)

-- retuns a list of squares a king can't access
getInaccessibleSquares :: Color -> [Piece] -> [Coordinate]
getInaccessibleSquares color pieces = [ end | Mv _ end <- (getPlayerMoves (inverseColor color) pieces)] ++ [ crd | Pc _ crd <- filter (\x -> (getId x) <= 10 || ((getColor x) /= color)) pieces]

-- runtime error precaution - Maybe Move should be used instead in cases where no move is expected outcome
nullMove :: Move
nullMove = Mv (Coord 0 0) (Coord 0 0)
----------------------------------------------------------------------
-- TESTS
moveTests :: IO()
moveTests = do
    test "move test 1" ((getPlayerMoves White [Pc 3 (Coord 1 3), Pc 3 (Coord 2 1), Pc 4 (Coord 3 2)]) == [Mv (Coord 1 3) (Coord 3 2),Mv (Coord 1 3) (Coord 3 4),Mv (Coord 1 3) (Coord 2 5),Mv (Coord 2 1) (Coord 4 2),Mv (Coord 2 1) (Coord 3 3)])
    test "move test 2" (length (getPlayerMoves White startingPosition) == 20)
    test "move test 3" (null (executeMove [Pc 3 (Coord 1 3), Pc 3 (Coord 2 1), Pc 4 (Coord 3 2)] (Mv (Coord 1 3) (Coord 3 2))))
    test "move test 4" ((length.nub) (getInaccessibleSquares White startingPosition) == 47) --nub removes duplicates
----------------------------------------------------------------------------------------------------

-- changes coordinates of piece that is on the first coordinate of Move to the second coordinate of Move
movePiece :: [Piece] -> Move -> [Piece]
movePiece [] _ = []
movePiece ((Pc pcid crd):xs) (Mv start fin)
    | crd == start = (Pc pcid fin) : xs
    | otherwise =  (Pc pcid crd) : movePiece xs (Mv start fin)

-- generates a full list of moves for selected piece and set board configuration (moves invalid due to king being under attack aren't filtered)
getMoves :: Piece -> [Piece] -> [Move]
getMoves (Pc id coord) pieces
    | id == 1 = whitePawnMoves (Pc id coord) pieces
    | id == 2 = blackPawnMoves (Pc id coord) pieces
    | id == 3 || id == 4 = knightMoves (Pc id coord) pieces
    | id == 5 || id == 6 = bishopMoves (Pc id coord) pieces
    | id == 7 || id == 8 = rookMoves (Pc id coord) pieces
    | id == 9 || id == 10 = queenMoves (Pc id coord) pieces
    | otherwise = kingMoves (Pc id coord) pieces

-- extracts pieces of set color
filterByColor :: [Piece] -> Color -> [Piece]
filterByColor pieces color = filter (\x -> getColor x == color) pieces

-- checks if square (defined as a piece) occupied
empty_ :: Piece -> [Piece] -> Bool
empty_ (Pc _ crd) pieces = crd `notElem` ([getCoordinate x | x <- pieces])

-- checks if square (defined as a piece with matching color) occupied by a piece of opposite color
opposite :: Piece -> [Piece] -> Bool
opposite piece pieces = (getCoordinate piece `elem` [getCoordinate x | x <- pieces]) && (colorAt (getCoordinate piece) pieces /= (getColor piece))

--filters out of bounds moves fdrom a list
filterOutOfBoundsMoves :: [Move] -> [Move]
filterOutOfBoundsMoves = concatMap filterOutOfBoundsMove --that's a good one

-- filters single out of bounds move -- returns as a list (Maybe is inconvinient for use in filterOutOfBoundsMoves)
filterOutOfBoundsMove :: Move -> [Move]
filterOutOfBoundsMove (Mv a (Coord x y))
    | x >= 1 && y >= 1 && x <= 8 && y <= 8  =[(Mv a (Coord x y))]
    | otherwise = []

-- returns move from original coordinate to new coordinate defined as Piece if the trget square is empty
move :: Coordinate -> Piece -> [Piece] -> [Move]
move start piece pieces
    | empty_ piece pieces = filterOutOfBoundsMoves [Mv start (getCoordinate piece)]
    | otherwise = []

-- returns move from original coordinate to new coordinate defined as Piece if the trget square is occupied by a piece of opposite color
take_ :: Coordinate ->Piece -> [Piece] -> [Move]
take_ start piece pieces
    | opposite piece pieces = filterOutOfBoundsMoves [Mv start (getCoordinate piece)]
    | otherwise = []

-- combines take_ and move
moveOrTake :: Coordinate -> [Piece] -> Piece  -> [Move]
moveOrTake start pieces piece = move start piece pieces ++ take_ start piece pieces

-- retuns all legal moves for white pawn
whitePawnMoves :: Piece -> [Piece] -> [Move]
whitePawnMoves piece pieces
    | startRow && empty_ (Pc (getId piece) up) pieces = filterOutOfBoundsMoves (move piece_coordinate (Pc (getId piece) up) pieces ++ move piece_coordinate (Pc (getId piece) up2) pieces ++ take_ piece_coordinate (Pc (getId piece) takeLeft) pieces ++ take_ piece_coordinate (Pc (getId piece) takeRight) pieces)
    | otherwise = filterOutOfBoundsMoves (move piece_coordinate (Pc (getId piece) up) pieces ++ take_ piece_coordinate (Pc (getId piece) takeLeft) pieces ++ take_ piece_coordinate (Pc (getId piece) takeRight) pieces)
    where
        piece_coordinate = getCoordinate piece
        up = Coord (xAxis piece_coordinate) (yAxis piece_coordinate + 1)
        up2 = Coord (xAxis piece_coordinate) (yAxis piece_coordinate + 2)
        startRow = yAxis piece_coordinate == 2
        takeLeft = Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate + 1)
        takeRight = Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate + 1)

-- retuns all legal moves for black pawn
blackPawnMoves :: Piece -> [Piece] -> [Move]
blackPawnMoves piece pieces
    | startRow && empty_ (Pc (getId piece) down) pieces = filterOutOfBoundsMoves (move piece_coordinate (Pc (getId piece) down) pieces ++ move piece_coordinate (Pc (getId piece) down2) pieces ++ take_ piece_coordinate (Pc (getId piece) takeLeft) pieces ++ (take_ piece_coordinate (Pc (getId piece) takeRight) pieces))
    | otherwise = filterOutOfBoundsMoves (move piece_coordinate (Pc (getId piece) down) pieces ++ take_ piece_coordinate (Pc (getId piece) takeLeft) pieces ++ take_ piece_coordinate (Pc (getId piece) takeRight) pieces)
    where
        piece_coordinate = getCoordinate piece
        down = Coord (xAxis piece_coordinate) (yAxis piece_coordinate - 1)
        down2 = Coord (xAxis piece_coordinate) (yAxis piece_coordinate - 2)
        startRow = yAxis piece_coordinate == 7
        takeLeft = Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate - 1)
        takeRight = Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate - 1)

-- retuns all legal moves for a knight
knightMoves :: Piece -> [Piece] -> [Move]
knightMoves piece pieces = filterOutOfBoundsMoves (concatMap (moveOrTake piece_coordinate pieces) [topLeft, topRight, bottomLeft, bottomRight, leftTop, leftBottom, rightTop, rightBottom])
    where
        piece_coordinate = getCoordinate piece
        topLeft = Pc (getId piece) (Coord (xAxis piece_coordinate +2) (yAxis piece_coordinate - 1))
        topRight = Pc (getId piece) (Coord (xAxis piece_coordinate +2) (yAxis piece_coordinate + 1))
        bottomLeft = Pc (getId piece) (Coord (xAxis piece_coordinate -2) (yAxis piece_coordinate + 1))
        bottomRight = Pc (getId piece) (Coord (xAxis piece_coordinate -2) (yAxis piece_coordinate - 1))
        leftTop = Pc (getId piece) (Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate +2))
        leftBottom = Pc (getId piece) (Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate +2))
        rightTop = Pc (getId piece) (Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate -2))
        rightBottom = Pc (getId piece) (Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate -2))

-- repeats directional move until a colision or out of bounds, direction is specified with integers x,y in set order
repeatUntilColision :: Int -> Int -> Int -> Piece -> [Piece] -> [Move]
repeatUntilColision x y iteration piece pieces
    | empty_ directionPiece pieces && inBounds = Mv piece_coordinate directionSquare : repeatUntilColision x y (iteration+1) piece pieces
    | opposite directionPiece pieces = [Mv piece_coordinate directionSquare] -- assumes no piece starts out of bounds
    | otherwise = []
    where
        piece_coordinate = getCoordinate piece
        newX = xAxis piece_coordinate + (x * iteration)
        newY = yAxis piece_coordinate + (y * iteration)
        directionSquare = Coord newX newY
        inBounds = newX >= 1 && newY >= 1 && newX <= 8 && newY <= 8
        directionPiece = Pc (getId piece) directionSquare

-- all directional variants of repeatUntilColision:
repeatUp :: Piece -> [Piece] -> [Move]
repeatUp = repeatUntilColision 0 1 1
repeatDown :: Piece -> [Piece] -> [Move]
repeatDown = repeatUntilColision 0 (-1) 1
repeatRight :: Piece -> [Piece] -> [Move]
repeatRight = repeatUntilColision 1 0 1
repeatLeft :: Piece -> [Piece] -> [Move]
repeatLeft = repeatUntilColision (-1) 0 1
repeatUL :: Piece -> [Piece] -> [Move]
repeatUL = repeatUntilColision (-1) 1 1
repeatDR :: Piece -> [Piece] -> [Move]
repeatDR = repeatUntilColision 1 (-1) 1
repeatDL :: Piece -> [Piece] -> [Move]
repeatDL = repeatUntilColision (-1) (-1) 1
repeatUR :: Piece -> [Piece] -> [Move]
repeatUR = repeatUntilColision 1 1 1

-- retuns all legal move for a rook
rookMoves :: Piece -> [Piece] -> [Move]
rookMoves x y = repeatDown x y ++ repeatUp x y ++ repeatLeft x y ++ repeatRight x y

-- retuns all legal move for a bishop
bishopMoves :: Piece -> [Piece] -> [Move]
bishopMoves x y = repeatDL x y ++ repeatDR x y ++ repeatUL x y ++ repeatUR x y

-- retuns all legal move for a queen
queenMoves :: Piece -> [Piece] -> [Move]
queenMoves x y = rookMoves x y ++ bishopMoves x y

-- retuns all moves for a king - legality of set moves is ignored
kingMoves :: Piece -> [Piece] -> [Move]
kingMoves piece pieces = filterOutOfBoundsMoves (concatMap (moveOrTake piece_coordinate pieces) [topLeft, topRight, bottomLeft, bottomRight, top, bottom, left, right])
    where
        piece_coordinate = getCoordinate piece
        topLeft = Pc (getId piece) (Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate + 1))
        topRight = Pc (getId piece) (Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate + 1))
        bottomLeft = Pc (getId piece) (Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate - 1))
        bottomRight = Pc (getId piece) (Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate + 1))
        top = Pc (getId piece) (Coord (xAxis piece_coordinate) (yAxis piece_coordinate +1))
        bottom = Pc (getId piece) (Coord (xAxis piece_coordinate) (yAxis piece_coordinate +1))
        left = Pc (getId piece) (Coord (xAxis piece_coordinate -1) (yAxis piece_coordinate))
        right = Pc (getId piece) (Coord (xAxis piece_coordinate +1) (yAxis piece_coordinate))