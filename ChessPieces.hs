{- HLINT ignore "Redundant bracket" -}
module ChessPieces where

data Coordinate = Coord Int Int
    deriving (Eq, Ord, Read, Show)

data Color = Black | White
    deriving (Eq, Ord, Read, Show)

data Piece = Pc Int Coordinate
    deriving (Eq, Ord, Read, Show)

data Move = Mv Coordinate Coordinate
    deriving (Eq, Ord, Read, Show)

-- base functions for introduced data types

getCoordinate :: Piece -> Coordinate
getCoordinate (Pc _ cd) = cd

getId :: Piece -> Int
getId (Pc id _) = id

getColor :: Piece -> Color
getColor piece
    | odd (getId piece) = White
    | otherwise = Black

xAxis :: Coordinate -> Int
xAxis (Coord x  _) = x

yAxis :: Coordinate -> Int
yAxis (Coord _  y) = y

inverseColor :: Color -> Color
inverseColor White = Black
inverseColor Black = White

idAt :: [Piece] -> Coordinate -> Maybe Int
idAt [] _ = Nothing
idAt ((Pc id crd1):pieces) crd2
    | crd1 == crd2 = Just id
    | otherwise = idAt pieces crd2
-----------------------------------------------------------------
-- universal functions for testing
test :: [Char] -> Bool -> IO()
test text res
    | res = putStrLn (text++": passed")
    | otherwise = putStrLn (text++": failed")
-----------------------------------------------------------------
-- ALL EXPLOSION MECHANICS

-- determines if piece explodes from Piece and coordinate of the explosion
exploded :: Piece -> Coordinate -> Bool
exploded (Pc t (Coord x1 y1)) (Coord x2 y2)
    | (x1 == x2) && (y1 == y2) = True
    | (t > 2) && (abs (x1-x2) <= 1) && (abs (y1-y2) <= 1) = True
    | otherwise = False

-- finds and retrns coordinate of explosion
explosionLocator :: [Piece] -> Maybe Coordinate
explosionLocator x = explosionLocatorHelper x []
-- hepler function that finds coordinate with 2 pieces on it
explosionLocatorHelper :: [Piece] -> [Coordinate] -> Maybe Coordinate
explosionLocatorHelper [] _ = Nothing
explosionLocatorHelper pieces coordinates
    | null coordinates = explosionLocatorHelper (tail pieces) [next_coord]
    | next_coord `elem` coordinates = Just next_coord
    | otherwise = explosionLocatorHelper (tail pieces) (next_coord:coordinates)
    where
        next_coord = getCoordinate (head pieces)

-- executes the explosion after move has been made - does nothing if no explosion occurs
explodePieces :: [Piece] -> [Piece]
explodePieces pieces = explodePiecesHelper pieces (explosionLocator pieces)
-- helper function of explodePieces - removes the exploded pieces after the explosion coordinate has been set
explodePiecesHelper :: [Piece] -> Maybe Coordinate -> [Piece]
explodePiecesHelper pieces Nothing = pieces --no explosion
explodePiecesHelper [] _ = []
explodePiecesHelper pieces (Just coord)
    | exploded next coord = explodePiecesHelper (tail pieces) (Just coord)
    | otherwise = next:explodePiecesHelper (tail pieces) (Just coord)
    where
        next = head pieces


explosionTests :: IO()
explosionTests = do
    test "explosion test 1" (exploded (Pc 1 (Coord 2 2) )(Coord 2 2))
    test "explosion test 2" (not(exploded (Pc 1 (Coord 2 2) ) (Coord 3 3)))
    test "explosion test 3" (exploded (Pc 2 (Coord 2 2) ) (Coord 2 2))
    test "explosion test 4" (exploded (Pc 3 (Coord 2 2) ) (Coord 3 3))
    test "explosion test 5" (not(exploded (Pc 2 (Coord 2 2) ) (Coord 3 4)))
    test "explosion test 6" (not(exploded (Pc 2 (Coord 2 2) ) (Coord 4 3)))
    test "explosion test 7" ((explodePieces [Pc 1 (Coord 6 6), Pc 2 (Coord 6 6), Pc 3 (Coord 5 5), Pc 1 (Coord 7 7)]) == [Pc 1 (Coord 7 7)])
-----------------------------------------------------------------------------------------------------------


colorAt :: Coordinate -> [Piece] -> Color
colorAt coord pieces
    | null pieces = Black --or noithing I need to decide later
    | getCoordinate first == coord = getColor first
    | otherwise = colorAt coord (tail pieces)
    where
        first = head pieces

startingPosition :: [Piece]
startingPosition = [(Pc 8 (Coord 1 8)), (Pc 4 (Coord 2 8)), (Pc 6 (Coord 3 8)), (Pc 10 (Coord 4 8)), (Pc 12 (Coord 5 8)), (Pc 6 (Coord 6 8)), (Pc 4 (Coord 7 8)), (Pc 8 (Coord 8 8)),
                    (Pc 2 (Coord 1 7)), (Pc 2 (Coord 2 7)), (Pc 2 (Coord 3 7)), (Pc 2 (Coord 4 7)), (Pc 2 (Coord 5 7)), (Pc 2 (Coord 6 7)), (Pc 2 (Coord 7 7)), (Pc 2 (Coord 8 7)),
                    (Pc 1 (Coord 1 2)), (Pc 1 (Coord 2 2)), (Pc 1 (Coord 3 2)), (Pc 1 (Coord 4 2)), (Pc 1 (Coord 5 2)), (Pc 1 (Coord 6 2)), (Pc 1 (Coord 7 2)), (Pc 1 (Coord 8 2)),
                    (Pc 7 (Coord 1 1)), (Pc 3 (Coord 2 1)), (Pc 5 (Coord 3 1)), (Pc 9 (Coord 4 1)), (Pc 11 (Coord 5 1)), (Pc 5 (Coord 6 1)), (Pc 3 (Coord 7 1)), (Pc 7 (Coord 8 1))
                    ]