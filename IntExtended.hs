module IntExtended where
import ChessPieces (test)


data InfInt = NegInf | PosInf | IntValue Int
    deriving (Eq, Read, Show)

instance Ord InfInt where
    compare NegInf NegInf = EQ
    compare PosInf PosInf = EQ
    compare NegInf _ = LT
    compare PosInf _ = GT
    compare _ PosInf = LT
    compare _ NegInf = GT
    compare (IntValue x) (IntValue y) = compare x y

zero :: InfInt
zero = IntValue 0

-- example use of InfInt
infIntTests :: IO()
infIntTests = do
    putStrLn "Type examples: "
    let thing1 = NegInf
        thing2 = IntValue 3
        thing3 = IntValue 5
        thing4 = PosInf
    
    test "InfInt test 1" (thing1 < thing2)   -- Output: True
    test "InfInt test 2" (thing1 == thing1)   -- Output: True
    test "InfInt test 3" (thing4 == thing4)   -- Output: True
    test "InfInt test 4" (thing1 < thing3)   -- Output: True
    test "InfInt test 5" (thing2 < thing3)   -- Output: True
    test "InfInt test 6" (thing3 > thing2)   -- Output: True
    test "InfInt test 7" (thing2 < thing4)   -- Output: True
