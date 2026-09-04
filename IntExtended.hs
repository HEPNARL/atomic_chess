module IntExtended where


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
example :: IO()
example = do
    putStrLn "Type examples: "
    let thing1 = NegInf
        thing2 = IntValue 3
        thing3 = IntValue 5
        thing4 = PosInf
    
    print $ thing1 < thing2   -- Output: True
    print $ thing1 == thing1   -- Output: True
    print $ thing4 == thing4   -- Output: True
    print $ thing1 < thing3   -- Output: True
    print $ thing2 < thing3   -- Output: True
    print $ thing3 > thing2   -- Output: True
    print $ thing2 < thing4   -- Output: True
