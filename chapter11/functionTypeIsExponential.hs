module FunctionTypeIsExponential where

data Quantum =
      Yes
    | No
    | Both
    deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = True
convert No = True
convert Both = True

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = False
