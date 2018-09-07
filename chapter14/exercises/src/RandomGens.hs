module RandomGens where

import Test.QuickCheck


-- 1
data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]


-- 2
genFool2 :: Gen Fool
genFool2 = frequency [ (6, return Fulse)
                     , (3, return Frue)]
