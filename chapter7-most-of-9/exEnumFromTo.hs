module ExEnumFromTo where


eftBool :: Bool -> Bool -> [Bool]
eftBool True True   = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd GT GT = [GT]

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x <= y = x:eftInt (x + 1) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x <= y = x:eftChar (succ x) y
