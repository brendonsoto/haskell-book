-- Why is there an 'a'? Can you create two different datatypes simultaneously?
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    x == x'
    && y == y'
