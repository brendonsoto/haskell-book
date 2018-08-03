data StringOrInt =
    TisAnInt    Int
  | TisAString  String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAnInt _) (TisAString _) = False
  (==) (TisAString x) (TisAString x') = x == x'
  (==) (TisAString _) (TisAnInt _) = False
