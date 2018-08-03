data Trivial = Trivial'

-- This reads "Give an instance of the `Eq` typeclass for the Trivial datatype"
instance Eq Trivial where
  Trivial' == Trivial' = True
