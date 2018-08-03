data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- phew = Papu "chases" True

-- truth = Papu (Rocks "chomskydoz")
--              (Yeah True)

-- equalityForAll :: Papu -> Papu -> Bool
-- equalityForAll p p' = p == p'

comparePapus :: Papu -> papu -> Bool
comparePapus p p' = p > p'
