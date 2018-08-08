module Natural where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x


realToNat :: Integer -> Nat
realToNat x
  | x < 0 = error "Input is not a real number"
  | x == 0 = Zero
  | otherwise = Succ (realToNat (x - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (realToNat x)
