{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n, o) = tooMany (n + o)

instance TooMany (Double, Int) where
  tooMany (a, b) = tooMany b

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, o) = tooMany n && tooMany o

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
