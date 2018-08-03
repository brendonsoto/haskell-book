foldBool :: a -> a -> Bool -> a
-- foldBool x y b
--   | b == False = x
--   | otherwise = y

foldBool x y b =
  case b of
    False -> x
    True -> y
