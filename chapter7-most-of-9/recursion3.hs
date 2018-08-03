module Recursion3 where


multBySum :: (Integral a) => a -> a -> a
multBySum 0 _ = 0
multBySum _ 0 = 0
multBySum 1 x = x
multBySum x 1 = x
multBySum x y
  | x > y = x + multBySum x (y - 1)
  | otherwise = y + multBySum (x - 1) y
