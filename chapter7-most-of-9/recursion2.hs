module Recursion2 where


sumNums :: (Eq a, Num a) => a -> a
sumNums 1 = 1
sumNums x = x + sumNums (x - 1)
