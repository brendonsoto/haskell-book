module Idempotence where

import Test.QuickCheck
import Data.Char
import Data.List


twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice


-- 1.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

prop_idem1 :: String -> Bool
prop_idem1 x =
    (capitalizeWord x
    == twice capitalizeWord x)
    &&
    (capitalizeWord x
    == fourTimes capitalizeWord x)


-- 2
prop_idem2 :: [Int] -> Bool
prop_idem2 x =
    (sort x
    == twice sort x)
    &&
    (sort x
    == fourTimes sort x)



runQc :: IO ()
runQc = do
    quickCheck prop_idem1
    quickCheck prop_idem2
