module ArithmeticTesting where

import Data.List (sort)
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

halfIdentity = (*(2 :: Float)) . half

prop_half :: Float -> Bool
prop_half x = halfIdentity x == x


-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


prop_listOrdered :: [Char] -> Bool
prop_listOrdered xs = listOrdered . sort $ xs 
-- prop_listOrdered = listOrdered -- NOTE This will produce failing results!


runQc :: IO ()
runQc = quickCheck prop_listOrdered
