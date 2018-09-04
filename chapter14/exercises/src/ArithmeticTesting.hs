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


-- runQc :: IO ()
-- runQc = quickCheck prop_listOrdered


-- 3
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
    x + y == y + x


runQc3 :: IO ()
runQc3 = do
    quickCheck plusAssociative
    quickCheck plusCommutative


-- 4
multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z =
    x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y =
    x * y == y * x


runQc4 :: IO ()
runQc4 = do
    quickCheck multAssociative
    quickCheck multCommutative


-- 5
-- TODO Check to see if there's a way to test numbers greater than 0
prop_quotRem :: Int -> Int -> Bool
prop_quotRem x y =
    (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> Int -> Bool
prop_divMod x y =
    (div x y) * y + (mod x y) == x


runQc5 :: IO ()
runQc5 = do
    quickCheck prop_quotRem
    quickCheck prop_divMod
