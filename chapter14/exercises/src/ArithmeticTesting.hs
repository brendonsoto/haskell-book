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
-- NOTE: NonZero I found on github here: https://github.com/CarlosMChica/HaskellBook/blob/a0cd94b94e5065c6cb36c54b1c9bb6a73f551ec1/chapter14/arithmetic/test/Spec.hs
-- QuickCheck doc: https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html#t:NonZero
prop_quotRem :: NonZero Int -> NonZero Int -> Bool
prop_quotRem (NonZero x) (NonZero y) =
    (quot x y) * y + (rem x y) == x

prop_divMod :: NonZero Int -> NonZero Int -> Bool
prop_divMod (NonZero x) (NonZero y) =
    (div x y) * y + (mod x y) == x


runQc5 :: IO ()
runQc5 = do
    quickCheck prop_quotRem
    quickCheck prop_divMod


-- 6
prop_expAssoc :: Int -> Int -> Int -> Bool
prop_expAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_expCommutative :: Int -> Int -> Bool
prop_expCommutative x y = x ^ y == y ^ x

runQc6 :: IO ()
runQc6 = do
    quickCheck prop_expAssoc
    quickCheck prop_expCommutative


-- 7
prop_doubleReversed :: Eq a => [a] -> Bool
prop_doubleReversed xs = (reverse . reverse $ xs) == id xs

prop_doubleReversed_int :: [Int] -> Bool
prop_doubleReversed_int = prop_doubleReversed

prop_doubleReversed_char :: [Char] -> Bool
prop_doubleReversed_char = prop_doubleReversed

runQc7 :: IO ()
runQc7 = do
    quickCheck prop_doubleReversed_int
    quickCheck prop_doubleReversed_char


-- 8
prop_application :: Eq a => (a -> a) -> a -> Bool
prop_application f a = (f $ a) == f a

prop_composition :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
prop_composition f g x = (f . g $ x) == f (g x)

prop_app_int :: Int -> Bool
prop_app_int = prop_application (+1)

prop_comp_int :: Int -> Bool
prop_comp_int = prop_composition (*2) (+3)

runQc8 :: IO ()
runQc8 = do
    quickCheck prop_app_int
    quickCheck prop_comp_int


-- 9
prop_foldr_concat :: [Char] -> [Char] -> Bool
prop_foldr_concat x y = foldr (:) x y == x ++ y

prop_foldr_empty_concat :: [[Char]] -> Bool
prop_foldr_empty_concat xs = foldr (++) [] xs == concat xs

runQc9 :: IO ()
runQc9 = do
    quickCheck prop_foldr_concat
    quickCheck prop_foldr_empty_concat


-- 10
prop_len :: Int -> [Int] -> Bool
prop_len n xs = length (take n xs) == n

runQc10 :: IO ()
runQc10 = quickCheck prop_len


-- 11
prop_round_trip :: Int -> Bool
prop_round_trip x = (read (show x)) == x

runQc11 :: IO ()
runQc11 = quickCheck prop_round_trip


-- Failure
square :: (Num a) => a -> a
square x = x * x

prop_square :: NonZero Float -> Bool
prop_square (NonZero x) = square x / x == x


runQcFail1 :: IO()
runQcFail1 = quickCheck prop_square

-- square and square identity does not hold because of the imprecision of floating numbers
-- hardware issues representing floats
