module ArithmeticTesting where

import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

halfIdentity = (*(2 :: Float)) . half

prop_half :: Float -> Bool
prop_half x = halfIdentity x == x



runQc :: IO ()
runQc = quickCheck prop_half
