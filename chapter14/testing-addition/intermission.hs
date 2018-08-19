module Intermission where

import Test.Hspec

mult :: (Eq a, Num a) => a -> a -> a
mult x y =
  if y == 1
  then x
  else x + mult x (y - 1)


main :: IO ()
main = hspec $ do
  describe "mult" $ do
    it "9x9 = 81" $ do
      mult 9 9 `shouldBe` 81
