-- Caesar cipher
module Cipher where

import Data.Char
import Test.QuickCheck

caesarCipher :: Int -> String -> String
caesarCipher _ [] = []
caesarCipher 0 xs = xs
caesarCipher n (x:xs) = x' : caesarCipher n xs
  where
    xOrd = ord x
    aOrd = ord 'a'
    x'Ord = ((xOrd - aOrd + n) `mod` 25) + aOrd
    x' = chr x'Ord


deCaesarCipher :: Int -> String -> String
deCaesarCipher _ [] = []
deCaesarCipher 0 xs = xs
deCaesarCipher n (x:xs) = x' : deCaesarCipher n xs
  where
    xOrd = ord x
    aOrd = ord 'a'
    x'Ord = ((xOrd - aOrd- n) `mod` 25) + aOrd
    x' = chr x'Ord


main :: IO ()
main = do
  putStr "Enter a number to cipher by: "
  num <- getLine
  putStr "Enter a string to cipher: "
  str <- getLine
  putStrLn $ "Your new string is: " ++ caesarCipher (digitToInt . head $ num) str


-- Testing
prop_cipher_int :: Int -> Bool
prop_cipher_int n = "sphaghetti" == (deCaesarCipher n . caesarCipher n $ "sphaghetti")

prop_cipher_string :: String -> Bool
prop_cipher_string x = x == (deCaesarCipher 12 . caesarCipher 12 $ x)

test :: IO ()
test = do
    quickCheck prop_cipher_int
    quickCheck prop_cipher_string
