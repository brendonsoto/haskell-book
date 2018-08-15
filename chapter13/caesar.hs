-- Caesar cipher
module Cipher where

import Data.Char

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
