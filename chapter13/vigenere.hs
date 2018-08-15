module Vigenere where

import Data.Char

charToInt :: Char -> Int
charToInt x = ord (toUpper x) - ord 'A'

getMovedChar :: Char -> Char -> Char
getMovedChar ' ' _ = ' '
getMovedChar x y =
  chr $ newCharIndex
  where
    newCharIndex = ((charToInt x + charToInt y) `mod` 26) + ord 'A'

getSubstituteString :: String -> String -> String
getSubstituteString "" _ = ""
getSubstituteString xs "" = xs
getSubstituteString (x:xt) (y:yt)
  | x /= ' ' = y : getSubstituteString xt yt
  | otherwise = ' ' : getSubstituteString xt (y:yt)

cipher :: String -> String -> String
cipher "" _ = ""
cipher xs "" = xs
cipher (x:xs) (y:ys) =
  getMovedChar x y : cipher xs ys

vCipher :: String -> String -> String
vCipher "" _ = ""
vCipher xs "" = xs
vCipher xs ys = cipher xs substitute
  where
    substitute = getSubstituteString xs (cycle ys)


main :: IO ()
main = do
  putStr "Enter a string to cipher: "
  toCipher <- getLine
  putStr "Enter a string to cipher by: "
  cipherBy <- getLine
  putStrLn $ "Your ciphered string is: " ++ vCipher toCipher cipherBy

