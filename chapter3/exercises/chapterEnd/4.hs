module LetterIndex where

letterIndex :: Int -> Char
letterIndex x = head (drop x string)
  where string = "Curry is awesome!"
