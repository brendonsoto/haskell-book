module LanguageExercises where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs


capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph xs =
  capitalizeAfterPeriod (capitalizeWord xs)
  where
    capitalizeAfterPeriod :: String -> String
    capitalizeAfterPeriod "" = ""
    capitalizeAfterPeriod ys@(y:yt)
      | y == '.' && yt /= "" = y : head yt : capitalizeWord (tail yt)
      | otherwise = y : capitalizeAfterPeriod yt
