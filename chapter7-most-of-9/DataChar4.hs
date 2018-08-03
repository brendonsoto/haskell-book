module DataChar4 where

import Data.Char


capWord :: [Char] -> [Char]
capWord [] = []
capWord (x:xs) = toUpper x : capWord xs
