module AsPatters where

import Data.Char

-- Ex1
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xt) ys@(y:yt)
  | x == y = isSubseqOf xt ys
  | otherwise = isSubseqOf xs yt

-- Ex2

tuplizeWord :: String -> (String, String)
tuplizeWord "" = ("", "")
tuplizeWord xs@(x:xt) = (xs, toUpper x : xt)

capitalizeWords :: String
                -> [(String, String)]
capitalizeWords "" = [("", "")]
capitalizeWords xs = map tuplizeWord $ words xs
