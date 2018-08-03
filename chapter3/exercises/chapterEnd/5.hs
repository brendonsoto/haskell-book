module Rvrs where

-- Note: Only to be used for the string "Curry is awesome"
-- Returns "awesome is Curry"
rvrs :: String -> String
rvrs x = (drop 8 x) ++ (take 4 (drop 5 x)) ++ (take 5 x)


main :: IO ()
main = print $ rvrs "Curry is awesome" 
