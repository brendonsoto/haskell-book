module GetFifthLetter where

getFifthLetter :: String -> String
getFifthLetter x = take 1 (drop 4 x)
