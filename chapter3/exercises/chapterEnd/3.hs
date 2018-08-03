module GetThirdChar where

getThirdChar :: String -> Char
getThirdChar x = head (drop 2 x)
