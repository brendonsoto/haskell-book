module Seekrit where


seekrit :: String -> Double
seekrit x = fromIntegral (sum (map length (words x))) /
            fromIntegral (length (words x))
