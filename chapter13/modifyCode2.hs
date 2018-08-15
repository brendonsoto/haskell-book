module ModifyCode2 where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)


lowerCaseStr :: String -> String
lowerCaseStr = map toLower

removeNonAlpha :: String -> String
removeNonAlpha = filter (flip elem ['a'..'z'])

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((removeNonAlpha . lowerCaseStr $ line1) == (removeNonAlpha . lowerCaseStr . reverse $ line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
