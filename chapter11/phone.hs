module Phone where

import Data.Char
import Data.List

data Key = Key Char [Char] deriving (Eq, Show)
type DaPhone = [Key]

phoneKeyboard :: DaPhone
phoneKeyboard = [
    Key '1' "1",
    Key '2' "abc2",
    Key '3' "def3",
    Key '4' "ghi4",
    Key '5' "jkl5",
    Key '6' "mno6",
    Key '7' "pqrs7",
    Key '8' "tuv8",
    Key '9' "wxyz9",
    Key '*' "^",
    Key '0' " +_0",
    Key '#' ".,"
  ]

getKey :: DaPhone -> Char -> Key
getKey [] _ = Key ' ' ""
getKey (k@(Key label _):ks) c
  | label == c = k
  | otherwise  = getKey ks c

getKeyFromChars :: DaPhone -> Char -> Key
getKeyFromChars [] _ = Key ' ' ""
getKeyFromChars ((Key _ []):_) _ = Key ' ' ""
getKeyFromChars (k@(Key _ xs):ks) c
  | c `elem` xs = k
  | otherwise = getKeyFromChars ks c

getKeyLabel :: Key -> Char
getKeyLabel (Key label _) = label

getKeyChars :: Key -> [Char]
getKeyChars (Key _ xs) = xs

getPresses :: Key -> Char -> Int
getPresses (Key _ []) _ = 0
getPresses (Key label (x:xs)) c
  | x == c = 1
  | otherwise = 1 + getPresses (Key label xs) c

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone c
  | isUpper c = ('*', 1) : reverseTaps phone (toLower c)
  | otherwise = [(keyChar, numPresses)]
  where
    selectedKey = getKeyFromChars phone c
    keyChar = getKeyLabel selectedKey
    numPresses = getPresses selectedKey c
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]


convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

convertStr :: String -> [[(Digit, Presses)]]
convertStr [] = []
convertStr (x:xs) = reverseTaps phoneKeyboard x : convertStr xs

convoConverted :: [String] -> [[[(Digit, Presses)]]]
convoConverted [] = []
convoConverted (x:xs) =
  convertStr x : convoConverted xs

convertedConvo = convoConverted convo

-- 3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((_, p):xs) = p + fingerTaps xs

-- 4
tapsToChar :: DaPhone -> [(Digit, Presses)] -> Char
tapsToChar _ [] = ' '
tapsToChar phone ((d, p):xs)
  | d == '*'  = toUpper $ tapsToChar phone xs 
  | otherwise = char
  where
    selectedKey = getKey phone d
    keyChars = getKeyChars selectedKey
    char = keyChars !! (p - 1)

mostLetters :: String -> String -> String
mostLetters [] [] = []
mostLetters [] ys = ys
mostLetters xs [] = xs
mostLetters xs ys =
  if length xs > length ys then xs else ys

mostPopularLetter :: String -> Char
mostPopularLetter [] = ' '
mostPopularLetter xs =
  head . foldr1 mostLetters . group . sort . filter (/= ' ') . map (tapsToChar phoneKeyboard) . convertStr $ xs

-- 5
coolestLtr :: [String] -> Char
coolestLtr [] = '_'
coolestLtr xs = mostPopularLetter . concat $ xs

coolestWord :: [String] -> String
coolestWord [] = []
coolestWord xs = head . foldr1 (\x y -> if length x > length y then x else y) . group . sort . words . concat $ xs
