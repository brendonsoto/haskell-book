module StringProcessing where


notThe :: String -> Maybe String
notThe "" = Just ""
notThe xs
  | xs == "the" = Nothing
  | otherwise   = Just xs

getJust :: Maybe String -> String
getJust Nothing = ""
getJust (Just x) = x

-- First Solution
-- replaceThe :: String -> String
-- replaceThe "" = ""
-- replaceThe xs
--   | not isEndOfStr && isThe =  "a " ++ (replaceThe . unwords $ endOfStr)
--   | isEndOfStr && isThe = "a"
--   | not isEndOfStr && not isThe = word ++ " " ++ (replaceThe . unwords $ endOfStr)
--   | otherwise = word
--   where
--     strSplit = words xs
--     word = head strSplit
--     endOfStr = tail $ strSplit
--     isEndOfStr = 0 == length endOfStr
--     validatedWord = getJust . notThe $ word
--     isThe = validatedWord == ""


-- Non-recursive
-- replaceThe :: String -> String
-- replaceThe xs =
--   unwords . map (\x -> if x == "the" then "a" else x) . words $ xs

-- Trying recursive again
-- This solution is shorter, but does not make use of Maybe and has a space at the end
-- replaceThe :: String -> String
-- replaceThe "" = ""
-- replaceThe xs
--   | word == "the" = unwords ["a", replaceThe . unwords $ rest]
--   | otherwise = unwords [word, replaceThe . unwords $ rest]
--   where
--     word = head . words $ xs
--     rest = tail . words $ xs

-- This solution uses the notThe and getJust bits!!
replaceThe :: String -> String
replaceThe "" = ""
replaceThe xs =
  (if word == "" then "a" else word) ++ endOfStr
  where
    word = getJust . notThe . head . words $ xs
    transformedRest = replaceThe . unwords . tail . words $ xs
    endOfStr = if transformedRest == "" then "" else " " ++ transformedRest


-- 2
-- Below is the original solution i came up with, but no maybes/just
isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"
-- isVowel x
--   | x == 'a'  = True
--   | x == 'e'  = True
--   | x == 'i'  = True
--   | x == 'o'  = True
--   | x == 'u'  = True
--   | otherwise = False
-- countTheBeforeVowel' :: [String] -> Integer
-- countTheBeforeVowel' [] = 0
-- countTheBeforeVowel' (_:[]) = 0
-- countTheBeforeVowel' (_:[]:_) = 0
-- countTheBeforeVowel' (a:bAll@(b:_):c)
--   | a == "the" && isVowel b = 1 + countTheBeforeVowel' c
--   | otherwise = 0 + countTheBeforeVowel' (bAll:c)
--
-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel "" = 0
-- countTheBeforeVowel xs =
--   countTheBeforeVowel' . words $ xs

-- 2-V2
-- Did it! Probably not the most efficient, but it's here
data Invalid = ConsonantAfterThe |
               NotThe |
               NoWordAfter |
               NoWords
               deriving (Eq, Show)
type ValidateWordSeq a =
    Either Invalid a

vowelAfterThe :: [String] -> ValidateWordSeq Int
vowelAfterThe [] = Left NoWords
vowelAfterThe [[]] = Left NoWords
vowelAfterThe [(_:[])] = Left NoWordAfter
vowelAfterThe [(_:_:_)] = Left NotThe
vowelAfterThe (x:y:_)
    | x == "the" && (isVowel . head $ y) = Right 1
    | otherwise = Left ConsonantAfterThe

countTheBeforeVowel :: String -> Int
countTheBeforeVowel "" = 0
countTheBeforeVowel xs =
    let
      pair = take 2 . words $ xs
      rest = unwords . tail . words $ xs
      go :: ValidateWordSeq Int -> Int
      go (Right _) = 1
      go (Left _) = 0
    in
      (go . vowelAfterThe $ pair) + (countTheBeforeVowel rest)

-- Same prob, diff approach
-- getThe :: String -> Maybe String
-- getThe x
--   | x == "the" = Just x
--   | otherwise = Nothing
--
-- getVowelAfterThe :: String -> Maybe String
-- getVowelAfterThe xs
--   | isVowel . head $ xs = Just xs
--   | otherwise = Nothing
--
-- countTheBeforeVowel' :: String -> String
-- countTheBeforeVowel' "" = 0
-- countTheBeforeVowel' xs =
--   go result
--   result = getVowelAfterThe . tail . (!! 2) . words $ xs
--   go :: Maybe String -> Int
--   go Nothing = 0
--   go Just _ = 1

-- 3

getVowels :: String -> String
getVowels = filter isVowel

countVowels :: String -> Integer
countVowels = toInteger . length . getVowels
