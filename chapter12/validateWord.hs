module ValidateWord where


newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

-- countVowels :: String -> Integer
-- countVowels "" = 0
-- countVowels (x:xs)
--     | x `elem` vowels = 1 + countVowels xs
--     | otherwise = countVowels xs

countLetters :: String -> (Char -> Bool) -> Integer
countLetters "" _ = 0
countLetters (x:xs) f
    | f x = 1 + countLetters xs f
    | otherwise = countLetters xs f

getVowelCount :: String -> Integer
getVowelCount xs = countLetters xs (flip elem vowels)

getConsonantCount :: String -> Integer
getConsonantCount xs = countLetters xs (flip notElem vowels)

mkWord :: String -> Maybe Word'
mkWord "" = Nothing
mkWord xs
  | vowels < consonants = Just (Word' xs)
  | otherwise = Nothing
  where
    vowels = getVowelCount xs
    consonants = getConsonantCount xs
