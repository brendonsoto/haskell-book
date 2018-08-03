module ExThyFearfulSymmetry1 where


myWords :: [Char] -> [[Char]]
myWords [] = []
myWords (x:[]) = [[x]]
myWords xs = takeWhile (\x -> x /= ' ') xs : myWords nextWords
  where firstWord = takeWhile isNotSpace xs
        nextWords = dropWhile isSpace $ dropWhile isNotSpace xs
        isSpace = (\x -> x == ' ')
        isNotSpace = (\x -> x /= ' ')
