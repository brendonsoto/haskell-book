module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen



-- From Ex3
breakOn :: (Eq a) => a -> [a] -> [[a]]
breakOn _ [] = []
breakOn x xs =
  takeWhile (/= x) xs
  : case dropWhile (/= x) xs of
      [] -> []
      (_:xs') -> breakOn x xs'


myLines :: String -> [String]
-- myLines [] = []
-- myLines xs = firstLine : myLines remainder
--   where firstLine = takeWhile (/= '\n') xs
--         remainder = dropWhile (== '\n') $ dropWhile (/= '\n') xs
-- myLines xs =
--   takeWhile (/= '\n') xs
--   : case dropWhile (/= '\n') xs of
--       [] -> []
--       (_:xs) -> myLines xs
myLines = breakOn '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)
