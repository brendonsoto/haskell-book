module ExThyFearfulSymmetry3 where

breakOn :: (Eq a) => a -> [a] -> [[a]]
breakOn _ [] = []
breakOn x xs =
  takeWhile (/= x) xs
  : case dropWhile (/= x) xs of
      [] -> []
      (_:xs') -> breakOn x xs'
