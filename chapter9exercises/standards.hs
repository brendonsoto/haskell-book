module Standards where

-- 1: myOr
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

-- 2: myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if f x == True
  then True
  else myAny f xs

-- 3: myElem
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) =
  if a == x
  then True
  else myElem a xs

-- 3': myElem with any
myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' a xs = any (== a) xs

-- 4: myReverse
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [a]    = [a]
myReverse (x:xs) = myReverse xs ++ [x]
-- XXX How can i do the above without concatenation?

-- 5: squish
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6: squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7: squishAgain
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap id xs

-- 8: myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy _ [] = []
myMaximumBy f [x] = x
myMaximumBy f (x:xs)
  | isGEQ = x
  | isLT = y
  where
    y = myMaximumBy f xs
    isLT = f x y == LT
    isGEQ = not isLT

-- 9: myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMinimumBy _ [] = []
myMinimumBy f [x] = x
myMinimumBy f (x:xs)
  | isLEQ = x
  | isGT = y
  where
    y = myMinimumBy f xs
    isGT = f x y == GT
    isLEQ = not isGT

-- 10: maximum & minimum
maximum' :: Ord a => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just $ myMaximumBy compare xs

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just $ myMinimumBy compare xs
