module RewritingFunctions where


-- 1
-- Not using ||
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = if x == True then True else myOr xs

-- Using ||
myOr' :: [Bool] -> Bool
myOr' []     = False
myOr' (x:xs) = x || myOr' xs

-- Using fold, not point-free
myOr'' :: [Bool] -> Bool
myOr'' = foldr (\x y -> if x == True then True else y) False

-- Using fold, point-free
myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False


--2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f [] = False
myAny' f (x:xs) = f x || myAny' f xs

myAny'' :: (a -> Bool) -> [a] -> Bool 
myAny'' f = foldr (\x y -> if f x == True then True else y) False

myAny''' :: (a -> Bool) -> [a] -> Bool 
myAny''' f = foldr ((||) . f) False


--3
myElem :: Eq a => a -> [a] -> Bool
-- myElem needle haystack = foldr ((||) . (== needle)) False haystack
myElem elem = foldr ((||) . (== elem)) False 

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem = any (== elem)


--4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = reverse' list []
  where
    reverse' [] reversed = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)


--5
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


--6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x == True = x : myFilter f xs
  | otherwise   = myFilter f xs


--7
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


--8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs


--9
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain list = squishMap id list


--10
myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f [x] = x
myMaximumBy f (x:xs)
  | f x y == GT = x
  | otherwise   = y
  where y = myMaximumBy f xs


--11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs)
  | f x y == LT = x
  | otherwise   = y
  where y = myMinimumBy f xs
