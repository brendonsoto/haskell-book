module EitherLib where

-- foldr (:) [] [0..10] -- = [0,1,2,3..10]

isLeft :: Either a b -> a
isLeft (Left a) = a
isLeft (Right _) = error "Right"

-- filtering with foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = foldr (\x acc-> if f x then x:acc else acc) [] xs

lefts' :: [Either a b] -> [a]
-- Non foldr solution
-- lefts' [] = []
-- lefts' ((Left x):xs) = x : lefts' xs
-- lefts' ((Right _):xs) = lefts' xs

-- my foldr solution
-- lefts' = foldr (\x acc -> if go x then getLeft x: acc else acc)  []
--   where
--     go :: Either a b -> Bool
--     go (Left _) = True
--     go _ = False
--     getLeft :: Either a b -> a
--     getLeft (Left a) = a
--     getLeft _ = error "Not left"

-- Awesome solution I found from Github:
-- kudos: https://github.com/smwhit/haskell-book/blob/c61e44e60f42cf1adc2ca6620b7b67bbf0e7c867/ch12/Ch12_either_exercises.hs
lefts' = foldr
    (\a b -> case a of 
        Left x -> x : b
        Right _ -> b)
    []


-- foldr (+) 0 [0..10] -- = 55
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- 2
rights' :: [Either a b] -> [b]
rights' = foldr
          (\a acc -> case a of
              Right x -> x : acc
              Left _ -> acc)
          []

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)


-- 4
eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x


-- 5
either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x


-- 6
eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' _ (Left a) = Nothing
eitherMaybe'' f (Right b) = Just $ either' id f (Right b)
