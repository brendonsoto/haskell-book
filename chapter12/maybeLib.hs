module MaybeLib where


-- 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing x = not . isJust $ x

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe x y = mayybee x id y

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs)
  | isJust x = getJust x : catMaybes xs
  | otherwise = catMaybes xs
  where
    getJust :: Maybe a -> a
    getJust (Just x) = x
    getJust Nothing = error "Nothing"

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise = Just list
  where
    list = concat . map maybeToList $ xs
