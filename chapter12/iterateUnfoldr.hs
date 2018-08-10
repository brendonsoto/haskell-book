module IterateUndUnfoldr where


-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


-- 2
myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f x = case f x of
  (Just (a, _)) -> a : []
  Nothing -> []

-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x =
    myUnfoldr (\b -> Just (b, f b)) x ++
    betterIterate f (f x)
