module Binary where


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- 1
unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  (Just (a, b, c)) -> Node (unfold f a) b (unfold f c)


-- 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where
  go :: Integer -> Maybe(Integer, Integer, Integer)
  go x =
    if x == n
    then Nothing
    else Just ((x + 1), x, (x + 1))
