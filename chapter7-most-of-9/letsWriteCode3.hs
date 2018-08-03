g :: (a -> b) -> (a, c) -> (b, c)
g aToB (x, y) = (aToB x, y)
