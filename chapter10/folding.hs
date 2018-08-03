-- This is a module to understand folds a bit more
module Folding where


listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr f z [] = z
listFoldr f z (x:xs) = f x (listFoldr f z xs)
-- foldr is recursive in the sense that the second parameter consists of the rest of the fold


listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl f z [] = z
listFoldl f z (x:xs) = listFoldl f (f z x) xs



-- listFoldr f z (x:xs) = f x (listFoldr f z xs)
-- listFoldl f z (x:xs) = listFoldl f (f z x) xs
