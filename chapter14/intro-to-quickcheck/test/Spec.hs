import Test.QuickCheck
import Data.List (intersperse)


prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs


split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs'  = takeWhile (/= c) xs
          xs'' = dropWhile (/= c) xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]

-- show
-- examples = [('@', "pbv@dcc.fc.up.pt"), ('/', "/usr/include")]
--
-- test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
--     where ys = split c xs

prop_split_inv xs
    = forAll (elements xs) $ \c ->
        unsplit c (split c xs) == xs


main :: IO ()
-- main = mapM_ (putStrLn.test) examples
main = quickCheck prop_split_inv
