module ReviewCurrying where


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- flippy :: (a -> b -> c) -> String -> String -> String
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
