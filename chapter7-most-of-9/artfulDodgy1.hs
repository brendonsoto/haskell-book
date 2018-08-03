dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- dodgy 1 0 will equal 1
