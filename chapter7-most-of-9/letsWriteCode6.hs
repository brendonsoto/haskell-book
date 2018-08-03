module LetsWriteCode5 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip x = (read :: a) . show $ x
