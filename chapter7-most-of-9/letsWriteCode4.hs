module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

-- Since show takes an 'a' and returns a 'string' and read takes a 'string'
-- and returns an 'a', `roundTrip` simply converts an 'a' to a 'string' and
-- back to an 'a'.
-- This is why it seems equivalent to the id func.
