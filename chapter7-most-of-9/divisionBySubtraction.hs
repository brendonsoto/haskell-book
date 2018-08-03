module DivisionBySubtraction where

data DividedResult = Result (Integer, Integer) | DividedByZero

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = Result (fromIntegral count, fromIntegral n)
          | otherwise = go (n - d) d (count + 1)
