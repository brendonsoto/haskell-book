module Fibs where


fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs' = take 20 $ fibs
fibs'' = takeWhile (<100) fibs


factorial = 1 : scanl (*) 2 factorial
takeFactorial n = take n factorial
