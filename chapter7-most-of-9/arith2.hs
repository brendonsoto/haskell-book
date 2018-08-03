module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int) -- 0 - Should just print 0
  print (add 1 0) -- 1 - simple function call
  print (addOne 0) -- 1 - uses the lambda, simple substitution
  print (addOnePF 0) -- 1 - applies the rest of the function
  print ((addOne . addOne) 0) -- 2 - function composition \x -> \y...
  
  print ((addOnePF . addOne) 0) -- 2 - addOne first, then PF
  print ((addOne . addOnePF) 0) -- 2 - PF first, then lambda
  print ((addOnePF . addOnePF) 0) -- 2 - PF applied twice
  print (negate (addOne 0)) -- -1 - adds one first, then negates
  print ((negate . addOne) 0) -- -1 - composes, then applies
  print ((addOne . addOne . addOne . negate . addOne) 0) -- 2 => -1 * (0 + 1) + 1 + 1 + 1
