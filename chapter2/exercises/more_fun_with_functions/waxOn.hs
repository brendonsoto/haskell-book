module WaxOn where

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple = (*3)

waxOff x = triple x + 5 ^ 3
