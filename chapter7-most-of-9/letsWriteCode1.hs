tensDigit :: Integral a => a -> a
-- tensDigit x = d
--   where xLast = x `div` 10
--         d = xLast `mod` 10

-- part a)
tensDigit x = d
  where postTens = fst (divMod x 10)
        d = snd (divMod postTens 10)

-- part b) -- yes

-- part c)

-- straightforward way
hunsD x = d
  where postHuns = fst $ divMod x 100
        d = snd $ divMod postHuns 10

-- More modular way -- how can you do?
-- getDigitFunc x = d
--   where postDigit = fst . flip divMod x
--         d = snd (divMod postDigit x)

-- getDigitFunc x = snd (divMod (fst (flip divMod x)) x)
