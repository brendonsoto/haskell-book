module WarmUp1 where


stops  = "pbtdkg"
vowels = "aeiou"

-- a
stopVowelStops = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- b
pSVS = filter (\(x, _, _) -> x == 'p') stopVowelStops

-- c
nouns = ["dog", "cat", "camel", "fox", "panda", "dolphin"]
verbs = ["ran", "ate", "walked", "strolled", "galloped", "skipped"]
nVn = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]
