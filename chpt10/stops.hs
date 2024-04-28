module StopsStarts where

stops = "pbtdkg"
vowels = "aeiou"

combos = [(a,b,c) | a <- stops, b <- vowels, c <- stops]

startsWithP ((a,b,c):xs)
  | null xs   = []
  | a == 'p'  = [(a,b,c)] ++ startsWithP xs
  | otherwise = startsWithP xs
