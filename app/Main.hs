module Main where

-- opdracht 1a
-- main :: IO ()
-- fact :: (Eq t, Num t) => t -> t
-- fact 0 = 1
-- fact x = x * fact(x-1)
-- main = print (fact 4)

-- opdracht 1b
main :: IO ()
fact :: (Eq t, Num t, Ord t) => t -> t
fact factorial
  | factorial <= 0 = 1
  | otherwise = factorial * fact(factorial-1)
main = print (fact 5)
