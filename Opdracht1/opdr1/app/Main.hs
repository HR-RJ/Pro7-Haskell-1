module Main where

main :: IO ()

-- opdracht 1a
faca :: (Eq t, Num t) => t -> t
faca 0 = 1
faca x = x * faca(x-1)


-- opdracht 1b
facb :: (Eq t, Num t, Ord t) => t -> t
facb factorial
  | factorial <= 0 = 1
  | otherwise = factorial * facb(factorial-1)
main = print (faca 5,facb 5)
