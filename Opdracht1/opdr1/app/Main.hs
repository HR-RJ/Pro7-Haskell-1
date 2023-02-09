module Main where

main :: IO ()

-- opdracht 1a
facb :: (Eq t, Num t) => t -> t
facb 0 = 1
facb x = x * facb(x-1)


-- opdracht 1b
faca :: (Eq t, Num t, Ord t) => t -> t
faca factorial
  | factorial <= 0 = 1
  | otherwise = factorial * facb(factorial-1)
main = print (faca 5,facb 5)
