module Main where

main :: IO ()

-- opdracht 1a
faca :: Int -> Int
faca 0 = 1
faca x = x * faca(x-1)


-- opdracht 1b
facb :: Int -> Int
facb factorial
  | factorial <= 0 = 1
  | otherwise = factorial * facb(factorial-1)
main = print (faca 5,facb 5)
