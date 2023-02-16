module Main where

main :: IO ()

mult::Integer->Integer->Integer
mult x y = x + mult x (y-1)

main = print (mult 4 5)
