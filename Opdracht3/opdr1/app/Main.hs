module Main where

main :: IO ()

differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = (f (x+p) - f x) / p

main = print(differentieer (\x -> x^2) 0.0001 2) -- ??
