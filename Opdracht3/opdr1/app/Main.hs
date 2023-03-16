module Main where

main :: IO ()

differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = (f (x+p) - f x) / p

integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p = sum [f x * p | x <- [a, a+p..b]]

main = print(differentieer (\x -> x^2) 0.0001 2) -- ??
