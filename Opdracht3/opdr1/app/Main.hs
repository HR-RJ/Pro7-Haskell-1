module Main where

main :: IO ()

differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = (f (x+p) - f x) / p

integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p = sum [f x * p | x <- [a, a+p..b]]

main = do
    print(differentieer (\x -> x^3 + 5*x) 0.00001 2.12) -- ??
    print(integreer (\x -> x^3 + 5*x) 0.0 2.12 0.00001) -- ??
