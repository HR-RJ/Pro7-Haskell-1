module Main where

main :: IO ()

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x = x^e `mod` m


rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x = x^d `mod` m



main = print(rsaencrypt (3,10807) 75, rsadecrypt(7067,10807) 402)
