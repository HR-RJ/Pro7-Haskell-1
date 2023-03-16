module Main where

main :: IO ()

keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen p q = (m, e, d)
    where
        m = p * q
        phiM = (p - 1) * (q - 1)
        e = head [e | e <-[2..], e <= phiM && gcd e phiM == 1]
        d = head [d | d <-[2..], d <= phiM && gcd d phiM == 1, e * d `mod` phiM == 1]

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x 
    | x < m = x^e `mod` m
    | otherwise = error "x is too big"

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x 
    | x < m = x^d `mod` m
    | otherwise = error "x is too big"



main = print(rsaencrypt (5,91) 75, rsadecrypt(29,91) 17)
