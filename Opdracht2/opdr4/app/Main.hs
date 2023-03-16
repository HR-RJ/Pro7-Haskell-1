module Main where

main :: IO ()

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x 
    | x < m = x^e `mod` m
    | otherwise = error "x is too big"

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x 
    | x < m = x^d `mod` m
    | otherwise = error "x is too big"

encryptChar::(Integer,Integer)->Char->Integer
encryptChar (e,m) x = rsaencrypt (e,m) (toInteger (fromEnum x))

decryptChar::(Integer,Integer)->Integer->Char
decryptChar (d,m) x = toEnum (fromInteger (rsadecrypt (d,m) x))

main = print(encryptChar (5,91) 'K', decryptChar(29,91) 17)
