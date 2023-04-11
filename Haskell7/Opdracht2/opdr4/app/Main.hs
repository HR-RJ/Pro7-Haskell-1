module Main where

main :: IO ()

rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x 
    | x < m = x^e `mod` m

rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (d,m) x 
    | x < m = x^d `mod` m

encryptChar::(Integer,Integer)->Char->Integer
encryptChar (e,m) x = rsaencrypt (e,m) (toInteger (fromEnum x))

decryptChar::(Integer,Integer)->Integer->Char
decryptChar (d,m) x = toEnum (fromInteger (rsadecrypt (d,m) x))

main = print(encryptChar (3,10807) 'K', decryptChar(7067,10807) 402)
