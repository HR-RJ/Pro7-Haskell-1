module Main where

import Data.Fixed
main :: IO ()

{-
this function checks if x is equal to y if it is it returns x
else it "tries to fit" y in x
if that doesnt work it flips it around and tries to fit x in y
it continues to do this until x == y
then it returns x
-}
--1a
euclid::Integer->Integer->Integer
euclid x y
  | x == y = x
  | x > y = euclid (x-y) y
  | otherwise = euclid x (y-x)

--1b
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (mod b a) a
    in (g, t - div b a * s, s)

egcd a b
    | s >= 0 = (g, t - div b a * s, s)
    | otherwise = s + mod b a
      where (g, s, t) = egcd (mod b a) a


main = print ({-euclid 1024 4096-} egcd 10 5)
