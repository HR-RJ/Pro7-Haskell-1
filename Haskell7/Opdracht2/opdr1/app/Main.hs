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

-- egcd :: Integer -> Integer -> (Integer,Integer,Integer)
-- egcd 0 b = (b, 0, 1)
-- egcd a b =
--     let (g, s, t) = egcd (mod b a) a
--     in (g, t - div b a * s, s)
    
eGCD :: Integer -> Integer -> Integer -> [Integer]
eGCD 0 b c = [b, 0, 1]
eGCD a b 0 = let [g, s, t] = eGCD (b `mod` a) a 0
       in [g, t - (b `div` a) * s, s]
eGCD a b 1 = do
         let [g, s, t] = eGCD (b `mod` a) a 0 in
          makePos[g, t - (b `div` a) * s, s] a b

makePos :: [Integer] -> Integer -> Integer -> [Integer]
makePos [a, b, c] e f
      | b < 0 = makePos [a, b + f, c] e f
      | c < 0 = makePos [a, b, c + e] e f
      | otherwise = [a,b,c]    

-- eGCD :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
-- eGCD 0 b c = (b, 0, 1)
-- eGCD a b 0 = let (g, s, t) = eGCD (b `mod` a) a 0
--        in (g, t - (b `div` a) * s, s)
-- eGCD a b 1 = let (g, s, t) = eGCD (b `mod` a) a 0 in
--       if s < 0 then 
--         (g, t - (b `div` a) * s, s + a)
--       else if t < 0 then
--         (g, (t - (b `div` a) * s) + b, s)
--       else 
--         (g, t - (b `div` a) * s, s)

-- egcd a b
--     | s >= 0 = (g, t - div b a * s, s)
--     | otherwise = s + mod b a
--       where (g, s, t) = egcd (mod b a) a

-- egcd :: Integer -> Integer -> (Integer, Integer, Integer)
-- egcd 0 b = (b, 0, 1)
-- egcd a b =
--     let (g, s, t) = egcd (b `mod` a) a
--         k = (s * a + t * b) `div` g
--     in (g, (t - (b `div` g) * s) `mod` (b `div` g), (s + (a `div` g) * k) `mod` (b `div` g))

main = print ({-euclid 1024 4096-} eGCD 1235 5238 1)
