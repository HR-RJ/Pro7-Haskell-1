module Main where

import Data.Fixed
main :: IO ()

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen p q = (m, e, d)
    where
        m = p * q
        phiM = (p - 1) * (q - 1)
        e = head [e | e <-[2..], e <= phiM && gcd e phiM == 1]
        d = mod phiM 1 `div` e

-- main = print (keygen 7 13)
main = mapM_ print [5 `modInv` 72, 42 `modInv` 2017]