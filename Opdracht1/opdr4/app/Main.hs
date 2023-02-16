module Main where

import Data.Bits
main :: IO ()

mult::Integer->Integer->Integer
mult x y
  | y <= 0 = 0
  | otherwise = x + mult x (y-1)

-- could compute mult 9999 999999999
-- overflow at   mult 9999 9999999999
-- opdr4: Stack space overflow: current size 33624 bytes.
-- opdr4: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it


fastmult::Integer->Integer->Integer
fastmult x y
  | y <= 0 = 0
  | even y = fastmult (shift x 1) (shift y (-1))
  | otherwise = x + fastmult (shift x 1) (shift y (-1))

-- fastmult doet dit zonder moeite
main = print (fastmult 9999 9999999999)


-- 2147483647