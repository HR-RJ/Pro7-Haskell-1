module Main where
import Data.Bits ( Bits(shift) )
main :: IO ()
functiepow::Integer->Integer->Integer
functiepow x y
  | y <= 0 = 1
  | otherwise = x * functiepow x (y-1)
--it didnt like 2 to the power of a max 32-bit int :) functiepow 2 2147483647
-- opdr5: Stack space overflow: current size 33624 bytes.
-- opdr5: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it.

fastmult::Integer->Integer->Integer
fastmult x y
  | y <= 0 = 0
  | even y = fastmult (shift x 1) (shift y (-1))
  | otherwise = x + fastmult (shift x 1) (shift y (-1))

fastpow::Integer->Integer->Integer
fastpow x y
  | y <= 0 = 1
  | even y = fastpow (fastmult x x) (shift y (-1))
  | otherwise = x * fastpow (fastmult x x) (shift y (-1))

main = print(fastpow 2 1000000)

-- after like half an hour of processing and over 95GB of Memory we decided to end the computer's suffering/program instead of finding the stackoverflow