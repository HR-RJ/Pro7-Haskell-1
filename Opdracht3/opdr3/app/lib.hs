module Lib where
import Data.List
-- Lijst van alle mogelijke combinaties van dobbelstenen
diceCombinations :: [[Double]]
diceCombinations = [[a,b,c,d,e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]

-- Aantal combinaties
totalDiceComb :: Double
totalDiceComb = genericLength diceCombinations


calcLength :: Int -> Double
calcLength l = genericLength (filter (any (\y -> length y == l) . group . sort) diceCombinations)

pairCount :: Int -> Double
pairCount c = genericLength (filter (\x -> length (filter (\y -> length y == 2) (group (sort x))) == c) diceCombinations)
