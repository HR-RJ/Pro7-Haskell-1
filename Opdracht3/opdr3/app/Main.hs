-- import Mylib file

module Main where
import Data.List
main :: IO ()

-- Lijst van alle mogelijke combinaties van dobbelstenen
diceCombinations :: [[Double]]
diceCombinations = [[a,b,c,d,e] | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6], e <- [1..6]]

totalDiceComb :: Double
totalDiceComb = genericLength diceCombinations

calcLength :: Int -> Double
calcLength l = genericLength (filter (any (\y -> length y == l) . group . sort) diceCombinations)

-- Bereken de kans op het krijgen van een poker
pokerChance :: Double
pokerChance = genericLength (filter (\x -> length (nub x) == 1) diceCombinations) / totalDiceComb

-- Bereken de kans op het krijgen van four of a kind
fourOfAKindChance :: Double
fourOfAKindChance = calcLength 4 / totalDiceComb
-- Bereken de kans op het krijgen van three of a kind
threeOfAKindChance :: Double
threeOfAKindChance = (calcLength 3 / totalDiceComb) - fullHouseChance

-- Bereken de kans op het krijgen van een full house
fullHouseChance :: Double
fullHouseChance = genericLength (filter (\x -> any (\y -> length y == 3) (group (sort x)) && any (\y -> length y == 2) (group (sort x))) diceCombinations) / totalDiceComb

-- Bereken de kans op het krijgen van two pair
twoPairChance :: Double
twoPairChance = genericLength (filter (\x -> length (nub (filter (\y -> length y == 2) (group (sort x)))) == 2) diceCombinations) / totalDiceComb
onePairChance :: Double
onePairChance = (genericLength (filter (\x -> length (nub (filter (\y -> length y == 2) (group (sort x)))) == 1) diceCombinations) / totalDiceComb) - fullHouseChance
-- Bereken de kans op het krijgen van een straight
straightChance :: Double
straightChance = genericLength (filter (\x -> sort x `elem` [[1,2,3,4,5], [2,3,4,5,6]]) diceCombinations) / totalDiceComb

-- Bereken de kans op het krijgen van bust
bustChance :: Double
bustChance = ((genericLength (filter (\x -> length (nub x) == 5) diceCombinations)) / fromIntegral (genericLength diceCombinations)) -straightChance

main = do
    print (pokerChance)
    print (fourOfAKindChance)
    print (threeOfAKindChance) -- !
    print (fullHouseChance)
    print (twoPairChance)
    print (onePairChance) -- !
    print (straightChance)
    print (bustChance) -- !
