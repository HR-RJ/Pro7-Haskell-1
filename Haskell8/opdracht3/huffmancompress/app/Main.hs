module Main where

import Data.List (group, sort, sortBy)
import qualified Data.Bifunctor

main :: IO ()

data Bintree a = Empty
    | Branch (Bintree a) Int (Bintree a)
    | Leaf Char
    deriving (Show, Eq, Ord, Read)

-- sorts a list of tuples containing each unique character and the amount of times it occurs
countChars :: String -> [(Char, Int)]
countChars = sortBy (\(_,a) (_,b) -> compare a b) . map (\x -> (head x, length x)) . group . sort -- . go brr

huffmanTree :: [(Char, Int)] -> Bintree a -> Bintree a
huffmanTree [] final = final
huffmanTree ((lowest, lowCount):(lessLowest, lessLowCount):tail) Empty = huffmanTree tail (Branch (Leaf lessLowest) (lowCount + lessLowCount) (Leaf lowest))
huffmanTree ((lowest, lowCount):tail) prevTree = huffmanTree tail (Branch (Leaf lowest) (lowCount + checkVal prevTree) prevTree)
-- huffmanTree [(lowest, lowCount)] final = final

checkVal :: Bintree a -> Int
checkVal Empty = 0
checkVal (Branch t1 v t2) = v

huffmanTable :: Bintree a -> [(Char, String)]
huffmanTable Empty = []
huffmanTable (Leaf c) = [(c, "")]
huffmanTable (Branch t1 _ t2) = map (Data.Bifunctor.second ('0' :)) (huffmanTable t1) ++ map (Data.Bifunctor.second ('1' :)) (huffmanTable t2)

huffmanCompress :: [(Char, String)] -> String -> String
huffmanCompress _ [] = []
huffmanCompress table (x:xs)  = snd (head (filter (\(a,_) -> a == x) table)) ++ huffmanCompress table xs



main = do
    putStrLn "Please enter the file to be compressed:"
    inputFile <- getLine
    rawContents <- readFile inputFile

    putStrLn "Please enter the output file to compress to:"
    outputFile <- getLine
    writeFile outputFile (show (huffmanCompress (huffmanTable (huffmanTree (countChars rawContents) Empty)) rawContents))
    putStrLn "Please enter the file to write the huffman tree to:"
    treeFile <- getLine
    writeFile treeFile (show  (huffmanTree (countChars rawContents) Empty))
    -- let rawContents = "Dit is een test met 1234321"
    -- print (huffmanTable (huffmanTree (countChars rawContents) Empty))
    -- let huffTree = huffmanTree rawContents
    -- writeFile treeFile (show (mapTree fromEnum huffTree))
    -- print (huffmanCompress (huffmanTable (huffmanTree (countChars rawContents) Empty)) rawContents)
    putStrLn "Finished compression"
