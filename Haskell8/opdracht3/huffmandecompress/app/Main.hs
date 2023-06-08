module Main where
import qualified Data.Bifunctor
import Data.List

main :: IO ()

data Bintree a = Empty
    | Branch (Bintree a) Int (Bintree a)
    | Leaf Char
    deriving (Show, Eq, Ord, Read)

checkVal :: Bintree a -> Int
checkVal Empty = 0
checkVal (Branch t1 v t2) = v

huffmanTree :: [(Char, Int)] -> Bintree a -> Bintree a
huffmanTree [] final = final
huffmanTree ((lowest, lowCount):(lessLowest, lessLowCount):tail) Empty = huffmanTree tail (Branch (Leaf lessLowest) (lowCount + lessLowCount) (Leaf lowest))
huffmanTree ((lowest, lowCount):tail) prevTree = huffmanTree tail (Branch (Leaf lowest) (lowCount + checkVal prevTree) prevTree)

importTree :: String -> Bintree a
importTree treeString = read treeString :: Bintree a


decompress :: String -> Bintree a -> String
decompress [] _ = []
decompress xs tree = decompressHelper xs tree
  where
    decompressHelper :: String -> Bintree a -> String
    decompressHelper [] (Leaf c) = [c]
    decompressHelper [] _ = []
    decompressHelper xs (Leaf c) = c : decompress xs tree
    decompressHelper (x:xs) (Branch l _ r)
      | x == '0' = decompressHelper xs l
      | x == '1' = decompressHelper xs r
    decompressHelper _ Empty = error "Invalid Huffman code"


main = do
    putStrLn "Please enter the file to be decompressed:"
    inputFile <- getLine

    putStrLn "Please enter the file to write the decompressed file to:"
    outputFile <- getLine

    putStrLn "Please enter the file to read the huffman tree from:"
    treeFile <- getLine

    inputFile <- readFile inputFile
    treeFile <- readFile treeFile

    let tree = importTree treeFile
    -- print tree
    -- print inputFile

    let d = decompress inputFile tree
    -- print d
    writeFile outputFile d
    putStrLn "Done decompressing"
