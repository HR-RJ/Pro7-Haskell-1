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

etree = importTree "Branch (Leaf ' ') 27 (Branch (Leaf 't') 22 (Branch (Leaf 'e') 18 (Branch (Leaf 's') 14 (Branch (Leaf 'i') 12 (Branch (Leaf '3') 10 (Branch (Leaf '2') 8 (Branch (Leaf '1') 6 (Branch (Leaf 'n') 4 (Branch (Leaf 'm') 3 (Branch (Leaf 'D') 2 (Leaf '4')))))))))))"
estring = "111111111101111010011110111001101101111111100101101110100111111111011010011111110111111011111011111111111111110111111011111110"
eresult = "1234321"

huffmanTree :: [(Char, Int)] -> Bintree a -> Bintree a
huffmanTree [] final = final
huffmanTree ((lowest, lowCount):(lessLowest, lessLowCount):tail) Empty = huffmanTree tail (Branch (Leaf lessLowest) (lowCount + lessLowCount) (Leaf lowest))
huffmanTree ((lowest, lowCount):tail) prevTree = huffmanTree tail (Branch (Leaf lowest) (lowCount + checkVal prevTree) prevTree)

-- huffmanTable :: Bintree a -> [(Char, String)]
-- huffmanTable Empty = []
-- huffmanTable (Leaf c) = [(c, "")]
-- huffmanTable (Branch t1 _ t2) = map (Data.Bifunctor.second ('0' :)) (huffmanTable t1) ++ map (Data.Bifunctor.second ('1' :)) (huffmanTable t2)

-- read string into tree not working yet
-- put sections of trees in tokens/a list?
importTree :: String -> Bintree a
importTree treeString = read treeString :: Bintree a

-- decompress :: String -> Bintree a -> String
-- decompress [] _ = []
-- decompress input tree = decompressHelper input tree tree
--   where
--     decompressHelper :: String -> Bintree a -> Bintree a -> String
--     decompressHelper [] _ _ = []
--     decompressHelper input@(x:xs) (Leaf c) root = c : decompressHelper input root root
--     decompressHelper (x:xs) (Branch l _ r) root
--       | x == '0'  = decompressHelper xs l root
--       | x == '1'  = decompressHelper xs r root
--     decompressHelper _ Empty _ = error "Invalid Huffman code"
    -- decompressHelper _ _ _ = error "Unexpected error in decompression"

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

isValidHuffmanTree :: Bintree a -> Bool
isValidHuffmanTree Empty = True
isValidHuffmanTree (Leaf _) = True
isValidHuffmanTree (Branch l _ r) = 
  case (l, r) of 
    (Empty, Empty) -> False  -- Shouldn't have Branch with two Empty children
    (Leaf _, Leaf _) -> True  -- Both children are Leaves
    (Leaf _, Branch _ _ _) -> isValidHuffmanTree r  -- Left child is Leaf, right is Branch
    (Branch _ _ _, Leaf _) -> isValidHuffmanTree l  -- Right child is Leaf, left is Branch
    (Branch _ _ _, Branch _ _ _) -> isValidHuffmanTree l && isValidHuffmanTree r  -- Both children are Branches
    _ -> False  -- Any other case is invalid

-- group get 1's skip 0's then check length of 1's compare to longest string of 1's else check charachter that matches
-- decompress :: String -> [(Char, String)] -> [String]
-- decompress [] _ = []
-- decompress input table = grouped 
--     where
--         maxLength = snd(last table)
--         grouped = filter (\[a] -> '0' `elem` a) (group input)

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
    print tree
    print inputFile
    -- print (isValidHuffmanTree tree)
    -- let table = huffmanTable tree
    -- print table
    let d = decompress inputFile tree
    print d
    writeFile outputFile d
    putStrLn "Done decompressing"
