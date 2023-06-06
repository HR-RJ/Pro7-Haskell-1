data Bintree a = Empty
    | Branch (Bintree a) Int (Bintree a)
    | Leaf Char
    deriving (Show, Eq, Ord, Read)

importTree :: String -> Bintree a
importTree treeString = read treeString :: Bintree a

-- example
etree = importTree "Branch (Leaf ' ') 27 (Branch (Leaf 't') 22 (Branch (Leaf 'e') 18 (Branch (Leaf 's') 14 (Branch (Leaf 'i') 12 (Branch (Leaf '3') 10 (Branch (Leaf '2') 8 (Branch (Leaf '1') 6 (Branch (Leaf 'n') 4 (Branch (Leaf 'm') 3 (Branch (Leaf 'D') 2 (Leaf '4')))))))))))"
estring = "111111111101111010011110111001101101111111100101101110100111111111011010011111110111111011111011111111111111110111111011111110"
eresult = "1234321"

decompress :: String -> Bintree a -> String
decompress [] _ = []
decompress xs tree = decompressHelper xs tree
  where
    decompressHelper :: String -> Bintree a -> String
    decompressHelper [] (Leaf c) = [c]
    decompressHelper [] _ = []
    decompressHelper xs (Leaf c) = c : decompress xs tree
    decompressHelper xss (Branch l _ r)
      | head xss == '0' = decompressHelper (drop 1 xss) l
      | otherwise       = decompressHelper (drop 1 xss) r
    decompressHelper _ Empty = error "Invalid Huffman code"

-- run: decompress estring etree