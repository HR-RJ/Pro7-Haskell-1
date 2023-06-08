import Data.List (sort)
import Data.Map.Strict (Map, insert, (!))

data HuffmanTree = Leaf Char Int | Node HuffmanTree HuffmanTree Int
    deriving (Show)

buildHuffmanTree :: String -> HuffmanTree
buildHuffmanTree str = buildTree (sort $ getFrequencies str)

buildTree :: [(Char, Int)] -> HuffmanTree
buildTree [(c, _)] = Leaf c 0
buildTree ((c1, f1) : (c2, f2) : cs) = buildTree $ insertNode (Node (Leaf c1 f1) (Leaf c2 f2) (f1 + f2)) cs

insertNode :: HuffmanTree -> [(Char, Int)] -> [(Char, Int)]
insertNode node [] = [(getNodeChar node, getNodeFreq node)]
insertNode node@(Node _ _ f) ((c, f') : cs)
    | f <= f' = (getNodeChar node, getNodeFreq node) : (c, f') : cs
    | otherwise = (c, f') : insertNode node cs

getFrequencies :: String -> [(Char, Int)]
getFrequencies str = foldr updateFreq [] str
  where
    updateFreq c freqs = case lookup c freqs of
        Just f -> map (\(x, y) -> if x == c then (x, y + 1) else (x, y)) freqs
        Nothing -> (c, 1) : freqs

getNodeChar :: HuffmanTree -> Char
getNodeChar (Leaf c _) = c
getNodeChar (Node _ _ _) = error "Internal node does not have a character"

getNodeFreq :: HuffmanTree -> Int
getNodeFreq (Leaf _ f) = f
getNodeFreq (Node _ _ f) = f

main :: IO ()
main = do
    let str = "GEEKSFORGEEKS"
        tree = buildHuffmanTree str
    print (tree)