module Main where
import Data.List
import Text.Printf (IsChar(toChar))
import Text.Read
import Data.Char
main :: IO ()

-- splitPairs :: String -> [String]
-- splitPairs [] = []
-- splitPairs xs = let (pair, rest) = splitAt 2 xs
--                 in pair : splitPairs rest

parsePairs :: String -> [(Int, Char)]
parsePairs [] = []
parsePairs (x:xs) =
  let (numStr, rest) = span isDigit (x:xs)
      num = read numStr :: Int
      (c:remaining) = rest
  in (num, c) : parsePairs remaining

rlDecompress :: [Char] -> [Char] -> IO ()
rlDecompress fileIn fileOut = do
    contents <- readFile fileIn
    let interm = parsePairs contents
    let res = (map (\(x,y) -> replicate x y) interm)
    writeFile fileOut (concat res)

main = do
    putStrLn "Please enter the compressed file:"
    input1 <- getLine

    putStrLn "Please enter the output file:"
    input2 <- getLine

    rlDecompress input1 input2
    -- arguments
    print ("Done")

-- cabal run exes -- compress.txt image.txt
