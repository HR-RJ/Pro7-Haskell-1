module Main where

import System.Environment
import System.IO
import Data.List
import Data.String
import Text.Printf (IsChar(fromChar, toChar))
main :: IO ()

tupleToString :: (Show a, Show b, IsChar a) => (a, b) -> String
tupleToString (x, y) = show y ++ [toChar x]

rlcompress:: [Char] -> [Char] -> IO ()
rlcompress fileIn fileOut = do
    contents <- readFile fileIn
    let compressed = map (\x -> (head x, length x)) (group contents)
    let res = concatMap tupleToString compressed
    writeFile fileOut res
    

main = do
    putStrLn "Please enter the file to be compressed:"
    input1 <- getLine

    putStrLn "Please enter the output file to compress to:"
    input2 <- getLine

    rlcompress input1 input2
    print("Done")
