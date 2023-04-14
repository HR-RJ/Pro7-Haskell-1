module Main where

import System.Environment
import System.IO
import Data.List
import Data.String
import Text.Printf (IsChar(fromChar))
main :: IO ()

tupleToString :: (Show a, Show b) => (a, b) -> String
tupleToString (x, y) = show y  ++ show x


rlcompress:: [Char] -> [Char] -> IO ()
rlcompress fileIn fileOut = do
    contents <- readFile fileIn
    let chars = contents
    let compressed = map (\x -> (head x, length x)) (group chars)
    -- first (toInteger compressed)
    -- let compressed = map (\x -> (head x, length x)) (group linesOfFile)
    let res = concatMap tupleToString compressed
    writeFile fileOut (filter (not . (`elem` "'")) res)
    
arguments:: IO ()
arguments = do
    args <- getArgs
    let fileIn = head args
    let fileOut = last args
    rlcompress fileIn fileOut

main = do
    arguments
    print("Done")

        -- cabal run exes -- image.txt compress.txt