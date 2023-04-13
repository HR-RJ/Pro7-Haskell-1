module Main where

import System.Environment
import System.IO
import Data.List
main :: IO ()

sortFile:: String -> String -> IO ()
sortFile fileIn fileOut = do
    contents <- readFile fileIn
    -- let linesOfFile = lines contents
    let sortedletters = sort contents
    writeFile fileOut sortedletters

arguments:: IO ()
arguments = do
    args <- getArgs
    let fileIn = head args
    let fileOut = last args
    sortFile fileIn fileOut

main = do
    arguments
    print("Done")

    -- cabal run exes -- testIn.txt testOut.txt