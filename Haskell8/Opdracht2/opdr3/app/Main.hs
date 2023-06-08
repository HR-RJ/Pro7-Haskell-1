module Main where

import System.Environment
import System.IO
import Data.String (IsString(fromString))
import Data.Char

data Bintree a = Empty
    | Branch  (Bintree a) a (Bintree a)
    deriving (Show, Eq, Ord, Read)

push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty a = Branch Empty a Empty
push (Branch t1 v t2) val
    | val == v = Branch t1 val t2
    | val > v = Branch t1 v (push t2 val)
    | val < v = Branch (push t1 val) v t2

pushList :: (Ord a) => Bintree a -> [a] -> Bintree a
pushList tree rest = foldl push tree rest

mapTree :: (a -> b) -> Bintree a -> Bintree b
mapTree f Empty = Empty
mapTree f (Branch t1 v t2) = Branch (mapTree f t1) (f v) (mapTree f t2)

filterTree :: (a->Bool) -> Bintree a -> [a]
filterTree _ Empty = []
filterTree f (Branch t1 v t2)
    | f v = filterTree f t1 ++ [v] ++ filterTree f t2
    | otherwise = filterTree f t1 ++ filterTree f t2

preorder :: Bintree a -> [a]
preorder Empty = []
preorder (Branch t1 v t2) = [v] ++ preorder t1 ++ preorder t2

postorder :: Bintree a -> [a]
postorder Empty = []
postorder (Branch t1 v t2) = postorder t1 ++ postorder t2 ++ [v]

inorder :: Bintree a -> [a]
inorder Empty = []
inorder (Branch t1 v t2) = inorder t1 ++ [v] ++ inorder t2


main :: IO ()
main = do
    putStrLn "Please enter the inputfile:"
    inputFile <- getLine

    putStrLn "Please enter the file to put the tree in:"
    outputFile <- getLine
    contents <- readFile inputFile
    -- print contents
    let tree = pushList Empty contents
    writeFile outputFile (show (mapTree fromEnum tree))
    var <- readFile outputFile
    let bintree = read var :: Bintree Int
    -- print bintree
    let nextTree = mapTree chr bintree
    -- print nextTree
    print (inorder nextTree)
    print (filterTree isDigit nextTree)
    print "done"


