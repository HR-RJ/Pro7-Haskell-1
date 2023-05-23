module Main where

import System.Environment
import System.IO
-- import Data.List
import Data.String
-- import Text.Printf (IsChar(fromChar, toChar))
-- import qualified GHC.IO.Device as Bintree
-- import qualified Text.Read as Bintree
main :: IO ()

data Bintree a = Empty
    | Branch  (Bintree a) a (Bintree a)
    deriving (Show, Eq, Ord, Read)
    
push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty a = Branch Empty a Empty
push (Branch t1 v t2) val 
    | val == v = Branch t1 val t2
    | val > v = Branch t1 v (push t2 val)
    | val < v = Branch (push t1 val) v t2


pushList :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushList tree [] = tree
pushList tree (head:rest) = pushList (push tree head) rest

mapTree :: (a -> b) -> (Bintree a) -> (Bintree b)
mapTree f Empty = Empty
mapTree f (Branch t1 v t2) = Branch (mapTree f t1) (f v) (mapTree f t2)

filterTree :: (a->Bool) -> (Bintree a) -> [a]
filterTree _ Empty = []
filterTree f (Branch t1 v t2)
    | f v = (filterTree f t1) ++ [v] ++ (filterTree f t2)
    | otherwise = (filterTree f t1) ++ (filterTree f t2)


preorder :: (Bintree a) -> [a]
preorder Empty = []
preorder (Branch t1 v t2) = [v] ++ (preorder t1) ++ (preorder t2)

postorder :: (Bintree a) -> [a]
postorder Empty = []
postorder (Branch t1 v t2) = (postorder t1) ++ (postorder t2) ++ [v]

inorder :: (Bintree a) -> [a]
inorder Empty = []
inorder (Branch t1 v t2) = (inorder t1) ++ [v] ++ (inorder t2)

    

main = do
    putStrLn "Please enter the file to be compressed:"
    fileInput <- getLine

    putStrLn "Please enter the output file to compress to:"
    fileOutput <- getLine
    contents <- readFile fileInput
    print contents
    print(pushList Empty contents)
    writeFile fileOutput (show (mapTree (fromEnum) (pushList Empty contents)))
    var <- readFile fileOutput
    read var :: Bintree Int
    --(readFile fileOutput) >>= let variable

    print("done")


