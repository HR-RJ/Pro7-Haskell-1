module Main where
import GHC.Generics

main :: IO ()

data Bintree a = Empty
    | Branch  (Bintree a) a (Bintree a)
    deriving (Show, Eq, Ord) -- ?


-- data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  ??
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types


push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty a = Branch Empty a Empty
push (Branch t1 v t2) val
    | val == v = Branch t1 val t2
    | val > v = Branch t1 v (push t2 val)
    | val < v = Branch (push t1 val) v t2

pushList :: (Ord a) => Bintree a -> [a] -> Bintree a
pushList tree [] = tree
pushList tree (head:rest) = pushList (push tree head) rest

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
postorder (Branch t1 v t2) = postorder t1 ++ postorder t2 ++ [v] -- ??

inorder :: Bintree a -> [a]
inorder Empty = []
inorder (Branch t1 v t2) = inorder t1 ++ [v] ++ inorder t2 -- ??

main = do
    print (push Empty 12)
    print (push (push (push (push Empty 12) 13) 11) 15)
    print (pushList Empty [5,4,6,2,8,1,3,7,9])
    print (mapTree (+1) (pushList Empty [2,1,3,4,7,3,6]))
    print (filterTree (>2) (pushList Empty [2,1,3,4,7,3,6]))
    print (preorder (pushList Empty [5,4,6,2,8,1,3,7,9]))
    print (postorder (pushList Empty [5,4,6,2,8,1,3,7,9]))
    print (inorder (pushList Empty [5,4,6,2,8,1,3,7,9]))

