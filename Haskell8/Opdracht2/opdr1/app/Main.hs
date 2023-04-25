module Main where
import GHC.Generics

main :: IO ()

-- maybe implements or derives int? -- not completely sure what that changes to be more exact
-- lets keep this for now
--type Value = Integer

data Bintree a = Empty
    | Branch  (Bintree a) a (Bintree a)
    deriving (Show, Eq, Ord) -- ?
    

-- data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  ??
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types


-- hmmm, shits fucked yo. is our Bintree generic? -- prolly now -- ye
-- alle functies zijn random bullshit that copilot hallucinated so shrug

push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty a = Branch Empty a Empty
push (Branch t1 v t2) val 
    | val == v = Branch t1 val t2
    | val > v = Branch t1 v (push t2 val)
    | val < v = Branch (push t1 val) v t2

-- maybe?

-- pushlist :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
-- pushlist Empty [] = Empty

-- maptree :: (a -> b) -> (Bintree a) -> (Bintree b)
-- maptree f Empty = Empty

-- filtertree :: (a->Bool) ->(Bintree a) -> [a]
-- filtertree f Empty = []

-- preorder :: (Bintree a) -> [a]
-- preorder Empty = []

-- postorder :: (Bintree a) -> [a]
-- postorder Empty = []

-- inorder :: (Bintree a) -> [a]
-- inorder Empty = []

-- random bullshit go brr

main = do 
    print (push Empty 12)
    print (push (push (push (push Empty 12) 13) 11) 15)
