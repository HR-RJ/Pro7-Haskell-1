module Main where

import Data.List


main :: IO ()

dubbelen::Ord a => [a]->[a]
--dubbelen a = nubBy (\ a  b -> a == b)  (sort a)
dubbelen a = ((>1).length) (group (sort a))
[(group (sort a)) | ]

    

main = do
    print (dubbelen [1,2,2,4,5,6,2,7,8,9,10])

-- elemIndices :: Eq a => a -> [a] -> [Int]
-- sort :: Ord a => [a] -> [a] 
-- deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] 
-- concat :: Foldable t => t [a] -> [a] 
-- groupBy (\a b -> a==b) sort a
-- length :: Foldable t => t a -> Int

-- sort elemindices deleteby concat