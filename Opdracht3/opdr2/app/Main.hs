module Main where

import Data.List


main :: IO ()

dubbelen::Ord a => [a]->[a]
dubbelen a = nub $ a \\ nub a
    

main = do
    print (dubbelen [1,2,2,4,5,6,2,7,6,8,9,10])
    print (dubbelen "aaafscvvsd")



-- previous attempts
--dubbelen a = nubBy (\ a  b -> a == b)  (sort a)
-- dubbelen a = ((>1).length) (group (sort a))
-- [(group (sort a)) | ]

-- sort elemindices deleteby concat