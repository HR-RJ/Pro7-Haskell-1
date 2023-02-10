module Main where

main :: IO ()
nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c
    | d < 0 = [0]
    | otherwise = [(-b+sqrt d)/(2*a),
                    (-b-sqrt d)/(2*a)]

    where d = (b^2 - 4*a*c)

-- 2c
-- let results =length [ (a,b,c) | a <- [1..20], b <- [1..20], c <- [1..20], mod (a+b+c) 5 ==0 ]

countd:: Int -> [(Int,Int,Int)]
countd n = [ (a,b,c) | a <- [1..20], b <- [1..20], c <- [1..20], mod (a+b+c) n ==0 ]

-- opdr3:: Int-> Int-> Int -> [Double]
-- opdr3 a b c = [a,b,c]
--     where
--         a = 2*(b-c)
--         b = a*c 
--         c = (a+b)/2

        


-- countShitshow:: Int -> Int
-- countShitshow n
--     | n <= 0 = 0
--     | mod (a) 5 /= 0 = 0
--     | otherwise = a
--     where  
--         a = elem [1..20] + countShitshow (n-1)

main = print(opdr3 1 1 1)

