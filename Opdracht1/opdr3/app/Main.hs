module Main where

main :: IO ()

-- a :: Double -> Double -> Double
-- b :: Double -> Double -> Double
-- c :: Double -> Double -> Double


-- a b c = 2 * (b - c)
-- b a c = a * c
-- c a b = (a + b) / 2
-- ??????????
-- calculateResult :: (Floating a, Eq a) => (a, a, a) -> (a, a, a)
-- calculateResult (a, b, c) =
--   let (a', b', c') = (2 * (b - c), a * c, (a + b) / 2)
--   in if a == a' && b == b' && c == c'
--     then (a, b, c)
--     else calculateResult (a', b', c')

findResult:: () -> [(Double, Double, Double)]
findResult () = [(a, b, c) | a <- [0..1000], b <- [0..1000], c <- [0..1000], a == 2*(b+c), b == a*c, c == (a+b)/2]

main = print (findResult())

