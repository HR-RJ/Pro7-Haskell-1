module Main where

main :: IO ()

eGCD :: Integer -> Integer -> Integer -> [Integer]
eGCD 0 b c = [b, 0, 1]
eGCD a b 0 = let [g, s, t] = eGCD (b `mod` a) a 0
       in [g, t - (b `div` a) * s, s]
eGCD a b 1 = do
         let [g, s, t] = eGCD (b `mod` a) a 0 in
          makePos[g, t - (b `div` a) * s, s] a b

makePos :: [Integer] -> Integer -> Integer -> [Integer]
makePos [a, b, c] e f
      | b < 0 = makePos [a, b + f, c] e f
      | c < 0 = makePos [a, b, c + e] e f
      | otherwise = [a,b,c]

keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen p q = (m, e, d)
    where
        m = p * q
        phiM = (p - 1) * (q - 1)
        e = head [e | e <-[2..], e <= phiM && gcd e phiM == 1]
        d = eGCD e phiM 1 !! 1


-- keygen :: Integer -> Integer -> (Integer, Integer, Integer)
-- keygen p q = (m, e, d)
--     where
--         m = p * q
--         phiM = (p - 1) * (q - 1)
--         e = head [e | e <-[2..], e <= phiM && gcd e phiM == 1]
--         d = head [d | d <-[2..], d <= phiM && gcd d phiM == 1, e * d `mod` phiM == 1]

main = print (keygen 101 107) 
