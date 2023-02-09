module Main where

main :: IO ()
nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c
    | d < 0 = [0]
    | otherwise = [(-b+sqrt d)/(2*a),
                    (-b-sqrt d)/(2*a)]

    where d = (b^2 - 4*a*c)

main = print(nulpuntena 1 8 6)

