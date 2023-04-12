module Main where

main :: IO ()

type Square = Double
type Circle = Double
type Rectangle = (Double, Double)
type Triangle = Double

data Color = Red | Green | Blue deriving Show
data Geofig = Square Square Color | Circle Circle Color | Rectangle Rectangle Color | Triangle Triangle Color deriving Show

diameterAndColor :: Geofig -> (Geofig, Double, Color)
diameterAndColor shape@(Square side color) = (shape, side * sqrt 2, color)
diameterAndColor shape@(Circle radius color) = (shape, radius * 2, color)
diameterAndColor shape@(Rectangle (width, height) color) = (shape, sqrt (width**2 + height**2), color)
diameterAndColor shape@(Triangle side color) = (shape, side, color) -- This assumes an equilateral triangle

surface :: Geofig -> Double
surface (Square side _) = side**2
surface (Circle radius _) = pi * radius**2
surface (Rectangle (width, height) _) = width * height
surface (Triangle side _) = side**2 * sqrt 3 / 4

circumference :: Geofig -> Double
circumference (Square side _) = side * 4
circumference (Circle radius _) = 2 * pi * radius
circumference (Rectangle (width, height) _) = 2 * (width + height)
circumference (Triangle side _) = side * 3

-- onlySquare :: (Geofig) -> Bool
-- | onlySquare (Square side _) = (Square side _)
-- | otherwise = ()

-- returnShape :: String -> (Geofig) -> (Geofig)
-- returnShape "Square" (Square _ _) = (Square _ _)
-- returnShape "Circle" (Circle radius _) = (Circle radius _)
-- returnShape "Rectangle" (Rectangle (width, height) _) = (Rectangle (width, height) _)
-- returnShape "Triangle" (Triangle side _) = (Triangle side _)

-- createFig :: Geofig -> [Double] -> Geofig
-- createFig Square s = Square
-- createFig Circle r = Circle
-- createFig Rectangle [x,y] = Rectangle
-- createFig Triangle h = Triangle

    --case Geofig of
    --    Square -> Square 
    --    Circle -> Circle
    --    Rectangle -> Rectangle
    --    Triangle -> Triangle




main = do
    print("surface")
    print(surface (Square 2 Red))
    print(surface (Circle 2 Blue))
    print(surface (Rectangle (2, 2) Green))
    print(surface (Triangle 2 Red))
    print("circumference")
    print(circumference (Square 2 Red))
    print(circumference (Circle 2 Blue))
    print(circumference (Rectangle (2, 2) Green))
    print(circumference (Triangle 2 Red))
    print("onlySquare")
    -- print(onlySquare (Square 2 Red),(Circle 2 Blue),(Rectangle (2, 2) Green),(Triangle 2 Red))
    -- print(returnShape "Square" (Square 2 Red),(Circle 2 Blue),(Rectangle (2, 2) Green),(Triangle 2 Red))
    -- print(diameterAndColor (Square 2 Red))
    -- print(diameterAndColor (Circle 2 Red))
    -- print(diameterAndColor (Rectangle (2, 2) Red))
    -- print(diameterAndColor (Triangle 2 Red))

