-- vim: ts=4 sw=4 expandtab syntax=haskell

module Coord (Coord, mkpoint, getx, gety, getlength, getangle) where
-- make it Coord(..) to export constructor

data Coord = Cartesian Double Double

-- mkpoint :: Double -> Double -> Coord
mkpoint x y = Cartesian x y

-- getx :: Coord -> Double
getx (Cartesian x _) = x

-- gety :: Coord -> Double
gety (Cartesian _ y) = y

-- getlength :: Coord -> Double
getlength (Cartesian x y) = sqrt (x * x + y * y)

-- getangle :: Coord -> Double
getangle c@(Cartesian x y) = asin (y / getlength c)
