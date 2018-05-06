--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point)
import Data.Text (pack)
import Data.List
import Model

-- a pixel coordinate is a pair of Int
type Coord = (Int, Int)

-- a pixel value is some shade of grey (0.0 == white, 1.0 = black)
type Shade = Double

-- a raster pixel is represented by a coordinate and a shade
type Pixel = (Coord, Shade)

-- a raster is a list of pixels
type Raster = [Pixel]

-- $setup
-- >>> import Data.List (sort)

coordToPoint :: Resolution -> Coord -> Point
coordToPoint z (x, y) = (x', y')
  where
    x' = fromIntegral x * z
    y' = fromIntegral y * z

pointToCoord :: Resolution -> Point -> Coord
pointToCoord z (x, y) = (x', y')
  where
    x' = round $ x / z
    y' = round $ y / z

-- Update the view based on the model by constructing a rasterised CodeWorld picture
updateView :: Model -> Picture
updateView (Model ss z s t) =
  coordinatePlane &
  pictures (map pixelToPicture $ concatMap (shapeToRaster z s) ss) &
  translated (-13.5) 8 $
  (text . pack) ("Shape: " ++ shapeToText)
  where
    shapeToText = take (length shape - 4) shape
    shape = takeWhile (/= ' ') $ maybe "" show t
    pixelToPicture (c, b) = translated x' y' p
      where
        p = coloured (grey $ 1 - b) $ solidRectangle z z
        (x', y') = coordToPoint z c

-- Construct a raster for a shape at the given resolution (optionally smoothed)
shapeToRaster :: Resolution -> Smooth -> Shape -> Raster
shapeToRaster z s shape =
  case shape of
    Point p1 -> pointRaster $ pointToCoord z p1
    Rectangle p1 p2 -> rectangleRaster (pointToCoord z p1) (pointToCoord z p2)
    Line p1 p2 -> lineRaster s (pointToCoord z p1) (pointToCoord z p2)
    Polygon p -> polyLineRaster s (polygonCord z p)
    Circle p1 p2 -> circleRaster s (pointToCoord z p1) (pointToCoord z p2)
    _ -> []

polygonCord :: Resolution -> [Point] -> [Coord]
polygonCord z [] = []
polygonCord z (p:ps) = pointToCoord z p: polygonCord z ps
-- | A raster for the point p
-- Examples:
-- >>> pointRaster (1,1)
-- [((1,1),1.0)]
pointRaster :: Coord -> Raster
pointRaster p = [(p, 1)]

-- | A raster for the rectangle with corner coordinates (x1,y1) and (x2,y2)
-- Examples:
-- >>> sort $ rectangleRaster (-1,-1) (1,1)
-- [((-1,-1),1.0),((-1,0),1.0),((-1,1),1.0),((0,-1),1.0),((0,1),1.0),((1,-1),1.0),((1,0),1.0),((1,1),1.0)]
rectangleRaster :: Coord -> Coord -> Raster
rectangleRaster (x1,y1) (x2,y2) =
    zip (zip [(min x1 x2)..(max x1 x2)] [y1,y1..]
  ++ zip [(min x1 x2)..(max x1 x2)] [y2,y2..]
  ++ zip [x1,x1..] [(min y1 y2)..(max y1 y2)]
  ++ zip [x2,x2..] [(min y1 y2)..(max y1 y2)]) [1,1..]


-- | A raster for the line with end coordinates given as arguments.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ lineRaster False (-1,-1) (1,1)
-- [((-1,-1),1.0),((0,0),1.0),((1,1),1.0)]
--
-- prop> a == (fst $ head $ lineRaster False a b)
-- prop> b == (fst $ last $ lineRaster False a b)

lineRaster :: Smooth -> Coord -> Coord -> Raster
lineRaster _ (x1,y1) (x2,y2) = zip [(x1+x,y1+y) | (x,y) <- bresenHam dx dy][1,1..]
  where dx= x2 - x1; dy= y2 - y1

bresenHam :: Integral a => a -> a -> [(a, a)]
bresenHam dx dy
    | dx  <  0  = [(-x, y) | (x, y) <- bresenHam (abs dx) dy]
    | dy <  0  = [(x, -y) | (x, y) <- bresenHam dx (abs dy)]
    | dy > dx = [(x,  y) | (y, x) <- bresenHam dy dx]
    | otherwise  = zip [0..dx] (map fst (iterate step (0, dx `div` 2)))
    where
        step (y, e)
            | e-dy < 0 = (y + 1, (e-dy) + dx)
            | otherwise  = (y, e-dy)


-- | A raster for the polyline with vertices vs.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ polyLineRaster False [(0,0),(2,2)]
-- [((0,0),1.0),((1,1),1.0),((2,2),1.0)]
-- >>> sort $ polyLineRaster False [(0,0),(1,1),(0,1)]
-- [((0,0),1.0),((0,1),1.0),((1,1),1.0)]
--
-- prop> lineRaster False a b == polyLineRaster False [a, b]

polyLineRaster' :: Smooth -> [Coord] -> Raster
polyLineRaster' z (p1:p2:ps) = lineRaster z p1 p2 ++ polyLineRaster' z (p2:ps)
polyLineRaster' _ [] = []
polyLineRaster' _ [_] = []

polyLineRaster:: Smooth -> [Coord] -> Raster
polyLineRaster z [p1,p2] = lineRaster z p1 p2
polyLineRaster z p = lineRaster z (head p) (last p) ++ polyLineRaster' z p

-- polyLineRaster' z p = lineRaster z (head p) (last p)

-- | A raster for the circle with center (x1,y1) and intersecting (x2,y2)
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ circleRaster False (0,0) (0,1)
-- [((-1,0),1.0),((0,-1),1.0),((0,1),1.0),((1,0),1.0)]
circleRaster :: Smooth -> Coord -> Coord -> Raster
circleRaster z (x0,y0) (x1,y1) = zip ((x0, y0 + r) : (x0, y0 - r) : (x0 + r, y0) : (x0 - r, y0) : points) [1,1..]
  where
  points = concatMap generatePoints $ unfoldr step (1-r,1,(-2)*r,0,r)
  r = isqrt((x1-x0)^2 + (y1-y0)^2)
  generatePoints (x, y)
          = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
  step (f,dx,dy,x,y) | x >= y = Nothing
                     | otherwise = Just ((x+1,y'),(f',dx+2,dy',x+1,y'))
                       where
                       (f',dy',y')|f >=0 = (f+dy'+(dx+2),dy+2,y-1)
                                  |otherwise = (f+dx,dy,y)

isqrt:: Int -> Int
isqrt n = round (sqrt (fromIntegral n))