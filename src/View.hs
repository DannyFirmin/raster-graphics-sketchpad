--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point)
import Data.Text (pack)
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
    -- TODO: add cases for rectangles, lines, polygons, and circles
    _ -> []

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
-- rectangleRaster (x1,y1) (x2,y2) = zip([x1,y1] [x2,y2])
rectangleRaster (x1,y1) (x2,y2) =
    zip (zip [(min x1 x2)..(max x1 x2)] [y1,y1..]
  ++ zip [(min x1 x2)..(max x1 x2)] [y2,y2..]
  ++ zip [x1,x1..] [(min y1 y2)..(max y1 y2)]
  ++ zip [x2,x2..] [(min y1 y2)..(max y1 y2)]) [1,1..]

-- rectangleRaster (x1,y1) (x2,y2) = [((x,y),1)|x<-[x1,x1..],y<-[minimum(y1,y2)..maximum(y1,y2)]]
--   ++[((x,y),1)|x<-[x2,x2..],y<-[minimum(y1,y2)..maximum(y1,y2)]]
--   ++[((x,y),1) |x<-[minimum(x1,x2)..maximum(x1,x2)],y<-[y1,y1..]]
--   ++[((x,y),1) |x<-[minimum(x1,x2)..maximum(x1,x2)],y<-[y2,y2..]]

-- | A raster for the line with end coordinates given as arguments.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ lineRaster False (-1,-1) (1,1)
-- [((-1,-1),1.0),((0,0),1.0),((1,1),1.0)]
--
-- prop> a == (fst $ head $ lineRaster False a b)
-- prop> b == (fst $ last $ lineRaster False a b)

lineRaster :: Smooth -> Coord -> Coord -> Raster
lineRaster s (x0,y0) (x1,y1) = [((x1,y1),1)]
  where dx = x1 - x0
        dy = y1 - y0
        d = 2*dy-dx

-- balancedWord :: Int -> Int -> Int -> [Int]
-- balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
-- balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- lineRaster s (x0,y0) (x1,y1) =
--   let (dx, dy) = (x1 - x0, y1 - y0)
--       xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
--       yxStep b (x, y) = (x + signum dx * b, y + signum dy)
--       (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
--                    | otherwise       = (abs dx, abs dy, yxStep)
--       walk w xy = xy : walk (tail w) (step (head w) xy)
--   in  zip (walk (balancedWord p q 0) (x0, y0)) [1,1..]

-- | A raster for the polyline with vertices vs.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ polyLineRaster False [(0,0),(2,2)]
-- [((0,0),1.0),((1,1),1.0),((2,2),1.0)]
-- >>> sort $ polyLineRaster False [(0,0),(1,1),(0,1)]
-- [((0,0),1.0),((0,1),1.0),((1,1),1.0)]
--
-- prop> lineRaster False a b == polyLineRaster False [a, b]
polyLineRaster :: Smooth -> [Coord] -> Raster
polyLineRaster = undefined -- TODO

-- | A raster for the circle with center (x1,y1) and intersecting (x2,y2)
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ circleRaster False (0,0) (0,1)
-- [((-1,0),1.0),((0,-1),1.0),((0,1),1.0),((1,0),1.0)]
circleRaster :: Smooth -> Coord -> Coord -> Raster
circleRaster = undefined -- TODO
