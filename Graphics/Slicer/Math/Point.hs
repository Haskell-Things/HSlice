{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- The purpose of this file is to hold point based arithmatic. -}

module Graphics.Slicer.Math.Point (Point(Point), x, y, z, crossProduct, twoDCrossProduct, scalePoint, addPoints, magnitude, distance, orderPoints) where

import Prelude (Num, RealFrac, Floating, sqrt, (*), (-), ($), fmap, (.), (+), Ord, Ordering, (==), compare, otherwise, fromRational)

import Graphics.Slicer.Math.Definitions (Point(Point), x, y, z)

-- import Graphics.Slicer.Math.Slicer(roundToFifth)

-- Basic Arithmatic
crossProduct :: RealFrac a => Point a -> Point a -> Point a
crossProduct (Point x1 y1 z1) (Point x2 y2 z2) = Point (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

twoDCrossProduct :: RealFrac a => Point a -> Point a -> a
twoDCrossProduct p1 p2 = z $ (crossProduct p1 {z = 0} p2 {z = 0})

-- Add the coordinates of two points
addPoints :: Num a => Point a -> Point a -> Point a
addPoints (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- Scale the coordinates of a point by s
scalePoint :: Num a => a -> Point a -> Point a
scalePoint = fmap . (*)

magnitude :: (Floating a) => Point a -> a
magnitude (Point x1 y1 z1) = sqrt $ x1 * x1 + y1 * y1 + z1 * z1

-- Distance between two points. needed for the equivilence instance of line, and to determine amount of extrusion.
distance :: (Floating a) => Point a -> Point a -> a
distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)

-- Orders points by x and y (x first, then sorted by y for the same x-values)
orderPoints :: (Ord a) => Point a -> Point a -> Ordering
orderPoints (Point x1 y1 _) (Point x2 y2 _)
  | x1 == x2 = compare y1 y2
  | otherwise = compare x1 x2
        
