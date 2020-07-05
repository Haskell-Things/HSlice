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

module Graphics.Slicer.Math.Point (crossProduct, twoDCrossProduct, orderPoints) where

import Prelude ((*), (-), ($), Ordering, (==), compare, otherwise)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), PlanePoint, xOf, yOf, zOf)

import Graphics.Slicer.Definitions (ℝ)

-- Basic Arithmatic
crossProduct :: Point3 -> Point3 -> Point3
crossProduct (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) = Point3 (y1*z2 - z1*y2 , z1*x2 - x1*z2, x1*y2 - y1*x2)

twoDCrossProduct :: Point2 -> Point2 -> ℝ
twoDCrossProduct p1 p2 = zOf $ crossProduct (zeroPoint p1) (zeroPoint p2)
  where
    zeroPoint :: Point2 -> Point3
    zeroPoint (Point2 (x,y)) = Point3 (x,y,0)

-- Orders points by x and y (x first, then sorted by y for the same x-values)
orderPoints :: (PlanePoint p) => p -> p -> Ordering
orderPoints p1 p2
  | x1 == x2 = compare y1 y2
  | otherwise = compare x1 x2
  where
    (x1,y1) = (xOf p1, yOf p1)
    (x2,y2) = (xOf p2, yOf p2)

