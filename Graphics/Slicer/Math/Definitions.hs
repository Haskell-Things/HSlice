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

{- The purpose of this file is to hold the definitions of the data
   structures used when performing slicing related math. -}

-- for adding Generic and NFData to Point.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.Definitions(Point(Point), LayerType(BaseOdd,BaseEven,Middle), Contour(Contour), magnitude, distance, addPoints, scalePoint) where

import Prelude (Eq, (++), Monoid(mempty, mappend), Semigroup((<>)), Show, (==), (*), sqrt, (+), (<), ($))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ, ℝ3)

-- A single Point in 3D space.
newtype Point = Point ℝ3
  deriving (Generic, NFData, Show)

-- Breaks STL reading by creating degenerate triangles.
instance Eq Point where
  (==) p1 p2 = distance p1 p2 < 0.00001

data LayerType = BaseOdd | BaseEven | Middle

magnitude :: Point -> ℝ
magnitude (Point (x1,y1,z1)) = sqrt $ x1 * x1 + y1 * y1 + z1 * z1

-- Distance between two points. needed for the equivilence instance of line, and to determine amount of extrusion.
distance :: Point -> Point -> ℝ
distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)

-- Add the coordinates of two points
addPoints :: Point -> Point -> Point
addPoints (Point (x1,y1,z1)) (Point (x2,y2,z2)) = Point (x1+x2 ,y1+y2 ,z1+z2)

-- Scale the coordinates of a point by s
scalePoint :: ℝ -> Point -> Point
scalePoint val (Point (a,b,c)) = Point (val*a ,val*b ,val*c)

-- a list of points around a (2d) shape.
newtype Contour = Contour [Point]
  deriving (Eq, Generic, NFData, Show)

instance Semigroup Contour where
  (<>) (Contour a) (Contour b) = Contour (a ++ b)

instance Monoid Contour where
  mempty = Contour []
  mappend = (<>)
