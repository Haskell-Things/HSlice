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
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, PolyKinds #-}

module Graphics.Slicer.Math.Definitions(Point3(Point3), Point2(Point2), Contour(PointSequence), SpacePoint, PlanePoint, xOf, yOf, zOf, flatten, magnitude, distance, addPoints, scalePoint, (~=), roundToFifth, roundPoint2) where

import Prelude (Eq, (++), Monoid(mempty, mappend), Semigroup((<>)), Show, (==), (*), sqrt, (+), ($), Bool, fromIntegral, round, (/))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ, ℝ2, ℝ3, Fastℕ)

-- A single Point in 2D or 3D linear space.
newtype Point3 = Point3 ℝ3
  deriving (Eq, Generic, NFData, Show)

newtype Point2 = Point2 ℝ2
  deriving (Eq, Generic, NFData, Show)

-- A single Point in 2D projective space.
-- 2D coresponds to a Clifford algebra of 2,0,1.
newtype PPoint2 = PPoint2 ℝ3
  deriving (Generic, NFData, Show)

-- | A typeclass containing our basic point algebra functions.
class LinAlg p where
  magnitude  :: p -> ℝ
  -- Distance between two points. needed for the equivilence instance of line, and to determine amount of extrusion.
  distance   :: p -> p -> ℝ
  -- Add the coordinates of two points
  addPoints  :: p -> p -> p
  -- Scale the coordinates of a point by s
  scalePoint :: ℝ -> p -> p
  -- Are these points the same point, after rounding for printing?
  (~=)       :: p -> p -> Bool
  
instance LinAlg Point3 where
  magnitude (Point3 (x1,y1,z1)) = sqrt $ x1 * x1 + y1 * y1 + z1 * z1
  distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)
  addPoints (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) = Point3 (x1+x2 ,y1+y2 ,z1+z2)
  scalePoint val (Point3 (a,b,c)) = Point3 (val*a ,val*b ,val*c)
  (~=) p1 p2 = roundPoint3 p1 == roundPoint3 p2

instance LinAlg Point2 where
  magnitude (Point2 (x1,y1)) = sqrt (x1 * x1 + y1 * y1) 
  distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)
  addPoints (Point2 (x1,y1)) (Point2 (x2,y2)) = Point2 (x1+x2, y1+y2)
  scalePoint val (Point2 (a,b)) = Point2 (val*a ,val*b)
  (~=) p1 p2 = roundPoint2 p1 == roundPoint2 p2

class PlanePoint p where
  xOf :: p -> ℝ
  yOf :: p -> ℝ

-- | functions for getting a point's position on a 2D plane. If the point is 3d, assume the plane is aligned with the xy basis axes.
instance PlanePoint Point2 where
  xOf (Point2 (x,_))   = x
  {-# INLINABLE xOf #-}
  yOf (Point2 (_,y))   = y
  {-# INLINABLE yOf #-}

instance PlanePoint Point3 where
  xOf (Point3 (x,_,_)) = x
  yOf (Point3 (_,y,_)) = y

class SpacePoint p where
  zOf :: p -> ℝ
  flatten :: p -> Point2

instance SpacePoint Point3 where
  zOf (Point3 (_,_,z)) = z
  flatten (Point3 (x,y,_)) = Point2 (x,y)

-- a list of points around a (2d) shape.
newtype Contour =
  PointSequence [Point2]
  deriving (Eq, Generic, NFData, Show)

instance Semigroup Contour where
  (<>) (PointSequence a) (PointSequence b) = PointSequence (a ++ b)

instance Monoid Contour where
  mempty = PointSequence []
  mappend = (<>)

-- round a value
roundToFifth :: ℝ -> ℝ
roundToFifth a = fromIntegral (round (100000 * a) :: Fastℕ) / 100000

-- round a point
roundPoint3 :: Point3 -> Point3
roundPoint3 (Point3 (x1,y1,z1)) = Point3 (roundToFifth x1, roundToFifth y1, roundToFifth z1)
roundPoint2 :: Point2 -> Point2
roundPoint2 (Point2 (x1,y1)) = Point2 (roundToFifth x1, roundToFifth y1)
