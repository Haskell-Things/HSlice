{- ORMOLU_DISABLE -}
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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, PolyKinds, FlexibleInstances #-}

-- | The purpose of this file is to hold the definitions of the data structures used when performing slicing related math.
module Graphics.Slicer.Math.Definitions(Point3(Point3), Point2(Point2), Contour(SafeContour), SpacePoint, PlanePoint, xOf, yOf, zOf, flatten, distance, addPoints, scalePoint, (~=), roundToFifth, roundPoint2, mapWithNeighbors, mapWithFollower, mapWithPredecessor) where

import Prelude (Eq, Show, (==), (*), sqrt, (+), ($), Bool, fromIntegral, round, (/), Ord(compare), otherwise, Int, null, zipWith3, zipWith, (<>), error)

import Control.DeepSeq (NFData)

import Control.Parallel.Strategies (withStrategy, parList, rpar)

import Control.Parallel (par, pseq)

import Data.List (uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe (Just, Nothing))

import GHC.Generics (Generic)

import Slist.Type (Slist)

import Graphics.Slicer.Definitions (ℝ, ℝ2, ℝ3, Fastℕ)

import Graphics.Slicer.Orphans ()

-- A single Point in 2D or 3D linear space.
newtype Point3 = Point3 ℝ3
  deriving (Eq, Generic, NFData, Show)

newtype Point2 = Point2 ℝ2
  deriving (Eq, Generic, NFData, Show)

-- | A typeclass containing our basic linear algebra functions.
class LinAlg p where
  -- Distance between two points. needed for the equivilence instance of line, and to determine amount of extrusion.
  distance   :: p -> p -> ℝ
  -- Add the coordinates of two points
  addPoints  :: p -> p -> p
  -- Scale the coordinates of a point by s
  scalePoint :: ℝ -> p -> p
  -- Are these points the same point, after rounding for printing?
  (~=)       :: p -> p -> Bool

-- | perform linear algebra on 3D points.
instance LinAlg Point3 where
  distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)
    where
      magnitude (Point3 (x1,y1,z1)) = sqrt (x1 * x1 + y1 * y1 + z1 * z1)
  addPoints (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) = Point3 (x1+x2 ,y1+y2 ,z1+z2)
  scalePoint val (Point3 (a,b,c)) = Point3 (val*a ,val*b ,val*c)
  (~=) p1 p2 = roundPoint3 p1 == roundPoint3 p2

-- | perform linear algebra on 2D points.
instance LinAlg Point2 where
  distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)
    where
      magnitude (Point2 (x1,y1)) = sqrt (x1 * x1 + y1 * y1)
  addPoints (Point2 (x1,y1)) (Point2 (x2,y2)) = Point2 (x1+x2, y1+y2)
  scalePoint val (Point2 (a,b)) = Point2 (val*a ,val*b)
  (~=) p1 p2 = roundPoint2 p1 == roundPoint2 p2

class PlanePoint p where
  xOf :: p -> ℝ
  yOf :: p -> ℝ

instance Ord Point2 where
  -- Orders points by x and y (x first, then sorted by y for the same x-values)
  compare p1 p2
    | x1 == x2 = compare y1 y2
    | otherwise = compare x1 x2
    where
      (x1,y1) = (xOf p1, yOf p1)
      (x2,y2) = (xOf p2, yOf p2)

-- | functions for getting a point's position on a 2D plane. If the point is 3d, assume the plane is aligned with the xy basis axes.
instance PlanePoint Point3 where
  xOf (Point3 (x,_,_)) = x
  yOf (Point3 (_,y,_)) = y

instance PlanePoint Point2 where
  xOf (Point2 (x,_))   = x
  {-# INLINABLE xOf #-}
  yOf (Point2 (_,y))   = y
  {-# INLINABLE yOf #-}

class SpacePoint p where
  zOf :: p -> ℝ
  flatten :: p -> Point2

instance SpacePoint Point3 where
  zOf (Point3 (_,_,z)) = z
  flatten (Point3 (x,y,_)) = Point2 (x,y)

-- | a list of points around a (2d) shape.
-- Note that the minPoint and maxPoint define a bounding box for the contour that it does not spill out of.
data Contour = SafeContour { _minPoint :: !Point2, _maxPoint :: !Point2, _firstPoint :: !Point2, _secondPoint :: !Point2, _thirdPoint :: !Point2 , morePoints :: !(Slist Point2) }
  deriving (Eq, Generic, NFData, Show)

-- | round a value
roundToFifth :: ℝ -> ℝ
roundToFifth a = fromIntegral (round (100000 * a) :: Fastℕ) / 100000

-- | round a point
roundPoint3 :: Point3 -> Point3
roundPoint3 (Point3 (x1,y1,z1)) = Point3 (roundToFifth x1, roundToFifth y1, roundToFifth z1)
roundPoint2 :: Point2 -> Point2
roundPoint2 (Point2 (x1,y1)) = Point2 (roundToFifth x1, roundToFifth y1)

-- | like map, only with previous, current, and next item, and wrapping around so the first entry gets the last entry as previous, and vica versa.
mapWithNeighbors :: (a -> a -> a -> b) -> [a] -> [b]
mapWithNeighbors f l
  | null l = []
  | otherwise = withStrategy (parList rpar) $ x `par` z `pseq` zipWith3 f x l z
  where
    z = zs <> [fz]
    (fz, zs) = case uncons l of
                 Nothing -> error "empty input list"
                 (Just (_,[])) -> error "too short of a list."
                 (Just vs) -> vs
    x = lx:xs
    (xs, lx) = case unsnoc l of
                 Nothing -> error "Empty input list"
                 (Just ([],_)) -> error "too short of a list."
                 (Just vs) -> vs

-- | like map, only with current, and next item, and wrapping around so the last entry gets the first entry as next.
mapWithFollower :: (a -> a -> b) -> [a] -> [b]
mapWithFollower f l = withStrategy (parList rpar) $ z `pseq` zipWith f l z
  where
    z = zs <> [fz]
    (fz, zs) = case uncons l of
                 Nothing -> error "empty input list"
                 (Just (_,[])) -> error "too short of a list."
                 (Just vs) -> vs

-- | like map, only with previous, and current item, and wrapping around so the first entry gets the last entry as previous.
mapWithPredecessor :: (a -> a -> b) -> [a] -> [b]
mapWithPredecessor f l = withStrategy (parList rpar) $ x `pseq` zipWith f x l
  where
    x = lx:xs
    (xs, lx) = case unsnoc l of
                 Nothing -> error "Empty input list"
                 (Just ([],_)) -> error "too short of a list."
                 (Just vs) -> vs
