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

--  for :->:
{-# LANGUAGE TypeOperators, TypeFamilies #-}

-- | The purpose of this file is to hold the definitions of the data structures used when performing slicing related math.
module Graphics.Slicer.Math.Definitions(
  Contour(PointContour, LineSegContour),
  LineSeg(LineSeg),
  PlanePoint,
  Point2(Point2),
  Point3(Point3),
  SpacePoint,
  (~=),
  addPoints,
  distance,
  endPoint,
  flatten,
  lineSegsOfContour,
  makeLineSeg,
  mapWithNeighbors,
  mapWithFollower,
  mapWithPredecessor,
  minMaxPoints,
  negatePoint,
  pointBetweenPoints,
  pointsOfContour,
  roundPoint2,
  roundToFifth,
  scalePoint,
  startPoint,
  xOf,
  yOf,
  zOf
  ) where

import Prelude (Eq, Show, (==), (<$>), (*), sqrt, (+), ($), Bool, fromIntegral, round, (/), Ord(compare), otherwise, zipWith3, (<>), error, show, negate)

import Prelude as PL (zipWith)

import Control.DeepSeq (NFData)

import Control.Parallel.Strategies (withStrategy, parList, rpar)

import Control.Parallel (par, pseq)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe (Just, Nothing))

import Data.MemoTrie (HasTrie(enumerate, trie, untrie), Reg, (:->:), enumerateGeneric, trieGeneric, untrieGeneric)

import GHC.Generics (Generic)

import Slist.Type (Slist(Slist), size)

import Slist.Size (Size(Infinity))

import Slist (safeLast)

import Slist as SL (uncons, zipWith)

import Graphics.Slicer.Definitions (ℝ, ℝ2, ℝ3, Fastℕ)

import Graphics.Slicer.Orphans ()

-- | A single Point on a 2D plane.
newtype Point2 = Point2 ℝ2
  deriving (Eq, Generic, NFData, Show)

-- | A single Point in 3D linear space.
newtype Point3 = Point3 ℝ3
  deriving (Eq, Generic, NFData, Show)

-- | A typeclass containing our basic linear algebra functions.
class LinAlg p where
  -- | Distance between two points. needed for the equivilence instance of line, and to determine amount of extrusion.
  distance    :: p -> p -> ℝ
  -- | Add the coordinates of two points
  addPoints   :: p -> p -> p
  -- | Scale the coordinates of a point by ℝ
  scalePoint  :: ℝ -> p -> p
  -- | negate a point.
  negatePoint :: p -> p
  -- | Are these points the same point, after rounding for printing?
  (~=)        :: p -> p -> Bool

-- | perform linear algebra on 3D points.
instance LinAlg Point3 where
  distance p1 p2 = magnitude $ addPoints p1 (negatePoint p2)
    where
      magnitude (Point3 (x1,y1,z1)) = sqrt (x1 * x1 + y1 * y1 + z1 * z1)
  addPoints (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) = Point3 (x1+x2 ,y1+y2 ,z1+z2)
  scalePoint val (Point3 (a,b,c)) = Point3 (val*a ,val*b ,val*c)
  negatePoint (Point3 (a,b,c)) = Point3 (negate a, negate b, negate c)
  (~=) p1 p2 = roundPoint3 p1 == roundPoint3 p2

-- | perform linear algebra on 2D points.
instance LinAlg Point2 where
  distance p1 p2
    | p1 == p2 = 0
    | otherwise = magnitude $ addPoints p1 (negatePoint p2)
    where
      magnitude (Point2 (x1,y1)) = sqrt (x1 * x1 + y1 * y1)
  addPoints (Point2 (x1,y1)) (Point2 (x2,y2)) = Point2 (x1+x2, y1+y2)
  scalePoint val (Point2 (a,b)) = Point2 (val*a ,val*b)
  negatePoint (Point2 (a,b)) = Point2 (negate a, negate b)
  (~=) p1 p2 = roundPoint2 p1 == roundPoint2 p2

-- | functions for working on points as if they are in a 2D plane.
class PlanePoint p where
  -- | The x value of a point in a plane.
  xOf :: p -> ℝ
  -- | The Y value of a point in a plane.
  yOf :: p -> ℝ

instance HasTrie Point2 where
  newtype (Point2 :->: b) = Point2Trie { unPoint2Trie :: Reg Point2 :->: b }
  trie = trieGeneric Point2Trie
  untrie = untrieGeneric unPoint2Trie
  enumerate = enumerateGeneric unPoint2Trie

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

-- | functions for working on points in 3D space.
class SpacePoint p where
  -- | The Z value of a point in a 3D space.
  zOf :: p -> ℝ
  -- | A function converting the point into a 2D point along a Z aligned plane.
  flatten :: p -> Point2

instance SpacePoint Point3 where
  zOf (Point3 (_,_,z)) = z
  flatten (Point3 (x,y,_)) = Point2 (x,y)

-- | A euclidian line segment, starting at startPoint and stopping at endPoint.
data LineSeg = LineSeg { startPoint :: !Point2, endPoint :: !Point2 }
  deriving (Eq, Generic, NFData, Show)

instance HasTrie LineSeg where
  newtype (LineSeg :->: b) = LineSegTrie { unLineSegTrie :: Reg LineSeg :->: b }
  trie = trieGeneric LineSegTrie
  untrie = untrieGeneric unLineSegTrie
  enumerate = enumerateGeneric unLineSegTrie

-- | a list of points around a (2d) shape.
-- Note that the minPoint and maxPoint define a bounding box for the contour that it does not spill out of.
data Contour = PointContour { _minPoint :: !Point2, _maxPoint :: !Point2, _firstPoint :: !Point2, _secondPoint :: !Point2, _thirdPoint :: !Point2 , morePoints :: !(Slist Point2) }
             | LineSegContour { _myMinPoint :: !Point2, _myMaxPoint :: !Point2, _firstSeg :: !LineSeg, _secondSeg :: !LineSeg, moreSegs :: !(Slist LineSeg) }
  deriving (Eq, Generic, NFData, Show)

instance HasTrie Contour where
  newtype (Contour :->: b) = ContourTrie { unContourTrie :: Reg Contour :->: b }
  trie = trieGeneric ContourTrie
  untrie = untrieGeneric unContourTrie
  enumerate = enumerateGeneric unContourTrie

-- | find the minimum point and maximum point of a given contour.
minMaxPoints :: Contour -> (Point2, Point2)
minMaxPoints contour = case contour of
                         (PointContour foundMinPoint foundMaxPoint _ _ _ _) -> (foundMinPoint, foundMaxPoint)
                         (LineSegContour foundMinPoint foundMaxPoint _ _ _) -> (foundMinPoint, foundMaxPoint)

-- | Find a point between the two given points.
pointBetweenPoints :: Point2 -> Point2 -> Point2
pointBetweenPoints point1 point2 = scalePoint 0.5 $ addPoints point1 point2

-- | round a value
roundToFifth :: ℝ -> ℝ
roundToFifth a = fromIntegral (round (100000 * a) :: Fastℕ) / 100000

-- | round a point (3d)
roundPoint3 :: Point3 -> Point3
roundPoint3 (Point3 (x1,y1,z1)) = Point3 (roundToFifth x1, roundToFifth y1, roundToFifth z1)

-- | round a point (2d)
roundPoint2 :: Point2 -> Point2
roundPoint2 (Point2 (x1,y1)) = Point2 (roundToFifth x1, roundToFifth y1)

-- | like map, only with previous, current, and next item, and wrapping around so the first entry gets the last entry as previous, and vica versa.
{-# INLINABLE mapWithNeighbors #-}
mapWithNeighbors :: (Show a) => (a -> a -> a -> b) -> [a] -> [b]
mapWithNeighbors f l =
  case l of
    []      -> error "Empty input list"
    [_]     -> error $ "Too short of a list.\n" <> show l <> "\n"
    (fz:zs) -> case unsnoc l of
                 Nothing -> error "Empty input list"
                 (Just ([],_)) -> error $ "too short of a list.\n" <> show l <> "\n"
                 (Just (xs, lx)) -> withStrategy (parList rpar) $ x `par` z `pseq` zipWith3 f x l z
                   where
                     z = zs <> [fz]
                     x = lx : xs

-- | Like map, only with current, and next item, and wrapping around so the last entry gets the first entry as next.
{-# INLINABLE mapWithFollower #-}
mapWithFollower :: (Show a) => (a -> a -> b) -> [a] -> [b]
mapWithFollower f l =
  case l of
    []      -> error "Empty input list."
    [_]     -> error $ "too short of a list.\n" <> show l <> "\n"
    (fz:zs) -> withStrategy (parList rpar) $ z `pseq` PL.zipWith f l z
      where
        z = zs <> [fz]

-- | Like map, only with previous, and current item, and wrapping around so the first entry gets the last entry as previous.
mapWithPredecessor :: (a -> a -> b) -> [a] -> [b]
mapWithPredecessor f l = withStrategy (parList rpar) $ x `pseq` PL.zipWith f x l
  where
    x = lx:xs
    (xs, lx) = case unsnoc l of
                 Nothing -> error "Empty input list"
                 (Just ([],_)) -> error "too short of a list."
                 (Just vs) -> vs

makeLineSeg :: Point2 -> Point2 -> LineSeg
makeLineSeg p1 p2
  | p1 == p2 = error $ "tried to make a zero length line segment at: " <> show p1 <> "\n"
  | otherwise = LineSeg p1 p2

-- | return the contour as a list of LineSegs.
lineSegsOfContour :: Contour -> [LineSeg]
lineSegsOfContour (PointContour _ _ p1 p2 p3 pts) = [makeLineSeg p1 p2,
                                                    makeLineSeg p2 p3] <> consSegsWithPoints p3 pts p1
  where
    consSegsWithPoints pointStart points pointEnd =
      case SL.uncons points of
        Nothing -> [makeLineSeg pointStart pointEnd]
        (Just (headVal,tailVals)) -> makeLineSeg pointStart headVal :
                                   case safeLast tailVals of
                                     Nothing -> [makeLineSeg headVal pointEnd]
                                     (Just lastVal) -> consSegsBetween points tailVals <> [makeLineSeg lastVal pointEnd]
      where
        consSegsBetween myPoints myTailVals = (\(Slist vals _)  -> vals) $ SL.zipWith makeLineSeg myPoints myTailVals
lineSegsOfContour (LineSegContour _ _ l1 l2 moreLines@(Slist lns _))
  | size moreLines == Infinity = error "cannot handle infinite contours."
  | otherwise                  = l1:l2:lns

-- | return the contour as a list of points.
pointsOfContour :: Contour -> [Point2]
pointsOfContour (PointContour _ _ p1 p2 p3 pts@(Slist vals _))
  | size pts == Infinity = error "cannot handle infinite contours."
  | otherwise            = p1:p2:p3:vals
pointsOfContour (LineSegContour _ _ l1 l2 moreLines@(Slist lns _))
  | size moreLines == Infinity = error "cannot handle infinite contours."
  | otherwise                  = startPoint l1:startPoint l2:(startPoint <$> lns)
