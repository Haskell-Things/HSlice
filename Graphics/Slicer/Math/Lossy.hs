{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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

-- | The purpose of this file is to provide versions of PGA functionality that are known to be lossy.

module Graphics.Slicer.Math.Lossy (
  angleBetween,
  canonicalizePPoint2,
  distanceBetweenPPoints,
  distanceBetweenNPLine2s,
  distanceCPPointToNPLine,
  distancePPointToPLine,
  eToCPPoint2,
  eToNPLine2,
  eToPLine2,
  eToPPoint2,
  getFirstArc,
  getInsideArc,
  join2CPPoint2,
  join2PPoint2,
  makePPoint2,
  makeCPPoint2,
  normalizePLine2,
  pLineFromEndpoints,
  pPointBetweenPPoints,
  pPointOnPerp,
  translatePLine2
  ) where

import Prelude (($), fst)

-- The numeric type in HSlice.
import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (LineSeg, Point2)

import Graphics.Slicer.Math.PGA (CPPoint2(CPPoint2), NPLine2, PLine2, PPoint2(PPoint2), ProjectiveLine2, ProjectivePoint2, angleBetweenWithErr, canonicalize, distanceBetweenPPointsWithErr, distanceBetweenNPLine2sWithErr, distanceCPPointToNPLineWithErr, distancePPointToPLineWithErr, eToCPPoint2WithErr, eToPLine2WithErr, eToPPoint2WithErr, getFirstArcWithErr, getInsideArcWithErr, join2CPPoint2WithErr, join2PPoint2WithErr, makeCPPoint2WithErr, normalize, pLineFromEndpointsWithErr, pPointBetweenPPointsWithErr, pPointOnPerpWithErr, translatePLine2WithErr)

angleBetween :: NPLine2 -> NPLine2 -> ℝ
angleBetween nPLine1 nPLine2 = fst $ angleBetweenWithErr nPLine1 nPLine2

-- | canonicalize a euclidian point.
canonicalizePPoint2 :: PPoint2 -> CPPoint2
canonicalizePPoint2 point = fst $ canonicalize point

distanceBetweenPPoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ
distanceBetweenPPoints point1 point2 = fst $ distanceBetweenPPointsWithErr point1 point2

distanceBetweenNPLine2s :: NPLine2 -> NPLine2 -> ℝ
distanceBetweenNPLine2s nPLine1 nPLine2 = fst $ distanceBetweenNPLine2sWithErr nPLine1 nPLine2

-- | Find the unsigned distance between a point and a line.
distanceCPPointToNPLine :: CPPoint2 -> NPLine2 -> ℝ
distanceCPPointToNPLine point line = fst $ distanceCPPointToNPLineWithErr point line

-- | Find the unsigned distance between a point and a line.
distancePPointToPLine :: PPoint2 -> PLine2 -> ℝ
distancePPointToPLine point line = fst $ distancePPointToPLineWithErr point line

-- | Create a projective point from a euclidian point.
eToCPPoint2 :: Point2 -> CPPoint2
eToCPPoint2 point = fst $ eToCPPoint2WithErr point

-- | Create a normalized projective line from a euclidian line segment.
eToNPLine2 :: LineSeg -> NPLine2
eToNPLine2 l1 = fst $ normalize $ fst $ eToPLine2WithErr l1

-- | Create an un-normalized projective line from a euclidian line segment.
eToPLine2 :: LineSeg -> PLine2
eToPLine2 l1 = fst $ eToPLine2WithErr l1

-- | Create a projective point from a euclidian point.
eToPPoint2 :: Point2 -> PPoint2
eToPPoint2 point = fst $ eToPPoint2WithErr point

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
getFirstArc :: Point2 -> Point2 -> Point2 -> PLine2
getFirstArc p1 p2 p3 = fst $ getFirstArcWithErr p1 p2 p3

getInsideArc :: PLine2 -> PLine2 -> PLine2
getInsideArc pl1 pl2 = fst $ getInsideArcWithErr pl1 pl2

-- | a typed join function. join two points, returning a line.
join2PPoint2 :: PPoint2 -> PPoint2 -> PLine2
join2PPoint2 pp1 pp2 = fst $ join2PPoint2WithErr pp1 pp2

-- | a typed join function. join two points, returning a line.
join2CPPoint2 :: CPPoint2 -> CPPoint2 -> PLine2
join2CPPoint2 pp1 pp2 = fst $ join2CPPoint2WithErr pp1 pp2

-- | create a canonical euclidian projective point from the given coordinates.
--   Really, just wraps makeCPPoint2WithErr
makeCPPoint2 :: ℝ -> ℝ -> CPPoint2
makeCPPoint2 x y = fst $ makeCPPoint2WithErr x y

-- | create a euclidian projective point from the given coordinates.
--   Really, just wraps makeCPPoint2WithErr
makePPoint2 :: ℝ -> ℝ -> PPoint2
makePPoint2 x y = (\(CPPoint2 p) -> PPoint2 p) $ fst $ makeCPPoint2WithErr x y

-- | Normalize a PLine2.
normalizePLine2 :: (ProjectiveLine2 a) => a -> NPLine2
normalizePLine2 pl = fst $ normalize pl

-- | Create a projective line from a pair of euclidian points.
pLineFromEndpoints :: Point2 -> Point2 -> PLine2
pLineFromEndpoints point1 point2 = fst $ pLineFromEndpointsWithErr point1 point2

-- | Find a point somewhere along the line between the two points given.
--  requires two weights. the ratio of these weights determines the position of the found points, E.G: (2/3,1/3) is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint. weights can sum to anything.
pPointBetweenPPoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> PPoint2
pPointBetweenPPoints startOfSeg stopOfSeg weight1 weight2 = fst $ pPointBetweenPPointsWithErr startOfSeg stopOfSeg weight1 weight2

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: PLine2 -> PPoint2 -> ℝ -> PPoint2
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

-- | translate a PLine2 along it's perpendicular bisector.
translatePLine2 :: PLine2 -> ℝ -> PLine2
translatePLine2 pline distance = fst $ translatePLine2WithErr pline distance
