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
  canonicalizePPoint2,
  distanceBetweenPPoints,
  distanceBetweenPPointsWithErr,
  distancePPointToPLine,
  distancePPointToPLineWithErr,
  eToPLine2,
  getFirstArc,
  getOutsideArc,
  pPointBetweenPPoints,
  pPointOnPerp,
  pToEPoint2,
  translatePLine2,
  translateRotatePPoint2
  ) where

import Prelude (($), fst, mempty)

-- The numeric type in HSlice.
import Graphics.Slicer.Definitions (ℝ)

import qualified Graphics.Slicer.Math.Arcs as Arcs (getFirstArc, getOutsideArc)

import Graphics.Slicer.Math.Definitions (LineSeg, Point2)

import Graphics.Slicer.Math.PGA (ProjectiveLine, ProjectiveLine2, ProjectivePoint, ProjectivePoint2, PPoint2Err, PLine2Err, canonicalizeP, distance2PP, distancePPToPL, eToPL, interpolate2PP, pPointOnPerpWithErr, pToEP, translateL, translateRotatePPoint2WithErr)

-- | Canonicalize a projective point.
canonicalizePPoint2 :: (ProjectivePoint2 a) => a -> ProjectivePoint
canonicalizePPoint2 point = fst $ canonicalizeP point

-- | Find the distance between two projective points.
distanceBetweenPPoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ
distanceBetweenPPoints point1 point2 = fst $ distance2PP (point1, mempty) (point2, mempty)

-- | Find the distance between two projective points, with attached error quotents.
distanceBetweenPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => (a, PPoint2Err) -> (b, PPoint2Err) -> ℝ
distanceBetweenPPointsWithErr point1 point2 = fst $ distance2PP point1 point2

-- | Find the distance between a point and a line.
distancePPointToPLine :: (ProjectivePoint2 a, ProjectiveLine2 b) => a -> b -> ℝ
distancePPointToPLine point line = fst $ distancePPToPL (point, mempty) (line, mempty)

-- | Find the distance between a point and a line, both with error quotents.
distancePPointToPLineWithErr :: (ProjectivePoint2 a, ProjectiveLine2 b) => (a, PPoint2Err) -> (b, PLine2Err) -> ℝ
distancePPointToPLineWithErr point line = fst $ distancePPToPL point line

-- | Create an un-normalized projective line from a euclidian line segment.
eToPLine2 :: LineSeg -> ProjectiveLine
eToPLine2 l1 = fst $ eToPL l1

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of a line segment from p1 to p2, and a line segment from p3 to p2.
getFirstArc :: Point2 -> Point2 -> Point2 -> ProjectiveLine
getFirstArc p1 p2 p3 = fst $ Arcs.getFirstArc p1 p2 p3

-- | Get a PLine in the direction of the out of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
getOutsideArc :: (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> ProjectiveLine
getOutsideArc a b c d = fst $ Arcs.getOutsideArc a b c d

-- | Find a point somewhere along the line between the two points given.
--   Requires two weights. the ratio of these weights determines the position of the found points, E.G: (2/3,1/3) is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint. weights can sum to anything.
pPointBetweenPPoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> ProjectivePoint
pPointBetweenPPoints startOfSeg stopOfSeg weight1 weight2 = fst $ interpolate2PP startOfSeg stopOfSeg weight1 weight2

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: (ProjectiveLine2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ProjectivePoint
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

pToEPoint2 :: (ProjectivePoint2 a) => a -> Point2
pToEPoint2 ppoint = fst $ pToEP ppoint

-- | Translate a projective line along it's perpendicular bisector.
translatePLine2 :: (ProjectiveLine2 a) => a -> ℝ -> ProjectiveLine
translatePLine2 pline distance = fst $ translateL pline distance

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: (ProjectivePoint2 a) => a -> ℝ -> ℝ -> ProjectivePoint
translateRotatePPoint2 ppoint d rotation = fst $ translateRotatePPoint2WithErr ppoint d rotation
