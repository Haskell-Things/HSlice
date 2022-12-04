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
  distanceBetweenPPoints,
  distancePPointToPLine,
  eToPLine2,
  getFirstArc,
  getOutsideArc,
  join2PPoints,
  normalizePLine2,
  pLineFromEndpoints,
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

import Graphics.Slicer.Math.Definitions (LineSeg, Point2, makeLineSeg)

import Graphics.Slicer.Math.PGA (distance2PP, distancePPointToPLineWithErr, eToPL, interpolate2PP, join2PP, pPointOnPerpWithErr, pToEP, translateL, translateRotatePPoint2WithErr)

import Graphics.Slicer.Math.PGAPrimitives (ProjectiveLine, ProjectiveLine2(normalizeL), ProjectivePoint, ProjectivePoint2, PPoint2Err, PLine2Err)

distanceBetweenPPoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ
distanceBetweenPPoints point1 point2 = fst $ distance2PP (point1, mempty) (point2, mempty)

-- | Find the unsigned distance between a point and a line.
distancePPointToPLine :: (ProjectivePoint,PPoint2Err) -> (ProjectiveLine,PLine2Err) -> ℝ
distancePPointToPLine point line = fst $ distancePPointToPLineWithErr point line

-- | Create an un-normalized projective line from a euclidian line segment.
eToPLine2 :: LineSeg -> ProjectiveLine
eToPLine2 l1 = fst $ eToPL l1

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
getFirstArc :: Point2 -> Point2 -> Point2 -> ProjectiveLine
getFirstArc p1 p2 p3 = fst $ Arcs.getFirstArc p1 p2 p3

getOutsideArc :: (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> ProjectiveLine
getOutsideArc a b c d = fst $ Arcs.getOutsideArc a b c d

-- | a typed join function. join two points, returning a line.
join2PPoints :: ProjectivePoint -> ProjectivePoint -> ProjectiveLine
join2PPoints pp1 pp2 = fst $ join2PP pp1 pp2

-- | Normalize a ProjectiveLine.
normalizePLine2 :: ProjectiveLine -> ProjectiveLine
normalizePLine2 pl = fst $ normalizeL pl

-- | Create a projective line from a pair of euclidian points.
pLineFromEndpoints :: Point2 -> Point2 -> ProjectiveLine
pLineFromEndpoints point1 point2 = eToPLine2 $ makeLineSeg point1 point2

-- | Find a point somewhere along the line between the two points given.
--  requires two weights. the ratio of these weights determines the position of the found points, E.G: (2/3,1/3) is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint. weights can sum to anything.
pPointBetweenPPoints :: ProjectivePoint -> ProjectivePoint -> ℝ -> ℝ -> ProjectivePoint
pPointBetweenPPoints startOfSeg stopOfSeg weight1 weight2 = fst $ interpolate2PP startOfSeg stopOfSeg weight1 weight2

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: ProjectiveLine -> ProjectivePoint -> ℝ -> ProjectivePoint
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

pToEPoint2 :: ProjectivePoint -> Point2
pToEPoint2 ppoint = fst $ pToEP ppoint

-- | translate a ProjectiveLine along it's perpendicular bisector.
translatePLine2 :: ProjectiveLine -> ℝ -> ProjectiveLine
translatePLine2 pline distance = fst $ translateL pline distance

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: ProjectivePoint -> ℝ -> ℝ -> ProjectivePoint
translateRotatePPoint2 ppoint d rotation = fst $ translateRotatePPoint2WithErr ppoint d rotation
