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

-- | The purpose of this file is to hold line segment arithmatic. really, we used to have a linear algebra implementation here, before we moved to PGA.
module Graphics.Slicer.Math.Line (makeLineSeg, makeLineSegs, pointAtZValue, flipLineSeg, combineLineSegs) where

import Prelude ((/), (<), ($), (-), otherwise, (&&), (<=), (==), error, zipWith, (<>), show)

import Data.Maybe (Maybe(Just, Nothing))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point3(Point3), LineSeg(LineSeg), Point2, addPoints, scalePoint, zOf, flatten, makeLineSeg)

-- | Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). Only call this if p2 == p3 and the lines are really close to colinear
combineLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
combineLineSegs (LineSeg s1 _) (LineSeg _ e2) = if s1 == e2 -- If line 2 ends where line 1 begins:
                                                then Nothing -- handle a contour that loops back on itsself.
                                                else Just $ LineSeg s1 e2

-- | Express a line segment in terms of the other endpoint
flipLineSeg :: LineSeg -> LineSeg
flipLineSeg (LineSeg s e) = LineSeg e s

-- | Given a list of points (in order), construct line segments that go between them.
makeLineSegs :: [Point2] -> [LineSeg]
makeLineSegs points = case points of
                        [] -> []
                        [p] -> error $ "tried to makeLineSegs a list with only one point: " <> show p <> "\n"
                        (_h:t) -> zipWith makeLineSeg points t

-- | Find the point where a line segment intersects the plane at a given z height.
--   Note that this evaluates to Nothing in the case that there is no point in the line segment that Z value, or if the line segment is z aligned.
--   Note that a different function is used to find plane aligned line segments.
pointAtZValue :: (Point3,Point3) -> ℝ -> Maybe Point2
pointAtZValue (startPoint,stopPoint) v
  -- don't bother returning a line segment that is axially aligned.
  | zOf startPoint == zOf stopPoint = Nothing
  | 0 <= t && t <= 1 = Just $ flatten $ addPoints lineSegStart (scalePoint t lineEnd)
  | otherwise = Nothing
  where
    t = (v - zOf lineSegStart) / zOf lineEnd
    lineEnd = (\ (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) -> Point3 (x2-x1, y2-y1, z2-z1)) lineSegStart lineSegEnd
    (lineSegStart,lineSegEnd) = if zOf startPoint < zOf stopPoint
                                then (startPoint,stopPoint)
                                else (stopPoint,startPoint)
