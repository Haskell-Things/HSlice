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

-- for adding Generic and NFData to LineSeg.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | The purpose of this file is to hold line segment rithmatic. really, we used to have a linear algebra implementation here, before we moved to PGA.
module Graphics.Slicer.Math.Line (LineSeg(LineSeg), LineSegError(LineSegFromPoint), lineSegFromEndpoints, makeLineSegs, midpoint, endpoint, pointAtZValue, pointsFromLineSegs, flipLineSeg, combineLineSegs, handleLineSegError) where

import Prelude ((/), (<), ($), (-), otherwise, (&&), (<=), (==), Eq, (<$>), Show, error, zipWith, (<>), show, Either(Left, Right))

import Data.Either (fromRight)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just, Nothing))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2, addPoints, scalePoint, zOf, flatten)

-- | Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
-- note that this means slope and endpoint are entangled. make sure to derive what you want before using slope.
data LineSeg = LineSeg { _point :: !Point2, _distanceToEnd :: !Point2 }
  deriving (Generic, NFData, Show, Eq)

-- | Possible errors from lineSegFromEndpoints.
data LineSegError = LineSegFromPoint !Point2
                  | EmptyList
  deriving (Eq, Show)

-- | Create a line segment given it's endpoints.
lineSegFromEndpoints :: Point2 -> Point2 -> Either LineSegError LineSeg
lineSegFromEndpoints p1 p2
  | p1 == p2 = Left $ LineSegFromPoint p1
  | otherwise = Right $ LineSeg p1 (addPoints (scalePoint (-1) p1) p2)

-- | generic handler for the error conditions of lineSegFromEndpoints
handleLineSegError :: Either LineSegError LineSeg -> LineSeg
handleLineSegError ln = case ln of
      Left (LineSegFromPoint point) -> error $ "tried to construct a line segment from two identical points: " <> show point <> "\n"
      Left EmptyList                -> error "tried to construct a line segment from an empty list."
      Right                    line -> line

-- | Get the endpoint of a line segment.
endpoint :: LineSeg -> Point2
endpoint (LineSeg p s) = addPoints p s

-- | Take a list of line segments, connected at their end points, and generate a list of the points in order.
pointsFromLineSegs :: [LineSeg] -> Either LineSegError [Point2]
pointsFromLineSegs lineSegs =  case unsnoc $ endpointsOf lineSegs of
                                 Nothing -> Left EmptyList
                                 Just (i,l) -> Right $ l: i
  where
    endpointsOf :: [LineSeg] -> [Point2]
    endpointsOf ls = endpoint <$> ls

-- | Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). Only call this if p2 == p3 and the lines are really close to parallel
combineLineSegs :: LineSeg -> LineSeg -> Maybe LineSeg
combineLineSegs l1@(LineSeg p _) l2@(LineSeg p1 s1) = if endpoint l2 == p -- If line 2 ends where line 1 begins:
                                                      then Nothing -- handle a contour that loops back on itsself.
                                                      else Just $ fromRight (error $ "cannot combine lines: " <> show l1 <> "\n" <> show l2 <> "\n") $ lineSegFromEndpoints p (addPoints p1 s1)

-- | Get the midpoint of a line segment
midpoint :: LineSeg -> Point2
midpoint (LineSeg p s) = addPoints p (scalePoint 0.5 s)

-- | Express a line segment in terms of the other endpoint
flipLineSeg :: LineSeg -> LineSeg
flipLineSeg l@(LineSeg _ s) = LineSeg (endpoint l) (scalePoint (-1) s)

-- | Given a list of points (in order), construct line segments that go between them.
makeLineSegs :: [Point2] -> [LineSeg]
makeLineSegs points = case points of
                        [] -> error "tried to makeLineSegs a list with no points."
                        [p] -> error $ "tried to makeLineSegs a list with only one point: " <> show p <> "\n"
                        (_h:t) -> zipWith consLineSeg points t
  where
    consLineSeg p1 p2 = handleLineSegError $ lineSegFromEndpoints p1 p2

-- | Find the point where a line segment intersects the plane at a given z height.
--   Note that this evaluates to Nothing in the case that there is no point in the line segment that Z value, or if the line segment is z aligned.
--   Note that a different function is used to find plane aligned line segments.
pointAtZValue :: (Point3,Point3) -> ℝ -> Maybe Point2
pointAtZValue (startPoint,stopPoint) v
  -- don't bother returning a line segment that is axially aligned.
  | zOf startPoint == zOf stopPoint = Nothing
  | 0 <= t && t <= 1 = Just $ flatten $ addPoints lineStart (scalePoint t lineEnd)
  | otherwise = Nothing
  where
    t = (v - zOf lineStart) / zOf lineEnd
    lineEnd = (\ (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) -> Point3 (x2-x1, y2-y1, z2-z1)) lineStart endPoint
    (lineStart,endPoint) = if zOf startPoint < zOf stopPoint
                           then (startPoint,stopPoint)
                           else (stopPoint,startPoint)

