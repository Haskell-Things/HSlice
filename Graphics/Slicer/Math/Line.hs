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

{- The purpose of this file is to hold line based arithmatic. -}

-- for adding Generic and NFData to Line.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.Line (Line(Line), lineFromEndpoints, makeLinesLooped, makeLines, midpoint, endpoint, pointAtZValue, pointsFromLines, flipLine) where

import Prelude ((/), (<), (>), ($), (-), otherwise, (&&), (<=), (==), Eq, length, head, tail, (++), last, init, (<$>), Show, error, null, zipWith, (<>), show, concat, Either(Left, Right))

import Data.List (nub)

import Data.Maybe (Maybe(Just, Nothing))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2, addPoints, scalePoint, zOf, flatten, (~=))

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
-- note that this means slope and endpoint are entangled. make sure to derive what you want before using slope.
data Line = Line { _point :: Point2, _slope :: Point2 }
  deriving (Generic, NFData, Show, Eq)

data LineError = LineFromPoint Point2
               | EmptyList
  deriving (Eq, Show)

-- | Create a line segment given it's endpoints.
lineFromEndpoints :: Point2 -> Point2 -> Either LineError Line
lineFromEndpoints p1 p2
  | p1 == p2 = Left $ LineFromPoint p1
  | otherwise = Right $ Line p1 (addPoints (scalePoint (-1) p1) p2)

-- | Get the endpoint of a line segment.
endpoint :: Line -> Point2
endpoint (Line p s) = addPoints p s

-- | Take a list of line segments, connected at their end points, and generate a list of the points in order.
pointsFromLines :: [Line] -> Either LineError [Point2]
pointsFromLines lines
  | null lines = Left EmptyList
  | otherwise = Right $ makePoints lines
  where
    makePoints ls = last (endpointsOf ls) : init (endpointsOf ls)
    -- FIXME: nub should not be necessary here. 
    endpointsOf :: [Line] -> [Point2]
    endpointsOf ls = nub $ endpoint <$> ls

-- | Get the midpoint of a line segment
midpoint :: Line -> Point2
midpoint (Line p s) = addPoints p (scalePoint 0.5 s)

-- | Express a line segment in terms of the other endpoint
flipLine :: Line -> Line
flipLine l@(Line _ s) = Line (endpoint l) (scalePoint (-1) s)

-- | Given a list of points (in order), construct line segments that go between them.
makeLines :: [Point2] -> [Line]
makeLines l
  | length l > 1 = res
  | otherwise = error $ "tried to makeLines a list with " <> show (length l) <> " entries.\n" <> concat (show <$> l) <> "\n"
  where
    res = zipWith consLine (init l) (tail l)
    consLine p1 p2 = errorIfLeft $ lineFromEndpoints p1 p2
    errorIfLeft :: Either LineError Line -> Line
    errorIfLeft ln = case ln of
      Left (LineFromPoint point) -> error $ "tried to construct a line from two identical points: " <> show point <> "\n" <> show l <> "\n"
      Right                 line -> line

-- | Given a list of points (in order), construct line segments that go between them. make sure to construct a line segment from the last point back to the first.
makeLinesLooped :: [Point2] -> [Line]
makeLinesLooped l
  -- too short, bail.
  | length l < 2 = error $ "tried to makeLinesLooped a list with " <> show (length l) <> " entries.\n" <> concat (show <$> l) <> "\n"
  -- already looped, use makeLines.
  | head l ~= last l = makeLines l
  -- ok, do the work and loop it.
  | otherwise = zipWith consLine l (tail l ++ l)
  where
    consLine p1 p2 = errorIfLeft $ lineFromEndpoints p1 p2
    errorIfLeft :: Either LineError Line -> Line
    errorIfLeft ln = case ln of
      Left (LineFromPoint point) -> error $ "tried to construct a line from two identical points: " <> show point <> "\n" <> show l <> "\n"
      Right                 line -> line

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

