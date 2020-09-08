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

module Graphics.Slicer.Math.Line (Line(Line), Intersection(Collinear, Parallel, HitEndpointL2, IntersectsAt, NoIntersection), lineFromEndpoints, makeLinesLooped, makeLines, point, endpoint, pointSlopeLength, midpoint, lineSlope, perpendicularBisector, pointAtZValue, flipLine, SearchDirection (Clockwise, CounterClockwise), pointsFromLines, combineLines, shortenLineBy, slope, Direction(Positive,Negative), Slope(IsOrigin, OnXAxis, OnYAxis, HasSlope)) where

import Prelude ((/), (<), (>), (*), ($), sqrt, (+), (-), otherwise, (&&), (<=), (==), Eq, length, head, tail, Bool, (++), last, init, (<$>), Show, error, negate, fst, snd, (.), null, zipWith, (<>), show, concat, (||), atan, pi)

import Data.Maybe (Maybe(Just, Nothing))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), addPoints, scalePoint, distance, magnitude, zOf, flatten)

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
-- note that this means slope and endpoint are entangled. make sure to derive what you want before using slope.
data Line = Line { point :: Point2, slope :: Point2 }
  deriving (Generic, NFData, Show)

-- a difference that makes no difference is no difference..
-- FIXME: magic numbers.
instance Eq Line where
  (==) (Line p1 m1) (Line p2 m2) = (distance p1 p2 + distance m1 m2) < 0.00001

-- FIXME: how does this handle endpoints?

-- The result of a line intersection in 2 dimensions.
data Intersection =
  Collinear
  | Parallel
  | IntersectsAt Point2
  | NoIntersection
  | HitEndpointL2 Point2
  deriving (Show)

-- | Create a line given it's endpoints.
lineFromEndpoints :: Point2 -> Point2 -> Line
lineFromEndpoints p1 p2
  | p1 == p2 = error $ "Trying to create a line from two identical points: " <> show p1 <> "\n"
  | otherwise = rawLineFromEndpoints p1 p2

-- Create a line given its endpoints, without equivalence checking.
rawLineFromEndpoints :: Point2 -> Point2 -> Line
rawLineFromEndpoints p1 p2 = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the endpoint of a line.
endpoint :: Line -> Point2
endpoint (Line p s) = addPoints p s

-- take a list of lines, connected at their end points, and generate a list of the points.
pointsFromLines :: [Line] -> [Point2]
pointsFromLines lines
  | null lines = error "no lines to make points of."
  | otherwise = (fst . pointsFromLine $ head lines) : (snd . pointsFromLine <$> lines)
  where
    pointsFromLine :: Line -> (Point2, Point2)
    pointsFromLine ln@(Line p _) = (p,endpoint ln)

-- Midpoint of a line
midpoint :: Line -> Point2
midpoint (Line p s) = addPoints p (scalePoint 0.5 s)

-- Express a line in terms of the other endpoint
flipLine :: Line -> Line
flipLine l@(Line _ s) = Line (endpoint l) (scalePoint (-1) s)

-- Given a list of points (in order), construct lines that go between them.
makeLines :: [Point2] -> [Line]
makeLines l
  | length l > 1 = zipWith lineFromEndpoints (init l) (tail l)
  | otherwise = error $ "tried to makeLines a list with " <> show (length l) <> " entries.\n" <> concat (show <$> l) <> "\n"

-- Given a list of points (in order), construct lines that go between them. make sure to construct a line from the last point back to the first.
makeLinesLooped :: [Point2] -> [Line]
makeLinesLooped l
  -- too short, bail.
  | length l < 2 = error $ "tried to makeLinesLooped a list with " <> show (length l) <> " entries.\n" <> concat (show <$> l) <> "\n"
  -- already looped, use makeLines.
  | head l == last l = makeLines l
  -- ok, do the work and loop it.
  | otherwise = zipWith lineFromEndpoints l (tail l ++ l)

data Direction =
    Positive
  | Negative
  deriving Eq

-- FIXME: try to move HasSlope to Ratio Integer
data Slope =
    IsOrigin
  | OnXAxis Direction
  | OnYAxis Direction
  | HasSlope ℝ
  deriving Eq

-- the slope of a line, on the Z plane.
lineSlope :: Point2 -> Slope
lineSlope (Point2 (x,y))
  | x == 0 && y == 0 = IsOrigin
  | x == 0 && y > 0 = OnYAxis Positive
  | x == 0 && y < 0 = OnYAxis Negative
  | x > 0 && y == 0 = OnXAxis Positive
  | x < 0 && y == 0 = OnXAxis Negative
  | otherwise = HasSlope sl
    where
      sl = y / x

-- make a new line with the given origin point, slope, and distance, with both ends in the same Z plane.
pointSlopeLength :: Point2 -> Slope -> ℝ -> Line
pointSlopeLength _  IsOrigin _ = error "trying to construct empty line?" -- Line p1 p1
pointSlopeLength p1 (OnXAxis Positive) dist = Line p1 (Point2 (dist,0))
pointSlopeLength p1 (OnXAxis Negative) dist = Line p1 (Point2 (-dist,0))
pointSlopeLength p1 (OnYAxis Positive) dist = Line p1 (Point2 (0,dist))
pointSlopeLength p1 (OnYAxis Negative) dist = Line p1 (Point2 (0,-dist))
pointSlopeLength p1 (HasSlope sl) dist = Line p1 s
  where s = scalePoint scale $ Point2 (1,yVal)
        yVal = sl
        scale = dist / sqrt (1 + yVal*yVal)

data SearchDirection = Clockwise | CounterClockwise
  deriving Show

-- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
-- if p2 == p3 and the lines are parallel.
combineLines :: Line -> Line -> Line
combineLines (Line p _) l2 = lineFromEndpoints p (endpoint l2)

-- Construct a perpendicular bisector of a line (with the same length, assuming a constant z value)
perpendicularBisector :: Line -> Line
perpendicularBisector l@(Line p s)
  | s == Point2 (0,0) = error $ "trying to bisect zero length line: " <> show l <> "\n"
  | otherwise = combineLines (flipLine (pointSlopeLength (midpoint l) m (negate ( distance p (endpoint l) / 2)))) (pointSlopeLength (midpoint l) m (distance p (endpoint l) / 2))
  where
    m = lineSlopeFlipped s

-- the slope of a line rotated by 90 degrees on the Z plane.
lineSlopeFlipped :: Point2 -> Slope
lineSlopeFlipped (Point2 (x,y))
  | x == 0 && y == 0 = IsOrigin
  | x == 0 && y > 0 = OnXAxis Positive
  | x == 0 && y < 0 = OnXAxis Negative
  | x > 0 && y == 0 = OnYAxis Positive
  | x < 0 && y == 0 = OnYAxis Negative
  | otherwise = HasSlope $ -x / y

-- Find the point on a line for a given Z value. Note that this evaluates to Nothing
-- in the case that there is no point with that Z value, or if that is the only
-- Z value present in that line. A different function is used to find plane aligned
-- lines.
pointAtZValue :: (Point3,Point3) -> ℝ -> Maybe Point2
pointAtZValue (startPoint,stopPoint) v
  -- don't bother returning a line that is axially aligned.
  | zOf startPoint == zOf stopPoint = Nothing
  | 0 <= t && t <= 1 = Just $ flatten $ addPoints lineStart (scalePoint t lineEnd)
  | otherwise = Nothing
  where
    t = (v - zOf lineStart) / zOf lineEnd
    lineEnd = (\ (Point3 (x1,y1,z1)) (Point3 (x2,y2,z2)) -> Point3 (x2-x1, y2-y1, z2-z1)) lineStart endPoint
    (lineStart,endPoint) = if zOf startPoint < zOf stopPoint
                           then (startPoint,stopPoint)
                           else (stopPoint,startPoint)

-- shorten line by an amount in millimeters on each end
shortenLineBy :: ℝ -> Line -> Line
shortenLineBy amt line = Line newStart newSlope
  where pct = amt / magnitude (slope line)
        newStart = addPoints (point line) $ scalePoint pct (slope line)
        newSlope = scalePoint (1 - 2 * pct) (slope line)
                                                    
