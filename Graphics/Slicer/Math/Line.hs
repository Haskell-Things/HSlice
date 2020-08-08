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

module Graphics.Slicer.Math.Line (Line(Line), point, slope, lineIntersection, lineFromEndpoints, rawLineFromEndpoints, pointsFromLines, endpoint, midpoint, flipLine, pointSlopeLength, combineLines, canCombineLines, perpendicularBisector, pointAtZValue, shortenLineBy, makeLines, makeLinesLooped, lineSlope, Direction(Positive, Negative), Slope(IsOrigin, OnXAxis, OnYAxis, HasSlope), combineConsecutiveLines, Intersection (IntersectsAt, NoIntersection, Parallel, HitEndpointL1, HitEndpointL2, Collinear), angleOf, SearchDirection(Clockwise,CounterClockwise), lineBetween) where

import Prelude ((/), (<), (>), (*), ($), sqrt, (+), (-), otherwise, (&&), (<=), (==), Eq, length, head, tail, Bool(False), (/=), (++), last, init, (<$>), Show, error, negate, fst, snd, (.), null, zipWith, (<>), show, concat, (||), atan, pi)

import Data.Maybe (Maybe(Just, Nothing))

import Data.List.Ordered (foldt)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), addPoints, scalePoint, distance, magnitude, zOf, flatten, roundToFifth, (~=))

import Graphics.Slicer.Math.Point (twoDCrossProduct)

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
  | IntersectsAt Point2 Point2
  | NoIntersection
  | HitEndpointL1 Point2
  | HitEndpointL2 Point2
  | ShareEndpoint
  deriving (Show)

-- Line intersection algorithm from http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
lineIntersection :: Line -> Line -> Intersection
lineIntersection (Line p r) (Line q s)
  -- the two lines are collinear
  | twoDCrossProduct r s == 0 &&
    twoDCrossProduct (addPoints q (scalePoint (-1) p)) r == 0 = Collinear
  -- the two lines are parallel, and non-interserting
  | twoDCrossProduct r s == 0 &&
    twoDCrossProduct (addPoints q (scalePoint (-1) p)) r /= 0 = Parallel
  -- the two lines have an intersection
  | 0 < t && t < 1 && 0 < u && u < 1 = IntersectsAt (addPoints p (scalePoint t r)) (addPoints q (scalePoint u s))
  -- the intersection lies at one of the endpoints of the first line.
  | t==0 || t==1 = HitEndpointL1 (addPoints q (scalePoint u s))
  -- the intersection lies at one of the endpoints of the second line.
  | u==0 || u==1 = HitEndpointL2 (addPoints p (scalePoint t r))
  | otherwise = NoIntersection
  where t = twoDCrossProduct (addPoints q (scalePoint (-1) p)) s / twoDCrossProduct r s
        u = twoDCrossProduct (addPoints q (scalePoint (-1) p)) r / twoDCrossProduct r s

-- | Create a line given it's endpoints.
lineFromEndpoints :: Point2 -> Point2 -> Line
lineFromEndpoints p1 p2
  | p1 == p2 = error $ "Trying to create a line from two identical points: " <> show p1 <> "\n"
  | otherwise = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Create a line given its endpoints, without equivalence checking.
rawLineFromEndpoints :: Point2 -> Point2 -> Line
rawLineFromEndpoints p1 p2 = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the other endpoint
endpoint :: Line -> Point2
endpoint l = addPoints (point l) (slope l)

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
-- given two lines with the same origin, start at the first one, and search in the given direction for the second one. if we hit it before we run into the third line (from the same origin), return true.
lineBetween :: Line -> SearchDirection -> Line -> Line -> Bool
lineBetween l1 CounterClockwise l2 l3
  | angleOf l3 > angleOf l1 = angleOf l3 > angleOf l2 && angleOf l2 > angleOf l1
  | angleOf l3 < angleOf l1 = angleOf l3 > angleOf l2 || angleOf l2 > angleOf l1
lineBetween l1 Clockwise l2 l3
  | angleOf l3 < angleOf l1 = angleOf l3 > angleOf l2 && angleOf l2 > angleOf l1
  | angleOf l3 > angleOf l1 = angleOf l3 > angleOf l2 || angleOf l2 > angleOf l1
lineBetween l1 dir l2 l3 = error $ "impossible situation: " <> show l1 <> " " <> show l2 <> " " <> show l3 <> " " <> show dir <> "\n"

-- given a line, determine the angle it is at, in radians. 
angleOf :: Line -> ℝ
angleOf (Line _ m)
  | lineSlope m == IsOrigin = error "tried to get the angle of a point."
  | lineSlope m == OnXAxis Positive = 0
  | lineSlope m == OnYAxis Positive = pi/2
  | lineSlope m == OnXAxis Negative = pi
  | lineSlope m == OnYAxis Negative = pi*1.5
  | x>0 && y>0 = atan (slopeOf $ lineSlope m)
  | x>0 && y<0 = atan (slopeOf $ lineSlope $ Point2 (-y, x)) + pi/2
  | x<0 && y<0 = atan (slopeOf $ lineSlope $ Point2 (-x, -y)) + pi
  | x<0 && y>0 = atan (slopeOf $ lineSlope $ Point2 (y, -x)) + (pi*1.5)
  | otherwise = error "unknown condition"
  where
    (x,y) = (\(Point2 p) -> p) m
    slopeOf (HasSlope sl) = sl
    slopeOf _             = error "can not happen."

-- Combine consecutive lines. expects lines with their end points connecting, EG, a contour generated by makeContours.
combineConsecutiveLines :: [Line] -> [Line]
combineConsecutiveLines lines
  | length lines > 1 = combineEnds $ foldt combine [last lines] ((:[]) <$> init lines)
  | otherwise = lines
  where
    combine :: [Line] -> [Line] -> [Line]
    combine  l1       [] = l1
    combine  []       l2 = l2
    combine [l1] [l2]    = if canCombineLines l1 l2 then [combineLines l1 l2] else l1 : [l2]
    combine [l1] (l2:ls) = if canCombineLines l1 l2 then combineLines l1 l2 : ls else l1:l2:ls
    combine  l1  [l2]    = if canCombineLines (last l1) l2 then init l1 ++ [combineLines (last l1) l2] else l1 ++ [l2]
    combine  l1  (l2:ls) = if canCombineLines (last l1) l2 then init l1 ++ combineLines (last l1) l2 : ls else l1 ++ l2:ls
    combineEnds :: [Line] -> [Line]
    combineEnds  []      = []
    combineEnds  [l1]    = [l1]
    combineEnds  (l1:ls)
      | length ls > 1 = if canCombineLines (last ls) l1 then init ls ++ [combineLines (last ls) l1] else l1:ls
      | otherwise = combine [l1] ls

-- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
-- if p2 == p3 and the lines are parallel (see canCombineLines)
combineLines :: Line -> Line -> Line
combineLines (Line p _) l2 = lineFromEndpoints p (endpoint l2)

-- Determine if two lines can be combined
canCombineLines :: Line -> Line -> Bool
canCombineLines l1@(Line _ s1) (Line p2 s2)
  | compareSlopes (lineSlope s1) (lineSlope s2) = endpoint l1 ~= p2
  | otherwise = False
  where
    compareSlopes sl1 sl2 =
      case sl1 of
        (HasSlope _) ->
          case sl2 of
            (HasSlope _) -> roundToFifth (slopeOf sl1) == roundToFifth (slopeOf sl2)
            _            -> False
        _    -> sl1 == sl2
    slopeOf (HasSlope sl) = sl
    slopeOf _ = error "impossible"

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
                                                    
