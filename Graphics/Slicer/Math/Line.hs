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

module Graphics.Slicer.Math.Line (Line(Line), point, slope, lineIntersection, lineFromEndpoints, pointsFromLines, endpoint, midpoint, flipLine, pointSlopeLength, combineLines, canCombineLines, perpendicularBisector, pointAtZValue, shortenLineBy, makeLines, makeLinesLooped, lineSlope, Direction(Positive, Negative), Slope(IsOrigin, OnXAxis, OnYAxis, HasSlope), combineConsecutiveLines) where

import Prelude ((/), (<), (>), (*), ($), sqrt, (+), (-), otherwise, (&&), (<=), (==), Eq, length, head, tail, Bool(False), (/=), (++), last, init, (<$>), Show, error, negate, fst, snd, (.), null, zipWith, (<>), show, concat)

import Data.Maybe (Maybe(Just, Nothing))

import Data.List.Ordered (foldt)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point(Point), addPoints, scalePoint, distance, magnitude)

import Graphics.Slicer.Math.Point (twoDCrossProduct)

--import Graphics.Slicer.Formats.GCode.Definitions (roundPoint, roundToFifth)

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
-- note that this means slope and endpoint are entangled. make sure to derive what you want before using slope.
data Line = Line { point :: Point, slope :: Point }
  deriving (Generic, NFData, Show)

-- a difference that makes no difference is no difference..
-- FIXME: magic numbers.
instance Eq Line where
  (==) (Line p1 m1) (Line p2 m2) = (distance p1 p2) + (distance m1 m2) < 0.00001

-- FIXME: how does this handle endpoints?
-- Line intersection algorithm from http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
lineIntersection :: Line -> Line -> Maybe Point
lineIntersection (Line p r) (Line q s)
  | twoDCrossProduct r s == 0 = Nothing
  | 0 <= t && t <= 1 && 0 <= u && u <= 1 = Just (addPoints p (scalePoint t r))
  | otherwise = Nothing
  where t = twoDCrossProduct (addPoints q (scalePoint (-1) p)) s / twoDCrossProduct r s
        u = twoDCrossProduct (addPoints q (scalePoint (-1) p)) r / twoDCrossProduct r s

-- Create a line given its endpoints
lineFromEndpoints :: Point -> Point -> Line
lineFromEndpoints p1 p2
  | p1 == p2 = error $ "creating an empty line for point: " <> show p1 <> "\n"
  | otherwise = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the other endpoint
endpoint :: Line -> Point
endpoint l = addPoints (point l) (slope l)

-- take a list of lines, connected at their end points, and generate a list of the points.
pointsFromLines :: [Line] -> [Point]
pointsFromLines lines
  | null lines = [] -- error "found no inner points for contour."
  | otherwise = (fst . pointsFromLine $ head lines) : (snd . pointsFromLine <$> lines)
  where
    pointsFromLine :: Line -> (Point, Point)
    pointsFromLine ln@(Line p _) = (p,endpoint ln)

-- Midpoint of a line
midpoint :: Line -> Point
midpoint (Line p s) = addPoints p (scalePoint 0.5 s)

-- Express a line in terms of the other endpoint
flipLine :: Line -> Line
flipLine l@(Line _ s) = Line (endpoint l) (scalePoint (-1) s)

-- Given a list of points (in order), construct lines that go between them.
makeLines :: [Point] -> [Line]
makeLines l
  | length l > 1 = zipWith lineFromEndpoints (init l) (tail l)
  | otherwise = error $ "tried to makeLines a list with " <> show (length l) <> " entries.\n" <> (concat $ show <$> l) <> "\n"

-- Given a list of points (in order), construct lines that go between them. make sure to construct a line from the last point back to the first.
makeLinesLooped :: [Point] -> [Line]
makeLinesLooped l
  -- too short, bail.
  | length l < 3 = []
  -- already looped, use makeLines.
  | head l == last l = makeLines l
  -- ok, do the work and loop it.
  | otherwise = zipWith lineFromEndpoints l ((tail l) ++ l)

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
lineSlope :: Point -> Slope
lineSlope (Point (x,y,_))
  | x == 0 && y == 0 = IsOrigin
  | x == 0 && y > 0 = OnYAxis Positive
  | x == 0 && y < 0 = OnYAxis Negative
  | x > 0 && y == 0 = OnXAxis Positive
  | x < 0 && y == 0 = OnXAxis Negative
  | otherwise = HasSlope $ sl
    where
      sl = y / x

-- make a new line with the given origin point, slope, and distance, with both ends in the same Z plane.
pointSlopeLength :: Point -> Slope -> ℝ -> Line
pointSlopeLength _  IsOrigin _ = error "trying to construct empty line?" -- Line p1 p1
pointSlopeLength p1 (OnXAxis Positive) dist = Line p1 (Point (dist,0,0))
pointSlopeLength p1 (OnXAxis Negative) dist = Line p1 (Point (-dist,0,0))
pointSlopeLength p1 (OnYAxis Positive) dist = Line p1 (Point (0,dist,0))
pointSlopeLength p1 (OnYAxis Negative) dist = Line p1 (Point (0,-dist,0))
pointSlopeLength p1 (HasSlope sl) dist = Line p1 s
  where s = scalePoint scale $ Point (1,yVal,0)
        yVal = sl
        scale = dist / sqrt (1 + yVal*yVal)

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
    combineEnds  []      = []
    combineEnds  [l1]    = [l1]
    combineEnds  (l1:ls)
      | length ls > 1 = if canCombineLines (last ls) l1 then init ls ++ [combineLines (last ls) l1] else l1:ls
      | otherwise = error "one length ls"

-- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
-- if p2 == p3 and the lines are parallel (see canCombineLines)
combineLines :: Line -> Line -> Line
combineLines (Line p _) l2 = lineFromEndpoints p (endpoint l2)

-- Determine if two lines can be combined
canCombineLines :: Line -> Line -> Bool
canCombineLines l1@(Line _ s1) (Line p2 s2)
  | lineSlope s1 /= lineSlope s2 = False
  | otherwise = endpoint l1 == p2

-- Construct a perpendicular bisector of a line (with the same length, assuming a constant z value)
perpendicularBisector :: Line -> Line
perpendicularBisector l@(Line p s)
  | s == Point (0,0,0) = error $ "trying to bisect zero length line: " <> show l <> "\n"
  | otherwise = combineLines (flipLine (pointSlopeLength (midpoint l) m (negate ( distance p (endpoint l) / 2)))) (pointSlopeLength (midpoint l) m (distance p (endpoint l) / 2))
  where
    m = lineSlopeFlipped s

-- the slope of a line rotated by 90 degrees on the Z plane.
lineSlopeFlipped :: Point -> Slope
lineSlopeFlipped (Point (x,y,_))
  | x == 0 && y == 0 = IsOrigin
  | x == 0 && y > 0 = OnXAxis Positive
  | x == 0 && y < 0 = OnXAxis Negative
  | x > 0 && y == 0 = OnYAxis Positive
  | x < 0 && y == 0 = OnYAxis Negative
  | otherwise = HasSlope $ -x / y

-- Find the point on a line for a given Z value. Note that this evaluates to Nothing
-- in the case that there is no point with that Z value, or if that is the only
-- Z value present in that line. The latter should be okay because the properties
-- of our meshes mean that the two endpoints of our line should be captured by
-- the other two segments of a triangle.
pointAtZValue :: Line -> ℝ -> Maybe Point
pointAtZValue (Line p m) v
  | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
  | otherwise = Nothing
  where
    t = (v - zOf p) / zOf m
    zOf :: Point ->  ℝ
    zOf (Point (_,_,z)) = z

-- shorten line by an amount in millimeters on each end
shortenLineBy :: ℝ -> Line -> Line
shortenLineBy amt line = Line newStart newSlope
  where pct = amt / magnitude (slope line)
        newStart = addPoints (point line) $ scalePoint pct (slope line)
        newSlope = scalePoint (1 - 2 * pct) (slope line)
                                                    
