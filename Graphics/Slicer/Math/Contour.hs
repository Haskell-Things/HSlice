-- Slicer.
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

module Graphics.Slicer.Math.Contour (followingLineSeg, preceedingLineSeg, getContours, makeContourTree, ContourTree(ContourTree), contourContainsContour) where

import Prelude ((==), otherwise, (.), null, (<$>), ($), (>), length, Show, filter, (/=), odd, snd, error, (<>), show, fst, Bool(True,False), Eq, Show, not, compare, (<>), (<), (/), floor)

import Data.List(tail, last, head, partition, reverse, sortBy)

import Data.Maybe(Maybe(Just,Nothing), catMaybes, mapMaybe)

import Data.Either (fromRight)

import Graphics.Slicer.Math.Definitions (Contour(PointSequence), Point2(Point2), mapWithNeighbors)

import Graphics.Slicer.Math.Line (LineSeg, lineSegFromEndpoints, makeLineSegsLooped, makeLineSegs, endpoint, midpoint)

import Graphics.Slicer.Math.PGA (Intersection(NoIntersection, IntersectsAt, Parallel, AntiParallel, HitStartPointL2, HitEndPointL2, Colinear), lineIntersection, lineIsLeft, pointOnPerp, distancePPointToPLine, eToPPoint2, PLine2, plinesIntersectIn, PIntersection(PColinear,IntersectsIn,PParallel,PAntiParallel), eToPLine2)

import Graphics.Implicit.Definitions (ℝ, Fastℕ)

-- Unapologetically ripped from ImplicitCAD.
-- Added the ability to look at line segments backwards.

-- | The goal of getLoops is to extract loops from a list of segments.
--   The input is a list of segments.
--   The output a list of loops, where each loop is a list of
--   segments, which each piece representing a "side".

-- For example:
-- Given points [[1,2],[5,1],[3,4,5], ... ]
-- notice that there is a loop 1,2,3,4,5... <repeat>
-- But we give the output [ [ [1,2], [3,4,5], [5,1] ], ... ]
-- so that we have the loop, and also knowledge of how
-- the list is built (the "sides" of it).

getLoops :: (Show a, Eq a) => [[a]] -> [[[a]]]
getLoops a = getLoops' a []

-- We will be actually doing the loop extraction with
-- getLoops'

-- | getLoops' has a first argument of the segments as before,
--   but a second argument which is the loop presently being
--   constructed.

-- | so we begin with the "building loop" being empty.
getLoops' :: (Show a, Eq a) => [[a]] -> [[a]] -> [[[a]]]

-- | If there aren't any segments, and the "building loop" is empty, produce no loops.
getLoops' [] [] = []

-- | If the building loop is empty, stick the first segment we have onto it to give us something to build on.
getLoops' (x:xs) [] = getLoops' xs [x]

-- | A loop is finished if its start and end are the same.
-- Return it and start searching for another loop.
getLoops' segs workingLoop
  | head (head workingLoop) == last (last workingLoop) = workingLoop : getLoops' segs []

-- | Finally, we search for pieces that can continue the working loop,
-- | and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop =
  let
    presEnd :: [[a]] -> a
    presEnd = last . last
    connectsBackwards [] = False
    connectsBackwards [_] = False
    connectsBackwards (_:xs) = last xs == presEnd workingLoop
    connects (x:_) = x == presEnd workingLoop
    -- Handle the empty case.
    connects [] = False
    -- divide our set into sequences that connect, and sequences that don't.
    (possibleConts, nonConts) = partition connects segs
    (possibleBackConts, nonBackConts) = partition connectsBackwards segs
    (next, unused)
      | not $ null possibleConts     = (head possibleConts, tail possibleConts <> nonConts)
      | not $ null possibleBackConts = (reverse $ head possibleBackConts, tail possibleBackConts <> nonBackConts)
      | otherwise = error $ "unclosed loop in paths given: \nWorking: " <> show workingLoop <> "\nRemainder:" <> show nonConts <> "\n"
  in
    if null next
    then workingLoop : getLoops' segs []
    else getLoops' unused (workingLoop <> [next])

-- | Turn pairs of points into lists of points in sequence.
--   The point pairs are the beginning and end of a line segment. 
getContours :: [(Point2,Point2)] -> [Contour]
getContours pointPairs = maybeFlipContour <$> foundContours
  where
    contourAsPoints :: [(Point2,Point2)] -> [Point2]
    contourAsPoints contour = fst <$> contour
    contourAsPointPairs :: [[Point2]] -> [(Point2,Point2)]
    contourAsPointPairs contourPointPairs = (\[a,b] -> (a,b)) <$> contourPointPairs
    foundContours = PointSequence . contourAsPoints . contourAsPointPairs <$> mapMaybe contourLongEnough foundContourSets
    contourLongEnough :: [[Point2]] -> Maybe [[Point2]]
    contourLongEnough pts
      | length pts > 2 = Just pts
      -- NOTE: returning nothing here, even though this is an error condition, and a sign that the input file has two triangles that intersect. should not happen.
      | otherwise = Nothing -- error $ "fragment insufficient to be a contour found: " <> show pts <> "\n"
    foundContourSets :: [[[Point2]]]
    foundContourSets = getLoops $ (\(a,b) -> [a,b]) <$> sortPairs pointPairs
      where
        -- Sort the list to begin with, so that differently ordered input lists give the same output.
        sortPairs :: [(Point2,Point2)] -> [(Point2,Point2)]
        sortPairs pairs = sortBy (\a b -> if fst a == fst b then compare (snd a) (snd b) else compare (fst a) (fst b)) pairs
    -- make sure a contour is wound the right way, so that the inside of the contour is on the left side of each line segment.
    maybeFlipContour :: Contour -> Contour
    maybeFlipContour contour@(PointSequence contourPoints)
      | insideIsLeft contour $ firstLineSegOf contour = contour
      | otherwise = PointSequence $ reverse contourPoints
      where
        firstLineSegOf (PointSequence ps) = head $ makeLineSegs ps

-- | A contour tree. A contour, which contains a list of contours that are cut out of the first contour, each of them contaiting a list of contours of positive space.. ad infinatum.
newtype ContourTree = ContourTree (Contour, [ContourTree])
  deriving (Show)

-- | Contstruct a set of contour trees. that is to say, a set of contours, containing a set of contours that is negative space, containing a set of contours that is positive space..
makeContourTree :: [Contour] -> [ContourTree]
makeContourTree []        = []
makeContourTree [contour] = [ContourTree (contour, [])]
makeContourTree contours  = [ContourTree (foundContour, makeContourTree $ contoursWithAncestor contours foundContour) | foundContour <- contoursWithoutParents contours]
  where
    contoursWithAncestor cs c = mapMaybe (\cx -> if contourContainsContour c cx then Just cx else Nothing) $ filter (/=c) cs
    contoursWithoutParents cs = catMaybes $ [ if null $ mapMaybe (\cx -> if contourContainedByContour contourToCheck cx then Just cx else Nothing) (filter (/=contourToCheck) cs) then Just contourToCheck else Nothing | contourToCheck <- cs ]

-- | Determine whether a contour is contained inside of another contour.
-- FIXME: magic numbers.
contourContainsContour :: Contour -> Contour -> Bool
contourContainsContour parent child = odd noIntersections
  where
    noIntersections = length $ getContourLineSegIntersections parent $ lineSegToEdge $ innerPointOf child
    lineSegToEdge p = fromRight (error "cannot construct lineToEdge") $ lineSegFromEndpoints p (Point2 (-1,-1))
    getContourLineSegIntersections :: Contour -> LineSeg -> [Point2]
    getContourLineSegIntersections (PointSequence contourPoints) line
      | null contourPoints = []
      | otherwise = mapMaybe (saneIntersection . lineIntersection line) $ makeLineSegsLooped contourPoints
    saneIntersection :: Intersection -> Maybe Point2
    saneIntersection (IntersectsAt p2) = Just p2
    saneIntersection NoIntersection = Nothing
    saneIntersection Parallel = Nothing
    saneIntersection AntiParallel = Nothing
    saneIntersection (Colinear _ _) = Nothing
    saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"
    innerPointOf contour = innerContourPoint 0.00001 contour $ oneLineSegOf contour
      where
        oneLineSegOf (PointSequence contourPoints) = head $ makeLineSegs contourPoints

-- | determine whether a contour is contained by another contour.
contourContainedByContour :: Contour -> Contour -> Bool
contourContainedByContour child parent = contourContainsContour parent child

-- Search the given sequential list of lines (assumedly generated from a contour), and return the line after this one.
followingLineSeg :: [LineSeg] -> LineSeg -> LineSeg
followingLineSeg x = followingLineSegLooped x x
  where
    followingLineSegLooped :: [LineSeg] -> [LineSeg] -> LineSeg -> LineSeg
    followingLineSegLooped [] _ l1 = error $ "reached beginning of contour, and did not find supplied line: " <> show l1 <> "\n"
    followingLineSegLooped _ [] l1 = error $ "reached end of contour, and did not find supplied line: " <> show l1 <> "\n"
    followingLineSegLooped [a] (b:_) l1 = if a == l1 then b else followingLineSegLooped [a] [] l1
    followingLineSegLooped (a:b:xs) set l1 = if a == l1 then b else followingLineSegLooped (b:xs) set l1

-- Search the given sequential list of lines (assumedly generated from a contour), and return the line before this one.
preceedingLineSeg :: [LineSeg] -> LineSeg -> LineSeg
preceedingLineSeg x = preceedingLineSegLooped x x
  where
    preceedingLineSegLooped :: [LineSeg] -> [LineSeg] -> LineSeg -> LineSeg
    preceedingLineSegLooped [] _ l1 = error $ "reached beginning of contour, and did not find supplied line: " <> show l1 <> "\n"
    preceedingLineSegLooped _ [] l1 = error $ "reached end of contour, and did not find supplied line: " <> show l1 <> "\n"
    preceedingLineSegLooped [a] (b:_) l1 = if b == l1 then a else preceedingLineSegLooped [a] [] l1
    preceedingLineSegLooped (a:b:xs) set l1 = if b == l1 then a else preceedingLineSegLooped (b:xs) set l1

-- | Check if the left hand side of this line in toward the inside of the contour it is a part of.
insideIsLeft :: Contour -> LineSeg -> Bool
insideIsLeft contour lineSegment = lineIsLeft lineSecondHalf lineToInside == Just True
  --error $ show (lineSecondHalf) <> "\n" <> show lineToInside <> "\n" <> show (innerContourPoint 0.1 contour line) <> "\n" <> show (lineIsLeft lineSecondHalf lineToInside) <> "\n" 
  where
    lineSecondHalf = fromRight (error "cannot construct SecondHalf") $ lineSegFromEndpoints (midpoint lineSegment) (endpoint lineSegment)
    lineToInside = fromRight (error "cannot construct lineToInside") $ lineSegFromEndpoints (midpoint lineSegment) $ innerContourPoint 0.00001 contour lineSegment

-- | Find a point on the interior of the given contour, on the perpendicular bisector of the given line, a given distance from the line.
-- FIXME: assumes we are in positive space.
innerContourPoint :: ℝ -> Contour -> LineSeg -> Point2
innerContourPoint distance contour l
    | odd numIntersections = perpPoint
    | otherwise            = otherPerpPoint
  where
      originPoint = Point2 (-1,-1)
      perpPoint      = pointOnPerp l (midpoint l) distance
      otherPerpPoint = pointOnPerp l (midpoint l) (-distance)
      numIntersections = length $ intersections originPoint contour $ pointOnPerp l (midpoint l) 0.00001

-- | return the intersections with a given contour when traveling a straight line from srcPoint to dstPoint.
intersections :: Point2 -> Contour -> Point2 -> [Point2]
intersections dstPoint contour srcPoint = saneIntersections l0 $ contourLineSegs contour
  where
    -- The line we are checking for intersections along.
    l0 = fromRight (error "cannot construct target line") $ lineSegFromEndpoints srcPoint dstPoint
    contourLineSegs (PointSequence c) = makeLineSegsLooped c
    -- a filter for results that make sense.
    saneIntersections :: LineSeg -> [LineSeg] -> [Point2]
    saneIntersections l1 ls = mapMaybe saneIntersectionOf ls
      where
        saneIntersectionOf :: LineSeg -> Maybe Point2
        saneIntersectionOf l2 = saneIntersection (lineIntersection l1 l2)
          where
            saneIntersection :: Intersection -> Maybe Point2
            saneIntersection (IntersectsAt p2) = Just p2
            saneIntersection NoIntersection = Nothing
            saneIntersection Parallel = Nothing
            saneIntersection AntiParallel = Nothing
    -- FIXME: fix these cases. steal the code / algorithms from infillLineSegInside.
    --          saneIntersection Collinear = Nothing
    --          saneIntersection LColinear _ _ = Nothing
    --          saneIntersection (HitStartPointL2 p2) = Just p2
    --          saneIntersection (HitEndPointL2 p2) = Just p2
            saneIntersection res = error $ "insane result of intersecting a line (" <> show l1 <> ") with it's bisector: " <> show l2 <> "\nwhen finding an inner contour point on contour " <> show ls <> "\n" <> show res <> "\n"

-- | A face.
--   A portion of a contour, with a real side, and arcs (line segments between nodes) dividing it from other faces.
--   Faces have no holes, and their arcs and nodes (lines and points) are from a straight skeleton of a contour.
data Face = Face { _realSide :: LineSeg, _arcs :: [PLine2] }
  deriving (Show, Eq)

-- | construct a set of faces, using a straight skeleton to divide up the area of a contour.
makeFaces :: Contour -> [Contour] -> [Face]
makeFaces = error "undefined!"
  where
    motorcycles :: Contour -> [(Point2, PLine2)]
    motorcycles (PointSequence c) = error "undefined!"

-- Place lines on a face.
addLineSegs :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe Face)
addLineSegs face@(Face realSide arcs) count lineWidth
  | length arcs == 2 = triangleFace face count lineWidth
  | otherwise        = error "undefined!"
    where
      -- | handle faces that are triangular. easy case.
      triangleFace :: Face -> Fastℕ -> ℝ -> ([LineSeg], Maybe Face)
      triangleFace f@(Face realSide (a1:a2:[])) n lw = (foundLineSegs, remainder)
        where
          foundLineSegs = error "undefined" -- [ lineFromEndpoints start fin | (a,b) <- (
          remainder  = error "undefined"
          linesToRender = if linesUntilEnd < n then linesUntilEnd else n
          linesUntilEnd = floor $ (distancePPointToPLine (intersectionOf a1 a2) (eToPLine2 realSide)) / lw
          intersectionOf pl1 pl2 = saneIntersection $ plinesIntersectIn pl1 pl2
            where
              saneIntersection PColinear            = error "impossible!"
              saneIntersection PParallel            = error "impossible!"
              saneIntersection PAntiParallel        = error "impossible!"
              saneIntersection (IntersectsIn point) = point
