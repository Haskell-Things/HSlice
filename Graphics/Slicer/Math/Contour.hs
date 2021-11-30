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

-- for not handling faces:
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | functions for handling contours.
module Graphics.Slicer.Math.Contour (followingLineSeg, getContours, makeContourTreeSet, ContourTree(ContourTree), ContourTreeSet(ContourTreeSet), contourContainsContour, numPointsOfContour, pointsOfContour, firstLineSegOfContour, firstPointOfContour, justOneContourFrom, lastPointOfContour, makePointContour, firstContourOfContourTreeSet, lineSegsOfContour, makeLineSegContour, contourIntersectionCount) where

import Prelude ((==), Int, (+), otherwise, (.), null, (<$>), ($), length, Show, filter, (/=), odd, snd, error, (<>), show, fst, Bool(True,False), Eq, Show, compare, maximum, minimum, min, zip, Either(Left, Right), (-), (++), (>))

import Data.List(head, partition, reverse, sortBy)

import Data.List as DL (last)

import Data.List.Extra (unsnoc)

import Data.Maybe(Maybe(Just,Nothing), catMaybes, mapMaybe)

import Data.Either (fromRight)

import Slist (len, size, slist, uncons, safeLast, zipWith)

import Slist as SL (last)

import Slist.Type (Slist(Slist))

import Slist.Size (Size(Infinity))

import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Contour(PointContour, LineSegContour), Point2(Point2), LineSeg, mapWithNeighbors, minMaxPoints, xOf, yOf, startPoint)

import Graphics.Slicer.Math.Line (lineSegFromEndpoints, endPoint, midPoint, handleLineSegError)

import Graphics.Slicer.Math.PGA (Intersection(NoIntersection, HitStartPoint, HitEndPoint), PIntersection (PParallel, PAntiParallel, IntersectsIn), eToPPoint2, lineIsLeft, pointOnPerp, intersectsWith, pToEPoint2, PPoint2, PLine2, join2PPoint2)

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
  | head (head workingLoop) == presEnd workingLoop = workingLoop : getLoops' segs []

-- | Finally, we search for pieces that can continue the working loop,
-- | and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop =
  let
    connectsBackwards [] = False
    connectsBackwards [_] = False
    connectsBackwards (_:xs) = DL.last xs == presEnd workingLoop
    connects [] = False     -- Handle the empty case.
    connects (x:_) = x == presEnd workingLoop
    -- divide our set into sequences that connect, and sequences that don't.
    (possibleForwardConts, nonForwardConts) = partition connects segs
    (possibleBackConts, nonBackConts) = partition connectsBackwards segs
    (next, unused) = case possibleForwardConts of
                       (hf:tf) -> (hf, tf <> nonForwardConts)
                       [] -> case possibleBackConts of
                               (hb:tb) -> (reverse hb, tb <> nonBackConts)
                               [] -> error $ "unclosed loop in paths given: \nWorking: " <> show workingLoop <> "\nRemainder:" <> show nonForwardConts <> "\n"
  in
    if null next
    then workingLoop : getLoops' segs []
    else getLoops' unused (workingLoop <> [next])

-- | get the end of a working loop.
presEnd :: [[a]] -> a
presEnd a = case unsnoc a of
              Nothing -> error "impossible!"
              (Just (_,b)) -> case unsnoc b of
                                Nothing -> error "more impossible!"
                                (Just (_,c)) -> c

-- | Turn pairs of points into lists of points in sequence.
--   The point pairs are the beginning and end of a line segment.
getContours :: [(Point2,Point2)] -> [Contour]
getContours pointPairs = maybeFlipContour <$> foundContours
  where
    contourAsLineSegs :: [[Point2]] -> [LineSeg]
    contourAsLineSegs contourPointPairs = (\[a,b] -> handleLineSegError $ lineSegFromEndpoints a b) <$> contourPointPairs
    foundContours = makeLineSegContour . contourAsLineSegs <$> mapMaybe contourLongEnough foundContourSets
    contourLongEnough :: [[Point2]] -> Maybe [[Point2]]
    contourLongEnough pts = case pts of
                              (_:_:_:_) -> Just pts
                              -- NOTE: returning nothing here, even though this is an error condition, and a sign that the input file is insane?
                              [] -> Nothing
                              -- NOTE: returning nothing here, even though this is an error condition, and a sign that the input file has two triangles that intersect. should not happen.
                              (_:_) -> Nothing
    foundContourSets :: [[[Point2]]]
    foundContourSets = getLoops $ (\(a,b) -> [a,b]) <$> sortPairs pointPairs
      where
        -- Sort the list to begin with, so that differently ordered input lists give the same output.
        sortPairs :: [(Point2,Point2)] -> [(Point2,Point2)]
        sortPairs = sortBy (\a b -> if fst a == fst b then compare (snd a) (snd b) else compare (fst a) (fst b))
    -- make sure a contour is wound the right way, so that the inside of the contour is on the left side of each line segment.
    maybeFlipContour :: Contour -> Contour
    maybeFlipContour contour
      | insideIsLeft contour (firstLineSegOfContour contour) = contour
      | otherwise = makePointContour $ reverse $ pointsOfContour contour

-- | A contour tree. A contour, which contains a list of contours that are cut out of the first contour, each of them contaiting a list of contours of positive space.. ad infinatum.
data ContourTree = ContourTree { _parentContour :: !Contour, _childContours :: !(Slist ContourTreeSet) }
  deriving (Show)

-- | A set of contour trees.
data ContourTreeSet = ContourTreeSet { _firstContourTree :: !ContourTree, _moreContourTrees :: !(Slist ContourTree)}
  deriving (Show)

-- | Contstruct a set of contour trees. that is to say, a set of contours, containing a set of contours that is negative space, containing a set of contours that is positive space..
makeContourTreeSet :: [Contour] -> ContourTreeSet
makeContourTreeSet contours =
  case contours of
    [] -> error "no contours to make a set out of."
    [contour] -> ContourTreeSet (ContourTree contour (slist [])) (slist [])
    (_:_) ->
      case contoursWithoutParents contours of
        [] -> error $ "impossible: contours given, but no top level contours found?" <> show contours <> "\n"
        [oneContour] -> ContourTreeSet (ContourTree oneContour (slist [makeContourTreeSet $ filter (/= oneContour) contours])) (slist [])
        (headParent:tailParents) -> ContourTreeSet (ContourTree headParent (slist $ recurseIfNotEmpty contours headParent))
                                                   (slist [ContourTree foundContour (slist $ recurseIfNotEmpty contours foundContour) | foundContour <- tailParents])
  where
    recurseIfNotEmpty cs c =
      case contoursWithAncestor cs c of
        [] -> []
        [oneContour] -> [makeContourTreeSet [oneContour]]
        manyContours@(_:_) -> [makeContourTreeSet manyContours]
    contoursWithAncestor cs c = mapMaybe (\cx -> if contourContainsContour c cx then Just cx else Nothing) $ filter (/=c) cs
    contoursWithoutParents cs = catMaybes $ [ if null $ mapMaybe (\cx -> if contourContainedByContour contourToCheck cx then Just cx else Nothing) (filter (/=contourToCheck) cs) then Just contourToCheck else Nothing | contourToCheck <- cs ]

-- | Determine whether a contour is contained inside of another contour.
-- FIXME: magic numbers.
contourContainsContour :: Contour -> Contour -> Bool
contourContainsContour parent child = odd noIntersections
  where
    (minPoint1, minPoint2) = (fst $ minMaxPoints parent, fst $ minMaxPoints child)
    outsidePointOfPair = Point2 (min (xOf minPoint1) (xOf minPoint2) - 1,min (yOf minPoint1) (yOf minPoint2) - 1)
    noIntersections = length $ getContourLineSegIntersections parent $ lineSegToEdge $ innerPointOf child
    lineSegToEdge p = fromRight (error "cannot construct lineToEdge") $ lineSegFromEndpoints p outsidePointOfPair
    getContourLineSegIntersections :: Contour -> LineSeg -> [Point2]
    getContourLineSegIntersections contour line = mapMaybe (saneIntersection . intersectsWith (Left line) . Left) $ lineSegsOfContour contour
    saneIntersection :: Either Intersection PIntersection -> Maybe Point2
    saneIntersection (Left NoIntersection)         = Nothing
    saneIntersection (Right (IntersectsIn ppoint)) = Just $ pToEPoint2 ppoint
    saneIntersection (Right PAntiParallel)         = Nothing
    saneIntersection (Right PParallel)             = Nothing
    -- FIXME: fix the remaining cases. steal the code / algorithms from closedRegion
    saneIntersection res = error $ "insane result drawing a line to the edge: " <> show res <> "\n"
    innerPointOf contour = innerContourPoint 0.00001 contour $ firstLineSegOfContour contour

-- | determine whether a contour is contained by another contour.
contourContainedByContour :: Contour -> Contour -> Bool
contourContainedByContour child parent = contourContainsContour parent child

-- | Search the given sequential list of lines (assumedly generated from a contour), and return the line after this one.
followingLineSeg :: [LineSeg] -> LineSeg -> LineSeg
followingLineSeg x = followingLineSegLooped x x
  where
    followingLineSegLooped :: [LineSeg] -> [LineSeg] -> LineSeg -> LineSeg
    followingLineSegLooped [] _ l1 = error $ "reached beginning of contour, and did not find supplied line: " <> show l1 <> "\n"
    followingLineSegLooped _ [] l1 = error $ "reached end of contour, and did not find supplied line: " <> show l1 <> "\n"
    followingLineSegLooped [a] (b:_) l1 = if a == l1 then b else followingLineSegLooped [a] [] l1
    followingLineSegLooped (a:b:xs) set l1 = if a == l1 then b else followingLineSegLooped (b:xs) set l1

-- | Check if the left hand side of a line segmnt is toward the inside of the contour it is a part of.
insideIsLeft :: Contour -> LineSeg -> Bool
insideIsLeft contour lineSegment = lineIsLeft lineSecondHalf lineToInside == Just True
  where
    lineSecondHalf = fromRight (error $ "cannot construct SecondHalf" <> show lineSegment <> "\n" <> show contour <> "/n") $ lineSegFromEndpoints (midPoint lineSegment) (endPoint lineSegment)
    lineToInside = fromRight (error "cannot construct lineToInside") $ lineSegFromEndpoints (midPoint lineSegment) $ innerContourPoint 0.00001 contour lineSegment

-- | Find a point on the interior of the given contour, on the perpendicular bisector of the given line segment, a given distance away from the line segment.
innerContourPoint :: ℝ -> Contour -> LineSeg -> Point2
innerContourPoint distance contour l
    | odd numIntersections = perpPoint
    | otherwise            = otherPerpPoint
  where
    minPoint       = fst (minMaxPoints contour)
    perpPoint      = pointOnPerp l (midPoint l) distance
    otherPerpPoint = pointOnPerp l (midPoint l) (-distance)
    outsidePoint   = Point2 (xOf minPoint - 1 , yOf minPoint - 1)
    numIntersections = contourIntersectionCount contour (Left (pointOnPerp l (midPoint l) 0.00001, outsidePoint))

-- | return the number of intersections with a given contour when traveling in a straight line from srcPoint to dstPoint.
-- Not for use against line segments that overlap and are collinear with one of the line segments are a part of the contour.
contourIntersectionCount :: Contour -> Either (Point2, Point2) (PPoint2, PPoint2) -> Int
contourIntersectionCount contour endPoints = length $ getIntersections contour endPoints
  where
    -- The line we are checking for intersections along.
    -- a filter for results that make sense.
    getIntersections :: Contour -> Either (Point2, Point2) (PPoint2, PPoint2) -> [(LineSeg, Maybe LineSeg, PPoint2)]
    getIntersections c pts = catMaybes $ mapWithNeighbors saneIntersection $ zip (lineSegsOfContour contour) $ intersectsWith (lineFromPoints pts) . Left <$> lineSegsOfContour contour
      where
        lineFromPoints ::  Either (Point2, Point2) (PPoint2, PPoint2) -> Either LineSeg PLine2
        lineFromPoints (Left (lstart, lend)) = Left $ handleLineSegError $ lineSegFromEndpoints lstart lend
        lineFromPoints (Right (pstart, pend)) = Right $ join2PPoint2 pstart pend
        saneIntersection :: (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> (LineSeg, Either Intersection PIntersection) -> Maybe (LineSeg, Maybe LineSeg, PPoint2)
        saneIntersection _ (seg,Right (IntersectsIn ppoint)) _ = Just (seg, Nothing, ppoint)
        saneIntersection _ (_,Left NoIntersection)         _ = Nothing
        saneIntersection _ (_,Right PParallel)             _ = Nothing
        saneIntersection _ (_,Right PAntiParallel)         _ = Nothing
        saneIntersection  _                              (seg , Left (HitStartPoint _ point)) (seg2 , Left (HitEndPoint   _ _)) = Just (seg, Just seg2, eToPPoint2 point)
        saneIntersection (_  , Left (HitStartPoint _ _)) (_   , Left (HitEndPoint   _ _))      _                                = Nothing
        saneIntersection  _                              (_   , Left (HitEndPoint   _ _))     (_    , Left (HitStartPoint _ _)) = Nothing
        saneIntersection (seg, Left (HitEndPoint   _ _)) (seg2, Left (HitStartPoint _ point))  _                                = Just (seg, Just seg2, eToPPoint2 point)
        saneIntersection res1 res2 res3 = error $ "insane result of intersecting a line (" <> show (lineFromPoints pts) <> ") with a contour: " <> show c <> "\n" <> show res1 <> "\n" <> show res2 <> "\n" <> show res3 <> "\n"

-- | return the contour as a list of points.
pointsOfContour :: Contour -> [Point2]
pointsOfContour (PointContour _ _ p1 p2 p3 pts@(Slist vals _))
  | size pts == Infinity = error "cannot handle infinite contours."
  | otherwise            = p1:p2:p3:vals
pointsOfContour (LineSegContour _ _ l1 l2 moreLines@(Slist lns _))
  | size moreLines == Infinity = error "cannot handle infinite contours."
  | otherwise                  = startPoint l1:startPoint l2:(startPoint <$> lns)

-- | return the contour as a list of LineSegs.
lineSegsOfContour :: Contour -> [LineSeg]
lineSegsOfContour (PointContour _ _ p1 p2 p3 pts) = [consLineSeg p1 p2,
                                                    consLineSeg p2 p3] ++ consSegsWithPoints p3 pts p1
  where
    consLineSeg point1 point2 = handleLineSegError $ lineSegFromEndpoints point1 point2
    consSegsWithPoints pointStart points pointEnd =
      case uncons points of
        Nothing -> [consLineSeg pointStart pointEnd]
        (Just (headVal,tailVals)) -> consLineSeg pointStart headVal :
                                   case safeLast tailVals of
                                     Nothing -> [consLineSeg headVal pointEnd]
                                     (Just lastVal) -> consSegsBetween points tailVals ++ [consLineSeg lastVal pointEnd]
      where
        consSegsBetween myPoints myTailVals = (\(Slist vals _)  -> vals) $ zipWith consLineSeg myPoints myTailVals
lineSegsOfContour (LineSegContour _ _ l1 l2 moreLines@(Slist lns _))
  | size moreLines == Infinity = error "cannot handle infinite contours."
  | otherwise                  = l1:l2:lns

-- | return the number of points in a contour.
numPointsOfContour :: Contour -> Int
numPointsOfContour (PointContour _ _ _ _ _ pts) = 3 + len pts
numPointsOfContour (LineSegContour _ _ l1 l2 lns) = case safeLast lns of
                                                     Nothing -> if startPoint l1 == endPoint l2
                                                                then error "2 point contour not legal"
                                                                else 3
                                                     (Just lastLine) -> if startPoint l1 == endPoint lastLine
                                                                        then 2 + len lns
                                                                        else 3 + len lns

-- | In an ideal world, only the test suite needs this.
justOneContourFrom :: ([Contour], [_]) -> Contour
justOneContourFrom ([contour],  _) = contour
justOneContourFrom (contours, _) = error $ "received multiple contours when we expected just one:\n" <> show contours <> "\n" -- <> show faces <> "\n"

-- | Find the last point in a contour.
-- since contours are a big loop, the first point IS the last point.
lastPointOfContour :: Contour -> Point2
lastPointOfContour (PointContour _ _ _ _ p3 pts)
  | len pts > 0 = SL.last pts
  | otherwise = p3
lastPointOfContour (LineSegContour _ _ l1 l2 lns) = case safeLast lns of
                                                      Nothing -> if startPoint l1 == endPoint l2
                                                                 then error "2 point contour not legal"
                                                                 else endPoint l2
                                                      (Just lastLine) -> if startPoint l1 == endPoint lastLine
                                                                         then startPoint lastLine
                                                                         else endPoint lastLine

-- | Find the first point in a contour.
firstPointOfContour :: Contour -> Point2
firstPointOfContour (PointContour _ _ p1 _ _ _) = p1
firstPointOfContour (LineSegContour _ _ l1 _ _) = startPoint l1

-- | A constructor for a contour from a set of points.
makePointContour :: [Point2] -> Contour
makePointContour points = case points of
                            [] -> error "tried to create an empty contour"
                            [p] -> error $ "tried to create a contour with a single point: " <> show p <> "\n"
                            [p1,p2] -> error $ "tried to create a contour with only two points:\n" <> show p1 <> "\n" <> show p2 <> "\n"
                            (p1:p2:p3:pts) -> PointContour pL pH p1 p2 p3 (slist pts)
  where
    pL = Point2 (minimum $ xOf <$> points, minimum $ yOf <$> points)
    pH = Point2 (maximum $ xOf <$> points, maximum $ yOf <$> points)

makeLineSegContour :: [LineSeg] -> Contour
makeLineSegContour lineSegs = case lineSegs of
                                [] -> error "tried to create an empty contour"
                                [l] -> error $ "tried to create a contour with a single line: " <> show l <> "\n"
                                [l1,l2] -> if startPoint l1 == endPoint l2
                                           then error $ "tried to create a contour with only two points:\n" <> show l1 <> "\n" <> show l2 <> "\n"
                                           else LineSegContour pL pH l1 l2 (slist [])
                                (l1:l2:lns) -> LineSegContour pL pH l1 l2 (slist lns)
  where
    pL = Point2 (minimum $ xOf <$> allPoints, minimum $ yOf <$> allPoints)
    pH = Point2 (maximum $ xOf <$> allPoints, maximum $ yOf <$> allPoints)
    allPoints = case lineSegs of
                  [] -> error "tried to create an empty contour"
                  (x:xs) -> startPoint x:endPoint x:(endPoint <$> xs)

-- | find the first line segment of a contour.
firstLineSegOfContour :: Contour -> LineSeg
firstLineSegOfContour (PointContour _ _ p1 p2 _ _) = handleLineSegError $ lineSegFromEndpoints p1 p2
firstLineSegOfContour (LineSegContour _ _ l1 _ _) = l1

-- | find the first outer contour of a contourTreeSet, and return it.
firstContourOfContourTreeSet :: ContourTreeSet -> Contour
firstContourOfContourTreeSet (ContourTreeSet (ContourTree contour _) _) = contour

