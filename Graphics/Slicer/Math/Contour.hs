{- ORMOLU_DISABLE -}
{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019-2022 Julia Longtin
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

-- For not handling faces:
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Functions for handling contours.
module Graphics.Slicer.Math.Contour (
  ContourTree(ContourTree),
  ContourTreeSet(ContourTreeSet),
  contourContainsContour,
  firstContourOfContourTreeSet,
  firstLineSegOfContour,
  firstPointOfContour,
  firstPointPairOfContour,
  followingLineSeg,
  getContours,
  innerContourPoint,
  insideIsLeft,
  justOneContourFrom,
  lastPointOfContour,
  makeContourTreeSet,
  makeLineSegContour,
  makePointContour,
  maybeFlipContour,
  minDistanceFromSegMidPoint,
  mostPerpPointAndLineSeg,
  numPointsOfContour
  ) where

import Prelude ((==), (&&), (||), (>), (<), (*), (/), Int, (+), abs, even, mempty, otherwise, (.), null, (<$>), ($), Show, filter, (/=), odd, snd, error, (<>), show, fst, Bool(True), Eq, compare, maximum, minimum, min, (-), not)

import Data.List (foldl', head, partition, reverse, sortBy, tail)

import Data.Maybe (Maybe(Just,Nothing), catMaybes, fromJust, fromMaybe, isJust, mapMaybe)

import Data.MemoTrie (memo)

import Slist (len, slist, safeLast, safeLast, safeHead)

import Slist as SL (last)

import Slist.Type (Slist(Slist))

-- The numeric type in HSlice.
import Graphics.Implicit.Definitions (ℝ)

import Graphics.Slicer.Math.ContourIntersections (contourIntersectionCount)

import Graphics.Slicer.Math.Definitions (Contour(PointContour, LineSegContour), LineSeg(endPoint, startPoint), Point2(Point2), lineSegsOfContour, makeLineSeg, minMaxPoints, pointBetweenPoints, pointsOfContour, xOf, yOf)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.Intersections (lineSegsIntersect, noIntersection, intersectionOf)

import Graphics.Slicer.Math.Lossy (pToEPoint2, eToPLine2)

import Graphics.Slicer.Math.PGA (ProjectivePoint, angleBetween2PL, distance2PP, eToPL, eToPP, fuzzinessOfL, fuzzinessOfP, join2EP, join2PP, pLineErrAtPPoint, pLineIsLeft, pPointOnPerpWithErr)

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

getLoops :: (Show a, Eq a) => [(a,a)] -> Maybe [[(a,a)]]
getLoops [] = Just []
getLoops (x:xs) = getLoops' xs (slist [x]) (snd x)
-- We will be actually doing the loop extraction with
-- getLoops'

-- | getLoops' has a first argument of the segments as before,
--   but a second argument which is the loop presently being
--   constructed.

-- | so we begin with the "building loop" being empty.
getLoops'
  :: (Show a, Eq a)
  => [(a,a)]     -- ^ input
  -> Slist (a,a) -- ^ accumulator
  -> a         -- ^ last element in the acumulator
  -> Maybe [[(a,a)]]

-- | If there aren't any segments, and the "building loop" is empty, produce no loops.
getLoops' [] (Slist [] _) _ = Just []

-- | If the building loop is empty, stick the first segment we have onto it to give us something to build on.
getLoops' (a:as) (Slist [] _) _ = getLoops' as (slist [a]) (snd a)

-- | A loop is finished if its start and end are the same.
-- Return it and start searching for another loop.
getLoops' segs workingLoop ultima
  -- FIXME: do we need ultima when recursing?
  | firstItemOf workingLoop == ultima = ([sListToList workingLoop] <>) <$> getLoops' segs (slist []) ultima
  where
    firstItemOf :: Slist (a,a) -> a
    firstItemOf a = case safeHead a of
                      Nothing -> error "empty Slist in workingLoop"
                      (Just v) -> fst v
    sListToList (Slist a _) = a

-- | Finally, we search for pieces that can continue the working loop,
-- | and stick one on if we find it.
-- Otherwise... something is really screwed up.
getLoops' segs workingLoop ultima =
  let
    connects x = fst x == presEnd workingLoop
    connectsBackwards x = snd x == presEnd workingLoop
    -- divide our set into segments that connect, and segments that don't.
    (possibleForwardConts, nonForwardConts) = partition connects segs
    (possibleBackConts, nonBackConts) = partition connectsBackwards segs
    (next, unused) = case possibleForwardConts of
                       (hf:tf) -> (hf, tf <> nonForwardConts)
                       [] -> case possibleBackConts of
                               (hb:tb) -> ((snd hb, fst hb), tb <> nonBackConts)
                               [] -> error $ "unclosed loop in paths given: \nWorking: " <> show workingLoop <> "\nRemainder:" <> show nonForwardConts <> "\n"
  in
    if null next
    then ([sListToList workingLoop] <>) <$> getLoops' segs (slist []) ultima
    else getLoops' unused (workingLoop <> slist [next]) (snd next)
  where
    sListToList (Slist a _) = a
    presEnd :: Slist (a,a) -> a
    presEnd a = case safeLast a of
                  Nothing -> error "impossible!"
                  (Just b) -> snd b

-- | Turn pairs of points into lists of points in sequence.
--   The point pairs are the beginning and end of a line segment.
getContours :: [(Point2,Point2)] -> [Contour]
getContours pointPairs = (\a -> fromMaybe (error $ "failed to flip a contour\n" <> show a <> "\n") $ maybeFlipContour a) <$> foundContours
  where
    contourAsLineSegs :: [(Point2,Point2)] -> [LineSeg]
    contourAsLineSegs contourPointPairs = (\(a,b)  -> makeLineSeg a b) <$> contourPointPairs
    foundContours = makeLineSegContour . contourAsLineSegs <$> mapMaybe contourLongEnough foundContourSets
    contourLongEnough :: [(Point2,Point2)] -> Maybe [(Point2,Point2)]
    contourLongEnough pts = case pts of
                              (_:_:_:_) -> Just pts
                              -- NOTE: returning nothing here, even though this is an error condition, and a sign that the input file is insane?
                              [] -> Nothing
                              -- NOTE: returning nothing here, even though this is an error condition, and a sign that the input file has two triangles that intersect. should not happen.
                              (_:_) -> Nothing
    foundContourSets :: [[(Point2,Point2)]]
    foundContourSets = fromMaybe (error "could not complete loop detection.") $ getLoops $ sortPairs pointPairs
      where
        -- Sort the list, so that differently ordered input lists give the same output.
        sortPairs :: [(Point2,Point2)] -> [(Point2,Point2)]
        sortPairs = sortBy (\a b -> if fst a == fst b then compare (snd a) (snd b) else compare (fst a) (fst b))

-- | A wrapper of maybeFlipContour, using memoization.
maybeFlipContour :: Contour -> Maybe Contour
maybeFlipContour = memo maybeFlipContour'

-- | Ensure a contour is wound the right way, so that the inside of the contour is on the left side of each line segment.
maybeFlipContour' :: Contour -> Maybe Contour
maybeFlipContour' contour
  | isJust maybeIsLeft = Just $ if maybeIsLeft == Just True
                                then contour
                                else makePointContour $ reverse $ pointsOfContour contour
  | otherwise = Nothing
  where
    maybeIsLeft = insideIsLeft contour

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
contourContainsContour :: Contour -> Contour -> Bool
contourContainsContour parent child
  | isJust maybeInsidePoint = odd noIntersections
  | otherwise = error "could not find inner point."
  where
    outsidePoint = pointFarOutsideContours parent child
    insidePoint = pToEPoint2 $ fromJust maybeInsidePoint
    maybeInsidePoint = innerContourPoint child
    noIntersections = contourIntersectionCount parent (insidePoint, outsidePoint)

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

-- | Check if the left hand side of the first line segment of a contour is toward the inside of the contour.
insideIsLeft :: Contour -> Maybe Bool
insideIsLeft contour
  | isJust (innerContourPoint contour) = Just $ line1 `pLineIsLeft` lineToInside == Just True
  | otherwise = Nothing
  where
    (lineToInside, _) = join2PP (eToPP midPoint) innerPoint
    midPoint   = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
    (line1,_)  = join2EP (startPoint lineSeg) (endPoint lineSeg)
    innerPoint = fromJust $ innerContourPoint contour
    lineSeg    = firstLineSegOfContour contour

-- | Find a point on the interior of a given contour, on the perpendicular bisector of the first line segment, a given distance away from the line segment.
-- FIXME: magic number.
innerContourPoint :: Contour -> Maybe ProjectivePoint
innerContourPoint contour = Just $ innerContourPoint' contour 3200 1

innerContourPoint' :: Contour -> ℝ -> ℝ -> ProjectivePoint
innerContourPoint' contour minDistanceMul recurses
  | recurses > 30 = error "fail, too many recurses"
  | lineSegsIntersect lineSeg (makeLineSeg (pToEPoint2 perpPoint) outsidePoint) &&
    lineSegsIntersect lineSeg (makeLineSeg (pToEPoint2 otherPoint) outsidePoint) =
    innerContourPoint' contour (minDistanceMul * 3) (recurses+1)
  | not (lineSegsIntersect lineSeg $ makeLineSeg (pToEPoint2 perpPoint) outsidePoint) &&
    not (lineSegsIntersect lineSeg $ makeLineSeg (pToEPoint2 otherPoint) outsidePoint) =
    innerContourPoint' contour (minDistanceMul / 2) (recurses+1)
  | odd numIntersections && even otherIntersections =
    if minDistanceFromSeg > ulpVal perpErr
    then perpPoint
    else error "cannot ensure perp point is on the correct side of contour."
  | odd otherIntersections && even numIntersections =
      if minDistanceFromSeg > ulpVal otherErr
      then otherPoint
      else error "cannot ensure other point is on the correct side of the contour."
  | otherwise = error "fail"
  where
    numIntersections   = contourIntersectionCount contour (pToEPoint2 perpPoint, outsidePoint)
    otherIntersections = contourIntersectionCount contour (pToEPoint2 otherPoint, outsidePoint)
    (perpPoint,  (_,_,_, perpErr))  = pPointOnPerpWithErr pLine midPoint (minDistanceMul * minDistanceFromSeg)
    (otherPoint, (_,_,_, otherErr)) = pPointOnPerpWithErr pLine midPoint (-minDistanceMul * minDistanceFromSeg)
    minDistanceFromSeg  = minDistanceFromSegMidPoint outsidePoint lineSeg
    midPoint = eToPP $ pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
    pLine = eToPLine2 lineSeg
    (outsidePoint, lineSeg) = mostPerpPointAndLineSeg contour

-- | The minimum measurable distance of a point from the midpoint of a line segment
-- Note: this should be the opposite of PGA.hs' onSegment. after all, we're trying to come up with a distance where onSegment won't trigger.
minDistanceFromSegMidPoint :: Point2 -> LineSeg -> ℝ
minDistanceFromSegMidPoint outsidePoint lineSeg = midDistance + ulpVal (midDistanceErr <> perpErr)
  where
    (midDistance, (_,_, midDistanceErr)) = distance2PP (eToPP midPoint, mempty) (perpPoint, mempty)
    (perpPoint,  (_,_,_, perpErr)) = pPointOnPerpWithErr (fst pLine) (eToPP midPoint) (lineFuzz + midPointFuzz)
    lineFuzz = ulpVal (fuzzinessOfL pLine <> pLineErrAtPPoint pLine (eToPP midPoint))
    -- The amount of error introduced by intersecting from the outsidePoint to the given line segment.
    midPointFuzz = ulpVal $ fuzzinessOfP $ intersectionOf pLine outsideLine
    outsideLine = join2EP midPoint outsidePoint
    midPoint = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
    pLine = eToPL lineSeg

-- | Find a point that is guaranteed to be outside of the contour, along with the line segment of the contour that is most perpendicular to it.
mostPerpPointAndLineSeg :: Contour -> (Point2, LineSeg)
mostPerpPointAndLineSeg contour = (resPoint, resSeg)
  where
    (resSeg, resPoint, _) = mostPerp contour
    -- | Find the most perpendicular line segment of a contour, when compared to the given projective line.
    mostPerp :: Contour -> (LineSeg, Point2, ℝ)
    mostPerp myContour = foldl' (\(a1, b1, c1) (a2, b2, c2) -> if c1 == 0 || c1 < c2 then (a1, b1, c1) else (a2, b2, c2)) (head lineSegsAndAnglesPos) (tail lineSegsAndAnglesPos <> lineSegsAndAnglesMid <> lineSegsAndAnglesNeg)
      where
        lineSegsAndAnglesPos = [(lineSeg, outsidePosPoint, abs $ fst $ angleBetween2PL (fst $ join2EP outsidePosPoint $ midPoint lineSeg) (eToPLine2 lineSeg)) | lineSeg <- lineSegsOfContour myContour]
        lineSegsAndAnglesMid = [(lineSeg, outsideMidPoint, abs $ fst $ angleBetween2PL (fst $ join2EP outsideMidPoint $ midPoint lineSeg) (eToPLine2 lineSeg)) | lineSeg <- lineSegsOfContour myContour]
        lineSegsAndAnglesNeg = [(lineSeg, outsideNegPoint, abs $ fst $ angleBetween2PL (fst $ join2EP outsideNegPoint $ midPoint lineSeg) (eToPLine2 lineSeg)) | lineSeg <- lineSegsOfContour myContour]
        midPoint lineSeg = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
        outsideMidPoint = pointBetweenPoints outsidePosPoint outsideNegPoint
        outsidePosPoint = Point2 (xOf minPoint - 0.1 , yOf maxPoint + 0.1)
        outsideNegPoint = Point2 (xOf minPoint - 0.1 , yOf minPoint - 0.1)
        (minPoint, maxPoint) = minMaxPoints contour

-- | Find a point that is guaranteed to be outside of the given contour, and is not on the same line as the first line segment of the contour.
pointFarOutsideContours :: Contour -> Contour -> Point2
pointFarOutsideContours contour1 contour2
  | not (noIntersection line1 firstLine) && not (noIntersection line1 secondLine) = outsidePoint1
  | not (noIntersection line2 firstLine) && not (noIntersection line2 secondLine) = outsidePoint2
  | not (noIntersection line3 firstLine) && not (noIntersection line3 secondLine) = outsidePoint3
  | otherwise = error "cannot get here...?"
  where
    (minPoint1, _)= minMaxPoints contour1
    (minPoint2, _)= minMaxPoints contour2
    minPoint      = Point2 (min (xOf minPoint1) (xOf minPoint2),min (yOf minPoint1) (yOf minPoint2))
    (p1, p2)      = firstPointPairOfContour contour1
    (p3, p4)      = firstPointPairOfContour contour2
    firstLine     = join2EP p1 p2
    secondLine    = join2EP p3 p4
    line1         = join2EP p1 outsidePoint1
    line2         = join2EP p1 outsidePoint2
    line3         = join2EP p1 outsidePoint3
    outsidePoint1 = Point2 (xOf minPoint - 0.1 , yOf minPoint - 0.1)
    outsidePoint2 = Point2 (xOf minPoint - 0.2 , yOf minPoint - 0.1)
    outsidePoint3 = Point2 (xOf minPoint - 0.1 , yOf minPoint - 0.2)

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
  | not $ null pts = SL.last pts
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
                                [l1,l2] -> if l1 == l2
                                           then error $ "tried to create a contour with two (almost) identical points:\n" <> show l1 <> "\n" <> show l2 <> "\n"
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
firstLineSegOfContour c@(PointContour _ _ p1 p2 _ _)
  | p1 == p2 = error $ "tried to create a line segment from a point when finding the first line segment!\nContour: " <> show c <> "\n"
  | otherwise = makeLineSeg p1 p2
firstLineSegOfContour (LineSegContour _ _ l1 _ _) = l1

firstPointPairOfContour :: Contour -> (Point2, Point2)
firstPointPairOfContour (PointContour _ _ p1 p2 _ _) = (p1,p2)
firstPointPairOfContour (LineSegContour _ _ l1 _ _) = (startPoint l1, endPoint l1)

-- | find the first outer contour of a contourTreeSet, and return it.
firstContourOfContourTreeSet :: ContourTreeSet -> Contour
firstContourOfContourTreeSet (ContourTreeSet (ContourTree contour _) _) = contour

