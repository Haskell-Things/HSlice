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

{-
 -  This file contains code for retrieving and reasoning about the intersection of two projective lines.
 -}

module Graphics.Slicer.Math.Intersections (
  intersectionBetween,
  intersectionBetweenArcsOf,
  intersectionOf,
  intersectionsAtSamePoint,
  isAntiCollinear,
  isAntiParallel,
  isCollinear,
  isParallel,
  lineSegsIntersect,
  noIntersection,
  outputIntersectsLineSeg,
  outputIntersectsPLineAt
  ) where

import Prelude (Bool(False, True), (<>), ($), (*), (<), (||), (==), (&&), (<$>), (<=), and, error, otherwise, show)

import Data.Either (Either(Left, Right), lefts, rights)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, isJust, isNothing)

-- The numeric type in HSlice.
import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (LineSeg, mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (Arcable(hasArc), Intersection, PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2Err, PPoint2Err, ProjectiveLine, ProjectiveLine2, ProjectivePoint, canonicalizedIntersectionOf2PL, distance2PL, distance2PP, distancePPToPL, eToPL, fuzzinessOfL, fuzzinessOfP, intersectsWithErr, outAndErrOf, pLineIntersectsLineSeg, pLineErrAtPPoint, plinesIntersectIn)

-- | Check if two lines cannot intersect.
{-# INLINABLE noIntersection #-}
noIntersection :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
noIntersection line1 line2 = res == PCollinear || res == PAntiCollinear || res == PParallel || res == PAntiParallel
    where
      res = plinesIntersectIn line1 line2

-- | Check if two lines are really the same line.
{-# INLINABLE isCollinear #-}
isCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isCollinear line1 line2 = plinesIntersectIn line1 line2 == PCollinear

-- | Check if two lines are really the same line, reversed.
{-# INLINABLE isAntiCollinear #-}
isAntiCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiCollinear line1 line2 = plinesIntersectIn line1 line2 == PAntiCollinear

-- | Check if two lines are parallel.
{-# INLINABLE isParallel #-}
isParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isParallel line1 line2 = plinesIntersectIn line1 line2 == PParallel

-- | Check if two lines are anti-parallel.
{-# INLINABLE isAntiParallel #-}
isAntiParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiParallel line1 line2 = plinesIntersectIn line1 line2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
{-# INLINABLE intersectionOf #-}
intersectionOf :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectivePoint, PPoint2Err)
intersectionOf line1 line2 = saneIntersection $ plinesIntersectIn line1 line2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection (IntersectsIn p (_,_, pErr)) = (p, pErr)

-- | Get the intersection point of two lines. If they are collinear, returns a line, and if they are parallel, returns Nothing.
{-# INLINABLE intersectionBetween #-}
intersectionBetween :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Maybe (Either (a, PLine2Err) (ProjectivePoint, PPoint2Err))
intersectionBetween line1@(l1, _) line2@(l2, _) = saneIntersection $ plinesIntersectIn line1 line2
  where
    (foundDistance, (_,_, foundErr)) = distance2PL l1 l2
    saneIntersection PAntiCollinear     = Just $ Left line1
    saneIntersection PCollinear         = Just $ Left line1
    saneIntersection PParallel          = if foundDistance < ulpVal foundErr
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < ulpVal foundErr
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection (IntersectsIn p (_,_, pErr)) = Just $ Right (p, pErr)

-- | Find out where the output of two Arcables intersect. returns Nothing if no intersection, and errors if no output Arc exists on either input.
{-# INLINABLE intersectionBetweenArcsOf #-}
intersectionBetweenArcsOf :: (Arcable a, Arcable b) => a -> b -> Maybe (ProjectivePoint, PPoint2Err)
intersectionBetweenArcsOf node1 node2
  | hasArc node1 && hasArc node2 = case res of
                                     (IntersectsIn p (_,_, pErr)) -> Just (p, pErr)
                                     _ -> Nothing
  | otherwise = error $ "Tried to check if the outputs of two nodes intersect, but a node with no output:\n" <> show node1 <> "\n" <> show node2 <> "\n"
  where
    res = plinesIntersectIn (outAndErrOf node1) (outAndErrOf node2)

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
{-# INLINABLE outputIntersectsLineSeg #-}
outputIntersectsLineSeg :: (Arcable a) => a -> LineSeg -> Either Intersection PIntersection
outputIntersectsLineSeg source l1
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | otherwise = pLineIntersectsLineSeg (pl1, pl1Err) l1
  where
    (pl2, pl2Err) = eToPL l1
    (pl1, pl1Err) = outAndErrOf source
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2

-- | Find out where the output of an Arcable intersects a given PLine2. errors if no intersection.
{-# INLINABLE outputIntersectsPLineAt #-}
outputIntersectsPLineAt :: (Arcable a, ProjectiveLine2 b) => a -> (b, PLine2Err) -> Maybe (ProjectivePoint, PPoint2Err)
outputIntersectsPLineAt n line
  | hasArc n = case res of
                 (IntersectsIn p (_,_, pErr)) -> Just (p, pErr)
                 _ -> Nothing
  | otherwise = error $ "Tried to check if the output of node intersects PLine on a node with no output:\n" <> show n <> "\n" <> show line <> "\n"
  where
    res = plinesIntersectIn (outAndErrOf n) line

-- | Check if two line segments intersect.
lineSegsIntersect :: LineSeg -> LineSeg -> Bool
lineSegsIntersect l1 l2 = isIntersection $ intersectsWithErr (Left l1 :: Either LineSeg (ProjectiveLine, PLine2Err)) (Left l2 :: Either LineSeg (ProjectiveLine, PLine2Err))
      where
        isIntersection i = case i of
                             (Right (IntersectsIn _ _)) -> True
                             _                          -> False

-- | Find out if all of the possible intersections between all of the given nodes are close enough to be considered intersecting at the same point.
{-# INLINABLE intersectionsAtSamePoint #-}
intersectionsAtSamePoint :: (ProjectiveLine2 a) => [(a, PLine2Err)] -> Bool
intersectionsAtSamePoint nodeOutsAndErrs
  = case nodeOutsAndErrs of
      [] -> error "given an empty list."
      [a] -> error $ "asked to check for same point intersection of:\n" <> show a <> "\n"
      [a,b] -> isJust $ intersectionBetween a b
      _ -> and (isJust <$> intersections) && pointsCloseEnough && linesCloseEnough
        where
          -- FIXME: magic number
          fuzzinessFactor :: ℝ
          fuzzinessFactor = 512
          intersections = mapWithFollower myIntersectionBetween nodeOutsAndErrs
            where
              myIntersectionBetween a b = case intersectionBetween a b of
                                            Nothing -> Nothing
                                            (Just (Right p)) -> Just $ Right (a,b,p)
                                            (Just (Left l)) -> Just $ Left (a,b,l)
          pointsCloseEnough = and $ mapWithFollower pairCloseEnough pointIntersections
            where
              -- intersections that resulted in a point.
              pointIntersections = rights $ catMaybes intersections
              -- Minor optimization: first check against resErr, then actually use the fuzziness.
              pairCloseEnough (a1, b1, point1@(c1,_)) (a2, b2, point2@(c2,_)) = res <= ulpVal resErr * fuzzinessFactor || res < errSum * fuzzinessFactor
                where
                  errSum = ulpVal $ resErr
                                  <> fuzzinessOfP point1
                                  <> pLineErrAtPPoint a1 c1
                                  <> pLineErrAtPPoint b1 c1
                                  <> fuzzinessOfP point2
                                  <> pLineErrAtPPoint a2 c2
                                  <> pLineErrAtPPoint b2 c2
                  (res, (_,_,resErr)) = distance2PP point1 point2
          linesCloseEnough =
            case lineIntersections of
              [] -> True
              [(a1,b1,l1)] -> case pointIntersections of
                                [] -> error "one line, no points.. makes no sense."
                                ((a2,b2,ppoint1@(p1,_)):_) -> pointsCloseEnough && foundDistance < errSum * fuzzinessFactor
                                  where
                                    (foundDistance, (_, _, _, _, _, resErr)) = distancePPToPL ppoint1 l1
                                    errSum = ulpVal $ resErr
                                                    <> fuzzinessOfP ppoint1
                                                    <> pLineErrAtPPoint a2 p1
                                                    <> pLineErrAtPPoint b2 p1
                                                    <> fuzzinessOfL a1
                                                    <> fuzzinessOfL b1
                                                    <> fuzzinessOfL l1
              (_:_) -> error $ "detected multiple lines?\n"
                             <> show lineIntersections <> "\n"
                             <> show pointIntersections <> "\n"
            where
              -- intersections that resulted in a point.
              pointIntersections = rights $ catMaybes intersections
              -- intersections that resulted in a line, but are not anticollinear.
              lineIntersections = lefts $ catMaybes intersections
