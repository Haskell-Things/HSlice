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
  noIntersection,
  outputIntersectsPLineAt
  ) where

import Prelude (Bool(True), ($), (<), (<=), (<>), (==), (||), (&&), (<$>), Maybe(Just, Nothing), Either(Right, Left), and, error, otherwise, realToFrac, show)

import Data.Either (rights, lefts)

import Data.Maybe (catMaybes, isJust)

import Graphics.Slicer.Math.Definitions (mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (ulpVal)

import Graphics.Slicer.Math.PGA (Arcable(hasArc), PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2Err, PPoint2Err, ProjectiveLine, ProjectiveLine2, ProjectivePoint, distance2PL, distance2PP, distancePPointToPLineWithErr, fuzzinessOfL, fuzzinessOfP, outAndErrOf, pLineErrAtPPoint, plinesIntersectIn)

-- | Check if two lines cannot intersect.
noIntersection :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
noIntersection line1 line2 = isCollinear line1 line2 || isParallel line1 line2 || isAntiCollinear line1 line2 || isAntiParallel line1 line2

-- | Check if two lines are really the same line.
isCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isCollinear line1 line2 = plinesIntersectIn line1 line2 == PCollinear

-- | Check if two lines are really the same line, reversed.
isAntiCollinear :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiCollinear line1 line2 = plinesIntersectIn line1 line2 == PAntiCollinear

-- | Check if two lines are parallel.
isParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isParallel line1 line2 = plinesIntersectIn line1 line2 == PParallel

-- | Check if two lines are anti-parallel.
isAntiParallel :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Bool
isAntiParallel line1 line2 = plinesIntersectIn line1 line2 == PAntiParallel

-- | Get the intersection point of two lines we know have an intersection point.
intersectionOf :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectivePoint, PPoint2Err)
intersectionOf line1 line2 = saneIntersection $ plinesIntersectIn line1 line2
  where
    saneIntersection PAntiCollinear     = error $ "cannot get the intersection of anti-collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PCollinear         = error $ "cannot get the intersection of collinear lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PParallel          = error $ "cannot get the intersection of parallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection PAntiParallel      = error $ "cannot get the intersection of antiparallel lines.\nline1: " <> show line1 <> "\nline2: " <> show line2 <> "\n"
    saneIntersection (IntersectsIn p (_,_, pErr)) = (p, pErr)

-- | Get the intersection point of two lines. if they are collinear, returns a line, and if they are parallel, returns Nothing.
intersectionBetween :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Maybe (Either (a, PLine2Err) (ProjectivePoint, PPoint2Err))
intersectionBetween line1@(l1, _) line2@(l2, _) = saneIntersection $ plinesIntersectIn line1 line2
  where
    (foundDistance, (_,_, foundErr)) = distance2PL l1 l2
    saneIntersection PAntiCollinear     = Just $ Left line1
    saneIntersection PCollinear         = Just $ Left line1
    saneIntersection PParallel          = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection PAntiParallel      = if foundDistance < realToFrac (ulpVal foundErr)
                                          then Just $ Left line1
                                          else Nothing
    saneIntersection (IntersectsIn p (_,_, pErr)) = Just $ Right (p, pErr)

-- | Find out where the output of an Arcable intersects a given PLine2. errors if no intersection.
outputIntersectsPLineAt :: (Arcable a) => a -> (ProjectiveLine, PLine2Err) -> Maybe (ProjectivePoint, PPoint2Err)
outputIntersectsPLineAt n line
  | hasArc n = case res of
                 (IntersectsIn p (_,_, pErr)) -> Just (p, pErr)
                 _ -> Nothing
  | otherwise = error $ "Tried to check if the output of node intersects PLine on a node with no output:\n" <> show n <> "\n" <> show line <> "\n"
  where
    res = plinesIntersectIn (outAndErrOf n) line

-- | Find out where the output two Arcables intersect. returns Nothing if no intersection, and errors if one of the inputs has no output Arc.
intersectionBetweenArcsOf :: (Arcable a, Arcable b) => a -> b -> Maybe (ProjectivePoint, PPoint2Err)
intersectionBetweenArcsOf node1 node2
  | hasArc node1 && hasArc node2 = case res of
                                     (IntersectsIn p (_,_, pErr)) -> Just (p, pErr)
                                     _ -> Nothing
  | otherwise = error $ "Tried to check if the outputs of two nodes intersect, but a node with no output:\n" <> show node1 <> "\n" <> show node2 <> "\n"
  where
    res = plinesIntersectIn (outAndErrOf node1) (outAndErrOf node2)

-- | Find out if all of the possible intersections between all of the given nodes are close enough to be considered intersecting at the same point.
intersectionsAtSamePoint :: [(ProjectiveLine, PLine2Err)] -> Bool
intersectionsAtSamePoint nodeOutsAndErrs
  = case nodeOutsAndErrs of
      [] -> error "given an empty list."
      [a] -> error $ "asked to check for same point intersection of:\n" <> show a <> "\n"
      [a,b] -> isJust $ intersectionBetween a b
      _ -> and (isJust <$> intersections) && pointsCloseEnough && linesCloseEnough
      where
        intersections = mapWithFollower myIntersectionBetween nodeOutsAndErrs
          where
            myIntersectionBetween a b = case intersectionBetween a b of
                                          Nothing -> Nothing
                                          (Just (Right p)) -> Just $ Right (a,b,p)
                                          (Just (Left l)) -> Just $ Left (a,b,l)
        -- intersections that resulted in a point.
        pointIntersections :: [((ProjectiveLine,PLine2Err), (ProjectiveLine,PLine2Err), (ProjectivePoint,PPoint2Err))]
        pointIntersections = rights $ catMaybes intersections
        -- intersections that resulted in a line, but are not anticollinear.
        lineIntersections = lefts $ catMaybes intersections
        pointsCloseEnough = and $ mapWithFollower pairCloseEnough pointIntersections
          where
            -- Minor optimization: first check against resErr, then actually use the fuzziness.
            pairCloseEnough (a1, b1, point1@(c1,_)) (a2, b2, point2@(c2,_)) = res <= realToFrac (ulpVal resErr) || res < realToFrac errSum
              where
                errSum = ulpVal $ resErr <> fuzzinessOfP point1
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
                           ((a2,b2,ppoint1@(p1,_)):_) -> pointsCloseEnough && foundDistance < realToFrac errSum
                             where
                               (foundDistance, (_, _, _, _, _, resErr)) = distancePPointToPLineWithErr ppoint1 l1
                               errSum = ulpVal $ resErr <> fuzzinessOfP ppoint1
                                                        <> pLineErrAtPPoint a2 p1
                                                        <> pLineErrAtPPoint b2 p1
                                                        <> fuzzinessOfL a1
                                                        <> fuzzinessOfL b1
                                                        <> fuzzinessOfL l1
            (_:_) -> error
                     $ "detected multiple lines?\n"
                     <> show lineIntersections <> "\n"
                     <> show pointIntersections <> "\n"
