{- ORMOLU_DISABLE -}
{-
 - Copyright 2021 Julia Longtin
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
   This file contains code for calculating the inside and outside bisectors of two lines.
-}

module Graphics.Slicer.Math.Arcs (getFirstArc, getInsideArc, getOutsideArc, towardIntersection) where

import Prelude (Bool, ($), (<>), (==), (>), (<=), (&&), (||), error, mempty, otherwise, show)

import Data.Maybe (isNothing)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionOf, isAntiCollinear, isCollinear, noIntersection)

import Graphics.Slicer.Math.PGA (PLine2Err(PLine2Err), PPoint2Err, ProjectiveLine(PLine2), ProjectiveLine2, ProjectivePoint2, angleBetween2PL, distance2PP, eToPL, flipL, join2PP, normalizeL, vecOfL)

import Graphics.Slicer.Math.PGAPrimitives (canonicalizedIntersectionOf2PL)

-- | Get a Projective Line in the direction of the inside of a contour. Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
getFirstArc :: Point2 -> Point2 -> Point2 -> (ProjectiveLine, PLine2Err)
getFirstArc a b c = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteArcFromPoints a b c

-- | Get a Projective Line in the direction of the inside of a contour.
--   Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
--   Note: we return normalization error, because we construct projective lines here.
getAcuteArcFromPoints :: Point2 -> Point2 -> Point2 -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getAcuteArcFromPoints p1 p2 p3
  | p1 == p2 || p2 == p3 = error "given two input points that are identical!"
  | isCollinear line1 line2 = error "Given colinear points."
  -- If we hawe two equal sides, we can draw a point on the other side of the quad, and use it for constructing our result.
  | distance p2 p1 == distance p2 p3 = (quad, (mempty, mempty, quadErr))
  | otherwise = (insideArc, (side1ConsErr <> side1NormErr, side2ConsErr <> side2NormErr, insideArcErr))
  where
    (insideArc, (_,_, insideArcErr)) = getAcuteAngleBisectorFromLines line1 line2
    -- FIXME: how do these error quotents effect the angle of the resulting line?
    line1@(_, side1NormErr) = normalizeL side1Raw
    (side1Raw, side1ConsErr) = eToPL (makeLineSeg p1 p2)
    line2@(_, side2NormErr) = normalizeL side2Raw
    (side2Raw, side2ConsErr) = eToPL (makeLineSeg p2 p3)
    -- Only used for the quad case.
    (quad, quadErr) = eToPL $ makeLineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3

-- | Get a projective line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   A wrapper of getAcuteAngleBisectorFromLines, dropping the returning of normalization error of the inputs.
-- Warning: assumes one PLine is toward the point of intersection, and the other is away from the point of intersection
-- FIXME: Precision Loss
getInsideArc :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectiveLine, PLine2Err)
getInsideArc line1 line2 = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteAngleBisectorFromLines line1 line2

-- | Get a projective line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Note that we assume that the first line points toward the intersection.
{-# INLINABLE getAcuteAngleBisectorFromLines #-}
getAcuteAngleBisectorFromLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getAcuteAngleBisectorFromLines line1@(pl1, _) line2@(pl2, _)
  -- something is wrong? throw an error.
  | isNothing (canonicalizedIntersectionOf2PL pl1 pl2) = makeError
  | otherwise = (PLine2 addVecRes, (npline1Err, npline2Err, PLine2Err addVecErrs mempty mempty mempty mempty mempty))
  where
    makeError
      | isCollinear line1 line2 = error $ "Asked to find the acute bisector of two colinear lines!\n" <> show pl1 <> "\n" <> show pl2 <> "\n"
      | isAntiCollinear line1 line2 = error $ "Asked to find the acute bisector of two anti-colinear lines!\n" <> show pl1 <> "\n" <> show pl2 <> "\n"
      | noIntersection line1 line2 = error $ "No intersection between line " <> show pl1 <> " and line " <> show pl2 <> ".\n"
      | otherwise = error "no error. just seeing how you're doing."
    (addVecRes, addVecErrs) = addVecPairWithErr lv1 lv2
    lv1 = vecOfL $ flipL npline1
    lv2 = vecOfL npline2
    (npline1, npline1Err) = normalizeL pl1
    (npline2, npline2Err) = normalizeL pl2

-- | Get a projective line along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
{-# INLINABLE getOutsideArc #-}
getOutsideArc :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => (a, PPoint2Err) -> (b, PLine2Err) -> (c, PPoint2Err) -> (d, PLine2Err) -> (ProjectiveLine, PLine2Err)
getOutsideArc a b c d = (res, resErr)
  where
    (res, (_,_, resErr)) = getObtuseAngleBisectorFromPointedLines a b c d

-- | Get a projective line along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
{-# INLINABLE getObtuseAngleBisectorFromPointedLines #-}
getObtuseAngleBisectorFromPointedLines :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => (a, PPoint2Err) -> (b, PLine2Err) -> (c, PPoint2Err) -> (d, PLine2Err) -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getObtuseAngleBisectorFromPointedLines ppoint1 line1 ppoint2 line2
  | isCollinear line1 line2 = error "Asked to find the obtuse bisector of two colinear lines!"
  | isAntiCollinear line1 line2 = error "Asked to find the obtuse bisector of two anti-colinear lines!"
  | noIntersection line1 line2 = error $ "No intersection between line " <> show line1 <> " and line " <> show line2 <> ".\n"
  | pointDistance <= ulpVal pointDistanceErr = error $ "cannot have two identical input points:\n" <> show ppoint1 <> "\n" <> show ppoint2 <> "\n"
  | point1IntersectDistance <= ulpVal point1IntersectDistanceErr = error $ "intersection of plines is at first ppoint:\n"
                                                                         <> show ppoint1 <> "\n"
                                                                         <> show line1 <> "\n"
                                                                         <> show line2 <> "\n"
  | point2IntersectDistance <= ulpVal point2IntersectDistanceErr = error $ "intersection of plines is at second ppoint:\n"
                                                                         <> show ppoint2 <> "\n"
                                                                         <> show line1 <> "\n"
                                                                         <> show line2 <> "\n"
  | l1TowardPoint && l2TowardPoint = flipFst $ getAcuteAngleBisectorFromLines line1 $ flipFst line2
  | l1TowardPoint                  = flipFst $ getAcuteAngleBisectorFromLines line1 line2
  | l2TowardPoint                  = getAcuteAngleBisectorFromLines line1 line2
  | otherwise                      = getAcuteAngleBisectorFromLines line1 $ flipFst line2
    where
      (pointDistance, (_,_,pointDistanceErr)) = distance2PP ppoint1 ppoint2
      (point1IntersectDistance, (_,_, point1IntersectDistanceErr)) = distance2PP ppoint1 intersectionPoint
      (point2IntersectDistance, (_,_, point2IntersectDistanceErr)) = distance2PP ppoint2 intersectionPoint
      flipFst (a,b) = (flipL a,b)
      intersectionPoint = intersectionOf line1 line2
      l1TowardPoint = towardIntersection ppoint1 line1 intersectionPoint
      l2TowardPoint = towardIntersection ppoint2 line2 intersectionPoint

-- | Determine if the line formed by the two given points goes through the first point first, or the second.
{-# INLINABLE towardIntersection #-}
towardIntersection :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c) => (a, PPoint2Err) -> (b, PLine2Err) -> (c, PPoint2Err) -> Bool
towardIntersection point1@(pp1, _) (pl1, _) point2@(pp2, _)
  | d <= ulpVal dErr = error $ "cannot resolve points finely enough.\nPPoint1: " <> show pp1 <> "\nPPoint2: " <> show pp2 <> "\nPLineIn: " <> show pl1 <> "\nnewPLine: " <> show newPLine <> "\n"
  | otherwise = angleFound > ulpVal angleErr
  where
    (angleFound, (_,_, angleErr)) = angleBetween2PL newPLine pl1
    (d, (_,_, dErr)) = distance2PP point1 point2
    (newPLine, _) = join2PP pp1 pp2
