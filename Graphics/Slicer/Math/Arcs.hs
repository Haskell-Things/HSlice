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

import Prelude (Bool, ($), (>), (<=), (<>), (==), (&&), (||), error, fst, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (isCollinear, isAntiCollinear, isParallel, isAntiParallel, intersectionOf)

import Graphics.Slicer.Math.PGA (PLine2Err(PLine2Err), PPoint2Err, ProjectiveLine(PLine2), ProjectiveLine2, ProjectivePoint, angleBetween2PL, canonicalizeP, distance2PP, eToPL, flipL, join2PP, normalizeL, vecOfL)

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
  -- Since we hawe two equal sides, we can draw a point on the other side of the quad, and use it for constructing our result.
  | distance p2 p1 == distance p2 p3 = (quad, (mempty, mempty, quadErr))
  | otherwise = (insideArc, (side1ConsErr <> side1NormErr, side2ConsErr <> side2NormErr, insideArcErr))
  where
    (insideArc, (_,_, insideArcErr)) = getAcuteAngleBisectorFromLines line1 line2
    -- FIXME: how do these error quotents effect the resulting line?
    line1@(_, side1NormErr) = normalizeL side1Raw
    (side1Raw, side1ConsErr) = eToPL (makeLineSeg p1 p2)
    line2@(_, side2NormErr) = normalizeL side2Raw
    (side2Raw, side2ConsErr) = eToPL (makeLineSeg p2 p3)
    -- Only used for the quad case.
    (quad, quadErr) = eToPL $ makeLineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   A wrapper of getAcuteAngleBisectorFromLines, dropping the returning of normalization error of the inputs.
getInsideArc :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectiveLine, PLine2Err)
getInsideArc line1 line2 = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteAngleBisectorFromLines line1 line2

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Note that we assume that the first line points toward the intersection.
getAcuteAngleBisectorFromLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getAcuteAngleBisectorFromLines line1@(rawLine1, _) line2@(rawLine2, _)
  | isCollinear line1 line2 = error "given two input lines that are colinear!"
  | isAntiCollinear line1 line2 = error "given two opposing lines, need to return two opposing lines from point of intersection!"
  | isParallel line1 line2 ||
    isAntiParallel line1 line2 = error $ "no intersection between line " <> show line1 <> " and " <> show line2 <> ".\n"
  | otherwise = (PLine2 addVecRes, (npline1Err, npline2Err, PLine2Err addVecErrs mempty mempty mempty mempty mempty))
  where
    (addVecRes, addVecErrs) = addVecPairWithErr lv1 lv2
    lv1 = vecOfL $ flipL npline1
    lv2 = vecOfL npline2
    (npline1, npline1Err) = normalizeL rawLine1
    (npline2, npline2Err) = normalizeL rawLine2

-- | Get a PLine along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
getOutsideArc :: ProjectivePoint -> ProjectiveLine -> ProjectivePoint -> ProjectiveLine -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getOutsideArc ppoint1 pline1 ppoint2 pline2 = getObtuseAngleBisectorFromPointedLines ppoint1 pline1 ppoint2 pline2

-- | Get a PLine along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
getObtuseAngleBisectorFromPointedLines :: ProjectivePoint -> ProjectiveLine -> ProjectivePoint -> ProjectiveLine -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getObtuseAngleBisectorFromPointedLines ppoint1 pline1 ppoint2 pline2
  | isCollinear (npline1,npline1Err) (npline2,npline2Err) = error "given two input lines that are colinear!"
  | isAntiCollinear (npline1,npline1Err) (npline2,npline2Err) = error "need to be able to return two Plines."
  | isParallel (npline1,npline1Err) (npline2,npline2Err) ||
    isAntiParallel (npline1,npline1Err) (npline2,npline2Err) = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
   -- FIXME: remove this Eq usage!
  | cppoint1 == cppoint2 = error $ "cannot have two identical input points:\n" <> show ppoint1 <> "\n" <> show ppoint2 <> "\n"
  -- FIXME: do not use == for points, use distance!
  | fst intersectionPoint == cppoint1 = error $ "intersection of plines is at first ppoint:\n"
                                          <> show ppoint1 <> "\n"
                                          <> show pline1 <> "\n"
                                          <> show pline2 <> "\n"
  | fst intersectionPoint == cppoint2 = error $ "intersection of plines is at second ppoint:\n"
                                          <> show ppoint2 <> "\n"
                                          <> show pline1 <> "\n"
                                          <> show pline2 <> "\n"
  | l1TowardPoint && l2TowardPoint = flipFst $ getAcuteAngleBisectorFromLines (npline1, npline1Err) (flipL npline2, npline2Err)
  | l1TowardPoint                  = flipFst $ getAcuteAngleBisectorFromLines (npline1, npline1Err) (npline2, npline2Err)
  | l2TowardPoint                  = getAcuteAngleBisectorFromLines (npline1, npline1Err) (npline2, npline2Err)
  | otherwise                      = getAcuteAngleBisectorFromLines (npline1, npline1Err) (flipL npline2, npline2Err)
    where
      flipFst (a,b) = (flipL a,b)
      intersectionPoint = intersectionOf (npline1,npline1Err) (npline2,npline2Err)
      l1TowardPoint = towardIntersection (cppoint1,c1Err) (npline1,npline1Err) intersectionPoint
      l2TowardPoint = towardIntersection (cppoint2,c2Err) (npline2,npline2Err) intersectionPoint
      (npline1, npline1Err) = normalizeL pline1
      (npline2, npline2Err) = normalizeL pline2
      (cppoint1,c1Err) = canonicalizeP ppoint1
      (cppoint2,c2Err) = canonicalizeP ppoint2

-- Determine if the line segment formed by the two given points starts with the first point, or the second.
towardIntersection :: (ProjectivePoint,PPoint2Err) -> (ProjectiveLine,PLine2Err) -> (ProjectivePoint,PPoint2Err) -> Bool
towardIntersection point1@(pp1,_) (pl1,_) point2@(pp2,_)
  | d <= realToFrac (ulpVal dErr) = error $ "cannot resolve points finely enough.\nPPoint1: " <> show pp1 <> "\nPPoint2: " <> show pp2 <> "\nPLineIn: " <> show pl1 <> "\nnewPLine: " <> show newPLine <> "\n"
  | otherwise = angleFound > realToFrac (ulpVal angleErr)
  where
    (angleFound, (_,_, angleErr)) = angleBetween2PL newPLine pl1
    (d, (_,_, dErr)) = distance2PP point1 point2
    (newPLine, _) = join2PP pp1 pp2
