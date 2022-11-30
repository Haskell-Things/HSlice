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

import Prelude (Bool, ($), (<>), (==), (>), (<=), (&&), (||), error, fst, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum), addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionOf, noIntersection)

import Graphics.Slicer.Math.PGA (CPPoint2, ProjectiveLine2, ProjectivePoint2, PLine2(PLine2), PLine2Err(PLine2Err), angleBetween2PL, distance2PP, eToPL, flipL, join2PP, normalizeL, vecOfL)

-- | Get a Projective Line in the direction of the inside of a contour. Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
getFirstArc :: Point2 -> Point2 -> Point2 -> (PLine2, PLine2Err)
getFirstArc a b c = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteArcFromPoints a b c

-- | Get a Projective Line in the direction of the inside of a contour.
--   Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
--   Note: we return normalization error, because we construct projective lines here.
getAcuteArcFromPoints :: Point2 -> Point2 -> Point2 -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getAcuteArcFromPoints p1 p2 p3
  | p1 == p2 || p2 == p3 = error "given two input points that are identical!"
  -- Since we hawe two equal sides, we can draw a point on the other side of the quad, and use it for constructing our result.
  | distance p2 p1 == distance p2 p3 = (quad, (mempty, mempty, quadErr))
  | otherwise = (insideArc, (side1ConsErr <> side1NormErr, side2ConsErr <> side2NormErr, insideArcErr))
  where
    (insideArc, (_,_, insideArcErr)) = getAcuteAngleBisectorFromLines (side1, side1NormErr) (side2, side2NormErr)
    -- FIXME: how do these error quotents effect the resulting line?
    (side1, side1NormErr) = normalizeL side1Raw
    (side1Raw, side1ConsErr) = eToPL (makeLineSeg p1 p2)
    (side2, side2NormErr) = normalizeL side2Raw
    (side2Raw, side2ConsErr) = eToPL (makeLineSeg p2 p3)
    -- Only used for the quad case.
    (quad, quadErr) = eToPL $ makeLineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   A wrapper of getAcuteAngleBisectorFromLines, dropping the returning of normalization error of the inputs.
getInsideArc :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (PLine2, PLine2Err)
getInsideArc line1 line2 = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteAngleBisectorFromLines line1 line2

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Note that we assume that the first line points toward the intersection.
getAcuteAngleBisectorFromLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getAcuteAngleBisectorFromLines (line1,_) (line2,_)
  | npline1 == npline2 = error "Given two identical lines."
  | npline1 == flipL npline2 = error "Need to be able to return two PLines."
  | otherwise = (PLine2 addVecRes, (npline1Err, npline2Err, PLine2Err addVecErrs mempty mempty mempty mempty mempty))
  where
    (addVecRes, addVecErrs) = addVecPairWithErr lv1 lv2
    lv1 = vecOfL $ flipL npline1
    lv2 = vecOfL npline2
    (npline1, npline1Err) = normalizeL line1
    (npline2, npline2Err) = normalizeL line2

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
getOutsideArc :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => a -> (b, PLine2Err) -> c -> (d, PLine2Err) -> (PLine2, PLine2Err)
getOutsideArc a b c d = (res, resErr)
  where
    (res, (_,_, resErr)) = getObtuseAngleBisector a b c d

-- | Get a PLine along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
-- FIXME: the outer line returned by two lines in the same direction should be two lines at a 90 degree angle to the input lines.
getObtuseAngleBisector :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => a -> (b, PLine2Err) -> c -> (d, PLine2Err) -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getObtuseAngleBisector ppoint1 line1@(pl1, pl1Err) ppoint2 line2@(pl2, pl2Err)
  | npline1 == npline2 = error "need to be able to return two PLines."
  | noIntersection line1 line2 = error $ "no intersection between pline " <> show line1 <> " and " <> show line2 <> ".\n"
  | l1TowardPoint && l2TowardPoint = (flipL resFlipped, resFlippedErr)
  | l1TowardPoint                  = (flipL resNormal, resNormalErr)
  | l2TowardPoint                  = (resNormal, resNormalErr)
  | otherwise                      = (resFlipped, resFlippedErr)
    where
      (resNormal, resNormalErr) = getAcuteAngleBisectorFromLines line1 line2
      (resFlipped, resFlippedErr) = getAcuteAngleBisectorFromLines line1 (flipL pl2, pl2Err)
      npline1 = normalizeL pl1
      npline2 = normalizeL pl2
      intersectionPoint = fst $ intersectionOf line1 line2
      l1TowardPoint = towardIntersection ppoint1 pl1 intersectionPoint
      l2TowardPoint = towardIntersection ppoint2 pl2 intersectionPoint

-- | Determine if the line segment formed by the two given points starts with the first point, or the second.
-- Note: Due to numeric uncertainty, we cannot rely on Eq here, and must check the sign of the angle.
-- FIXME: shouldn't we be given an error component in our inputs?
towardIntersection :: (ProjectivePoint2 a, ProjectiveLine2 b) => a -> b -> CPPoint2 -> Bool
towardIntersection pp1 pl1 pp2
  | d <= realToFrac dErr = error $ "cannot resolve points finely enough.\nPPoint1: " <> show pp1 <> "\nPPoint2: " <> show pp2 <> "\nPLineIn: " <> show pl1 <> "\nnewPLine: " <> show newPLine <> "\n"
  | otherwise = angleFound > realToFrac (ulpVal angleErr)
  where
    (angleFound, (_,_, angleErr)) = angleBetween2PL newPLine pl1
    (d, (_,_,UlpSum dErr)) = distance2PP (pp1, mempty) (pp2, mempty)
    newPLine = fst $ join2PP pp1 pp2
