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

import Prelude (Bool, ($), (<>), (==), (>), (<=), (&&), error, fst, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum), addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (intersectionOf, noIntersection)

import Graphics.Slicer.Math.PGA (CPPoint2, ProjectiveLine2, ProjectivePoint2, PLine2(PLine2), PLine2Err(PLine2Err), angleBetween2PL, distance2PP, eToPL, flipL, join2PP, normalizeL, vecOfL)

-- | Get a Projective Line in the direction of the inside of a contour. Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
getFirstArc :: Point2 -> Point2 -> Point2 -> (PLine2, PLine2Err)
getFirstArc a b c = (res, resErr)
  where
    (res, (_,_, resErr)) = getAcuteArcFromPoints a b c


-- | Get a Projective Line in the direction of the inside of a contour. Generates a line bisecting the angle of the intersection between a line constructed from the first two points, and another line constrected from the last two points.
getAcuteArcFromPoints :: Point2 -> Point2 -> Point2 -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getAcuteArcFromPoints p1 p2 p3
  -- since we hawe two equal sides, we can draw a point ot the other side of the quad, and use it for constructing.
  | distance p2 p1 == distance p2 p3 = (quad, (side1ConsErr <> side1NormErr, side2ConsErr <> side2NormErr, quadErr))
  {-
  | distance p2 p1 > distance p2 p3 = scaleSide p1 p3 True (distance p2 p1 / distance p2 p3)
  | otherwise = scaleSide p3 p1 True (distance p2 p3 / distance p2 p1)
  -}
  | otherwise = (insideArc, insideArcErr)
  where
    (insideArc, insideArcErr) = getAcuteAngleBisector side1 side2
    -- FIXME: how do these errors effect the result?
    (side1, side1NormErr) = normalizeL side1Raw
    (side1Raw, side1ConsErr) = eToPL (makeLineSeg p1 p2)
    (side2, side2NormErr) = normalizeL side2Raw
    (side2Raw, side2ConsErr) = eToPL (makeLineSeg p2 p3)
    (quad, quadErr) = eToPL $ makeLineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3
    {-
    scaleSide ps1 ps2 t v
      | t == True = (PLine2 scaledRes, UlpSum $ scaledUlp + scaledResUlp)
      | otherwise = (flipPLine2 $ PLine2 scaledRes, UlpSum $ scaledUlp + scaledResUlp)
      where
        (NPLine2 scaledRes, UlpSum scaledResUlp) = normalizePLine2WithErr scaled
        -- FIXME: poor ULP tracking on this linear math.
        (scaled, UlpSum scaledUlp) = pLineFromEndpointsWithErr p2 $ scalePoint 0.5 $ addPoints ps1 $ addPoints p2 $ scalePoint v $ addPoints ps2 $ negatePoint p2
    -}

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Wrapper, stripping the normalization error quotents of inputs from its output
getInsideArc :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PLine2, PLine2Err)
getInsideArc line1 line2 = (res, resErr)
  where
    (res, (_, _, resErr)) = getAcuteAngleBisector line1 line2

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'acute' direction.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getAcuteAngleBisector :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getAcuteAngleBisector line1 line2
  | npline1 == npline2 = error "Given two identical lines."
  | npline1 == flipL npline2 = error "Need to be able to return two PLines."
  | otherwise = (PLine2 addVecRes, (npline1Err, npline2Err, PLine2Err addVecErrs mempty mempty mempty mempty mempty))
  where
    (addVecRes, addVecErrs) = addVecPairWithErr lv1 lv2
    lv1 = vecOfL $ flipL npline1
    lv2 = vecOfL npline2
    (npline1, npline1Err) = normalizeL line1
    (npline2, npline2Err) = normalizeL line2

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'obtuse' direction.
getOutsideArc :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => a -> b -> c -> d -> (PLine2, PLine2Err)
getOutsideArc a b c d = (res, resErr)
  where
    (res, (_,_, resErr)) = getObtuseAngleBisector a b c d

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'obtuse' direction.
-- FIXME: the outer PLine returned by two PLines in the same direction should be two PLines, whch are the same line in both directions.
getObtuseAngleBisector :: (ProjectivePoint2 a, ProjectiveLine2 b, ProjectivePoint2 c, ProjectiveLine2 d) => a -> b -> c -> d -> (PLine2, (PLine2Err, PLine2Err, PLine2Err))
getObtuseAngleBisector ppoint1 pline1 ppoint2 pline2
  | npline1 == npline2 = error "need to be able to return two PLines."
  | noIntersection (pline1, mempty) (pline2, mempty) = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | l1TowardPoint && l2TowardPoint = (flipL resFlipped, resFlippedErr)
  | l1TowardPoint                  = (flipL resNormal, resNormalErr)
  | l2TowardPoint                  = (resNormal, resNormalErr)
  | otherwise                      = (resFlipped, resFlippedErr)
    where
      (resNormal, resNormalErr) = getAcuteAngleBisector pline1 pline2
      (resFlipped, resFlippedErr) = getAcuteAngleBisector pline1 (flipL pline2)
      npline1 = normalizeL pline1
      npline2 = normalizeL pline2
      intersectionPoint = fst $ intersectionOf (pline1, mempty) (pline2, mempty)
      l1TowardPoint = towardIntersection ppoint1 pline1 intersectionPoint
      l2TowardPoint = towardIntersection ppoint2 pline2 intersectionPoint

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
