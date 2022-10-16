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

module Graphics.Slicer.Math.Arcs (getFirstArcWithErr, getInsideArcWithErr) where

import Prelude (Bool, ($), (>), (<=), (<>), (==), (&&), (||), (+), error, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2, LineSeg(LineSeg), addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (UlpSum(UlpSum), addVecPairWithErr, sumErrVals, ulpVal)

import Graphics.Slicer.Math.Intersections (isCollinear, isAntiCollinear, isParallel, isAntiParallel, intersectionOf)

import Graphics.Slicer.Math.PGA (PLine2Err(PLine2Err), PPoint2Err, ProjectiveLine2, PLine2(PLine2), ProjectivePoint2, angleBetweenWithErr, canonicalize, distanceBetweenPPointsWithErr, eToPLine2WithErr, flipL, join2PP, normalize, vecOfL)

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
--   Note that we normalize our output, but return it as a PLine2. this is safe, because double normalization (if it happens) only raises the ULP.
getFirstArcWithErr :: Point2 -> Point2 -> Point2 -> (PLine2, UlpSum)
getFirstArcWithErr p1 p2 p3
  -- since we hawe two equal sides, we can draw a point ot the other side of the quad, and use it for constructing.
  | distance p2 p1 == distance p2 p3 = (PLine2 $ vecOfL quadRes, UlpSum $ quadErr + quadResErr)
  {-
  | distance p2 p1 > distance p2 p3 = scaleSide p1 p3 True (distance p2 p1 / distance p2 p3)
  | otherwise = scaleSide p3 p1 True (distance p2 p3 / distance p2 p1)
  -}
  | otherwise = (insideArc, UlpSum $ side1Err + side2Err + insideArcErr)
  where
    (insideArc, UlpSum insideArcErr) = getInsideArcWithErr side1 side2
    (side1, UlpSum side1NormErr) = normalize side1Raw
    (side1Raw, UlpSum side1RawErr) = eToPLine2WithErr (LineSeg p1 p2)
    side1Err = side1NormErr+side1RawErr
    (side2, UlpSum side2NormErr) = normalize side2Raw
    (side2Raw, UlpSum side2RawErr) = eToPLine2WithErr (LineSeg p2 p3)
    side2Err = side2NormErr+side2RawErr
    (quadRes, UlpSum quadResErr) = normalize quad
    (quad, UlpSum quadErr) = eToPLine2WithErr $ LineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3
    {-
    scaleSide ps1 ps2 t v
      | t == True = (PLine2 scaledRes, UlpSum $ scaledUlp + scaledResUlp)
      | otherwise = (flipPLine2 $ PLine2 scaledRes, UlpSum $ scaledUlp + scaledResUlp)
      where
        (NPLine2 scaledRes, UlpSum scaledResUlp) = normalizePLine2WithErr scaled
        -- FIXME: poor ULP tracking on this linear math.
        (scaled, UlpSum scaledUlp) = pLineFromEndpointsWithErr p2 $ scalePoint 0.5 $ addPoints ps1 $ addPoints p2 $ scalePoint v $ addPoints ps2 $ negatePoint p2
    -}

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'acute' direction.
--   Note that we normalize our output, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArcWithErr outputs are normalized.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArcWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PLine2, UlpSum)
getInsideArcWithErr line1 line2
--  | pline1 == pline2 = error "need to be able to return two PLines."
  | otherwise = (PLine2 $ vecOfL normRes, ulpSum)
  where
    ulpSum = resUlp <> sumErrVals addVecErr
    (normRes, resUlp) = normalize $ PLine2 $ addVecRes
    (addVecRes, addVecErr) = addVecPairWithErr flippedPV1 pv2
    flippedPV1 = vecOfL $ flipL line1
    pv2 = vecOfL line2

