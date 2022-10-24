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

import Prelude (($), (<>), (==), mempty, otherwise)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPairWithErr)

import Graphics.Slicer.Math.PGA (ProjectiveLine2, PLine2(PLine2), PLine2Err(PLine2Err), eToPL, flipL, normalizeL, vecOfL)

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
--   Note that we normalize our output, but return it as a PLine2. this is safe, because double normalization (if it happens) only raises the ULP.
getFirstArcWithErr :: Point2 -> Point2 -> Point2 -> (PLine2, PLine2Err)
getFirstArcWithErr p1 p2 p3
  -- since we hawe two equal sides, we can draw a point ot the other side of the quad, and use it for constructing.
  | distance p2 p1 == distance p2 p3 = (PLine2 $ vecOfL quadRes, quadErr <> quadResErr)
  {-
  | distance p2 p1 > distance p2 p3 = scaleSide p1 p3 True (distance p2 p1 / distance p2 p3)
  | otherwise = scaleSide p3 p1 True (distance p2 p3 / distance p2 p1)
  -}
  | otherwise = (insideArc, insideArcErr)
  where
    (insideArc, insideArcErr) = getInsideArcWithErr side1 side2
    -- FIXME: how do these errors effect the result?
    (side1, _) = normalizeL side1Raw
    (side1Raw, _) = eToPL (makeLineSeg p1 p2)
    (side2, _) = normalizeL side2Raw
    (side2Raw, _) = eToPL (makeLineSeg p2 p3)
    (quadRes, quadResErr) = normalizeL quad
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

-- | Get a PLine along the angle bisector of the intersection of the two given line segments, pointing in the 'acute' direction.
--   Note that we normalize our output, but don't bother normalizing our input lines, as the ones we output and the ones getFirstArcWithErr outputs are normalized.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArcWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PLine2, PLine2Err)
getInsideArcWithErr line1 line2
--  | pline1 == pline2 = error "need to be able to return two PLines."
  | otherwise = (PLine2 $ vecOfL normRes, errTotal)
  where
    errTotal = normErr <>  PLine2Err addVecErrs mempty mempty mempty mempty mempty
    (normRes, normErr) = normalizeL $ PLine2 $ addVecRes
    (addVecRes, addVecErrs) = addVecPairWithErr flippedPV1 pv2
    flippedPV1 = vecOfL $ flipL line1
    pv2 = vecOfL line2

