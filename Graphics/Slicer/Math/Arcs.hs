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

module Graphics.Slicer.Math.Arcs (getFirstArcWithErr, getOutsideArcWithErr, towardIntersection, getInsideArcWithErr) where

import Prelude (Bool, ($), (>), (<=), (<>), (==), (&&), (||), error, mempty, otherwise, realToFrac, show)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (isCollinear, isAntiCollinear, isParallel, isAntiParallel, intersectionOf)

import Graphics.Slicer.Math.PGA (PIntersection(IntersectsIn, PParallel, PAntiParallel, PCollinear, PAntiCollinear), PLine2Err(PLine2Err), ProjectiveLine(NPLine2, PLine2), ProjectivePoint, angleBetweenWithErr, distanceBetweenPPointsWithErr, flipPLine2, join2PPointsWithErr, normalizePLine2WithErr, pLineFromEndpointsWithErr, plinesIntersectIn)

-- | Get a PLine along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
-- FIXME: the outer PLine returned by two PLines in the same direction should be two PLines, whch are the same line in both directions.
-- FIXME: shouldn't we be given an error component in our inputs?
getOutsideArcWithErr :: ProjectivePoint -> ProjectiveLine -> ProjectivePoint -> ProjectiveLine -> (ProjectiveLine,PLine2Err)
getOutsideArcWithErr ppoint1 pline1 ppoint2 pline2
  | pline1 == pline2 = error $ "cannot have two identical input lines:\n" <> show pline1 <> "\n" <> show pline2 <> "\n"
  | isCollinear (pline1,mempty) (pline2,mempty) ||
    isAntiCollinear (pline1,mempty) (pline2,mempty) = error "need to be able to return two Plines."
  | isParallel  (pline1,mempty) (pline2,mempty) ||
    isAntiParallel (pline1,mempty) (pline2,mempty) = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | ppoint1 == ppoint2 = error $ "cannot have two identical input points:\n" <> show ppoint1 <> "\n" <> show ppoint2 <> "\n"
  | l1TowardPoint && l2TowardPoint = flipFst $ getInsideArcWithErr pline1 (flipPLine2 pline2)
  | l1TowardPoint                  = flipFst $ getInsideArcWithErr pline1 pline2
  | l2TowardPoint                  = getInsideArcWithErr pline1 pline2
  | otherwise                      = getInsideArcWithErr pline1 (flipPLine2 pline2)
    where
      flipFst (a,b) = (flipPLine2 a,b)
      intersectionPoint = intersectionOf pline1 pline2
      l1TowardPoint = towardIntersection ppoint1 pline1 intersectionPoint
      l2TowardPoint = towardIntersection ppoint2 pline2 intersectionPoint

-- Determine if the line segment formed by the two given points starts with the first point, or the second.
-- Note that due to numeric uncertainty, we cannot rely on Eq here, and must check the sign of the angle.
-- FIXME: shouldn't we be given an error component in our inputs?
towardIntersection :: ProjectivePoint -> ProjectiveLine -> ProjectivePoint -> Bool
towardIntersection pp1 pl1 pp2
  | d <= totalErr = error $ "cannot resolve points finely enough.\nPPoint1: " <> show pp1 <> "\nPPoint2: " <> show pp2 <> "\nPLineIn: " <> show pl1 <> "\nnewPLine: " <> show newPLine <> "\n"
  | otherwise = angleFound > 0
  where
    (angleFound, _) = angleBetweenWithErr newPLine pl1
    (d, (_,_,PLine2Err _ _ _ transErr _, canonicalizeErr)) = distanceBetweenPPointsWithErr (pp1,mempty) (pp2,mempty)
    (newPLine, joinErr) = join2PPointsWithErr pp1 pp2
    totalErr = realToFrac $ ulpVal $ transErr <> canonicalizeErr

-- | Get a PLine in the direction of the inside of the contour, given three points on the edge of the contour.
-- FIXME: outputs that are normalized should be using the right constructor.
getFirstArcWithErr :: Point2 -> Point2 -> Point2 -> (ProjectiveLine, PLine2Err)
getFirstArcWithErr p1 p2 p3
  -- since we hawe two equal sides, we can draw a point ot the other side of the quad, and use it for constructing.
  | distance p2 p1 == distance p2 p3 = (PLine2 quadRes, quadPLineErr)
  | otherwise = (insideArc, insideArcErr)
  where
    -- only used for the quad case.
    -- FIXME: how do we track error from normalization?
    (NPLine2 quadRes, _) = normalizePLine2WithErr quad
    (quad, quadPLineErr) = pLineFromEndpointsWithErr p2 $ scalePoint 0.5 $ addPoints p1 p3
    -- used for all other cases.
    (insideArc, PLine2Err _ insideArcNormErr _ _ _) = getInsideArcWithErr (PLine2 side1) (PLine2 side2)
    (NPLine2 side1, PLine2Err _ side1NormErr _ _ _) = normalizePLine2WithErr side1Raw
    (side1Raw, PLine2Err _ _ side1AngleErr side1TranslateErr _) = pLineFromEndpointsWithErr p1 p2
    (NPLine2 side2, PLine2Err _ side2NormErr _ _ _) = normalizePLine2WithErr side2Raw
    (side2Raw, PLine2Err _ _ side2AngleErr side2TranslateErr _) = pLineFromEndpointsWithErr p2 p3
    insideArcErr = PLine2Err
                   mempty
                   (side1NormErr <> side2NormErr <> insideArcNormErr)
                   (side1AngleErr <> side2AngleErr)
                   (side1TranslateErr <> side2TranslateErr)
                   mempty

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArcWithErr :: ProjectiveLine -> ProjectiveLine -> (ProjectiveLine, PLine2Err)
getInsideArcWithErr pline1 pline2
   -- FIXME: remove this Eq usage!
  | npline1 == npline2 = error "need to be able to return two PLines."
  | otherwise = (res, resNormErr <> PLine2Err addErr mempty mempty mempty mempty)
  where
      (res, resNormErr) = normalizePLine2WithErr $ PLine2 rawPLine2
      (rawPLine2, addErr)       = addVecPairWithErr pv1 pv2
      (NPLine2 pv1)             = flipPLine2 npline1
      (npline1, npline1Err) = case pline1 of
              a@(NPLine2 _) -> (a,mempty)
              a@(PLine2 _) -> normalizePLine2WithErr a        
      (npline2, npline2Err) = case pline2 of
              a@(NPLine2 _) -> (a,mempty)
              a@(PLine2 _) -> normalizePLine2WithErr a        
      pv2 = case pline2 of
              (NPLine2 a) -> a
              (PLine2 a) -> a

