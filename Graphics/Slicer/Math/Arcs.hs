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

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2, addPoints, distance, makeLineSeg, scalePoint)

import Graphics.Slicer.Math.GeometricAlgebra (addVecPairWithErr, ulpVal)

import Graphics.Slicer.Math.Intersections (isCollinear, isAntiCollinear, isParallel, isAntiParallel, intersectionOf)

import Graphics.Slicer.Math.PGA (PLine2Err(PLine2Err), PPoint2Err, ProjectiveLine(NPLine2, PLine2), ProjectivePoint, angleBetweenWithErr, canonicalize, distanceBetweenPPointsWithErr, eToPLine2WithErr, flipPLine2, join2PPointsWithErr, normalize)

-- | Get a PLine along the angle bisector of the intersection of the two given lines, pointing in the 'obtuse' direction.
-- FIXME: the outer PLine returned by two PLines in the same direction should be two PLines, whch are the same line in both directions.
-- FIXME: shouldn't we be given an error component in our inputs?
getOutsideArcWithErr :: ProjectivePoint -> ProjectiveLine -> ProjectivePoint -> ProjectiveLine -> (ProjectiveLine,(PLine2Err, PLine2Err, PLine2Err))
getOutsideArcWithErr ppoint1 pline1 ppoint2 pline2
  | npline1 == npline2 = error $ "cannot have two identical input lines:\n" <> show pline1 <> "\n" <> show pline2 <> "\n"
  | isCollinear (npline1,npline1Err) (npline2,npline2Err) ||
    isAntiCollinear (npline1,npline1Err) (npline2,npline2Err) = error "need to be able to return two Plines."
  | isParallel  (npline1,npline1Err) (npline2,npline2Err) ||
    isAntiParallel (npline1,npline1Err) (npline2,npline2Err) = error $ "no intersection between pline " <> show pline1 <> " and " <> show pline2 <> ".\n"
  | cppoint1 == cppoint2 = error $ "cannot have two identical input points:\n" <> show ppoint1 <> "\n" <> show ppoint2 <> "\n"
  | intersectionPoint == cppoint1 = error $ "intersection of plines is at first ppoint:\n"
                                          <> show ppoint1 <> "\n"
                                          <> show pline1 <> "\n"
                                          <> show pline2 <> "\n"
  | intersectionPoint == cppoint2 = error $ "intersection of plines is at second ppoint:\n"
                                          <> show ppoint2 <> "\n"
                                          <> show pline1 <> "\n"
                                          <> show pline2 <> "\n"
  | l1TowardPoint && l2TowardPoint = flipFst $ getInsideArcWithErr npline1 (flipPLine2 npline2)
  | l1TowardPoint                  = flipFst $ getInsideArcWithErr npline1 npline2
  | l2TowardPoint                  = getInsideArcWithErr npline1 npline2
  | otherwise                      = getInsideArcWithErr npline1 (flipPLine2 npline2)
    where
      flipFst (a,b) = (flipPLine2 a,b)
      intersectionPoint = intersectionOf (npline1,npline1Err) (npline2,npline2Err)
      l1TowardPoint = towardIntersection (cppoint1,c1Err) (npline1,npline1Err) (intersectionPoint,mempty)
      l2TowardPoint = towardIntersection (cppoint2,c2Err) (npline2,npline2Err) (intersectionPoint,mempty)
      (npline1, npline1Err) = normalize pline1
      (npline2, npline2Err) = normalize pline2
      (cppoint1,c1Err) = canonicalize ppoint1
      (cppoint2,c2Err) = canonicalize ppoint2

-- Determine if the line segment formed by the two given points starts with the first point, or the second.
towardIntersection :: (ProjectivePoint,PPoint2Err) -> (ProjectiveLine,PLine2Err) -> (ProjectivePoint,PPoint2Err) -> Bool
towardIntersection pp1@(rawPp1,_) pl1@(rawPl1,_) pp2@(rawPp2,_)
  | d <= totalErr = error $ "cannot resolve points finely enough.\nPPoint1: " <> show pp1 <> "\nPPoint2: " <> show pp2 <> "\nPLineIn: " <> show pl1 <> "\nnewPLine: " <> show newPLine <> "\n"
  | otherwise = angleFound > 0
  where
    (angleFound, _) = angleBetweenWithErr newPLine rawPl1
    (d, (_,_,_,dErr)) = distanceBetweenPPointsWithErr pp1 pp2
    (newPLine, _) = join2PPointsWithErr rawPp1 rawPp2
    totalErr :: ℝ
    totalErr = realToFrac $ ulpVal dErr

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
    (NPLine2 quadRes, _) = normalize quad
    (quad, quadPLineErr) = eToPLine2WithErr (makeLineSeg p2 $ scalePoint 0.5 $ addPoints p1 p3)
    -- used for all other cases.
    (insideArc, (_,_,insideArcRawErr)) = getInsideArcWithErr (PLine2 side1) (PLine2 side2)
    (NPLine2 side1, side1NormErr) = normalize side1Raw
    (side1Raw, side1RawErr) = eToPLine2WithErr (makeLineSeg p1 p2)
    (NPLine2 side2, side2NormErr) = normalize side2Raw
    (side2Raw, side2RawErr) = eToPLine2WithErr (makeLineSeg p2 p3)
    insideArcErr = side1NormErr <> side1RawErr <> side2NormErr <> side2RawErr <> insideArcRawErr

-- | Get a Projective Line along the angle bisector of the intersection of the two given lines, pointing in the 'acute' direction.
--   Note that we know that the inside is to the right of the first line given, and that the first line points toward the intersection.
getInsideArcWithErr :: ProjectiveLine -> ProjectiveLine -> (ProjectiveLine, (PLine2Err, PLine2Err, PLine2Err))
getInsideArcWithErr pline1 pline2
   -- FIXME: remove this Eq usage!
  | npline1 == npline2 = error "need to be able to return two PLines."
  | otherwise = (res, (npline1Err, npline2Err, resNormErr <> PLine2Err addErr mempty mempty mempty mempty mempty))
  where
      (res, resNormErr) = normalize $ PLine2 rawPLine2
      (rawPLine2, addErr)       = addVecPairWithErr pv1 pv2
      (NPLine2 pv1)             = flipPLine2 npline1
      (npline1, npline1Err) = normalize pline1
      (npline2, npline2Err) = normalize pline2
      pv2 = case pline2 of
              (NPLine2 a) -> a
              (PLine2 a) -> a

