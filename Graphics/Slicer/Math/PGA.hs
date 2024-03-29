{- ORMOLU_DISABLE -}
{-
 - Copyright 2020-2022 Julia Longtin
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

-- For using Rounded flexibly.
{-# LANGUAGE DataKinds #-}

-- | The purpose of this file is to hold projective geometric algebraic arithmatic. It defines a 2D PGA with mixed linear components.

module Graphics.Slicer.Math.PGA(
  Arcable(
      errOfOut,
      hasArc,
      outOf
      ),
  Intersection(HitStartPoint, HitEndPoint, NoIntersection),
  PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2Err(PLine2Err),
  Pointable(
      canPoint,
      cPPointOf,
      ePointOf,
      errOfCPPoint
      ),
  PPoint2Err,
  ProjectiveLine(PLine2, NPLine2),
  ProjectiveLine2(
      normalizeL,
      vecOfL
      ),
  ProjectivePoint(PPoint2, CPPoint2),
  ProjectivePoint2(
      canonicalizeP,
      vecOfP
      ),
  angleBetween2PL,
  angleCosBetween2PL,
  canonicalizedIntersectionOf2PL,
  combineConsecutiveLineSegs,
  cPPointAndErrOf,
  distancePPToPL,
  distance2PL,
  distance2PP,
  eToPL,
  eToPP,
  flipL,
  fuzzinessOfL,
  fuzzinessOfP,
  interpolate2PL,
  interpolate2PP,
  intersectsWithErr,
  intersect2PL,
  join2EP,
  join2PP,
  makeCPPoint2,
  onSegment,
  oppositeDirection,
  outAndErrOf,
  pLineErrAtPPoint,
  pLineIntersectsLineSeg,
  pLineIsLeft,
  plinesIntersectIn,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEP,
  sameDirection,
  translateL,
  translateRotatePPoint2WithErr
  ) where

import Prelude (Bool, Eq((==)), Monoid(mempty), Semigroup((<>)), Show(show), ($), (-), (&&), (<$>), (>), (>=), (<=), (+), (/), (||), (<), abs, cos, error, negate, otherwise, realToFrac, signum, sin)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Extra (unsnoc)

import Data.List.Ordered (foldt)

import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, isJust, isNothing)

import Data.Set (singleton, fromList)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, TowardNegInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (LineSeg, Point2(Point2), distance, endPoint, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal, GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎤+), (⨅+), (•), addValWithoutErr, addVecPairWithoutErr, eValOf, getVal, mulScalarVecWithErr, ulpRaw, ulpVal, valOf)

import Graphics.Slicer.Math.Line (combineLineSegs, makeLineSeg)

import Graphics.Slicer.Math.PGAPrimitives(ProjectivePoint(CPPoint2,PPoint2), ProjectiveLine(NPLine2,PLine2), PLine2Err(PLine2Err), PPoint2Err, ProjectiveLine2(normalizeL, vecOfL), ProjectivePoint2(canonicalizeP, isIdealP, vecOfP), angleBetween2PL, angleCosBetween2PL, canonicalizedIntersectionOf2PL, distance2PL, distance2PP, flipL, forceBasisOfL, forceBasisOfP, fuzzinessOfL, fuzzinessOfP, idealNormOfP, interpolate2PL, interpolate2PP, intersect2PL, join2PP, pLineErrAtPPoint, pLinesWithinErr, pToEP, translateL, xIntercept, yIntercept)

-- Our 2D plane coresponds to a Clifford algebra of 2,0,1.

-------------------------------------------------------------
-------------- Projective Line Interface --------------------
-------------------------------------------------------------

-- | The Projective result of line intersection in 2 dimensions.
data PIntersection =
  PCollinear
  | PAntiCollinear
  | PParallel
  | PAntiParallel
  | IntersectsIn !ProjectivePoint !(PLine2Err, PLine2Err, PPoint2Err)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
{-# INLINABLE plinesIntersectIn #-}
plinesIntersectIn :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> PIntersection
plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | isNothing canonicalizedIntersection
  || (idealNorm <= ulpVal idnErr
     && (sameDirection pl1 pl2 ||
         oppositeDirection pl1 pl2)) = if sameDirection pl1 pl2
                                       then PCollinear
                                       else PAntiCollinear
  | sameDirection pl1 pl2            = if d < parallelFuzziness
                                       then PCollinear
                                       else PParallel
  | oppositeDirection pl1 pl2        = if d < parallelFuzziness
                                       then PAntiCollinear
                                       else PAntiParallel
  | otherwise                        = IntersectsIn res (pl1Err <> npl1Err, pl2Err <> npl2Err, resErr)
  where
    -- | The distance within which we consider (anti)parallel lines to be (anti)colinear.
    -- FIXME: pLineErrAtPPoint may give too much for parallel lines.
    parallelFuzziness = ulpVal $ dErr <> pLineErrAtPPoint (pl1, pl1Err) res <> pLineErrAtPPoint (pl2, pl2Err) res
    -- | When two lines are really close to parallel or antiparallel, we use the distance between the lines to decide whether to promote them to being (anti)colinear.
    (d, (_, _, dErr)) = distance2PL pl1 pl2
    (idealNorm, idnErr) = idealNormOfP res
    (res, (npl1Err, npl2Err, resErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2

-- | Check if the first line's direction is on the 'left' side of the second line, assuming they intersect. If they don't intersect, return Nothing.
{-# INLINABLE pLineIsLeft #-}
pLineIsLeft :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Maybe Bool
pLineIsLeft line1 line2
  | abs res <= ulpVal angleFuzz = Nothing
  | otherwise                   = Just $ res > 0
  where
    (res, (_,_, angleFuzz)) = angleCosBetween2PL line2 line1

-- | Find the distance between a projective point and a projective line, along with the difference's error quotent.
-- Note: Fails in the case of ideal points.
{-# INLINABLE distanceProjectivePointToProjectiveLine #-}
distanceProjectivePointToProjectiveLine, distancePPToPL :: (ProjectivePoint2 a, ProjectiveLine2 b) => (a, PPoint2Err) -> (b, PLine2Err) -> (ℝ, (PPoint2Err, PLine2Err, ([ErrVal],[ErrVal]), PLine2Err, PPoint2Err, UlpSum))
distanceProjectivePointToProjectiveLine (inPoint, inPointErr) (inLine, inLineErr)
  | isIdealP inPoint = error "attempted to get the distance of an ideal point."
  | otherwise = (res, resErr)
  where
    resErr = (cPointErr, nLineErr, (plMulErr, plAddErr), perpLineNormErr, crossPointErr, errSum)
      where
        errSum = distanceErr <> fuzzinessOfP (inPoint, inPointErr) <> pLineErrAtPPoint (inLine, inLineErr) crossPoint <> pLineErrAtPPoint (perpLine, perpLineNormErr) crossPoint
    -- | use distance2PP to find the distance between this crossover point, and the given point.
    (res, (_, _, distanceErr)) = distance2PP (cPoint, pointErr) (crossPoint, crossPointErr)
    -- | Get the point where the perpendicular line and the input line meet.
    -- FIXME: how does perpLineErr effect the result of canonicalizedIntersectionOf2PL?
    (crossPoint, (_, perpLineNormErr, crossPointErr)) = fromJust $ canonicalizedIntersectionOf2PL nLine perpLine
    -- | Get a perpendicular line, crossing the input line at the given point.
    -- FIXME: where should we put this in PLine2Err?
    (perpLine, (_, _, (plMulErr, plAddErr))) = perpLineAt nLine cPoint
    pointErr = inPointErr <> cPointErr
    (nLine, nLineErr) = normalizeL inLine
    (cPoint, cPointErr) = canonicalizeP inPoint
-- FIXME: return result is a bit soupy.
{-# INLINABLE distancePPToPL #-}
distancePPToPL = distanceProjectivePointToProjectiveLine

-- | Determine if two points are on the same side of a given line.
-- Returns Nothing if one of the points is on the line.
{-# INLINABLE pPointsOnSameSideOfPLine #-}
pPointsOnSameSideOfPLine :: (ProjectivePoint2 a, ProjectivePoint2 b, ProjectiveLine2 c) => a -> b -> c -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  |  abs foundP1 < foundErr1 ||
     abs foundP2 < foundErr2    = Nothing
    | otherwise = Just $ signum foundP1 == signum foundP2
  where
    foundErr1 = ulpVal $ eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1AddErr) <> eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1MulErr)
    foundErr2 = ulpVal $ eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2AddErr) <> eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2MulErr)
    foundP1 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1
    foundP2 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2
    (GVec unlikeP1, (unlikeP1MulErr, unlikeP1AddErr)) = pv1 ⎤+ lv1
    (GVec unlikeP2, (unlikeP2MulErr, unlikeP2AddErr)) = pv2 ⎤+ lv1
    pv1 = vecOfP $ forceBasisOfP point1
    pv2 = vecOfP $ forceBasisOfP point2
    lv1 = vecOfL $ forceBasisOfL line

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel, or colinear.
{-# INLINABLE sameDirection #-}
sameDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
sameDirection a b = res >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = realToFrac (1 - ulpRaw resErr :: Rounded 'TowardInf ℝ)
    (res, (_,_, resErr)) = angleBetween2PL a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel.
{-# INLINABLE oppositeDirection #-}
oppositeDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
oppositeDirection a b = res <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = realToFrac (realToFrac (ulpRaw resErr) + (-1) :: Rounded 'TowardNegInf ℝ)
    (res, (_,_, resErr)) = angleBetween2PL a b

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
-- FIXME: many operators here have error preserving forms, use those!
-- FIXME: we were skipping canonicalization, are canonicalization and normalization necessary?
pPointOnPerpWithErr :: (ProjectiveLine2 a, ProjectivePoint2 b) => a -> b -> ℝ -> (ProjectivePoint, (PLine2Err, PPoint2Err, ([ErrVal],[ErrVal]), UlpSum))
{-# INLINABLE pPointOnPerpWithErr #-}
pPointOnPerpWithErr line point d = (PPoint2 res, resErr)
  where
    -- translate the input point along the perpendicular bisector.
    res = motor•pVec•reverseGVec motor
    resErr = (nLineErr, cPointErr, perpLineErrs, gaIScaledErr)
    motor = addVecPairWithoutErr (perpLineVec • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to reduce the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIScaledErr = UlpSum $ realToFrac $ doubleUlp $ realToFrac (realToFrac (abs d) / 2 :: Rounded 'TowardInf ℝ)
    -- | Get a perpendicular line, crossing the input line at the given point.
    -- FIXME: where should we put this in the error quotent of PLine2Err?
    perpLineVec = vecOfL perpLine
    (perpLine, (nLineErr, _, perpLineErrs)) = perpLineAt line cPoint
    pVec = vecOfP $ forceBasisOfP cPoint
    (cPoint, cPointErr) = canonicalizeP point

-- Find a projective line crossing the given projective line at the given projective point at a 90 degree angle.
perpLineAt :: (ProjectiveLine2 a, ProjectivePoint2 b) => a -> b -> (ProjectiveLine, (PLine2Err, PPoint2Err, ([ErrVal],[ErrVal])))
{-# INLINABLE perpLineAt #-}
perpLineAt line point = (PLine2 res, resErr)
  where
    (res, perpLineErrs) = lvec ⨅+ pvec
    resErr = (nLineErr, cPointErr, perpLineErrs)
    lvec = vecOfL $ forceBasisOfL nLine
    (nLine, nLineErr) = normalizeL line
    pvec = vecOfP $ forceBasisOfP cPoint
    (cPoint, cPointErr) = canonicalizeP point

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
-- FIXME: throw this error into PPoint2Err.
{-# INLINABLE translateRotatePPoint2WithErr #-}
translateRotatePPoint2WithErr :: (ProjectivePoint2 a) => a -> ℝ -> ℝ -> (ProjectivePoint, (UlpSum, UlpSum, [ErrVal], PLine2Err, PLine2Err, PPoint2Err, ([ErrVal],[ErrVal])))
translateRotatePPoint2WithErr point d rotation = (res, resErr)
  where
    res = PPoint2 $ translator•pVec•reverseGVec translator
    resErr = (gaIScaledErr, rotationErr, scaledPVecErr, yLineErr, nYLineErr, cPointErr, xLineErr)
    -- Our translation motor, which translates the provided distance along the angled line.
    translator = addVecPairWithoutErr (gaIScaled • angledLineThroughPPoint2) (GVec [GVal 1 (singleton G0)])
    -- A line crossing the provided point, at the provided angle.
    angledLineThroughPPoint2 = rotator • vecOfL xLineThroughPPoint2 • reverseGVec rotator
    -- A line along the X axis, crossing the provided point.
    (xLineThroughPPoint2, (nYLineErr, cPointErr, xLineErr)) = perpLineAt yLine point
    -- A line along the Y axis, crossing the origin.
    (yLine, yLineErr) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (0,1))
    -- Our rotation motor, which rotates the provided angle around the provided point, in the clockwise direction.
    rotator = addVecPairWithoutErr scaledPVec (GVec [GVal (cos $ rotation/2) (singleton G0)])
    (scaledPVec, scaledPVecErr) = mulScalarVecWithErr (sin $ rotation/2) pVec
    rotationErr = UlpSum $ realToFrac $ doubleUlp $ realToFrac (realToFrac rotation / 2 :: Rounded 'TowardInf ℝ)
    -- I, in this geometric algebra system. We multiply it times -d/2, to reduce the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (-d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIScaledErr = UlpSum $ realToFrac $ doubleUlp $ realToFrac (realToFrac (abs d) / 2 :: Rounded 'TowardInf ℝ)
    pVec = vecOfP point

----------------------------------------------------------
-------------------- Node Interface ----------------------
----------------------------------------------------------

-- | Does this node have an output (resulting) line?
class (Show a) => Arcable a where
  -- | Return the error quotent of the output arc, if the output arc exists.
  errOfOut :: a -> PLine2Err
  -- | Is there an output arc from this node?
  hasArc :: a -> Bool
  -- | If there is an output arc, return it.
  outOf :: a -> ProjectiveLine

-- | If there is an output arc, return it, along with it's error quotent.
outAndErrOf :: (Arcable a) => a -> (ProjectiveLine, PLine2Err)
{-# INLINABLE outAndErrOf #-}
outAndErrOf a
  | hasArc a = (outOf a, errOfOut a)
  | otherwise = error $ "Asked for out and err of Arcable with no out!\n" <> show a <> "\n"

-- | Typeclass for nodes that may be able to be resolved into a point.
class (Show a) => Pointable a where
  -- | Can this node be resolved into a point in 2d space?
  canPoint :: a -> Bool
  -- | Get a canonicalized projective representation of this point.
  cPPointOf :: a -> ProjectivePoint
  -- | Get a euclidian representation of this point.
  ePointOf :: a -> Point2
  -- | If the point is not a native euclidian point, the error generated while converting from a projective form. otherwise mempty.
  errOfCPPoint :: a -> PPoint2Err

-- | If the given node can be resolved to a point, return it, along with it's error quotent.
cPPointAndErrOf :: (Pointable a) => a -> (ProjectivePoint, PPoint2Err)
cPPointAndErrOf node
  | canPoint node = (cPPointOf node, errOfCPPoint node)
  | otherwise = error "not able to resolve node to a point."

----------------------------------------------------------
-------------- Euclidian Mixed Interface -----------------
----------------------------------------------------------

-- | Intersection events that can only happen with line segments.
data Intersection =
    NoIntersection !ProjectivePoint !(PLine2Err, PLine2Err, PPoint2Err)
  | HitStartPoint !LineSeg
  | HitEndPoint !LineSeg
  deriving Show

-- | Entry point usable for common intersection needs, complete with passed in error values.
{-# INLINABLE intersectsWithErr #-}
intersectsWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => Either LineSeg (a, PLine2Err) -> Either LineSeg (b, PLine2Err) -> Either Intersection PIntersection
intersectsWithErr (Left l1)   (Left l2)   =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right pl1) (Right pl2) = Right $ plinesIntersectIn pl1 pl2
intersectsWithErr (Left l1)   (Right pl1) =         pLineIntersectsLineSeg pl1 l1
intersectsWithErr (Right pl1) (Left l1)   =         pLineIntersectsLineSeg pl1 l1

-- | Check if/where a line segment and a PLine intersect.
{-# INLINABLE pLineIntersectsLineSeg #-}
pLineIntersectsLineSeg :: (ProjectiveLine2 a) => (a, PLine2Err) -> LineSeg -> Either Intersection PIntersection
pLineIntersectsLineSeg (pl1, pl1ErrOrigin) l1
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | res == PCollinear = Right PCollinear
  | res == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && distance (startPoint l1) (endPoint l1) < ulpVal (startFudgeFactor <> endFudgeFactor <> tFuzz) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\n" <> dumpMiss
  | hasIntersection && startDistance <= ulpStartSum = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= ulpEndSum = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, rawIntersectionErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, rawIntersectionErr)
  | otherwise = Left $ NoIntersection rawIntersect (pl1Err, pl2Err, rawIntersectErr)
  where
    res = plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
    ulpStartSum = ulpVal startDistanceErr
    ulpEndSum = ulpVal endDistanceErr
    (startDistance, (_,_, startDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start, mempty)
    (endDistance, (_,_, endDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end, mempty)
    startFudgeFactor = startDistanceErr <> startErr
    endFudgeFactor = endDistanceErr <> endErr
    startErr = pLineErrAtPPoint (pl2, pl2Err) start
    endErr = pLineErrAtPPoint (pl2, pl2Err) end
    tFuzz = fuzzinessOfL (pl2, pl2Err)
    hasIntersection = hasRawIntersection && hitSegment
    hitSegment = onSegment l1 (rawIntersection, rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2
    pl1Err = pl1ErrOrigin <> npl1Err
    pl2Err = pl2ErrOrigin <> npl2Err
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) $ vecOfP rawIntersect
    (rawIntersect, (npl1Err, npl2Err, rawIntersectErr)) = intersect2PL pl1 pl2
    start = eToPP $ startPoint l1
    end = eToPP $ endPoint l1
    (pl2, pl2ErrOrigin) = eToPL l1
    dumpMiss = "startFudgeFactor: " <> show startFudgeFactor <> "\n"
               <> "endFudgeFactor: " <> show endFudgeFactor <> "\n"
               <> "startDistanceErr: " <> show startDistanceErr <> "\n"
               <> "endDistanceErr: " <> show endDistanceErr <> "\n"
               <> "startErr: " <> show startErr <> "\n"
               <> "endErr: " <> show endErr <> "\n"
               <> "pl2: " <> show pl2 <> "\n"
               <> "pl2Err: " <> show pl2Err <> "\n"
               <> "xIntercept: " <> show (xIntercept (pl2,pl2Err)) <> "\n"
               <> "yIntercept: " <> show (yIntercept (pl2,pl2Err)) <> "\n"
               <> "tfuzz: " <> show tFuzz <> "\n"

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: LineSeg -> LineSeg -> Either Intersection PIntersection
lineSegIntersectsLineSeg l1 l2
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | hasIntersection && res == PCollinear = Right PCollinear
  | hasIntersection && res == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && distance (startPoint l1) (endPoint l1) < ulpVal (start1FudgeFactor <> end1FudgeFactor <> tFuzz1) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\n" <> dumpMiss
  | hasIntersection && start1Distance <= ulpStart1Sum = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= ulpEnd1Sum = Left $ HitEndPoint l1
  | hasIntersection && distance (startPoint l2) (endPoint l2) < ulpVal (start2FudgeFactor <> end2FudgeFactor <> tFuzz2) = error $ "cannot resolve endpoints of segment: " <> show l2 <> ".\n" <> dumpMiss
  | hasIntersection && start2Distance <= ulpStart2Sum = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= ulpEnd2Sum = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, rawIntersectionErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, rawIntersectionErr)
  | otherwise = Left $ NoIntersection rawIntersect (pl1Err, pl2Err, rawIntersectErr)
  where
    res = plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
    start1FudgeFactor = start1DistanceErr <> pLineErrAtPPoint (pl1,pl1Err) start1
    end1FudgeFactor = end1DistanceErr <> pLineErrAtPPoint (pl1,pl1Err) end1
    start2FudgeFactor = start2DistanceErr <> pLineErrAtPPoint (pl2,pl2Err) start2
    end2FudgeFactor = end2DistanceErr <> pLineErrAtPPoint (pl2,pl2Err) end2
    ulpStart1Sum = ulpVal start1DistanceErr
    ulpEnd1Sum = ulpVal end1DistanceErr
    ulpStart2Sum = ulpVal start2DistanceErr
    ulpEnd2Sum = ulpVal end2DistanceErr
    (start1Distance, (_,_, start1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start1, mempty)
    (start2Distance, (_,_, start2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start2, mempty)
    (end1Distance, (_,_, end1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end1, mempty)
    (end2Distance, (_,_, end2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end2, mempty)
    tFuzz1 = fuzzinessOfL (pl1, pl1Err)
    tFuzz2 = fuzzinessOfL (pl2, pl2Err)
    hasIntersection = hasRawIntersection && hitSegment
    hitSegment = onSegment l1 (rawIntersection, rawIntersectionErr) && onSegment l2 (rawIntersection, rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2
    pl1Err = pl1ErrOrigin <> npl1Err
    pl2Err = pl2ErrOrigin <> npl2Err
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) $ vecOfP rawIntersect
    (rawIntersect, (npl1Err, npl2Err, rawIntersectErr)) = intersect2PL pl1 pl2
    start1 = eToPP $ startPoint l1
    end1 = eToPP $ endPoint l1
    start2 = eToPP $ startPoint l2
    end2 = eToPP $ endPoint l2
    (pl1, pl1ErrOrigin) = eToPL l1
    (pl2, pl2ErrOrigin) = eToPL l2
    dumpMiss = "start2FudgeFactor: " <> show start2FudgeFactor <> "\n"
               <> "end2FudgeFactor: " <> show end2FudgeFactor <> "\n"
               <> "start2DistanceErr: " <> show start2DistanceErr <> "\n"
               <> "end2DistanceErr: " <> show end2DistanceErr <> "\n"
               <> "pl2: " <> show pl2 <> "\n"
               <> "pl2Err: " <> show pl2Err <> "\n"
               <> "xIntercept2: " <> show (xIntercept (pl2,pl2Err)) <> "\n"
               <> "yIntercept2: " <> show (yIntercept (pl2,pl2Err)) <> "\n"
               <> "tfuzz2: " <> show tFuzz2 <> "\n"

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: (ProjectivePoint2 a) => LineSeg -> (a, PPoint2Err) -> Bool
{-# INLINABLE onSegment #-}
onSegment lineSeg iPoint@(iP, _) =
     (startDistance <= startFudgeFactor)
  || (lineDistance <= lineFudgeFactor && midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    startFudgeFactor = ulpVal $ startDistanceErr <> iErr <> tErr <> pLineErrAtPPoint pLine start
    midFudgeFactor   = ulpVal $ midDistanceErr   <> iErr <> tErr <> pLineErrAtPPoint pLine mid
    lineFudgeFactor  = ulpVal $                     iErr <> tErr <> pLineErrAtPPoint pLine iP
    endFudgeFactor   = ulpVal $ endDistanceErr   <> iErr <> tErr <> pLineErrAtPPoint pLine end
    (startDistance, (_,_, startDistanceErr)) = distance2PP iPoint (start, mempty)
    (midDistance, (_,_, midDistanceErr))     = distance2PP iPoint (mid, midErr)
    (lineDistance, _)                        = distancePPToPL iPoint pLine
    (endDistance, (_,_, endDistanceErr))     = distance2PP iPoint (end, mempty)
    lengthOfSegment = distance (startPoint lineSeg) (endPoint lineSeg)
    (mid, (_, _, midErr)) = interpolate2PP start end 0.5 0.5
    start = eToPP $ startPoint lineSeg
    end = eToPP $ endPoint lineSeg
    tErr = fuzzinessOfL pLine
    iErr = fuzzinessOfP iPoint
    pLine = eToPL lineSeg

-- | Combine consecutive line segments. expects line segments with their end points connecting, EG, a contour generated by makeContours.
combineConsecutiveLineSegs :: [LineSeg] -> [LineSeg]
combineConsecutiveLineSegs lines = case lines of
                                     [] -> []
                                     [a] -> [a]
                                     (firstLine:manyLines) -> combineEnds $ foldt combine [firstLine] ((:[]) <$> manyLines)
  where
    combine :: [LineSeg] -> [LineSeg] -> [LineSeg]
    combine  ls      []      = ls
    combine  []      ls      = ls
    combine (l1:ls) (l2:l2s) = case unsnoc ls of
                                  Nothing -> if canCombineLineSegs l1 l2 then fromMaybe (error "failed to combine!") (combineLineSegs l1 l2) : l2s else l1 : l2 : l2s
                                  (Just (vs, vl)) -> if canCombineLineSegs vl l2 then l1:vs <> (fromMaybe (error "failed to combine!") (combineLineSegs vl l2) : l2s) else (l1:ls) <> (l2:l2s)
    combineEnds :: [LineSeg] -> [LineSeg]
    combineEnds  []      = []
    combineEnds  [l1]    = [l1]
    combineEnds  (l1:l2:ls)  = case unsnoc ls of
                                 Nothing -> if canCombineLineSegs l2 l1 then [fromMaybe (error "failed to combine!") $ combineLineSegs l2 l1] else [l2, l1]
                                 (Just (vs, vl)) -> if canCombineLineSegs vl l1 then fromMaybe (error "failed to combine!") (combineLineSegs vl l1) : l2 : vs else vl:l1:l2:vs
    -- | determine if two euclidian line segments are on the same projective line, and if they share a middle point.
    canCombineLineSegs :: LineSeg -> LineSeg -> Bool
    canCombineLineSegs l1 l2 = pLinesWithinErr line1 line2 && sameDirection pl1 pl2
      where
        line1@(pl1,_) = eToPL l1
        line2@(pl2,_) = eToPL l2

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

-- | Create a canonical euclidian projective point from the given euclidian point.
euclidianToProjectivePoint2, eToPP :: Point2 -> ProjectivePoint
euclidianToProjectivePoint2 (Point2 (x,y)) = res
  where
    res = makeCPPoint2 x y
eToPP = euclidianToProjectivePoint2

-- | Create a canonical euclidian projective point from the given coordinates.
makeCPPoint2 :: ℝ -> ℝ -> ProjectivePoint
makeCPPoint2 x y = cPPoint
  where
    cPPoint = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1])]

-- | Reverse a vector. Really, take every value in it, and recompute it in the reverse order of the vectors (so instead of e0∧e1, e1∧e0). which has the effect of negating bi and tri-vectors.
reverseGVec :: GVec -> GVec
reverseGVec (GVec vals) = GVec $ foldl' addValWithoutErr []
                  [
                    GVal (         valOf 0 $ getVal [G0] vals)                           (singleton G0)
                  , GVal (         valOf 0 $ getVal [GEZero 1] vals)                     (singleton (GEZero 1))
                  , GVal (         valOf 0 $ getVal [GEPlus 1] vals)                     (singleton (GEPlus 1))
                  , GVal (         valOf 0 $ getVal [GEPlus 2] vals)                     (singleton (GEPlus 2))
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)           (fromList [GEZero 1, GEPlus 1])
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals)           (fromList [GEZero 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVal [GEPlus 1, GEPlus 2] vals)           (fromList [GEPlus 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                  ]

euclidianToProjectiveLine, eToPL :: LineSeg -> (ProjectiveLine, PLine2Err)
euclidianToProjectiveLine l = (res, resErr)
  where
    (res, (_, _, resErr)) = join2PP (eToPP $ startPoint l) (eToPP $ endPoint l)
eToPL = euclidianToProjectiveLine

joinTwoEuclidianPoints, join2EP :: Point2 -> Point2 -> (ProjectiveLine, PLine2Err)
joinTwoEuclidianPoints p1 p2 = (res, resErr)
  where
    (res, (_, _, resErr)) = join2PP (eToPP p1) (eToPP p2)
join2EP = joinTwoEuclidianPoints
