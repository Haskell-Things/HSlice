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

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}

-- | The purpose of this file is to hold projective geometric algebraic arithmatic. It defines a 2D PGA with mixed linear components.

module Graphics.Slicer.Math.PGA(
  Arcable(errOfOut, hasArc, outAndErrOf, outOf),
  CPPoint2(CPPoint2),
  Intersection(HitStartPoint, HitEndPoint, NoIntersection),
  NPLine2(NPLine2),
  PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2(PLine2),
  PLine2Err(PLine2Err),
  Pointable(canPoint, pPointOf, ePointOf),
  PPoint2(PPoint2),
  PPoint2Err,
  ProjectiveLine2(
      angleBetween2PL,
      distance2PL,
      flipL,
      intersect2PL,
      normalizeL,
      translateL,
      vecOfL
      ),
  ProjectivePoint2(
      canonicalize,
      distance2PP,
      interpolate2PP,
      join2PP,
      pToEP
      ),
  combineConsecutiveLineSegs,
  distancePPointToPLineWithErr,
  eToPL,
  eToPPoint2,
  intersectsWith,
  intersectsWithErr,
  makePPoint2,
  outputIntersectsLineSeg,
  pLineIsLeft,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  plinesIntersectIn,
  translateRotatePPoint2,
  ) where

import Prelude (Bool, Eq((==),(/=)), Monoid(mempty), Semigroup((<>)), Show(show), ($), (*), (-), (>=), (&&), (<$>), otherwise, signum, snd, (>), (<=), (+), negate, (/), (||), (<), abs, error, sin, cos, realToFrac, fst, sum, (.))

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (foldt)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromJust, fromMaybe, isNothing, maybeToList)

import Data.Set (singleton, fromList)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, TowardNegInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎤+), (⨅), (⨅+), (•), addValWithoutErr, addVecPairWithoutErr, getVal, mulScalarVecWithErr, sumErrVals, ulpVal, valOf)

import Graphics.Slicer.Math.Line (combineLineSegs)

import Graphics.Slicer.Math.PGAPrimitives(Arcable(errOfOut, hasArc, outAndErrOf, outOf), CPPoint2(CPPoint2), NPLine2(NPLine2), PLine2(PLine2), PLine2Err(PLine2Err), Pointable(canPoint, ePointOf, pPointOf), PPoint2(PPoint2), PPoint2Err, ProjectiveLine2(angleBetween2PL, angleCosBetween2PL, distance2PL, flipL, forceBasisOfL, intersect2PL, normalizeL, normOfL, translateL, vecOfL), ProjectivePoint2(canonicalize, distance2PP, forceBasisOfP, idealNormOfP, interpolate2PP, join2PP, pToEP, vecOfP), canonicalizedIntersectionOf2PL, pLineErrAtPPoint, xIntercept, yIntercept)

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
  | IntersectsIn !CPPoint2 !(PLine2Err, PLine2Err, UlpSum, PPoint2Err)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> PIntersection
plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | isNothing canonicalizedIntersection
  || (idealNorm <= realToFrac (ulpVal idnErr)
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
  | otherwise                        = IntersectsIn res (pl1Err <> npl1Err, pl2Err <> npl2Err, idnErr, resErr)
  where
    -- | The distance within which we consider (anti)parallel lines to be (anti)colinear.
    parallelFuzziness :: ℝ
    parallelFuzziness = realToFrac $ ulpVal $ dErr <> pLineErrAtPPoint (npl1, npl1Err <> pl1Err) res <> pLineErrAtPPoint (npl2, npl2Err <> pl2Err) res
    -- | When two lines are really close to parallel or antiparallel, we use the distance between the lines to decide whether to promote them to being (anti)colinear.
    (d, (_, _, dErr)) = distance2PL npl1 npl2
    (idealNorm, idnErr) = idealNormOfP res
    (res, (_, _, resErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL npl1 npl2
    (npl1, npl1Err) = normalizeL pl1
    (npl2, npl2Err) = normalizeL pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Maybe Bool
pLineIsLeft (pl1, _) (pl2, _)
-- FIXME: Is there a way we can use Eq on a and b if they are the same type, rather than normalizing them first?
  | npl1 == npl2         = Nothing
  | abs res <= angleFuzz = Nothing
  | otherwise            = Just $ res > 0
  where
    angleFuzz :: ℝ
    angleFuzz = realToFrac $ ulpVal angleFuzzRaw
    (res, (_,_, angleFuzzRaw)) = angleCosBetween2PL pl1 pl2
    (npl1, _) = normalizeL pl1
    (npl2, _) = normalizeL pl2

-- FIXME: use the distance to increase ULP appropriately?
-- FIXME: we lose a lot of error in this function.
distancePPointToPLineWithErr :: (ProjectivePoint2 a, ProjectiveLine2 b) => a -> b -> (ℝ, UlpSum)
distancePPointToPLineWithErr point line
  | valOf 0 foundVal == 0 = error "attempted to get the distance of an ideal point."
  | otherwise = (res, ulpTotal)
  where
    (res, _)                       = normOfL newPLine
    (newPLine, _)                  = join2PP point linePoint
    (perpLine, (plMulErr,plAddErr))= lvec ⨅+ npvec
    lvec                           = vecOfL $ forceBasisOfL (PLine2 nplvec)
    npvec                          = vecOfP $ forceBasisOfP point
    (linePoint, _)                 = fromJust $ canonicalizedIntersectionOf2PL (PLine2 lvec) (PLine2 perpLine)
    ulpTotal                       = sumErrVals plMulErr <> sumErrVals plAddErr
    foundVal                       = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) $ vecOfP $ point
    (NPLine2 nplvec,_)             = normalizeL line

-- | Determine if two points are on the same side of a given line.
pPointsOnSameSideOfPLine :: (ProjectivePoint2 a, ProjectivePoint2 b, ProjectiveLine2 c) => a -> b -> c -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  -- Return nothing if one of the points is on the line.
  |  abs foundP1 < realToFrac (ulpVal unlikeP1UlpSum) ||
     abs foundP2 < realToFrac (ulpVal unlikeP2UlpSum)    = Nothing
    | otherwise = Just $ signum foundP1 == signum foundP2
  where
    foundP1 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1
    foundP2 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2
    unlikeP1UlpSum = sumErrVals unlikeP1MulErr <> sumErrVals unlikeP1AddErr
    unlikeP2UlpSum = sumErrVals unlikeP2MulErr <> sumErrVals unlikeP2AddErr
    (GVec unlikeP1, (unlikeP1MulErr, unlikeP1AddErr)) = pv1 ⎤+ lv1
    (GVec unlikeP2, (unlikeP2MulErr, unlikeP2AddErr)) = pv2 ⎤+ lv1
    pv1 = vecOfP $ forceBasisOfP point1
    pv2 = vecOfP $ forceBasisOfP point2
    lv1 = vecOfL $ forceBasisOfL line

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
-- FIXME: precision on inputs?
sameDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
sameDirection a b = res >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = realToFrac (1 - ulpVal resErr :: Rounded 'TowardInf ℝ)
    (res, (_,_,resErr)) = angleBetween2PL a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel.
-- FIXME: precision on inputs?
oppositeDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
oppositeDirection a b = res <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = realToFrac (realToFrac (ulpVal resErr) + (-1) :: Rounded 'TowardNegInf ℝ)
    (res, (_,_,resErr)) = angleBetween2PL a b

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: (ProjectiveLine2 a, ProjectivePoint2 b) => a -> b -> ℝ -> (PPoint2, UlpSum)
pPointOnPerpWithErr line ppoint d = (PPoint2 res,
                                      ulpTotal)
  where
    res = motor•pvec•reverseGVec motor
    (perpLine, (plMulErr,plAddErr))= lvec ⨅+ pvec
    lvec                           = vecOfL $ forceBasisOfL npl
    (npl,_)                        = normalizeL line
    pvec                           = vecOfP $ forceBasisOfP ppoint
    motor = addVecPairWithoutErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr = UlpSum $ abs $ realToFrac $ doubleUlp $ d/2
    ulpTotal = sumErrVals plMulErr <> sumErrVals plAddErr <> gaIErr

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: (ProjectivePoint2 a) => a -> ℝ -> ℝ -> PPoint2
translateRotatePPoint2 ppoint d rotation = PPoint2 $ translator•pvec•reverseGVec translator
  where
    pvec = vecOfP ppoint
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        xLineVec = vecOfL $ forceBasisOfL $ fst $ eToPL (LineSeg (Point2 (0,0)) (Point2 (1,0)))
    angledLineThroughPPoint2 = vecOfL $ forceBasisOfL $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
      where
        rotator = addVecPairWithoutErr (fst $ mulScalarVecWithErr (sin $ rotation/2) pvec) (GVec [GVal (cos $ rotation/2) (singleton G0)])
    translator = addVecPairWithoutErr (angledLineThroughPPoint2 • gaIScaled) (GVec [GVal 1 (singleton G0)])
      where
        -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

----------------------------------------------------------
-------------- Euclidian Mixed Interface -----------------
----------------------------------------------------------

-- | Intersection events that can only happen with line segments.
data Intersection =
    NoIntersection !CPPoint2 !(UlpSum, UlpSum, UlpSum, UlpSum)
  | HitStartPoint !LineSeg
  | HitEndPoint !LineSeg
  deriving Show

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2 = Either LineSeg PLine2

-- FIXME: as long as this is required, we're not accounting for ULP correctly everywhere.
ulpMultiplier :: Rounded 'TowardInf ℝ
ulpMultiplier = 570

-- | Check if/where lines/line segments intersect.
-- entry point usable for all intersection needs.
-- FIXME: take UlpSums here.
intersectsWith :: SegOrPLine2 -> SegOrPLine2 -> Either Intersection PIntersection
intersectsWith (Left l1)   (Left l2)   =         lineSegIntersectsLineSeg (l1, ulpOfLineSeg l1) (l2, ulpOfLineSeg l2)
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn (pl1, mempty) (pl2, mempty)
intersectsWith (Left l1)   (Right pl1) =         pLineIntersectsLineSeg (pl1, ulpOfPLine2 pl1) (l1, ulpOfLineSeg l1) 0
intersectsWith (Right pl1) (Left l1)   =         pLineIntersectsLineSeg (pl1, ulpOfPLine2 pl1) (l1, ulpOfLineSeg l1) 0

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
outputIntersectsLineSeg :: (Show a, Arcable a) => a -> LineSeg -> Either Intersection PIntersection
outputIntersectsLineSeg source l1
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | otherwise = pLineIntersectsLineSeg (pl1, pl1Ulp) (l1, pl2Ulp) 1
  where
    pl2Ulp = snd (fromMaybe (Right 0,mempty) $ xIntercept (pl2,pl2Err)) <> snd (fromMaybe (Right 0,mempty) $ yIntercept (pl2,pl2Err))
    (pl2, pl2Err) = eToPL l1
    pl1Ulp = snd (fromMaybe (Right 0,mempty) $ xIntercept (pl1,pl1Err)) <> snd (fromMaybe (Right 0,mempty) $ yIntercept (pl1,pl1Err))
    (pl1, pl1Err)
      | hasArc source = (outOf source, errOfOut source)
      | otherwise = error
                    $ "no arc from source?\n"
                    <> show source <> "\n"
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2WithErr = Either (LineSeg, UlpSum) (PLine2, UlpSum)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)       (Left l2)       =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right (pl1,_)) (Right (pl2,_)) = Right $ plinesIntersectIn (pl1,mempty) (pl2,mempty)
intersectsWithErr (Left l1@(rawL1,_)) (Right pl1@(rawPL1, _)) = pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle) + angleErr)
    (angle, (_,_, UlpSum angleErr)) = angleBetween2PL rawPL1 pl2
    (pl2, _) = eToPL rawL1
intersectsWithErr (Right pl1@(rawPL1, _)) (Left l1@(rawL1,_)) = pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle) + angleErr)
    (angle, (_,_, UlpSum angleErr)) = angleBetween2PL rawPL1 pl2
    (pl2, _) = eToPL rawL1

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (PLine2, UlpSum) -> (LineSeg, UlpSum) -> ℝ -> Either Intersection PIntersection
pLineIntersectsLineSeg (pl1, UlpSum pl1Err) (l1, UlpSum l1Err) ulpScale
  | plinesIntersectIn (pl1,mempty) (pl2,pl2Err) == PParallel = Right PParallel
  | plinesIntersectIn (pl1,mempty) (pl2,pl2Err) == PAntiParallel = Right PAntiParallel
  | plinesIntersectIn (pl1,mempty) (pl2,pl2Err) == PCollinear = Right PCollinear
  | plinesIntersectIn (pl1,mempty) (pl2,pl2Err) == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (startDistanceErr+endDistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nulpScale: " <> show ulpScale <> "\nrawIntersect" <> show rawIntersect <> dumpULPs
  | hasIntersection && valOf 0 foundVal == 0 = error "intersection, but cannot cannonicalize."
  | hasIntersection && startDistance <= ulpStartSum = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= ulpEndSum = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (mempty, mempty, mempty, mempty)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum $ realToFrac ulpStartSum, UlpSum $ realToFrac ulpEndSum, mempty, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (mempty, mempty, mempty, mempty)
  where
    (startDistance, (_,_,UlpSum startDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start,mempty)
    (endDistance, (_,_,UlpSum endDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end,mempty)
    start = eToPPoint2 $ startPoint l1
    end = eToPPoint2 $ endPoint l1
    ulpStartSum, ulpEndSum :: ℝ
    ulpStartSum = realToFrac $ ulpTotal+startDistanceErr
    ulpEndSum = realToFrac $ ulpTotal+endDistanceErr
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    -- Note: we do not use rawIntersectionErr here.
    ulpTotal = l1Err
    dumpULPs = "pl1Err: " <> show pl1Err <> "\npl2Err: " <> show pl2Err <> "\nl1Err: " <> show l1Err <> "\nrawIntersectErr: " <> show rawIntersectErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection, rawIntersectionErr) ulpStartSum ulpEndSum
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    rawIntersectionErr = rawIntersectErr <> cRawIntersectErr
    (rawIntersection, cRawIntersectErr) = canonicalize rawIntersect
    (rawIntersect, (_,_,rawIntersectErr)) = intersect2PL pl1 pl2
    (pl2, pl2Err) = eToPL l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, UlpSum l1Err) (l2, UlpSum ulpL2)
  | plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err) == PParallel = Right PParallel
  | plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err) == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (start1DistanceErr+end1DistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < realToFrac (start2DistanceErr+end2DistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err) == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err) == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && start1Distance <= ulpStartSum1 = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= ulpEndSum1 = Left $ HitEndPoint l1
  | hasIntersection && start2Distance <= ulpStartSum2 = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= ulpEndSum2 = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (mempty, mempty, mempty, mempty)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum $ realToFrac ulpStartSum1, UlpSum $ realToFrac ulpEndSum1, UlpSum $ realToFrac ulpStartSum2, UlpSum $ realToFrac ulpEndSum2)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (mempty, mempty, mempty, mempty)
  where
    ulpStartSum1, ulpEndSum1, ulpStartSum2, ulpEndSum2 :: ℝ
    ulpStartSum1 = realToFrac $ ulpTotal+start1DistanceErr
    ulpStartSum2 = realToFrac $ ulpTotal+start2DistanceErr
    ulpEndSum1 = realToFrac $ ulpTotal
    ulpEndSum2 = realToFrac $ ulpTotal
    (start1Distance, (_,_,UlpSum start1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start1, mempty)
    (start2Distance, (_,_,UlpSum start2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start2, mempty)
    (end1Distance, (_,_,UlpSum end1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end1, mempty)
    (end2Distance, (_,_,UlpSum end2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end2, mempty)
    start1 = eToPPoint2 $ startPoint l1
    end1 = eToPPoint2 $ endPoint l1
    start2 = eToPPoint2 $ startPoint l2
    end2 = eToPPoint2 $ endPoint l2
    (pl1, pl1Err) = eToPL l1
    (pl2, pl2Err) = eToPL l2
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal = l1Err + ulpL2
    dumpULPs = "pl1Err: " <> show pl1Err <> "\npl2Err: " <> show pl2Err <> "\nl1Err: " <> show l1Err <> "\nrawIntersectionErr: " <> show rawIntersectionErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr) ulpStartSum1 ulpEndSum1 && onSegment l2 (rawIntersection,rawIntersectionErr) ulpStartSum2 ulpEndSum2
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    rawIntersectionErr = rawIntersectErrRaw <> cRawIntersectErr
    (rawIntersection, cRawIntersectErr) = canonicalize rawIntersect
    (rawIntersect, (_,_,rawIntersectErrRaw)) = intersect2PL pl1 pl2

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> (CPPoint2, PPoint2Err) -> ℝ -> ℝ -> Bool
onSegment ls (i, iErr) startUlp endUlp =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    (startDistance, (_,_,UlpSum startDistanceErr)) = distance2PP (start, mempty) (i, iErr)
    (midDistance, (_,_,UlpSum midDistanceErr)) = distance2PP (mid, midErr) (i, iErr)
    (endDistance, (_,_,UlpSum endDistanceErr)) = distance2PP (end, mempty) (i, iErr)
    start = eToPPoint2 $ startPoint ls
    (mid, (_, _, midErr)) = interpolate2PP start end 0.5 0.5
    end = eToPPoint2 $ endPoint ls
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ realToFrac startUlp + startDistanceErr
    midFudgeFactor = realToFrac $ abs (realToFrac $ doubleUlp lengthOfSegment) + midDistanceErr
    endFudgeFactor = realToFrac $ realToFrac endUlp + endDistanceErr

-- | Combine consecutive line segments. expects line segments with their end points connecting, EG, a contour generated by makeContours.
combineConsecutiveLineSegs :: [LineSeg] -> [LineSeg]
combineConsecutiveLineSegs lines = case lines of
                                     [] -> []
                                     [a] -> [a]
                                     (firstLine:manyLines) -> res firstLine manyLines
  where
    res first many = combineEnds $ foldt combine [first] ((:[]) <$> many)
    combine :: [LineSeg] -> [LineSeg] -> [LineSeg]
    combine  l1      []  = l1
    combine  []      l2  = l2
    combine (l1:ls) (l2:l2s) = case lastMay ls of
                               Nothing -> if canCombineLineSegs l1 l2 then maybeToList (combineLineSegs l1 l2) <> l2s else l1 : l2 : l2s
                               (Just v) -> if canCombineLineSegs v l2 then l1:initSafe ls <> maybeToList (combineLineSegs v l2) <> l2s else (l1:ls) <> (l2:l2s)
    -- | responsible for placing the last value at the front of the list, to make up for the fold of combine putting the first value last.
    combineEnds :: [LineSeg] -> [LineSeg]
    combineEnds  []      = []
    combineEnds  [l1]    = [l1]
    combineEnds  (l1:l2:ls) = case lastMay ls of
                                   Nothing -> maybeToList $ combineLineSegs l1 l2
                                   (Just v) -> if canCombineLineSegs v l1 then maybeToList (combineLineSegs v l1) <> (l2:initSafe ls) else v:l1:l2:initSafe ls
    -- | determine if two euclidian line segments are on the same projective line, and if they share a middle point.
    canCombineLineSegs :: LineSeg -> LineSeg -> Bool
    canCombineLineSegs l1@(LineSeg p1 s1) l2@(LineSeg p2 _) = sameLineSeg && sameMiddlePoint
      where
        -- FIXME: this does not take into account the Err introduced by eToPLine2.
        sameLineSeg = plinesIntersectIn (eToPL l1) (eToPL l2) == PCollinear
        sameMiddlePoint = p2 == addPoints p1 s1

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

eToPPoint2 :: Point2 -> CPPoint2
eToPPoint2 (Point2 (x,y)) = res
  where
    res = makePPoint2 x y

-- | Create a canonical euclidian projective point from the given coordinates.
makePPoint2 :: ℝ -> ℝ -> CPPoint2
makePPoint2 x y = pPoint
  where
    pPoint = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1])]

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

eToPL :: LineSeg -> (PLine2, PLine2Err)
eToPL l1 = (res, resErr)
  where
    (res, (_,_,resErr)) = join2PP (eToPPoint2 $ startPoint l1) (eToPPoint2 $ endPoint l1)

-- | Get the sum of the error involved in storing the values in a given PLine2.
ulpOfPLine2 :: (ProjectiveLine2 a) => a -> UlpSum
ulpOfPLine2 line = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVal [GEZero 1] vals
                                   ,getVal [GEPlus 1] vals
                                   ,getVal [GEPlus 2] vals]
  where
    (GVec vals) = vecOfL line

-- | Get the sum of the error involved in storing the values in a given Line Segment.
ulpOfLineSeg :: LineSeg -> UlpSum
ulpOfLineSeg (LineSeg (Point2 (x1,y1)) (Point2 (x2,y2))) = UlpSum $ sum $ abs . realToFrac . doubleUlp <$> [x1, y1, x2, y2]


