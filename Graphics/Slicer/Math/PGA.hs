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

{-# Language TypeSynonymInstances #-}

-- | The purpose of this file is to hold projective geometric algebraic arithmatic. It defines a 2D PGA with mixed linear components.

module Graphics.Slicer.Math.PGA(
  Arcable(hasArc, outOf, errOfOut),
  Intersection(HitStartPoint, HitEndPoint, NoIntersection),
  PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2Err(PLine2Err),
  Pointable(canPoint, canEPoint, pPointOf, ePointOf, errOfPPoint, errOfEPoint),
  PPoint2Err,
  ProjectiveLine(PLine2, NPLine2),
  ProjectivePoint(PPoint2, CPPoint2),
  ProjectivePoint2,
  canonicalize,
  combineConsecutiveLineSegs,
  distance2PP,
  distanceBetweenPLinesWithErr,
  distancePPointToPLineWithErr,
  pLineErrAtPPoint,
  eToPLine2WithErr,
  eToPPoint2,
  flipL,
  fuzzinessOfL,
  fuzzinessOfP,
  intersect2PL,
  interpolate2PP,
  intersectsWith,
  intersectsWithErr,
  join2PP,
  makePPoint2,
  normalizeL,
  outputIntersectsLineSeg,
  oppositeDirection,
  pLineIsLeft,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEP,
  plinesIntersectIn,
  sameDirection,
  translateL,
  translateRotatePPoint2WithErr
  ) where

import Prelude (Bool, Eq((==)), Monoid(mempty), Semigroup((<>)), Show(show), ($), (-), (&&), (<$>), (>), (>=), (<=), (+), (/), (||), (<), abs, cos, error, fst, negate, otherwise, realToFrac, signum, sin)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, fromJust, isJust, isNothing, maybeToList)

import Data.Set (singleton, fromList)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, TowardNegInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, makeLineSeg, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal, GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (•), addValWithoutErr, addVecPairWithoutErr, eValOf, getVal, mulScalarVecWithErr, ulpVal, valOf)

import Graphics.Slicer.Math.Line (combineLineSegs)

import Graphics.Slicer.Math.PGAPrimitives(Arcable(errOfOut, hasArc, outOf), ProjectivePoint(CPPoint2,PPoint2), ProjectiveLine(NPLine2,PLine2), PLine2Err(PLine2Err), Pointable(canEPoint, canPoint, errOfEPoint, errOfPPoint, ePointOf, pPointOf), PPoint2Err, ProjectiveLine2(angleBetween2PL, angleCosBetween2PL, canonicalizedIntersectionOf2PL, flipL, forceBasisOfL, fuzzinessOfL, intersect2PL, normalizeL, normOfL, translateL, vecOfL), ProjectivePoint2(canonicalize, distance2PP, forceBasisOfP, fuzzinessOfP, idealNormOfP, interpolate2PP, join2PP, pToEP, vecOfP), pLineErrAtPPoint)
  

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
  | IntersectsIn !ProjectivePoint !(PLine2Err, PLine2Err, UlpSum, PPoint2Err)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a,PLine2Err) -> (b,PLine2Err) -> PIntersection
plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err)
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
    -- distance within which we  consider parallel lines as the same line.
    parallelFuzziness :: ℝ
    parallelFuzziness = realToFrac $ ulpVal (dErr <> pLineErrAtPPoint npline1 res <> pLineErrAtPPoint npline2 res)
    -- when we're close to parallel or antiparallel, use the distance between the lines to promote to colinear/anticolinear
    (d, (_, _, _, dErr)) = distanceBetweenPLinesWithErr npl1 npl2
    (idealNorm, idnErr) = idealNormOfP res
    (res, (_, _, resErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL npl1 npl2
    npline1@(npl1, npl1Err) = normalizeL pl1
    npline2@(npl2, npl2Err) = normalizeL pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
-- FIXME: error quotent handling in here is trash.
pLineIsLeft :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> Maybe Bool
pLineIsLeft (pl1, pl1Err) (pl2,pl2Err)
-- FIXME: is there a way we can use Eq on a and b?
--  | pl1 == pl2                    = Nothing
  | npl1 == npl2                  = Nothing
  | abs res < realToFrac ulpTotal = Nothing
  | otherwise                     = Just $ res > 0
  where
    (res, _) = angleCosBetween2PL npl1 npl2
    ulpTotal = ulpVal $ fuzzinessOfL (npl1, pl1Err <> npl1Err) <> fuzzinessOfL (npl2, pl2Err <> npl2Err)
    (npl1, npl1Err) = normalizeL pl1
    (npl2, npl2Err) = normalizeL pl2

-- | Find the distance between a projective point and a projective line.
distancePPointToPLineWithErr :: (ProjectiveLine2 b) => (ProjectivePoint, PPoint2Err) -> (b, PLine2Err) -> (ℝ, (PPoint2Err, PLine2Err, ([ErrVal],[ErrVal]), PPoint2Err, PLine2Err, PLine2Err, PLine2Err, UlpSum))
distancePPointToPLineWithErr (rawPoint,rawPointErr) (rawLine,rawLineErr)
  | isNothing foundVal = error "attempted to get the distance of an ideal point."
  | otherwise = (res, resErr)
  where
    resErr = (pointErr, nLineErr, perpLineErr, lpErr <> lpcErr, lvErr, plErr, nplErr <> normErr, ulpSum)
      where
        (lvErr, plErr, lpErr) = lastPointErr
        (_, lpcErr, nplErr)   = newPLineErr
        -- FIXME: this is incomplete (see the bellow FIXMEs).
        ulpSum = (\(PLine2Err _ _ normUlp _ _ _) -> normUlp) normErr
    -- FIXME: how does the error in newPLine effect the found norm here?
    (res, normErr)            = normOfL newPLine
    -- FIXME: how does the error in linePoint and point effect this result?
    (newPLine, newPLineErr)   = join2PP point linePoint
    -- FIXME: how does perpLineErr effect the result of canonicalizedIntersectionOf2PL?
    (linePoint, lastPointErr) = fromJust $ canonicalizedIntersectionOf2PL rawNLine (PLine2 perpLine)
    (perpLine, perpLineErr)   = lVec ⨅+ pVec
    lVec = vecOfL $ forceBasisOfL rawNLine
    nLineErr = rawNLineErr <> rawLineErr
    (rawNLine, rawNLineErr) = normalizeL rawLine
    foundVal = getVal [GEPlus 1, GEPlus 2] pVals
    point@(CPPoint2 pVec@(GVec pVals)) = forceBasisOfP cpoint
    pointErr = cPointErr <> rawPointErr
    (cpoint, cPointErr) = canonicalize rawPoint

-- | Determine if two points are on the same side of a given line.
-- Returns nothing if one of the points is on the line.
-- FIXME: accept input error amounts, take input error amounts into consideration.
pPointsOnSameSideOfPLine :: (ProjectivePoint2 a, ProjectiveLine2 c) => a -> a -> c -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  |  abs foundP1 < foundErr1 ||
     abs foundP2 < foundErr2    = Nothing
    | otherwise = Just $ signum foundP1 == signum foundP2
  where
    foundErr1, foundErr2 :: ℝ
    foundErr1 = realToFrac $ ulpVal (eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1AddErr)) +
                             ulpVal (eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1MulErr))
    foundErr2 = realToFrac $ ulpVal (eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2AddErr)) +
                             ulpVal (eValOf mempty (getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2MulErr))
    foundP1 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1
    foundP2 = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2
    (GVec unlikeP1, (unlikeP1MulErr, unlikeP1AddErr)) = pv1 ⎤+ lv1
    (GVec unlikeP2, (unlikeP2MulErr, unlikeP2AddErr)) = pv2 ⎤+ lv1
    pv1 = vecOfP $ forceBasisOfP point1
    pv2 = vecOfP $ forceBasisOfP point2
    lv1 = vecOfL $ forceBasisOfL line

-- | Find the unsigned distance between two parallel or antiparallel projective lines.
-- FIXME: accept input error amounts, take input error amounts into consideration.
distanceBetweenPLinesWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal]), UlpSum))
distanceBetweenPLinesWithErr line1 line2 = (res, resErr)
  where
    (res, idealErr) = idealNormOfP $ PPoint2 like
    resErr = (pv1Err, pv2Err, likeErr, idealErr)
    (like, likeErr) = p1 ⎣+ p2
    p1 = vecOfL $ forceBasisOfL npl1
    p2 = vecOfL $ forceBasisOfL npl2
    (npl1,pv1Err) = normalizeL line1
    (npl2,pv2Err) = normalizeL line2

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
-- FIXME: precision on inputs?
sameDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
sameDirection a b = res >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = 1.0 - realToFrac (ulpVal resErr)
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
pPointOnPerpWithErr :: ProjectiveLine -> ProjectivePoint -> ℝ -> (ProjectivePoint, (PLine2Err,([ErrVal],[ErrVal]), UlpSum))
pPointOnPerpWithErr pline rppoint d = (res, (rlErr, perpPLineErr, ulpTotal))
  where
    res = case valOf 0 ( getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) resRaw) of
            1 -> CPPoint2 resRaw
            _ -> PPoint2 resRaw
    resRaw = motor•pvec•reverseGVec motor
    (perpLine,perpPLineErr) = lvec ⨅+ pvec
    lvec = vecOfL $ forceBasisOfL $ PLine2 rlvec
    motor = addVecPairWithoutErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
      where
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac (realToFrac d / 2 :: Rounded 'TowardInf ℝ)
    ulpTotal = gaIErr <> lErr
    pvec = vecOfP $ forceBasisOfP rppoint
    lErr = pLineErrAtPPoint nPLine rppoint
    nPLine@((NPLine2 rlvec),rlErr) = normalizeL pline

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2WithErr :: (ProjectivePoint2 a) => a -> ℝ -> ℝ -> (ProjectivePoint, [ErrVal])
translateRotatePPoint2WithErr ppoint d rotation = (PPoint2 res, scaledPVecErr)
  where
    res = translator•pvec•reverseGVec translator
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        xLineVec = vecOfL $ forceBasisOfL $ fst $ eToPLine2WithErr $ makeLineSeg (Point2 (0,0)) (Point2 (1,0))
    angledLineThroughPPoint2 = vecOfL $ forceBasisOfL $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
      where
        rotator = addVecPairWithoutErr scaledPVec (GVec [GVal (cos $ rotation/2) (singleton G0)])
    (scaledPVec, scaledPVecErr) = mulScalarVecWithErr (sin $ rotation/2) pvec
    translator = addVecPairWithoutErr (angledLineThroughPPoint2 • gaIScaled) (GVec [GVal 1 (singleton G0)])
      where
        -- I, in this geometric algebra system. we multiply it times d/2, to reduce the number of multiples we have to do when creating the motor.
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    pvec = vecOfP ppoint

----------------------------------------------------------
-------------- Euclidian Mixed Interface -----------------
----------------------------------------------------------

-- | Intersection events that can only happen with line segments.
data Intersection =
    NoIntersection !ProjectivePoint !(PLine2Err, PLine2Err, PPoint2Err, UlpSum)
  | HitStartPoint !LineSeg
  | HitEndPoint !LineSeg
  deriving Show

-- | A type alias, for cases where either input is acceptable.
type SegOrProjectiveLine = Either LineSeg (ProjectiveLine, PLine2Err)

-- | Check if/where lines/line segments intersect.
-- entry point usable for all intersection needs.
intersectsWith :: SegOrProjectiveLine -> SegOrProjectiveLine -> Either Intersection PIntersection
intersectsWith (Left l1)   (Left l2)   =         lineSegIntersectsLineSeg l1 l2
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   pl1 pl2
intersectsWith (Left l1)   (Right pl1) =         pLineIntersectsLineSeg pl1 l1
intersectsWith (Right pl1) (Left l1)   =         pLineIntersectsLineSeg pl1 l1

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
outputIntersectsLineSeg :: (Show a, Arcable a) => a -> LineSeg -> Either Intersection PIntersection
outputIntersectsLineSeg source l1
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | otherwise = pLineIntersectsLineSeg (pl1, pl1Err) l1
  where
    (pl2, pl2Err) = eToPLine2WithErr l1
    -- the multiplier to account for distance between our Pointable, and where it intersects.
    (pl1, pl1Err)
      | hasArc source = (outOf source, errOfOut source)
      | otherwise = error
                    $ "no arc from source?\n"
                    <> show source <> "\n"
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2WithErr = Either LineSeg (ProjectiveLine, PLine2Err)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)    (Left l2)  =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right pl1) (Right pl2) = Right $ plinesIntersectIn pl1 pl2
intersectsWithErr (Left l1)   (Right pl1) =         pLineIntersectsLineSeg pl1 l1
intersectsWithErr (Right pl1) (Left l1)   =         pLineIntersectsLineSeg pl1 l1

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (ProjectiveLine2 a) => (a, PLine2Err) -> LineSeg -> Either Intersection PIntersection
pLineIntersectsLineSeg pline1@(pl1, pl1Err) l1
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | res == PCollinear = Right PCollinear
  | res == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (startFudgeFactor + endFudgeFactor) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstartFudgeFactor: " <> show startFudgeFactor <> "\nendFudgeFactor: " <> show endFudgeFactor <> "\n" <> "startDistanceErr: " <> show startDistanceErr <> "\nendDistanceErr: " <> show endDistanceErr <> "\n" <> "startErr:" <> show startErr <> "\n" <> "endErr: " <> show endErr <> "\n" <> "pl2: " <> show pl2 <> "\n"
  | hasIntersection && isNothing foundVal = error "intersection, but cannot canonicalize."
  | hasIntersection && startDistance <= realToFrac startFudgeFactor = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= realToFrac endFudgeFactor = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, mempty, rawIntersectionErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, rawIntersectionErr, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (pl1Err, pl2Err, mempty, mempty)
  where
    res = plinesIntersectIn pline1 pline2
    startFudgeFactor = ulpVal $ startDistanceErr <> startErr
    startErr = pLineErrAtPPoint pline2 start
    endFudgeFactor = ulpVal $ endDistanceErr <> endErr
    endErr = pLineErrAtPPoint pline2 end
    (startDistance, (_,_, startDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start,mempty)
    (endDistance, (_,_, endDistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end,mempty)
    start = eToPPoint2 $ startPoint l1
    end = eToPPoint2 $ endPoint l1
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2
    (rawIntersect, _) = intersect2PL pl1 pl2
    pline2@(pl2, pl2Err) = eToPLine2WithErr l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: LineSeg -> LineSeg -> Either Intersection PIntersection
lineSegIntersectsLineSeg l1 l2
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (start1FudgeFactor + end1FudgeFactor) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstart1FudgeFactor: " <> show start1FudgeFactor <> "\nrawIntersection" <> show rawIntersection
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < realToFrac (start2FudgeFactor + end2FudgeFactor) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstart2FudgeFactor: " <> show start2FudgeFactor <> "\nrawIntersection" <> show rawIntersection
  | hasIntersection && res == PCollinear = Right PCollinear
  | hasIntersection && res == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && start1Distance <= realToFrac start1FudgeFactor = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= realToFrac end1FudgeFactor = Left $ HitEndPoint l1
  | hasIntersection && start2Distance <= realToFrac start2FudgeFactor = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= realToFrac end2FudgeFactor = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (npl1Err, npl2Err, mempty, rawIntersectionErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (npl1Err, npl2Err, rawIntersectionErr, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (npl1Err, npl2Err, rawIntersectErr, mempty)
  where
    res = plinesIntersectIn (pl1,npl1Err) (pl2,npl2Err)
    start1FudgeFactor = ulpVal $ start1DistanceErr <> pLineErrAtPPoint (pl1,npl1Err) start1
    end1FudgeFactor = ulpVal $ end1DistanceErr <> pLineErrAtPPoint (pl1,npl1Err) end1
    start2FudgeFactor = ulpVal $ start2DistanceErr <> pLineErrAtPPoint (pl2,npl2Err) start2
    end2FudgeFactor = ulpVal $ end2DistanceErr <> pLineErrAtPPoint (pl2,npl2Err) end2
    (start1Distance, (_,_, start1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start1,mempty)
    (start2Distance, (_,_, start2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (start2,mempty)
    (end1Distance, (_,_, end1DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end1,mempty)
    (end2Distance, (_,_, end2DistanceErr)) = distance2PP (rawIntersection, rawIntersectionErr) (end2,mempty)
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr) && onSegment l2 (rawIntersection,rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizedIntersectionOf2PL pl1 pl2
    (rawIntersect, (npl1Err, npl2Err, rawIntersectErr)) = intersect2PL pl1 pl2
    start1 = eToPPoint2 $ startPoint l1
    end1 = eToPPoint2 $ endPoint l1
    start2 = eToPPoint2 $ startPoint l2
    end2 = eToPPoint2 $ endPoint l2
    (pl1, _) = eToPLine2WithErr l1
    (pl2, _) = eToPLine2WithErr l2

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
-- FIXME: check start and end distances in order of: closest to the origin.
onSegment :: LineSeg -> (ProjectivePoint,PPoint2Err) -> Bool
onSegment ls i =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    start = eToPPoint2 $ startPoint ls
    end = eToPPoint2 $ endPoint ls
    (startDistance, (_,_, startDistanceErr)) = distance2PP i (start, mempty)
    (midDistance, (_,_, midDistanceErr)) = distance2PP i (mid,midErr)
    (endDistance, (_,_, endDistanceErr)) = distance2PP i (end,mempty)
    (mid, (_, _, midErr)) = interpolate2PP (start,mempty) (end,mempty) 0.5 0.5
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ ulpVal $ startDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) start
    midFudgeFactor = realToFrac $ ulpVal $ midDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) mid
    endFudgeFactor = realToFrac $ ulpVal $ endDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) end

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
        sameLineSeg = plinesIntersectIn (eToPLine2WithErr l1) (eToPLine2WithErr l2) == PCollinear
        sameMiddlePoint = p2 == addPoints p1 s1

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

eToPPoint2 :: Point2 -> ProjectivePoint
eToPPoint2 (Point2 (x,y)) = PPoint2 res
  where
    CPPoint2 res = makePPoint2 x y

-- | Create a canonical euclidian projective point from the given coordinates.
makePPoint2 :: ℝ -> ℝ -> ProjectivePoint
makePPoint2 x y = pPoint
  where
    pPoint = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]

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

eToPLine2WithErr :: LineSeg -> (ProjectiveLine, PLine2Err)
eToPLine2WithErr l1 = (res, resErr)
  where
    (res, (_, _, resErr)) = join2PP (eToPPoint2 $ startPoint l1) (eToPPoint2 $ endPoint l1)

