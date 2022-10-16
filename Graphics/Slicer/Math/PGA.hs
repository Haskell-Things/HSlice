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
  PPoint2Err(PPoint2Err),
  ProjectiveLine(PLine2, NPLine2),
  ProjectivePoint(PPoint2, CPPoint2),
  ProjectivePoint2,
  angleBetweenWithErr,
  canonicalize,
  combineConsecutiveLineSegs,
  distanceBetweenPPointsWithErr,
  distanceBetweenPLinesWithErr,
  distancePPointToPLineWithErr,
  pLineErrAtPPoint,
  eToPLine2WithErr,
  eToPPoint2,
  flipL,
  intersectsWith,
  intersectsWithErr,
  join2PP,
  makePPoint2,
  normalize,
  outputIntersectsLineSeg,
  oppositeDirection,
  pLineFuzziness,
  pLineIntersectionWithErr,
  pLineIsLeft,
  pPointBetweenPPointsWithErr,
  pPointFuzziness,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEP,
  plinesIntersectIn,
  sameDirection,
  translateL,
  translateRotatePPoint2WithErr
  ) where

import Prelude (Bool, Eq((==),(/=)), Monoid(mempty), Semigroup((<>)), Show(show), Ord, ($), (*), (-), (&&), (<$>), (>), (>=), (<=), (+), (/), (||), (<), abs, cos, error, filter, fst, negate, otherwise, realToFrac, signum, sin, sqrt)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right), isRight, fromRight)

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, fromJust, fromMaybe, isJust, isNothing, maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, TowardNegInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, makeLineSeg, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addErr, addValWithoutErr, addVecPairWithErr, addVecPairWithoutErr, divVecScalarWithErr, eValOf, getVal, mulScalarVecWithErr, scalarPart, ulpVal, valOf)

import Graphics.Slicer.Math.Line (combineLineSegs)

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
    canonicalizedIntersection = canonicalizeIntersectionWithErr npline1 npline2
    npline1@(npl1, npl1Err) = normalize pl1
    npline2@(npl2, npl2Err) = normalize pl2

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
    (res, _) = angleCos (npl1, pl1Err <> npl2Err) (npl2, pl2Err <> npl2Err)
    ulpTotal = ulpVal $ pLineFuzziness (npl1, pl1Err <> npl1Err) <> pLineFuzziness (npl2, pl2Err <> npl2Err)
    (npl1, npl1Err) = normalize pl1
    (npl2, npl2Err) = normalize pl2
    -- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
    angleCos :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ℝ, PPoint2Err)
    angleCos (pline1, pline1Err) (pline2, pline2Err)
      | isNothing canonicalizedIntersection = (0, mempty)
      | otherwise = (angle, iPointErr)
      where
        angle = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
        (CPPoint2 iPointVec,(_,_,iPointErr)) = fromJust canonicalizedIntersection
        motor                     = addVecPairWithoutErr (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
        antiMotor                 = addVecPairWithoutErr (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
        canonicalizedIntersection = canonicalizeIntersectionWithErr (pline1, pline1Err) (pline2, pline2Err)
        -- I, the infinite point.
        gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
        lvec1 = vecOfL pline1
        lvec2 = vecOfL pline2

-- | Find out where two lines intersect, returning a projective point, and the error quotents.
-- Note: this should only be used when you can guarantee these are not collinear, or parallel.
pLineIntersectionWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a, PLine2Err) -> (b, PLine2Err) -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err) = (res, (pl1Err <> npl1Err, pl2Err <> npl2Err, resErr))
  where
    (res, (npl1Err,npl2Err,resErr)) = meetOf2PL pl1 pl2

-- | Generate a point between the two given points, where the weights given determine "how far between".
--   If the weights are equal, the distance will be right between the two points.
-- FIXME: automatically raise addVecRes to a CPPoint2 if it turns out to be canonical?
pPointBetweenPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => (a,PPoint2Err) -> (b, PPoint2Err) -> ℝ -> ℝ -> (ProjectivePoint, (PPoint2Err, PPoint2Err, PPoint2Err))
pPointBetweenPPointsWithErr (startP,startErr) (stopP,stopErr) weight1 weight2
  | isNothing foundVal = error "tried to generate an ideal point?"
  | otherwise = (res, resErr)
  where
    (res@(CPPoint2 (GVec resVals)), cResErr) = canonicalize $ PPoint2 rawRes
    resErr = (cStartErr, cStopErr, cResErr <> startErr <> stopErr <> PPoint2Err mempty mempty pointAddErr weighedStartErr weighedStopErr mempty mempty)
    foundVal = getVal [GEPlus 1, GEPlus 2] resVals
    (rawRes,pointAddErr) = addVecPairWithErr weighedStart weighedStop
    (weighedStart, weighedStartErr) = mulScalarVecWithErr weight1 rawStartPoint
    (weighedStop, weighedStopErr) = mulScalarVecWithErr weight2 rawStopPoint
    rawStartPoint = vecOfP startP'
    rawStopPoint = vecOfP stopP'
    (startP', cStartErr) = canonicalize startP
    (stopP', cStopErr) = canonicalize stopP

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
    (res, normErr)            = normOfPLine2WithErr newPLine
    -- FIXME: how does the error in linePoint and point effect this result?
    (newPLine, newPLineErr)   = join2PP point linePoint
    -- FIXME: how does perpLineErr effect the result of canonicalizeIntersectionWithErr?
    (linePoint, lastPointErr) = fromJust $ canonicalizeIntersectionWithErr (rawNLine, nLineErr) (PLine2 perpLine, mempty)
    (perpLine, perpLineErr)   = lVec ⨅+ pVec
    lVec = vecOfL $ forceBasisOfL rawNLine
    nLineErr = rawNLineErr <> rawLineErr
    (rawNLine, rawNLineErr) = normalize rawLine
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

-- | Find the unsigned distance between two projective points.
distanceBetweenPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => (a,PPoint2Err) -> (b,PPoint2Err) -> (ℝ, (PPoint2Err, PPoint2Err, PLine2Err, UlpSum))
distanceBetweenPPointsWithErr (ppoint1,p1Err) (ppoint2,p2Err)
  -- FIXME: does not take into account error while canonicalizing.
  | cppoint1 == cppoint2 = (0, (c1Err, c2Err, mempty, mempty))
  | otherwise = (abs res, resErr)
  where
    resErr = (rawC1Err
             ,rawC2Err
             ,newPLineErr
             ,ulpSum)
    ulpSum = pPointFuzziness (cppoint1,c1Err) <> pPointFuzziness (cppoint2,c2Err) <> pLineFuzziness (newPLine,newPLineErr)
    newPLineErr = newPLineErrRaw <> normErr
    -- FIXME: how does the error in newPLine effect the found norm here?
    (res, normErr) = normOfPLine2WithErr newPLine
    -- FIXME: how does error in canonicalization effect the PLine generated here?
    (newPLine, (_, _, newPLineErrRaw)) = join2PP cppoint1 cppoint2
    c1Err = rawC1Err <> p1Err
    c2Err = rawC2Err <> p2Err
    (cppoint1,rawC1Err) = canonicalize ppoint1
    (cppoint2,rawC2Err) = canonicalize ppoint2

-- | determine the amount of error in resolving a projective point.
pPointFuzziness :: (ProjectivePoint,PPoint2Err) -> UlpSum
pPointFuzziness (inPPoint, inErr) = UlpSum $ sumTotal * realToFrac (1+(1000*(abs angleIn + realToFrac (ulpVal $ sumPPointErrs angleUnlikeAddErr <> sumPPointErrs angleUnlikeMulErr))))
  where
    sumTotal = ulpVal $ sumPPointErrs pJoinAddErr
                     <> sumPPointErrs pJoinMulErr
                     <> sumPPointErrs pCanonicalizeErr
                     <> sumPPointErrs pAddErr
                     <> sumPPointErrs pIn1MulErr
                     <> sumPPointErrs pIn2MulErr
    (PPoint2Err (pJoinAddErr, pJoinMulErr) pCanonicalizeErr pAddErr pIn1MulErr pIn2MulErr angleIn (angleUnlikeAddErr,angleUnlikeMulErr)) = cpErr <> inErr
    (_, cpErr) = canonicalize inPPoint

-- | determine the amount of error in resolving a projective line.
pLineFuzziness :: (ProjectiveLine2 a) => (a,PLine2Err) -> UlpSum
pLineFuzziness (inPLine, inErr) = transErr
  where
    transErr = tUlp <> eValOf mempty (getVal [GEZero 1] resAddErr) <> eValOf mempty (getVal [GEZero 1] resMulErr)
    (PLine2Err _ _ _ _ tUlp (resAddErr, resMulErr)) = inErr <> nplineErr
    (_, nplineErr) = normalize inPLine

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
    (npl1,pv1Err) = normalize line1
    (npl2,pv2Err) = normalize line2

-- | Return the sine of the angle between the two lines, along with the error.
-- Results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
angleBetweenWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal],[ErrVal]), UlpSum))
angleBetweenWithErr line1 line2 = (res, resErr)
  where
    (res, scalarErr) = (scalarPart like, (eValOf mempty $ getVal [G0] likeMulErr) <> (eValOf mempty $ getVal [G0] likeMulErr))
    resErr = (pv1Err, pv2Err, (likeMulErr,likeAddErr), scalarErr)
    (like, (likeMulErr, likeAddErr)) = p1 ⎣+ p2
    p1 = vecOfL $ forceBasisOfL np1
    p2 = vecOfL $ forceBasisOfL np2
    (np1,pv1Err) = normalize line1
    (np2,pv2Err) = normalize line2

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
-- FIXME: precision on inputs?
sameDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
sameDirection a b = res >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = 1.0 - realToFrac (ulpVal resErr)
    (res, (_,_,_,resErr)) = angleBetweenWithErr a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel.
-- FIXME: precision on inputs?
oppositeDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
oppositeDirection a b = res <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = realToFrac (realToFrac (ulpVal resErr) + (-1) :: Rounded 'TowardNegInf ℝ)
    (res, (_,_,_,resErr)) = angleBetweenWithErr a b

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
    nPLine@((NPLine2 rlvec),rlErr) = normalize pline

-- | Translate a line a given distance along it's perpendicular bisector.
-- Uses the property that translation of a line is expressed on the GEZero component.
translateProjectiveLine2WithErr :: (ProjectiveLine2 a) => a -> ℝ -> (ProjectiveLine, PLine2Err)
translateProjectiveLine2WithErr line d = (PLine2 res, normErr <> PLine2Err resErr mempty mempty mempty tUlp mempty)
  where
    (res,resErr) = addVecPairWithErr m $ vecOfL line
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    -- the amount to add to GEZero 1 component.
    tAdd = d * norm
    tUlp = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    (norm, normErr) = normOfPLine2WithErr line

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
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)

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
pLineIntersectsLineSeg pl1@(_, pl1Err) l1
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
    res = plinesIntersectIn pl1 pl2
    startFudgeFactor = ulpVal $ startDistanceErr <> startErr
    startErr = pLineErrAtPPoint pl2 start
    endFudgeFactor = ulpVal $ endDistanceErr <> endErr
    endErr = pLineErrAtPPoint pl2 end
    (startDistance, (_,_,_, startDistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start,mempty)
    (endDistance, (_,_,_, endDistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end,mempty)
    start = eToPPoint2 $ startPoint l1
    end = eToPPoint2 $ endPoint l1
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pl1 pl2
    (rawIntersect, _) = pLineIntersectionWithErr pl1 pl2
    pl2@(_, pl2Err) = eToPLine2WithErr l1

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
    (start1Distance, (_,_,_, start1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start1,mempty)
    (start2Distance, (_,_,_, start2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start2,mempty)
    (end1Distance, (_,_,_, end1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end1,mempty)
    (end2Distance, (_,_,_, end2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end2,mempty)
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr) && onSegment l2 (rawIntersection,rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (rawIntersect, (npl1Err, npl2Err, rawIntersectErr)) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    start1 = eToPPoint2 $ startPoint l1
    end1 = eToPPoint2 $ endPoint l1
    start2 = eToPPoint2 $ startPoint l2
    end2 = eToPPoint2 $ endPoint l2
    (pl1, pl1Err) = eToPLine2WithErr l1
    (pl2, pl2Err) = eToPLine2WithErr l2

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
    (startDistance, (_,_,_, startDistanceErr)) = distanceBetweenPPointsWithErr i (start, mempty)
    (midDistance, (_,_,_, midDistanceErr)) = distanceBetweenPPointsWithErr i (mid,midErr)
    (endDistance, (_,_,_, endDistanceErr)) = distanceBetweenPPointsWithErr i (end,mempty)
    (mid, (_, _, midErr)) = pPointBetweenPPointsWithErr (start,mempty) (end,mempty) 0.5 0.5
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ ulpVal $ startDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) start
    midFudgeFactor = realToFrac $ ulpVal $ midDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) mid
    endFudgeFactor = realToFrac $ ulpVal $ endDistanceErr <> pLineErrAtPPoint (eToPLine2WithErr ls) end

-- | when given a PLine, and two points guaranteed to be on it, return the maximum distance between a given projective point known to be on the PLine and the 'real' line.
-- FIXME: accept a error on the projectivePoint, and return an error estimate.
pLineErrAtPPoint :: (ProjectiveLine2 a, ProjectivePoint2 b) => (a, PLine2Err) -> b -> UlpSum
pLineErrAtPPoint (inPLine, inPLineErr) errPoint
  -- both intercepts are real. this line is not parallel or collinear to X or Y axises.
  | xInterceptIsRight && yInterceptIsRight && realToFrac (ulpVal res2Axis) < (0.0001 :: ℝ)  = res2Axis -- <> res2Axis
  | xInterceptIsRight && yInterceptIsRight = error $ dumpInputs
                                             <> "res2Axis: " <> show res2Axis <> "\n"
                                             <> "interceptDistance: " <> show interceptDistance <> "\n"
                                             <> "xInterceptFuzz: " <> show xInterceptFuzz <> "\n"
                                             <> "yInterceptFuzz: " <> show yInterceptFuzz <> "\n"
  -- only the xIntercept is real. this line is parallel to the Y axis.
  | xInterceptIsRight = resYAxis -- <> resYAxis
  -- only the yIntercept is real. this line is parallel to the X axis.
  | yInterceptIsRight = resXAxis -- <> resXAxis
  | otherwise = UlpSum $ realToFrac errT
  where
    dumpInputs = "npline: " <> show npline <> "\n"
              <> "inPLineErr: " <> show inPLineErr <> "\n"
              <> "errX: " <> show errX <> "\n"
              <> "errY: " <> show errY <> "\n"
              <> "errT: " <> show errT <> "\n"
    -- this line is parallel to the X axis.
    resXAxis = UlpSum $ realToFrac $ errT + yInterceptFuzz
    -- this line is parallel to the Y axis.
    resYAxis = UlpSum $ realToFrac $ errT + xInterceptFuzz
    -- this line is not parallel to either axis.
    res2Axis = UlpSum $ realToFrac $ errT +
               case interceptDistance of
                 0 -> originFuzz
                 _ -> xInterceptFuzz + yInterceptFuzz
    originFuzz = (errX + errY) * ( 1 + fst (distanceBetweenPPointsWithErr (origin, mempty) (errPoint, mempty)))
    origin = eToPPoint2 $ Point2 (0,0)
    interceptDistance = distance (Point2 (fromRight 0 $ fromJust $ xIntercept npline,0))
                                 (Point2 (0,fromRight 0 $ fromJust $ yIntercept npline))
    -- FIXME: collect this error.
    (Point2 (xPos,yPos),_) = pToEP errPoint
    xInterceptIsRight = isJust (xIntercept npline) && isRight (fromJust $ xIntercept npline)
    -- NOTE: can't check the distance here, because distancePPointToPLineWithErr calls this. ;)
    xInterceptFuzz = errX * (1 + abs xPos)
    yInterceptIsRight = isJust (yIntercept npline) && isRight (fromJust $ yIntercept npline)
    -- NOTE: can't check a distance here, because distancePPointToPLineWithErr calls this. ;)
    yInterceptFuzz = errY * (1 + abs yPos)
    (npline, PLine2Err pLineAddErr pLineNormalizationErr pLineNormErr _ _ (pLineJoinAddErr, pLineJoinMulErr)) = normalize inPLine
    errX, errY, errT :: ℝ
    errX = abs $ realToFrac $ ulpVal $ eValOf mempty (getVal [GEPlus 1] pLineAddErr)
                                    <> eValOf mempty (getVal [GEPlus 1] pLineNormalizationErr)
                                    <> eValOf mempty (getVal [GEPlus 1] pLineJoinAddErr)
                                    <> eValOf mempty (getVal [GEPlus 1] pLineJoinMulErr)
    errY = abs $ realToFrac $ ulpVal $ eValOf mempty (getVal [GEPlus 2] pLineAddErr)
                                    <> eValOf mempty (getVal [GEPlus 2] pLineNormalizationErr)
                                    <> eValOf mempty (getVal [GEPlus 2] pLineJoinAddErr)
                                    <> eValOf mempty (getVal [GEPlus 2] pLineJoinMulErr)
    errT = abs $ realToFrac $ ulpVal $ eValOf mempty (getVal [GEZero 1] pLineAddErr)
                                    <> eValOf mempty (getVal [GEZero 1] pLineNormalizationErr)
                                    <> eValOf mempty (getVal [GEZero 1] pLineJoinAddErr)
                                    <> eValOf mempty (getVal [GEZero 1] pLineJoinMulErr)
                                    <> pLineNormErr

-- | find the point that a given line crosses the X axis.
-- FIXME: return normalization Err
xIntercept :: ProjectiveLine -> Maybe (Either ProjectiveLine ℝ)
xIntercept inPLine
  -- handle a line that is parallel to the X axis.
  | isNothing rawX = Nothing
  -- use X and T to calculate our answer
  | isJust rawT = Just $ Right $ negate $ valOf 0 rawT / valOf 0 rawX
  -- is colinear with the X axis.
  | isNothing rawY = Just $ Left inPLine
  -- we have an X and a Y? this line passes through the origin.
  | isNothing rawT = Just $ Right 0
  | otherwise = error "we should never get here"
  where
    (NPLine2 (GVec pLineVals),_) = normalize inPLine
    rawT = getVal [GEZero 1] pLineVals
    rawX = getVal [GEPlus 1] pLineVals
    rawY = getVal [GEPlus 2] pLineVals

-- | find the point that a given line crosses the Y axis.
-- FIXME: return normErr
yIntercept :: ProjectiveLine -> Maybe (Either ProjectiveLine ℝ)
yIntercept inPLine
  -- handle a line that is parallel to the Y axis.
  | isNothing rawY = Nothing
  -- use Y and T to calculate our answer
  | isJust rawT = Just $ Right $ negate $ valOf 0 rawT / valOf 0 rawY
  -- is along the Y axis.
  | isNothing rawX = Just $ Left inPLine
  -- we have an X and a Y? this line passes through the origin.
  | isNothing rawT = Just $ Right 0
  | otherwise = error "we should never get here"
  where
    (NPLine2 (GVec pLineVals),_) = normalize inPLine
    rawT = getVal [GEZero 1] pLineVals
    rawX = getVal [GEPlus 1] pLineVals
    rawY = getVal [GEPlus 2] pLineVals

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

data ProjectivePoint =
  PPoint2 GVec
  | CPPoint2 GVec
  deriving (Ord, Generic, NFData, Show)

instance Eq ProjectivePoint where
  (==) (CPPoint2 a1) (CPPoint2 a2) = a1 == a2
  (==) p1@(PPoint2 a1) p2@(PPoint2 a2) = a1 == a2 || fst (canonicalize p1) == fst (canonicalize p2)
  (==) p1 p2 = fst (canonicalize p1) == fst (canonicalize p2)

-- | the error accumulated when calculating a projective point.
data PPoint2Err =
  PPoint2Err
    -- MeetErr. max error amounts while meeting two PLines to find this point. divided into add err, and multiply err.
    ([ErrVal], [ErrVal])
    -- CanonicalizeErr. error caused during canonicalization of a point.
    [ErrVal]
    -- AddErr. error created when adding two points to create a third point.
    [ErrVal]
    -- WeighedStartErr. error created when scaling one of the two input points used to create this point.
    [ErrVal]
    -- WeighedStopErr. error created when scaling one of the two input points used to create this point.
    [ErrVal]
    -- angle between the two input lines, when we generate this point via intersecting two lines.
    ℝ
    -- angleUnlikeErr - the error of the unlike operation that generates the angle in the previous field.
    ([ErrVal], [ErrVal])
  deriving (Eq, Show)

instance Semigroup PPoint2Err where
  (<>) (PPoint2Err (a1,b1) c1 d1 e1 f1 g1 (h1,i1)) (PPoint2Err (a2,b2) c2 d2 e2 f2 g2 (h2,i2)) =
    PPoint2Err
      (foldl' addErr a1 a2, foldl' addErr b1 b2)
      (foldl' addErr c1 c2)
      (foldl' addErr d1 d2)
      (foldl' addErr e1 e2)
      (foldl' addErr f1 f2)
      (g1 <> g2)
      (foldl' addErr h1 h2, foldl' addErr i1 i2)

instance Monoid PPoint2Err where
  mempty = PPoint2Err mempty mempty mempty mempty mempty mempty mempty

-- | A line (not a line SEGMENT) in projective space.
-- NOTE: two constructors. one for normalized lines, one for un-normalized lines.
data ProjectiveLine =
  PLine2 GVec
  | NPLine2 GVec
  deriving (Generic, NFData, Show)

class ProjectiveLine2 a where
  consLikeL :: a -> (GVec -> a)
  flipL :: a -> a
  forceBasisOfL :: a -> a
  meetOf2PL :: (ProjectiveLine2 b) => a -> b -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
  normalize :: a -> (ProjectiveLine, PLine2Err)
  translateL :: a -> ℝ -> (ProjectiveLine, PLine2Err)
  vecOfL :: a -> GVec

instance ProjectiveLine2 ProjectiveLine where
  consLikeL a = case a of
                  (NPLine2 _) -> NPLine2
                  (PLine2 _) -> PLine2
  flipL a = flipProjectiveLine a
  forceBasisOfL a = forceProjectiveLineBasis a
  meetOf2PL l1 l2 = meet2ProjectiveLinesWithErr l1 l2
  normalize a = case a of
                  n@(NPLine2 _) -> (n,mempty)
                  p@(PLine2 _) -> normalizePLine2WithErr p
  translateL a d = translateProjectiveLine2WithErr a d
  vecOfL a = case a of
               (NPLine2 v) -> v
               (PLine2 v) -> v
  
instance Eq ProjectiveLine where
  (==) (NPLine2 gvec1) (NPLine2 gvec2) = gvec1 == gvec2
  (==) pl1@(PLine2 gvec1) pl2@(PLine2 gvec2) = gvec1 == gvec2 || normalize pl1 == normalize pl2
  (==) pl1 pl2 = normalize pl1 == normalize pl2

-- | the two types of error of a projective line.
data PLine2Err = PLine2Err
  -- AddErr
    [ErrVal]
  -- NormalizationErr
    [ErrVal]
  -- NormErr
    UlpSum
  -- SqNormErr
    UlpSum
  -- Translation Error. always in GEZero 1.
    UlpSum
  -- Join Error. when a PLine2 is constructed via join.
    ([ErrVal], [ErrVal])
  deriving (Eq, Show)

instance Semigroup PLine2Err where
  (<>) (PLine2Err a1 b1 c1 d1 e1 (f1,g1)) (PLine2Err a2 b2 c2 d2 e2 (f2,g2)) =
    PLine2Err (foldl' addErr a1 a2)
              (foldl' addErr b1 b2)
              (c1 <> c2)
              (d1 <> d2)
              (e1 <> e2)
              (foldl' addErr f1 f2,foldl' addErr g1 g2)

instance Monoid PLine2Err where
  mempty = PLine2Err mempty mempty mempty mempty mempty mempty

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  -- | Can this node be resolved into a point in 2d space?
  canPoint :: a -> Bool
  -- | does this point originate from our input set of euclidian points?
  canEPoint :: a -> Bool
  pPointOf :: a -> ProjectivePoint
  ePointOf :: a -> Point2
  errOfEPoint :: a -> PPoint2Err
  errOfPPoint :: a -> PPoint2Err

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> ProjectiveLine
  errOfOut :: a -> PLine2Err

class ProjectivePoint2 a where
  canonicalize :: a -> (ProjectivePoint, PPoint2Err)
  consLikeP :: a -> (GVec -> a)
  forceBasisOfP :: a -> a
  idealNormOfP :: a -> (ℝ, UlpSum)
  join2PP :: (ProjectivePoint2 b) => a -> b -> (ProjectiveLine, (PPoint2Err, PPoint2Err, PLine2Err))
  pToEP :: a -> (Point2, PPoint2Err)
  vecOfP :: a -> GVec

instance ProjectivePoint2 ProjectivePoint where
  consLikeP p = case p of
                  (CPPoint2 _) -> CPPoint2
                  (PPoint2 _) -> PPoint2
  canonicalize p = case p of
                     (CPPoint2 _) -> (p,mempty)
                     _ -> canonicalizePPoint2WithErr p
  forceBasisOfP p = forceProjectivePointBasis p
  idealNormOfP p = idealNormPPoint2WithErr p
  join2PP p1 p2 = join2ProjectivePointsWithErr p1 p2
  pToEP p = fromMaybe (error "Attempted to create an infinite point when trying to convert from a Projective Point to a Euclidian Point") $ projectivePointToPoint2 p
  vecOfP p = case p of
               (CPPoint2 v) -> v
               (PPoint2 v) -> v

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
(∨+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
(∨+) a b = (dual2DGVec res
           ,(dual2DErrs unlikeAddErr, dual2DErrs unlikeMulErr))
  where
    (res, (unlikeAddErr, unlikeMulErr)) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | a typed join function. join two points, returning a line.
join2ProjectivePointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> (ProjectiveLine, (PPoint2Err, PPoint2Err, PLine2Err))
join2ProjectivePointsWithErr pp1 pp2 = (PLine2 res,
                                        (pv1Ulp, pv2Ulp, PLine2Err mempty mempty mempty mempty mempty resUlp))
  where
    (res,resUlp)  = pv1 ∨+ pv2

    pv1 = vecOfP $ forceBasisOfP cp1
    pv2 = vecOfP $ forceBasisOfP cp2
    (cp1, pv1Ulp) = canonicalize pp1
    (cp2, pv2Ulp) = canonicalize pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2ProjectiveLinesWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
meet2ProjectiveLinesWithErr line1 line2 = (PPoint2 res,
                               (npl1Err,
                                npl2Err,
                                PPoint2Err resUnlikeErr mempty mempty mempty mempty iAngleErr iAngleUnlikeErr))
  where
    (iAngleErr,(_,_,iAngleUnlikeErr,_)) = angleBetweenWithErr npl1 npl2
    (res, resUnlikeErr) = pv1 ⎤+ pv2
    pv1 = vecOfL $ forceBasisOfL npl1
    pv2 = vecOfL $ forceBasisOfL npl2
    (npl1,npl1Err) = normalize line1
    (npl2,npl2Err) = normalize line2

eToPPoint2 :: Point2 -> ProjectivePoint
eToPPoint2 (Point2 (x,y)) = PPoint2 res
  where
    CPPoint2 res = makePPoint2 x y

-- | Create a canonical euclidian projective point from the given coordinates.
makePPoint2 :: ℝ -> ℝ -> ProjectivePoint
makePPoint2 x y = pPoint
  where
    pPoint = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]

-- | Maybe create a euclidian point from a projective point. Will fail if the projective point is ideal.
projectivePointToPoint2 :: (ProjectivePoint2 a) => a -> Maybe (Point2, PPoint2Err)
projectivePointToPoint2 ppoint
 | e12Val == 0 = Nothing
 | otherwise = Just (Point2 (xVal, yVal), cpErrs)
  where
    (CPPoint2 (GVec vals), cpErrs) = canonicalize ppoint
    xVal = negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)
    (GVec rawVals) = vecOfP ppoint

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

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DGVec :: GVec -> GVec
dual2DGVec (GVec vals) = GVec $ foldl' addValWithoutErr []
                 [
                   GVal (         valOf 0 $ getVal [G0] vals)                           (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , GVal (         valOf 0 $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DErrs :: [ErrVal] -> [ErrVal]
dual2DErrs vals = filter (\(ErrVal a _) -> a /= mempty)
                 [
                   ErrVal  realVal                                                     (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]
  where
    realVal = foldl' (<>) mempty $ filter (/= mempty) $ realValue <$> vals
      where
        realValue (ErrVal r gnums) = if gnums == singleton G0 then r else mempty

-- | perform basis coersion. ensure all of the required '0' components exist. required before using basis sensitive raw operators.
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVal (elems needs) has) needs

-- | runtime basis coersion. ensure all of the '0' components exist on a Projective Line.
forceProjectiveLineBasis :: (ProjectiveLine2 a) => a -> a
forceProjectiveLineBasis line
  | gnums == Just [singleton (GEZero 1),
                   singleton (GEPlus 1),
                   singleton (GEPlus 2)] = line
  | otherwise = (consLikeL line) res
  where
    res = forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec
    gnums = case vals of
              [GVal _ g1, GVal _ g2, GVal _ g3] -> Just [g1,g2,g3]
              _                                 -> Nothing
    pvec@(GVec vals) = vecOfL line

-- | runtime basis coersion. ensure all of the '0' components exist on a Projective Point.
forceProjectivePointBasis :: (ProjectivePoint2 a) => a -> a
forceProjectivePointBasis point
  | gnums == Just [fromList [GEZero 1, GEPlus 1],
                   fromList [GEZero 1, GEPlus 2],
                   fromList [GEPlus 1, GEPlus 2]] = point
  | otherwise = (consLikeP point) res
  where
    res = forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
    gnums = case vals of
              [GVal _ g1, GVal _ g2, GVal _ g3] -> Just [g1,g2,g3]
              _                                 -> Nothing
    pvec@(GVec vals) = vecOfP point

-- | Reverse a line. same line, but pointed in the other direction.
flipProjectiveLine :: (ProjectiveLine2 a) => a -> a
flipProjectiveLine pline = (consLikeL pline) rawRes
  where
    rawRes = GVec $ foldl' addValWithoutErr []
             [
               GVal (negate $ valOf 0 $ getVal [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 2] vals) (singleton (GEPlus 2))
             ]
    (GVec vals) = vecOfL pline

eToPLine2WithErr :: LineSeg -> (ProjectiveLine, PLine2Err)
eToPLine2WithErr l1 = (res, resErr)
  where
    (res, (_, _, resErr)) = join2PP (eToPPoint2 $ startPoint l1) (eToPPoint2 $ endPoint l1)

---------------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalarWithErr. ----
---------------------------------------------------------------------

-- | find the idealized norm of a projective point (ideal or not).
idealNormPPoint2WithErr :: (ProjectivePoint2 a) => a -> (ℝ, UlpSum)
idealNormPPoint2WithErr ppoint
  | preRes == 0 = (0, mempty)
  | otherwise   = (res, ulpTotal)
  where
    res = sqrt preRes
    preRes = x*x+y*y
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ x*x)
               + abs (realToFrac $ doubleUlp $ y*y)
               + abs (realToFrac $ doubleUlp preRes)
               + abs (realToFrac $ doubleUlp res)
    (x,y)
     | e12Val == 0 = ( negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals
                     ,          valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals)
     | otherwise = (\(Point2 (x1,y1),_) -> (x1,y1)) $ pToEP ppoint
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)
    (GVec rawVals) = vecOfP ppoint

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- FIXME: return the error of divVecScalarWithErr
canonicalizePPoint2WithErr :: (ProjectivePoint2 a, Show a) => a -> (ProjectivePoint, PPoint2Err)
canonicalizePPoint2WithErr point
  | isNothing foundVal = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = (CPPoint2 $ GVec rawVals, mempty)
  | otherwise = (res, PPoint2Err mempty scaledErrs mempty mempty mempty mempty mempty)
  where
    res = CPPoint2 $ GVec $ foldl' addValWithoutErr []
          $  ( if isNothing (getVal [GEZero 1, GEPlus 1] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
             )
          <> ( if isNothing (getVal [GEZero 1, GEPlus 2] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
             )
          <> [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    newVec = GVec [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])
                  ,GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2])]
    (GVec scaledVals, scaledErrs) = divVecScalarWithErr newVec $ valOf 1 foundVal
    (GVec rawVals) = vecOfP point
    foundVal = getVal [GEPlus 1, GEPlus 2] rawVals

-- | Canonicalize the intersection resulting from two PLines.
-- NOTE: Returns nothing when the PLines are (anti)parallel.
canonicalizeIntersectionWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => (a,PLine2Err) -> (b,PLine2Err) -> Maybe (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
canonicalizeIntersectionWithErr pl1 pl2
  | isNothing foundVal = Nothing
  | otherwise = Just (cpp1, (pl1ResErr, pl2ResErr, intersectionErr <> canonicalizationErr))
  where
    (cpp1, canonicalizationErr) = canonicalize pp1
    (pp1, (pl1ResErr, pl2ResErr, intersectionErr)) = pLineIntersectionWithErr pl1 pl2
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) pp1

-- | Normalize a Projective Line.
normalizePLine2WithErr :: (ProjectiveLine2 a) => a -> (ProjectiveLine, PLine2Err)
normalizePLine2WithErr line = (res, resErr)
  where
    (res, resErr) = case norm of
                      1.0 -> (NPLine2 vec      , normErr)
                      _   -> (NPLine2 scaledVec, normErr <> PLine2Err mempty scaledVecErrs mempty mempty mempty mempty)
    (scaledVec, scaledVecErrs) = divVecScalarWithErr vec norm
    (norm, normErr) = normOfPLine2WithErr line
    vec = vecOfL line

-- | find the norm of a given Projective Line.
normOfPLine2WithErr :: (ProjectiveLine2 a) => a -> (ℝ, PLine2Err)
normOfPLine2WithErr pline = (res, resErr)
  where
    (res, resErr) = case sqNormOfPLine2 of
                      1.0 -> (1.0                , PLine2Err mempty mempty mempty sqNormUlp mempty mempty)
                      _   -> (sqrt sqNormOfPLine2, PLine2Err mempty mempty rawResUlp sqNormUlp mempty mempty)
    (sqNormOfPLine2, sqNormUlp) = sqNormOfPLine2WithErr pline
    rawRes = sqrt sqNormOfPLine2
    rawResUlp = UlpSum (abs $ realToFrac $ doubleUlp rawRes)

-- | find the squared norm of a given Projective Line.
sqNormOfPLine2WithErr :: (ProjectiveLine2 a) => a -> (ℝ, UlpSum)
sqNormOfPLine2WithErr line = (res, ulpTotal)
  where
    res = a*a+b*b
    a = valOf 0 $ getVal [GEPlus 1] vals
    b = valOf 0 $ getVal [GEPlus 2] vals
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ a*a)
               + abs (realToFrac $ doubleUlp $ b*b)
               + abs (realToFrac $ doubleUlp res)
    (GVec vals) = vecOfL line

sumPPointErrs :: [ErrVal] -> UlpSum
sumPPointErrs errs = eValOf mempty (getVal [GEZero 1, GEPlus 1] errs)
                  <> eValOf mempty (getVal [GEZero 1, GEPlus 2] errs)
                  <> eValOf mempty (getVal [GEPlus 1, GEPlus 2] errs)
