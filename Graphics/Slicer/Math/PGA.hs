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
  Intersection(HitStartPoint, HitEndPoint, NoIntersection),
  NPLine2(NPLine2),
  PIntersection (PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2(PLine2),
  PPoint2(PPoint2),
  CPPoint2(CPPoint2),
  Arcable(hasArc, outOf, ulpOfOut, outUlpMag),
  Pointable(canPoint, pPointOf, ePointOf),
  PPoint2PosErr(PPoint2PosErr),
  angleBetweenWithErr,
  combineConsecutiveLineSegs,
  canonicalizePPoint2WithErr,
  cPPointBetweenCPPointsWithErr,
  cPToEPoint2,
  distanceBetweenCPPointsWithErr,
  distanceBetweenNPLine2sWithErr,
  distanceCPPointToNPLineWithErr,
  distancePPointToPLineWithErr,
  eToCPPoint2WithErr,
  eToPLine2WithErr,
  eToPPoint2WithErr,
  flipPLine2,
  getInsideArcWithErr,
  getFirstArcWithErr,
  intersectsWith,
  intersectsWithErr,
  join2PPoint2WithErr,
  join2CPPoint2WithErr,
  makeCPPoint2WithErr,
  normalizePLine2WithErr,
  outputIntersectsLineSeg,
  pLineFromEndpointsWithErr,
  pLineIntersectionWithErr,
  pLineIsLeft,
  pPointBetweenPPointsWithErr,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEPoint2,
  plinesIntersectIn,
  translatePLine2WithErr,
  translateRotatePPoint2,
  ulpOfLineSeg,
  ulpOfPLine2
  ) where

import Prelude (Bool, Eq((==),(/=)), Show(show), Ord, ($), (*), (-), (>=), (&&), (<$>), mempty, otherwise, signum, (>), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, error, sin, cos, realToFrac, fst, sum, (.), realToFrac)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes, fromJust, isNothing, maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, TowardNegInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, scalePoint, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal, GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addValWithoutErr, addVecPairWithErr, addVecPairWithoutErr, divVecScalarWithErr, getVal, mulScalarVecWithErr, scalarPart, sumErrVals, ulpVal, valOf, vectorPart)

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
  | IntersectsIn !CPPoint2 !(UlpSum, UlpSum, UlpSum, UlpSum, UlpSum, UlpSum)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> PIntersection
plinesIntersectIn pl1 pl2
  | isNothing canonicalizedIntersection
  || (idealNorm <= realToFrac (ulpVal idnErr)
     && (sameDirection pl1 pl2 ||
         oppositeDirection pl1 pl2)) = if sameDirection pl1 pl2
                                       then PCollinear
                                       else PAntiCollinear
  | sameDirection pl1 pl2            = PParallel
  | oppositeDirection pl1 pl2        = PAntiParallel
  | otherwise                        = IntersectsIn res (resUlp, intersectUlp, npl1Ulp, npl2Ulp, iaErr, mempty)
  where
    (idealNorm, idnErr) = idealNormPPoint2WithErr intersectPoint
    (_, iaErr) = angleBetweenWithErr pl1 pl2
    -- FIXME: how much do the potential normalization errors have an effect on the resultant angle?
    (intersectPoint, intersectUlp) = pLineIntersectionWithErr pl1 pl2
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (res, resUlp) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pl1 pl2
    (_, npl1Ulp) = normalize pl1
    (_, npl2Ulp) = normalize pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Maybe Bool
pLineIsLeft pl1 pl2
  | npl1 == npl2       = Nothing
  | abs res <= 0       = Nothing
  | otherwise          = Just $ res > 0
  where
    -- FIXME: naieve implementation. use npl1Err and npl2Err to get two angle differences.
    (res, _) = angleCos npl1 npl2
    (npl1, _) = normalize pl1
    (npl2, _) = normalize pl2
    -- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
    angleCos :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, UlpSum)
    angleCos l1 l2
      | isNothing canonicalizedIntersection = (0, mempty)
      | otherwise = (angle, iPointErr)
      where
        angle = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
        (CPPoint2 iPointVec, iPointErr) = fromJust canonicalizedIntersection
        motor                     = addVecPairWithoutErr (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
        antiMotor                 = addVecPairWithoutErr (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
        canonicalizedIntersection = canonicalizeIntersectionWithErr (NPLine2 lvec1) (NPLine2 lvec2)
        -- I, the infinite point.
        gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
        lvec1 = vecOfL l1
        lvec2 = vecOfL l2

-- | Find out where two lines intersect, returning a projective point, and the error quotent. Note that this should only be used when you can guarantee these are not collinear, or parallel.
pLineIntersectionWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PPoint2, UlpSum)
pLineIntersectionWithErr pl1 pl2 = (res, ulpTotal)
  where
    (res, resErr) = meet2PLine2WithErr npl1 npl2
    (npl1, npl1Err) = normalize pl1
    (npl2, npl2Err) = normalize pl2
    ulpTotal = resErr <> npl1Err <> npl2Err

-- NOTE: returns a canonicalized point.
pPointBetweenPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> (PPoint2, UlpSum)
pPointBetweenPPointsWithErr start stop weight1 weight2 = (PPoint2 cRes, UlpSum $ cResErr + cStartErr + cStopErr)
  where
    (CPPoint2 cRes, UlpSum cResErr) = cPPointBetweenCPPointsWithErr cStart cStop weight1 weight2
    (cStart, UlpSum cStartErr) = canonicalize start
    (cStop, UlpSum cStopErr) = canonicalize stop

cPPointBetweenCPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> (CPPoint2, UlpSum)
cPPointBetweenCPPointsWithErr startP stopP weight1 weight2
  | isNothing foundVal = error "tried to generate an ideal point?"
  | otherwise = (res, ulpSum)
  where
    (res, resUlpSum) = canonicalizePPoint2WithErr $ PPoint2 addVecRes
    ulpSum = resUlpSum <> sumErrVals addVecResErr <> sumErrVals weighedStartErr <> sumErrVals weighedStopErr
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) addVecRes
    (addVecRes, addVecResErr) = addVecPairWithErr weighedStart weighedStop
    (weighedStart, weighedStartErr) = mulScalarVecWithErr weight1 rawStartPoint
    (weighedStop, weighedStopErr) = mulScalarVecWithErr weight2 rawStopPoint
    rawStartPoint = vecOfP startP
    rawStopPoint = vecOfP stopP

distancePPointToPLineWithErr :: (ProjectiveLine2 b) => PPoint2 -> b -> (ℝ, UlpSum)
distancePPointToPLineWithErr point line = (res, resErr <> normErr <> nPVecErr)
  where
    (res, resErr)         = distanceCPPointToNPLineWithErr rnpvec normedLine
    (rnpvec, nPVecErr)    = canonicalizePPoint2WithErr point
    (normedLine, normErr) = normalize line

-- FIXME: use the distance to increase ULP appropriately?
distanceCPPointToNPLineWithErr :: (ProjectiveLine2 b) => CPPoint2 -> b -> (ℝ, UlpSum)
distanceCPPointToNPLineWithErr point line
  | valOf 0 foundVal == 0 = error "attempted to get the distance of an ideal point."
  | otherwise = (res, ulpTotal)
  where
    (res, resErr)                  = normOfPLine2WithErr newPLine
    (newPLine, newPLineErr)        = join2CPPoint2WithErr point linePoint
    (perpLine, (plMulErr,plAddErr))= lvec ⨅+ npvec
    (PLine2 lvec)                  = forcePLine2Basis (PLine2 nplvec)
    (CPPoint2 npvec)               = forceCPPoint2Basis point
    (linePoint, lpErr)             = fromJust $ canonicalizeIntersectionWithErr (PLine2 lvec) (PLine2 perpLine)
    ulpTotal                       = sumErrVals plMulErr <> sumErrVals plAddErr <> resErr <> newPLineErr <>  lpErr
    foundVal                       = getVal [GEPlus 1, GEPlus 2] $ (\(CPPoint2 (GVec vals)) -> vals) point
    (NPLine2 nplvec,_)               = normalize line

-- | Determine if two points are on the same side of a given line.
pPointsOnSameSideOfPLine :: (ProjectiveLine2 c) => PPoint2 -> PPoint2 -> c -> Maybe Bool
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
    (PPoint2 pv1) = forcePPoint2Basis point1
    (PPoint2 pv2) = forcePPoint2Basis point2
    lv1 = vecOfL $ forcePLine2Basis line

distanceBetweenCPPointsWithErr :: CPPoint2 -> CPPoint2 -> (ℝ, UlpSum)
distanceBetweenCPPointsWithErr cpoint1 cpoint2 = (res, ulpTotal)
  where
    (res, UlpSum resErr)           = normOfPLine2WithErr newPLine
    (newPLine, UlpSum newPLineErr) = join2CPPoint2WithErr cpoint1 cpoint2
    ulpTotal                       = UlpSum $ resErr + newPLineErr

-- | Find the unsigned distance between two parallel or antiparallel projective lines.
distanceBetweenNPLine2sWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, UlpSum)
distanceBetweenNPLine2sWithErr line1 line2 = (ideal, resUlpSum)
  where
    (ideal, idealUlpSum) = idealNormPPoint2WithErr $ PPoint2 likeRes
    resUlpSum = idealUlpSum <> sumErrVals likeMulErr <> sumErrVals likeAddErr
    (likeRes, (likeMulErr, likeAddErr)) = p1 ⎣+ p2
    p1 = vecOfL $ forcePLine2Basis npl1
    p2 = vecOfL $ forcePLine2Basis npl2
    (npl1, _) = normalize line1
    (npl2, _) = normalize line2

-- | Return the sine of the angle between the two lines, along with the error. results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
-- FIXME: not generating large enough ULPs. why?
angleBetweenWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, UlpSum)
angleBetweenWithErr pl1 pl2 = (scalarPart likeRes
                              , ulpSum)
  where
    ulpSum = sumErrVals likeMulErr <> sumErrVals likeAddErr
    (likeRes, (likeMulErr, likeAddErr)) = p1 ⎣+ p2
    (PLine2 p1) = forcePLine2Basis $ PLine2 pv1
    (PLine2 p2) = forcePLine2Basis $ PLine2 pv2
    (NPLine2 pv1, _) = normalize pl1
    (NPLine2 pv2, _) = normalize pl2

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
-- FIXME: precision on inputs?
sameDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
sameDirection a b = res  >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = 1.0 - realToFrac (ulpVal resErr)
    (res, resErr) = angleBetweenWithErr a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel.
-- FIXME: precision on inputs?
oppositeDirection :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
oppositeDirection a b = res <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = realToFrac (realToFrac (ulpVal resErr) + (-1) :: Rounded 'TowardNegInf ℝ)
    (res, resErr) = angleBetweenWithErr a b

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: (ProjectiveLine2 a) => a -> PPoint2 -> ℝ -> (PPoint2, UlpSum)
pPointOnPerpWithErr line rppoint d = (PPoint2 res,
                                       ulpTotal)
  where
    res = motor•pvec•reverseGVec motor
    (NPLine2 rlvec, lErr)          = normalize line
    (perpLine, (plMulErr,plAddErr))= lvec ⨅+ pvec
    (PLine2 lvec)                  = forcePLine2Basis $ PLine2 rlvec
    (PPoint2 pvec)                 = forcePPoint2Basis rppoint
    motor = addVecPairWithoutErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr = UlpSum $ abs $ realToFrac $ doubleUlp $ d/2
    ulpTotal = sumErrVals plMulErr <> sumErrVals plAddErr <> gaIErr <>lErr

-- | Translate a line a given distance along it's perpendicular bisector.
-- Abuses the property that translation of a line is expressed on the GEZero component.
translateProjectiveLine2WithErr :: GVec -> ℝ -> (GVec, PLine2Err)
translateProjectiveLine2WithErr lineVec d = (res, PLine2Err resErr mempty nErr tErr mempty)
  where
    (res, resErr) = addVecPairWithErr m lineVec
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    -- the amount to add to the GEZero 1 component.
    tAdd = d * n
    tErr = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    (n, nErr) = normOfPLine2WithErr (PLine2 lineVec)

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: PPoint2 -> ℝ -> ℝ -> PPoint2
translateRotatePPoint2 ppoint d rotation = PPoint2 $ translator•pvec•reverseGVec translator
  where
    (PPoint2 pvec)      = ppoint
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        (PLine2 xLineVec) = forcePLine2Basis $ fst $ pLineFromEndpointsWithErr (Point2 (0,0)) (Point2 (1,0))
    (PLine2 angledLineThroughPPoint2) = forcePLine2Basis $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
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
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   pl1 pl2
intersectsWith (Left l1)   (Right pl1) =         pLineIntersectsLineSeg (pl1, ulpOfPLine2 pl1) (l1, ulpOfLineSeg l1) 0
intersectsWith (Right pl1) (Left l1)   =         pLineIntersectsLineSeg (pl1, ulpOfPLine2 pl1) (l1, ulpOfLineSeg l1) 0

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
outputIntersectsLineSeg :: (Show a, Arcable a, Pointable a) => a -> (LineSeg, UlpSum) -> Either Intersection PIntersection
outputIntersectsLineSeg source (l1, UlpSum l1Err)
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn pl1 pl2
  | intersectionDistance < foundError = pLineIntersectsLineSeg (pl1, UlpSum pl1Err) (l1, UlpSum l1Err) 1
  | ulpScale > 100000 = error
                        $ "wtf\n"
                        <> "ulpScale: " <> show ulpScale <> "\n"
                        <> "travelUlpMul: " <> show travelUlpMul <> "\n"
                        <> "ulpMultiplier: " <> show ulpMultiplier <> "\n"
                        <> "angle: " <> show angle <> "\n"
                        <> "angleErr: " <> show angleErr <> "\n"
                        <> "pl1Mag: " <> show pl1Mag <> "\n"
                        <> "intersectionDistance: " <> show intersectionDistance <> "\n"
                        <> show source <> "\n"
                        <> show pl2 <> "\n"
                        <> show l1 <> "\n"
                        <> show foundError <> "\n"
  | otherwise = pLineIntersectsLineSeg (pl1, UlpSum pl1Err) (l1, UlpSum l1Err) ulpScale
  where
    foundError :: ℝ
    foundError = realToFrac $ l1Err + pl1Err + pl2Err + npl1Err + npl2Err + rawIntersectionErr + intersectionDistanceErr + canonicalizedSourceErr
    -- | the multiplier used to expand the hitcircle of an endpoint.
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * realToFrac travelUlpMul * abs (realToFrac angle + angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, UlpSum npl1Err) = normalizePLine2WithErr pl1
    (npl2, UlpSum npl2Err) = normalizePLine2WithErr pl2
    (pl2, UlpSum pl2Err) = eToPLine2WithErr l1
    -- the multiplier to account for distance between our Pointable, and where it intersects.
    travelUlpMul
      | canPoint source = pl1Mag / intersectionDistance
      | otherwise = error
                    $ "cannot resolve source to a point?\n"
                    <> show source <> "\n"
    (pl1, UlpSum pl1Err, pl1Mag)
      | hasArc source = (outOf source, ulpOfOut source, outUlpMag source)
      | otherwise = error
                    $ "no arc from source?\n"
                    <> show source <> "\n"
    (rawIntersection, UlpSum rawIntersectionErr) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pl1 pl2
    (canonicalizedSource, UlpSum canonicalizedSourceErr) = canonicalizePPoint2WithErr $ pPointOf source
    (intersectionDistance, UlpSum intersectionDistanceErr) = distanceBetweenCPPointsWithErr canonicalizedSource rawIntersection

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2WithErr = Either (LineSeg, UlpSum) (PLine2,UlpSum)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)       (Left l2)       =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right (pl1,_)) (Right (pl2,_)) = Right $ plinesIntersectIn pl1 pl2
intersectsWithErr (Left l1@(rawL1,_))       (Right pl1@(rawPL1,_))     =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle) + angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr rawPL1
    (npl2, _) = normalizePLine2WithErr pl2
    (pl2, _) = eToPLine2WithErr rawL1
intersectsWithErr (Right pl1@(rawPL1,_))     (Left l1@(rawL1,_))       =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle) + angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr rawPL1
    (npl2, _) = normalizePLine2WithErr pl2
    (pl2, _) = eToPLine2WithErr rawL1

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (PLine2, UlpSum) -> (LineSeg, UlpSum) -> ℝ -> Either Intersection PIntersection
pLineIntersectsLineSeg (pl1, UlpSum pl1Err) (l1, UlpSum l1Err) ulpScale
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (startErr + endErr + startDistanceErr+endDistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nulpScale: " <> show ulpScale <> "\nrawIntersect" <> show rawIntersect <> dumpULPs
  | hasIntersection && valOf 0 foundVal == 0 = error "intersection, but cannot cannonicalize."
  | hasIntersection && startDistance <= ulpStartSum = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= ulpEndSum = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum $ realToFrac ulpStartSum, UlpSum $ realToFrac ulpEndSum, UlpSum pl1Err, UlpSum pl2Err, UlpSum rawIntersectErr, UlpSum rawIntersectionErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum $ realToFrac ulpStartSum, UlpSum $ realToFrac ulpEndSum, mempty, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (mempty, mempty, mempty, mempty)
  where
    (startDistance, UlpSum startDistanceErr) = distanceBetweenCPPointsWithErr rawIntersection start
    (endDistance, UlpSum endDistanceErr) = distanceBetweenCPPointsWithErr rawIntersection end
    (start, PPoint2PosErr startErr) = eToCPPoint2WithErr $ startPoint l1
    (end, PPoint2PosErr endErr) = eToCPPoint2WithErr $ endPoint l1
    ulpStartSum, ulpEndSum :: ℝ
    ulpStartSum = realToFrac $ ulpTotal+startDistanceErr
    ulpEndSum = realToFrac $ ulpTotal+endDistanceErr
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    -- Note: we do not use rawIntersectionErr here.
    ulpTotal
      | pl1Err < 0 || pl2Err < 0 || l1Err < 0 || rawIntersectErr < 0 = error "negative ULP?\n"
      | otherwise = pl1Err + pl2Err + l1Err + (rawIntersectErr * realToFrac ulpScale)
    dumpULPs = "pl1Err: " <> show pl1Err <> "\npl2Err: " <> show pl2Err <> "\nl1Err: " <> show l1Err <> "\nrawIntersectErr: " <> show rawIntersectErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum ulpEndSum
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum rawIntersectionErr) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum rawIntersectErr) = pLineIntersectionWithErr pl1 pl2
    (pl2, UlpSum pl2Err) = eToPLine2WithErr l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, UlpSum l1Err) (l2, UlpSum ulpL2)
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (start1Err + end1Err + start1DistanceErr+end1DistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < realToFrac (start2Err + end2Err + start2DistanceErr+end2DistanceErr+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && start1Distance <= ulpStartSum1 = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= ulpEndSum1 = Left $ HitEndPoint l1
  | hasIntersection && start2Distance <= ulpStartSum2 = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= ulpEndSum2 = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum $ realToFrac ulpStartSum1, UlpSum $ realToFrac ulpEndSum1, UlpSum $ realToFrac ulpStartSum2, UlpSum $ realToFrac ulpEndSum2, UlpSum ulpTotal, UlpSum rawIntersectErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum $ realToFrac ulpStartSum1, UlpSum $ realToFrac ulpEndSum1, UlpSum $ realToFrac ulpStartSum2, UlpSum $ realToFrac ulpEndSum2)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (mempty, mempty, mempty, mempty)
  where
    ulpStartSum1, ulpEndSum1, ulpStartSum2, ulpEndSum2 :: ℝ
    ulpStartSum1 = realToFrac $ ulpTotal+start1DistanceErr
    ulpStartSum2 = realToFrac $ ulpTotal+start2DistanceErr
    ulpEndSum1 = realToFrac $ ulpTotal+end1Err
    ulpEndSum2 = realToFrac $ ulpTotal+end2Err
    (start1Distance, UlpSum start1DistanceErr) = distanceBetweenCPPointsWithErr rawIntersection start1
    (start2Distance, UlpSum start2DistanceErr) = distanceBetweenCPPointsWithErr rawIntersection start2
    (end1Distance, UlpSum end1DistanceErr) = distanceBetweenCPPointsWithErr rawIntersection end1
    (end2Distance, UlpSum end2DistanceErr) = distanceBetweenCPPointsWithErr rawIntersection end2
    (start1, PPoint2PosErr start1Err) = eToCPPoint2WithErr $ startPoint l1
    (end1, PPoint2PosErr end1Err) = eToCPPoint2WithErr $ endPoint l1
    (start2, PPoint2PosErr start2Err) = eToCPPoint2WithErr $ startPoint l2
    (end2, PPoint2PosErr end2Err) = eToCPPoint2WithErr $ endPoint l2
    (pl1, UlpSum pl1Err) = eToPLine2WithErr l1
    (pl2, UlpSum pl2Err) = eToPLine2WithErr l2
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal
      | pl1Err < 0 || pl2Err < 0 || l1Err < 0 || ulpL2 < 0 || rawIntersectErr < 0 || rawIntersectionErr < 0 = error "negative ULP?\n"
      | otherwise = pl1Err + pl2Err + l1Err + ulpL2 + (rawIntersectErr * ulpScale)
    ulpScale = 120 + ulpMultiplier * (realToFrac angle+angleErr) * (realToFrac angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr pl1
    (npl2, _) = normalizePLine2WithErr pl2
    dumpULPs = "pl1Err: " <> show pl1Err <> "\npl2Err: " <> show pl2Err <> "\nl1Err: " <> show l1Err <> "\nrawIntersectErr: " <> show rawIntersectErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum1 ulpEndSum1 && onSegment l2 rawIntersection ulpStartSum2 ulpEndSum2
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum rawIntersectionErr) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum rawIntersectErr) = pLineIntersectionWithErr pl1 pl2

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> CPPoint2 -> ℝ -> ℝ -> Bool
onSegment ls i startUlp endUlp =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    (startDistance, UlpSum startDistanceErr) = distanceBetweenCPPointsWithErr start i
    (midDistance, UlpSum midDistanceErr) = distanceBetweenCPPointsWithErr mid i
    (endDistance, UlpSum endDistanceErr) = distanceBetweenCPPointsWithErr end i
    (start, PPoint2PosErr startErr) = eToCPPoint2WithErr $ startPoint ls
    (mid, UlpSum midErr) = cPPointBetweenCPPointsWithErr start end 0.5 0.5
    (end, PPoint2PosErr endErr) = eToCPPoint2WithErr $ endPoint ls
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ realToFrac startUlp + startDistanceErr + startErr
    midFudgeFactor = realToFrac $ abs (realToFrac $ doubleUlp lengthOfSegment) + midDistanceErr + midErr
    endFudgeFactor = realToFrac $ realToFrac endUlp + endDistanceErr + endErr

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
        sameLineSeg = plinesIntersectIn (fst $ eToPLine2WithErr l1) (fst $ eToPLine2WithErr l2) == PCollinear
        sameMiddlePoint = p2 == addPoints p1 s1

-- | Get a PLine in the direction of the inside of the contour, at the angle bisector of the intersection of the line segment, and another segment from the end of the given line segment, toward the given point.
--   Note that we normalize our output, but return it as a PLine2. this is safe, because double normalization (if it happens) only raises the ULP.
getFirstArcWithErr :: Point2 -> Point2 -> Point2 -> (PLine2, UlpSum)
getFirstArcWithErr p1 p2 p3
  -- since we hawe two equal sides, we can draw a point ot the other side of the quad, and use it for constructing.
  | distance p2 p1 == distance p2 p3 = (PLine2 quadRes, UlpSum $ quadErr + quadResErr)
  {-
  | distance p2 p1 > distance p2 p3 = scaleSide p1 p3 True (distance p2 p1 / distance p2 p3)
  | otherwise = scaleSide p3 p1 True (distance p2 p3 / distance p2 p1)
  -}
  | otherwise = (insideArc, UlpSum $ side1Err + side2Err + insideArcErr)
  where
    (insideArc, UlpSum insideArcErr) = getInsideArcWithErr (PLine2 side1) (PLine2 side2)
    (NPLine2 side1, UlpSum side1NormErr) = normalizePLine2WithErr side1Raw
    (side1Raw, UlpSum side1RawErr) = pLineFromEndpointsWithErr p1 p2
    side1Err = side1NormErr+side1RawErr
    (NPLine2 side2, UlpSum side2NormErr) = normalizePLine2WithErr side2Raw
    (side2Raw, UlpSum side2RawErr) = pLineFromEndpointsWithErr p2 p3
    side2Err = side2NormErr+side2RawErr
    (NPLine2 quadRes, UlpSum quadResErr) = normalizePLine2WithErr quad
    (quad, UlpSum quadErr) = pLineFromEndpointsWithErr p2 $ scalePoint 0.5 $ addPoints p1 p3
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
  | otherwise = (PLine2 normRes, ulpSum)
  where
    ulpSum = resUlp <> sumErrVals addVecErr
    (NPLine2 normRes, resUlp) = normalize $ PLine2 $ addVecRes
    (addVecRes, addVecErr) = addVecPairWithErr flippedPV1 pv2
    flippedPV1 = vecOfL $ flipPLine2 line1
    pv2 = vecOfL line2

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

-- | A projective point in 2D space.
newtype PPoint2 = PPoint2 GVec
  deriving (Eq, Ord, Generic, NFData, Show)

-- | A canonicalized projective point in 2D space.
newtype CPPoint2 = CPPoint2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  canPoint :: a -> Bool
  pPointOf :: a -> PPoint2
  ePointOf :: a -> Point2
--  ulpOfPoint :: a -> UlpSum

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> PLine2
  ulpOfOut :: a -> UlpSum
  outUlpMag :: a -> ℝ

class ProjectivePoint2 a where
  canonicalize :: a -> (CPPoint2, UlpSum)
  vecOfP :: a -> GVec

instance ProjectivePoint2 PPoint2 where
  canonicalize p = canonicalizePPoint2WithErr p
  vecOfP (PPoint2 a) = a

instance ProjectivePoint2 CPPoint2 where
  canonicalize p = (p, mempty)
  vecOfP (CPPoint2 a) = a

-- | A projective line in 2D space.
newtype PLine2 = PLine2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | A normalized projective line in 2D space.
newtype NPLine2 = NPLine2 GVec
  deriving (Eq, Generic, NFData, Show)

class ProjectiveLine2 a where
  normalize :: a -> (NPLine2, UlpSum)
  flipPLine2 :: a -> a
  forcePLine2Basis :: a -> a
  translatePLine2WithErr :: a -> ℝ -> (a, UlpSum)
  vecOfL :: a -> GVec

instance ProjectiveLine2 NPLine2 where
  normalize a = (a, mempty)
  flipPLine2 (NPLine2 a)  = NPLine2 $ flipGVec a
  forcePLine2Basis (NPLine2 a) = NPLine2 $ forceProjectiveLine2Basis a
  translatePLine2WithErr (NPLine2 a) d = (\(b,(PLine2Err _ _ _ t _)) -> (NPLine2 b,t)) $ translateProjectiveLine2WithErr a d
  vecOfL (NPLine2 a) = a

instance ProjectiveLine2 PLine2 where
  normalize a = normalizePLine2WithErr a
  flipPLine2 (PLine2 a) = PLine2 $ flipGVec a
  forcePLine2Basis (PLine2 a) = PLine2 $ forceProjectiveLine2Basis a
  translatePLine2WithErr (PLine2 a) d = (\(b,(PLine2Err _ _ _ t _)) -> (PLine2 b,t)) $ translateProjectiveLine2WithErr a d
  vecOfL (PLine2 a) = a

-- | the two types of error of a projective line.
data PLine2Err = PLine2Err
  -- AddErr
    [ErrVal]
  -- NormalizationErr
    [ErrVal]
  -- NormErr
    UlpSum
  -- Translation Error. always in GEZero 1.
    UlpSum
  -- JoinErr
    ([ErrVal], [ErrVal])
  deriving (Eq, Show)

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
(∨+) :: GVec -> GVec -> (GVec, UlpSum)
(∨+) a b = (vec
           , ulpSum)
  where
    vec = dual2DGVec $ res
    ulpSum = sumErrVals mulErr <> sumErrVals addErr
    (res, (addErr, mulErr)) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | a typed join function. join two points, returning a line.
join2PPoint2WithErr :: PPoint2 -> PPoint2 -> (PLine2, UlpSum)
join2PPoint2WithErr pp1 pp2 = (res,
                               errTotal)
  where
    (res,UlpSum resErr)  = join2CPPoint2WithErr cp1 cp2
    (cp1, UlpSum pv1Err) = canonicalizePPoint2WithErr pp1
    (cp2, UlpSum pv2Err) = canonicalizePPoint2WithErr pp2
    errTotal = UlpSum $ resErr + pv1Err + pv2Err

-- | a typed join function. join two points, returning a line.
join2CPPoint2WithErr :: CPPoint2 -> CPPoint2 -> (PLine2, UlpSum)
join2CPPoint2WithErr pp1 pp2 = (PLine2 res,
                                resUlp)
  where
    (res,resUlp)  = pv1 ∨+ pv2
    (CPPoint2 pv1) = forceCPPoint2Basis pp1
    (CPPoint2 pv2) = forceCPPoint2Basis pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2PLine2WithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PPoint2, UlpSum)
meet2PLine2WithErr line1 line2 = (PPoint2 res,
                                   ulpSum)
  where
    ulpSum = sumErrVals mulErr <> sumErrVals addErr
    (res, (addErr, mulErr)) = pv1 ⎤+ pv2
    (PLine2 pv1) = forcePLine2Basis $ PLine2 plr1
    (PLine2 pv2) = forcePLine2Basis $ PLine2 plr2
    (NPLine2 plr1,_) = normalize line1
    (NPLine2 plr2,_) = normalize line2

newtype PPoint2PosErr = PPoint2PosErr (Rounded 'TowardInf ℝ)

eToPPoint2WithErr :: Point2 -> (PPoint2, PPoint2PosErr)
eToPPoint2WithErr (Point2 (x,y)) = (PPoint2 res, resUlp)
  where
    (CPPoint2 res, resUlp) = makeCPPoint2WithErr x y

eToCPPoint2WithErr :: Point2 -> (CPPoint2, PPoint2PosErr)
eToCPPoint2WithErr (Point2 (x,y)) = (res, resUlp)
  where
    (res, resUlp) = makeCPPoint2WithErr x y

-- | Create a canonical euclidian projective point from the given coordinates, with error.
-- FIXME: is there any chance of loss of precision on y?
makeCPPoint2WithErr :: ℝ -> ℝ -> (CPPoint2, PPoint2PosErr)
makeCPPoint2WithErr x y = (pPoint
                         , posErr)
  where
    pPoint = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]
    posErr = PPoint2PosErr $ abs (realToFrac $ doubleUlp $ negate x) -- + abs (realToFrac $ doubleUlp y)

-- | Create a euclidian point from a projective point.
pToEPoint2 :: PPoint2 -> Point2
pToEPoint2 ppoint
  | isNothing res = error "created an infinite point when trying to convert from a PPoint2 to a Point2"
  | otherwise = fromJust res
  where
    res = pPointToPoint2 ppoint

-- | Create a euclidian point from a projective point.
cPToEPoint2 :: CPPoint2 -> Point2
cPToEPoint2 (CPPoint2 rawPoint)
  | isNothing res = error "created an infinite point when trying to convert from a PPoint2 to a Point2"
  | otherwise = fromJust res
  where
    res = pPointToPoint2 $ PPoint2 rawPoint

-- | Maybe create a euclidian point from a projective point.
-- FIXME: does negate cause a precision loss?
-- FIXME: canonicalization certainly does...
pPointToPoint2 :: PPoint2 -> Maybe Point2
pPointToPoint2 point@(PPoint2 (GVec rawVals))
 | e12Val == 0 = Nothing
 | e12Val == 1 = Just $ Point2 (xVal, yVal)
 | otherwise = Just $ Point2 (xVal, yVal)
  where
    (CPPoint2 (GVec vals)) = fst $ canonicalizePPoint2WithErr point
    xVal = negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)

-- | Reverse a vector. Really, take every value in it, and recompute it in the reverse order of the vectors (so instead of e0∧e1, e1∧e0). which has the effect of negating bi and tri-vectors.
reverseGVec :: GVec -> GVec
reverseGVec vec = GVec $ foldl' addValWithoutErr []
                  [
                    GVal           realVal                                               (singleton G0)
                  , GVal (         valOf 0 $ getVal [GEZero 1] vals)                     (singleton (GEZero 1))
                  , GVal (         valOf 0 $ getVal [GEPlus 1] vals)                     (singleton (GEPlus 1))
                  , GVal (         valOf 0 $ getVal [GEPlus 2] vals)                     (singleton (GEPlus 2))
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)           (fromList [GEZero 1, GEPlus 1])
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals)           (fromList [GEZero 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVal [GEPlus 1, GEPlus 2] vals)           (fromList [GEPlus 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                  ]
  where
    realVal     = scalarPart vec
    (GVec vals) = vectorPart vec

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DGVec :: GVec -> GVec
dual2DGVec vec = GVec $ foldl' addValWithoutErr []
                 [
                   GVal           realVal                                               (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , GVal (         valOf 0 $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]
  where
    realVal     = scalarPart vec
    (GVec vals) = vectorPart vec

-- | perform basis coersion. ensure all of the required '0' components exist. required before using basis sensitive raw operators.
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVal (elems needs) has) needs

-- | runtime basis coersion. ensure all of the '0' components exist on a PLine2.
forceProjectiveLine2Basis :: GVec -> GVec
forceProjectiveLine2Basis pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3])
  | gnum1 == singleton (GEZero 1) &&
    gnum2 == singleton (GEPlus 1) &&
    gnum3 == singleton (GEPlus 2)    = pvec
  | otherwise                        = forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec
forceProjectiveLine2Basis pvec = forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec

-- | runtime basis coersion. ensure all of the '0' components exist on a PPoint2.
forcePPoint2Basis :: PPoint2 -> PPoint2
forcePPoint2Basis pt@(PPoint2 pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3]))
  | gnum1 == fromList [GEZero 1, GEPlus 1] &&
    gnum2 == fromList [GEZero 1, GEPlus 2] &&
    gnum3 == fromList [GEPlus 1, GEPlus 2]    = pt
  | otherwise                                 = PPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
forcePPoint2Basis (PPoint2 pvec)              = PPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec

-- | runtime basis coersion. ensure all of the '0' components exist on a PPoint2.
forceCPPoint2Basis :: CPPoint2 -> CPPoint2
forceCPPoint2Basis pt@(CPPoint2 pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3]))
  | gnum1 == fromList [GEZero 1, GEPlus 1] &&
    gnum2 == fromList [GEZero 1, GEPlus 2] &&
    gnum3 == fromList [GEPlus 1, GEPlus 2]    = pt
  | otherwise                                 = CPPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
forceCPPoint2Basis (CPPoint2 pvec)            = CPPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec

-- | Reverse a line. same line, but pointed in the other direction.
flipGVec :: GVec -> GVec
flipGVec (GVec vals) = rawRes
  where
    rawRes = GVec $ foldl' addValWithoutErr []
             [
               GVal (negate $ valOf 0 $ getVal [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 2] vals) (singleton (GEPlus 2))
             ]

eToPLine2WithErr :: LineSeg -> (PLine2, UlpSum)
eToPLine2WithErr l1 = pLineFromEndpointsWithErr (startPoint l1) (endPoint l1)

pLineFromEndpointsWithErr :: Point2 -> Point2 -> (PLine2, UlpSum)
pLineFromEndpointsWithErr (Point2 (x1,y1)) (Point2 (x2,y2)) = (PLine2 $ GVec $ foldl' addValWithoutErr [] [ GVal c (singleton (GEZero 1)), GVal a (singleton (GEPlus 1)), GVal b (singleton (GEPlus 2)) ], ulpTotal)
  where
    a=y2-y1
    b=x1-x2
    c=y1*x2-x1*y2
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ y1*x2)
               + abs (realToFrac $ doubleUlp $ x1*y2)
               + abs (realToFrac $ doubleUlp a)
               + abs (realToFrac $ doubleUlp b)
               + abs (realToFrac $ doubleUlp c)

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

-- | Get the sum of the error involved in storing the values in a given PPoint2.
ulpOfCPPoint2 :: CPPoint2 -> UlpSum
ulpOfCPPoint2 (CPPoint2 (GVec vals)) = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                       [getVal [GEZero 1, GEPlus 1] vals
                                       ,getVal [GEZero 1, GEPlus 2] vals
                                       ,getVal [GEPlus 1, GEPlus 2] vals]

---------------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalarWithErr. ----
---------------------------------------------------------------------

-- | find the idealized norm of a projective point (ideal or not).
idealNormPPoint2WithErr :: PPoint2 -> (ℝ, UlpSum)
idealNormPPoint2WithErr ppoint@(PPoint2 (GVec rawVals))
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
     | otherwise = (\(Point2 (x1,y1)) -> (x1,y1)) $ pToEPoint2 ppoint
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
canonicalizePPoint2WithErr :: PPoint2 -> (CPPoint2, UlpSum)
canonicalizePPoint2WithErr point@(PPoint2 (GVec rawVals))
  | isNothing foundVal = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = ((\(PPoint2 v) -> CPPoint2 v) point, ulpSum)
  | otherwise = (res, ulpSum)
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
    newVec = GVec $ addValWithoutErr [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])]
                                     (GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2]))
    (GVec scaledVals, _) = divVecScalarWithErr newVec $ valOf 1 foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] rawVals
    ulpSum = ulpOfCPPoint2 res

-- | Canonicalize the intersection resulting from two PLines.
-- NOTE: Returns nothing when the PLines are (anti)parallel.
canonicalizeIntersectionWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Maybe (CPPoint2, UlpSum)
canonicalizeIntersectionWithErr pl1 pl2
  | isNothing foundVal = Nothing
  | otherwise = Just (cpp1, ulpTotal)
  where
    (cpp1, UlpSum canonicalizationErr) = canonicalizePPoint2WithErr pp1
    (pp1, UlpSum intersectionErr) = pLineIntersectionWithErr pl1 pl2
    ulpTotal = UlpSum $ intersectionErr + canonicalizationErr
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) pp1

-- | Normalize a PLine2.
normalizePLine2WithErr :: (ProjectiveLine2 a) => a -> (NPLine2, UlpSum)
normalizePLine2WithErr line = (res, ulpTotal)
  where
    res = NPLine2 $ fst $ divVecScalarWithErr vec normOfMyPLine
    (normOfMyPLine, UlpSum normErr) = normOfPLine2WithErr line
    ulpTotal = UlpSum $ normErr + resErr
    (UlpSum resErr) = ulpOfPLine2 res
    vec = vecOfL line

-- | find the norm of a given PLine2
normOfPLine2WithErr :: (ProjectiveLine2 a) => a -> (ℝ, UlpSum)
normOfPLine2WithErr pline = (res, ulpTotal)
  where
    res = sqrt sqNormOfPLine2
    (sqNormOfPLine2, UlpSum sqNormErr) = sqNormOfPLine2WithErr pline
    ulpTotal = UlpSum $ abs (realToFrac $ doubleUlp res) + sqNormErr

-- | find the squared norm of a given PLine2
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
