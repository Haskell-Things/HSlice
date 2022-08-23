{- ORMOLU_DISABLE -}
{-
 - Copyright 2022 Julia Longtin
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
  PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2Err(PLine2Err),
  ProjectiveLine(PLine2, NPLine2),
  ProjectivePoint(PPoint2, CPPoint2),
  Arcable(hasArc, outOf, errOfOut),
  Pointable(canPoint, pPointOf, ePointOf),
  PPoint2Err(PPoint2Err),
  angleBetweenWithErr,
  combineConsecutiveLineSegs,
  distanceBetweenPPointsWithErr,
  distanceBetweenPLinesWithErr,
  distancePPointToPLineWithErr,
  errAtPPoint,
  eToPLine2WithErr,
  eToPPoint2,
  flipPLine2,
  intersectsWith,
  intersectsWithErr,
  join2PPointsWithErr,
  makePPoint2,
  normalizePLine2WithErr,
  outputIntersectsLineSeg,
  opposingDirection,
  pLineIntersectionWithErr,
  pLineIsLeft,
  pPointBetweenPPointsWithErr,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEPoint2,
  plinesIntersectIn,
  sameDirection,
  translatePLine2WithErr,
  translateRotatePPoint2WithErr,
  ulpOfLineSeg,
  ulpOfPLine2
  ) where

import Prelude (Eq((==),(/=)), Monoid(mempty), Semigroup, Show, Ord, ($), (*), (-), Bool(True, False), (&&), (<$>), not, null, otherwise, (>), (>=), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, show, error, sin, cos, realToFrac, filter, fst, sum, (.), realToFrac, signum)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right), isRight, fromRight)

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes, fromJust, isJust, isNothing, maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, makeLineSeg, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addVal, addVecPair, addVecPairWithErr, divVecScalarWithErr, eValOf, getVal, mulScalarVecWithErr, scalarPart, ulpVal, valOf)

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
  | IntersectsIn !ProjectivePoint !(PLine2Err, PLine2Err, PPoint2Err, UlpSum, UlpSum, UlpSum)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> PIntersection
plinesIntersectIn pline1@(pl1,pl1Err@(PLine2Err _ _ _ pl1TransErr _ )) pline2@(pl2,pl2Err@(PLine2Err _ _ _ pl2TransErr _))
  | isNothing canonicalizedIntersection
  || (idealNorm < realToFrac (ulpVal idnErr)
     && (sameDirection pl1 pl2 ||
         opposingDirection pl1 pl2)) = if sameDirection pl1 pl2
                                       then PCollinear
                                       else PAntiCollinear
  | sameDirection pl1 pl2            = if d < parallelFuzziness
                                       then PCollinear
                                       else PParallel
  | opposingDirection pl1 pl2        = if d < parallelFuzziness
                                       then PAntiCollinear
                                       else PAntiParallel
  | otherwise                        = IntersectsIn res (pl1Err, pl2Err, resErr, idnErr, mempty, mempty)
  where
    -- distance within which we  consider parallel lines as the same line.
    parallelFuzziness :: ℝ
    parallelFuzziness = realToFrac $ ulpVal $ (dErr <> pl1TransErr <> pl2TransErr)
    -- when we're close to parallel or antiparallel, use the distance between the lines to promote to colinear/anticolinear
    (d, (_, _, _, dErr)) = distanceBetweenPLinesWithErr pl1 pl2
    (idealNorm, idnErr) = idealNormPPoint2WithErr res
    (res, (_,_,resErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pline1 pline2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Maybe Bool
pLineIsLeft (pl1, pl1Err) (pl2, pl2Err)
  | abs res < realToFrac ulpTotal = Nothing
  | pl1 == pl2                    = Nothing
  | otherwise                     = Just $ res > 0
  where
    (res, _) = angleCos (npl1, pl1Err) (npl2, pl2Err)
    (npl1, (PLine2Err _ npl1Ulp _ _ _)) = case pl1 of
                                            l@(NPLine2 _) -> (l, mempty)
                                            l@(PLine2 _) -> normalizePLine2WithErr l
    (npl2, (PLine2Err _ npl2Ulp _ _ _)) = case pl2 of
                                            l@(NPLine2 _) -> (l, mempty)
                                            l@(PLine2 _) -> normalizePLine2WithErr l
    ulpTotal = ulpVal $ npl1Ulp <> npl2Ulp
    -- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
    angleCos :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> (ℝ, ([ErrVal], [ErrVal]))
    angleCos (pline1, pline1Err) (pline2, pline2Err)
      | isNothing canonicalizedIntersection = (0, mempty)
      | not (null motorErr) || not (null antiMotorErr) = error "impossible?"
      | otherwise = (angle, iPointErr)
      where
        angle = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
        (motor,motorErr)          = addVecPairWithErr (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
        (antiMotor,antiMotorErr)  = addVecPairWithErr (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
        canonicalizedIntersection = canonicalizeIntersectionWithErr (pline1, pline1Err) (pline2, pline2Err)
        -- safe, because we only accept normalized PLines.
        (CPPoint2 iPointVec,(_,_,(PPoint2Err iPointErr _))) = fromJust canonicalizedIntersection
        -- I, the infinite point.
        gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
        lvec1 = case pline1 of
                  (NPLine2 vec) -> vec
                  (PLine2 vec) -> vec
        lvec2 = case pline2 of
                  (NPLine2 vec) -> vec
                  (PLine2 vec) -> vec

-- | Find out where two lines intersect, returning a projective point, and the error quotents.
-- Note: this should only be used when you can guarantee these are not collinear, or parallel.
pLineIntersectionWithErr :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err) = (res, (pl1Err <> npl1Err, pl2Err <> npl2Err, resErr))
  where
    (res, (_,_,resErr)) = meet2PLine2WithErr npl1 npl2
    (npl1, npl1Err) = case pl1 of
                        l@(NPLine2 _) -> (l, mempty)
                        l@(PLine2 _) -> normalizePLine2WithErr l
    (npl2, npl2Err) = case pl2 of
                        l@(NPLine2 _) -> (l, mempty)
                        l@(PLine2 _) -> normalizePLine2WithErr l

-- | Generate a point between the two given points, where the weights given determine "how far between".
--   If the weights are equal, the distance will be right between the two points.
-- FIXME: how do we account for input error here?
pPointBetweenPPointsWithErr :: (ProjectivePoint,PPoint2Err) -> (ProjectivePoint, PPoint2Err) -> ℝ -> ℝ -> (ProjectivePoint, (PPoint2Err, PPoint2Err, [ErrVal], [ErrVal], [ErrVal], UlpSum))
pPointBetweenPPointsWithErr start stop weight1 weight2
  | isNothing foundVal = error "tried to generate an ideal point?"
  | otherwise = (res, resErr)
  where
    (rawStartPoint, cStartErr) = case start of
                                  (p@(PPoint2 _),_) -> (\(CPPoint2 v, e) -> (v,e)) $ canonicalizePPoint2WithErr p
                                  (CPPoint2 v,_) -> (v, mempty)
    (rawStopPoint, cStopErr) = case stop of
                                 (p@(PPoint2 _),_) -> (\(CPPoint2 v, e) -> (v,e)) $ canonicalizePPoint2WithErr p
                                 (CPPoint2 v,_) -> (v, mempty)
    resErr = (PPoint2Err mempty cStartErr, PPoint2Err mempty cStopErr, weighedStartErr, weighedStopErr, rawResErr, cResErr)
    foundVal = getVal [GEPlus 1, GEPlus 2] resVals
    (res@(CPPoint2 (GVec resVals)), cResErr) = canonicalizePPoint2WithErr $ PPoint2 rawRes
    (rawRes,rawResErr) = addVecPairWithErr weighedStart weighedStop
    (weighedStart, weighedStartErr) = mulScalarVecWithErr weight1 rawStartPoint
    (weighedStop, weighedStopErr) = mulScalarVecWithErr weight2 rawStopPoint

-- | Find the distance between a projective point and a projective line.
distancePPointToPLineWithErr :: (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> (ℝ, (PPoint2Err, PLine2Err, ([ErrVal],[ErrVal]), PPoint2Err, PLine2Err, PLine2Err, PLine2Err, UlpSum))
distancePPointToPLineWithErr (rawPoint,rawPointErr) (rawLine,rawLineErr)
  | isNothing foundVal = error "attempted to get the distance of an ideal point."
  | otherwise = (res, resErr)
  where
    -- FIXME: how does the error in newPLine effect the found norm here?
    (res, normErr)               = normOfPLine2WithErr newPLine
    resErr                       = (pointErr, lineErr, perpLineRawErr, lpErr <> lpcErr, lvErr, plErr, nplErr, normErr)
      where
        (lvErr, plErr, lpErr) = lastPointRawErr
        (_, lpcUlp, nplErr) = newPLineErr
        lpcErr = PPoint2Err mempty lpcUlp
    (newPLine, newPLineErr)      = join2PPointsWithErr point linePoint
    (linePoint, lastPointRawErr) = fromJust $ canonicalizeIntersectionWithErr (line, lineErr) (PLine2 perpLine, mempty)
    (perpLine, perpLineRawErr)   = lVec ⨅+ pVec
    foundVal = getVal [GEPlus 1, GEPlus 2] pVals
    line@(NPLine2 lVec) = forcePLine2Basis nline
    (nline, lineErr) = case rawLine of
                         (NPLine2 _) -> (rawLine, rawLineErr)
                         l@(PLine2 _) -> (\(a,b) -> (a, rawLineErr <> b)) $ normalizePLine2WithErr l
    point@(CPPoint2 pVec@(GVec pVals)) = forceProjectivePointBasis cpoint
    (cpoint, pointErr) = case rawPoint of
                           p@(CPPoint2 _) -> (p, rawPointErr)
                           p@(PPoint2 _) -> (\(a,b) -> (a, rawPointErr <> PPoint2Err mempty b)) $ canonicalizePPoint2WithErr p

-- | Determine if two points are on the same side of a given line.
-- Returns nothing if one of the points is on the line.
-- FIXME: accept input error amounts, take input error amounts into consideration.
pPointsOnSameSideOfPLine :: ProjectivePoint -> ProjectivePoint -> ProjectiveLine -> Maybe Bool
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
    (GVec unlikeP1, (unlikeP1AddErr, unlikeP1MulErr)) = pv1 ⎤+ lv1
    (GVec unlikeP2, (unlikeP2AddErr, unlikeP2MulErr)) = pv2 ⎤+ lv1
    (PPoint2 pv1) = forceProjectivePointBasis point1
    (PPoint2 pv2) = forceProjectivePointBasis point2
    rlv1 = forcePLine2Basis line
    lv1 =  case rlv1 of
      (PLine2 v) -> v
      (NPLine2 v) -> v

-- | Find the unsigned distance between two projective points.
-- FIXME: take the input error amounts into consideration.
distanceBetweenPPointsWithErr :: (ProjectivePoint,PPoint2Err) -> (ProjectivePoint,PPoint2Err) -> (ℝ, (PPoint2Err, PPoint2Err, PLine2Err, UlpSum))
distanceBetweenPPointsWithErr (point1,_) (point2,_) = (abs res, resErr)
  where
    resErr = (PPoint2Err mempty c1Ulp
             ,PPoint2Err mempty c2Ulp
             ,joinErr
             ,normUlp)
    (c1Ulp, c2Ulp, joinErr) = newPLineErr
    -- FIXME: how does the error in newPLine effect the found norm here?
    (res, normUlp)          = normOfPLine2WithErr newPLine
    (newPLine, newPLineErr) = join2PPointsWithErr point1 point2

-- | Find the unsigned distance between two parallel or antiparallel projective lines.
-- FIXME: accept input error amounts, take input error amounts into consideration.
distanceBetweenPLinesWithErr :: ProjectiveLine -> ProjectiveLine -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal]), UlpSum))
distanceBetweenPLinesWithErr pl1 pl2 = (res, resErr)
  where
    (res, idealErr) = idealNormPPoint2WithErr $ PPoint2 like
    resErr = (pv1Err, pv2Err, likeErr, idealErr)
    (like, likeErr) = p1 ⎣+ p2
    (PLine2 p1) = forcePLine2Basis $ PLine2 pv1
    (PLine2 p2) = forcePLine2Basis $ PLine2 pv2
    (pv1,pv1Err) = case pl1 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)
    (pv2,pv2Err) = case pl2 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)

-- | Return the sine of the angle between the two lines, along with the error.
-- Results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
angleBetweenWithErr :: ProjectiveLine -> ProjectiveLine -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal],[ErrVal]), UlpSum))
angleBetweenWithErr pl1 pl2 = (res, resErr)
  where
    (res, scalarErr) = (scalarPart like, (ulpVal $ eValOf mempty $ getVal [G0] likeAddErr) + (ulpVal $ eValOf mempty $ getVal [G0] likeMulErr))
    resErr = (pv1Err, pv2Err, (likeAddErr, likeMulErr), UlpSum scalarErr)
    (like, (likeAddErr, likeMulErr)) = p1 ⎣+ p2
    (PLine2 p1) = forcePLine2Basis $ PLine2 pv1
    (PLine2 p2) = forcePLine2Basis $ PLine2 pv2
    (pv1,pv1Err) = case pl1 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)
    (pv2,pv2Err) = case pl2 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
-- FIXME: precision on inputs?
sameDirection :: ProjectiveLine -> ProjectiveLine -> Bool
sameDirection a b = res  >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = 1.0 - 2 * realToFrac (ulpVal resErr)
    (res, (_,_,_,resErr)) = angleBetweenWithErr a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel.
-- FIXME: precision on inputs?
opposingDirection :: ProjectiveLine -> ProjectiveLine -> Bool
opposingDirection a b = res <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = (-1) + 2 * realToFrac (ulpVal resErr)
    (res, (_,_,_,resErr)) = angleBetweenWithErr a b

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: ProjectiveLine -> ProjectivePoint -> ℝ -> (ProjectivePoint, (([ErrVal],[ErrVal]), UlpSum))
pPointOnPerpWithErr pline rppoint d
  | not (null motorErr) = error "impossible!"
  | otherwise = (res, (perpPLineErr, ulpTotal))
  where
    res = case valOf 0 ( getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) resRaw) of
            1 -> CPPoint2 resRaw
            _ -> PPoint2 resRaw
    resRaw = motor•pvec•reverseGVec motor
    (perpLine,perpPLineErr) = lvec ⨅+ pvec
    (PLine2 lvec)           = forcePLine2Basis $ PLine2 rlvec
    (motor, motorErr)       = addVecPairWithErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac (realToFrac d / 2 :: Rounded 'TowardInf ℝ)
    ulpTotal = gaIErr <> lErr
    pvec = case forceProjectivePointBasis rppoint of
             (PPoint2 a) -> a
             (CPPoint2 a) -> a
    (NPLine2 rlvec,PLine2Err _ lErr _ _ _) = case pline of
                                               ln@(NPLine2 _) -> (ln, mempty)
                                               ln -> normalizePLine2WithErr ln

-- | Translate a line a given distance along it's perpendicular bisector.
-- Uses the property that translation of a line is expressed on the GEZero component.
translatePLine2WithErr :: ProjectiveLine -> ℝ -> (ProjectiveLine, PLine2Err)
translatePLine2WithErr pLine d = (PLine2 res, PLine2Err resErr mempty mempty (mErr <> m2Err <> nErr) mempty)
  where
    (res,resErr) = addVecPairWithErr m rawPLine
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    m2Err = UlpSum $ abs $ realToFrac $ doubleUlp $ tAdd + foundT
    mErr = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    -- the amount to add to GEZero 1 component.
    tAdd = d * n
    (n, nErr) = normOfPLine2WithErr pLine
    foundT = valOf 0 $ getVal [GEZero 1] $ (\(GVec vals) -> vals) rawPLine
    rawPLine = case pLine of
                 (PLine2 vec) -> vec
                 (NPLine2 vec) -> vec

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2WithErr :: ProjectivePoint -> ℝ -> ℝ -> (ProjectivePoint, ([ErrVal]))
translateRotatePPoint2WithErr ppoint d rotation = (PPoint2 res, (scaledPVecErr))
  where
    res = translator•pvec•reverseGVec translator
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        (PLine2 xLineVec) = forcePLine2Basis $ fst $ eToPLine2WithErr $ makeLineSeg (Point2 (0,0)) (Point2 (1,0))
    (PLine2 angledLineThroughPPoint2) = forcePLine2Basis $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
    rotator = addVecPair scaledPVec (GVec [GVal (cos $ rotation/2) (singleton G0)])
    (scaledPVec, scaledPVecErr) = mulScalarVecWithErr (sin $ rotation/2) pvec
    translator = addVecPair (angledLineThroughPPoint2 • gaIScaled) (GVec [GVal 1 (singleton G0)])
      where
        -- I, in this geometric algebra system. we multiply it times d/2, to reduce the number of multiples we have to do when creating the motor.
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    pvec = case ppoint of
             (PPoint2 vec) -> vec
             (CPPoint2 vec) -> vec

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
intersectsWith (Left l1)   (Left l2)   =         lineSegIntersectsLineSeg (l1, ulpOfLineSeg l1) (l2, ulpOfLineSeg l2)
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   pl1 pl2
intersectsWith (Left l1)   (Right pl1) =         pLineIntersectsLineSeg pl1 (l1, ulpOfLineSeg l1)
intersectsWith (Right pl1) (Left l1)   =         pLineIntersectsLineSeg pl1 (l1, ulpOfLineSeg l1)

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
outputIntersectsLineSeg :: (Show a, Arcable a) => a -> (LineSeg, UlpSum) -> Either Intersection PIntersection
outputIntersectsLineSeg source (l1, UlpSum l1Err)
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | otherwise = pLineIntersectsLineSeg (pl1, pl1Err) (l1, UlpSum l1Err)
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
type SegOrPLine2WithErr = Either (LineSeg, UlpSum) (ProjectiveLine, PLine2Err)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)    (Left l2)  =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right pl1) (Right pl2) = Right $ plinesIntersectIn pl1 pl2
intersectsWithErr (Left l1)   (Right pl1) =         pLineIntersectsLineSeg pl1 l1
intersectsWithErr (Right pl1) (Left l1)   =         pLineIntersectsLineSeg pl1 l1

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (ProjectiveLine, PLine2Err) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
pLineIntersectsLineSeg pl1@(_, pl1Err) (l1, _)
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | res == PCollinear = Right PCollinear
  | res == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < startFudgeFactor + endFudgeFactor = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstartFudgeFactor: " <> show startFudgeFactor <> "\nendFudgeFactor: " <> show endFudgeFactor <> "\n" <> "startDistanceErr: " <> show startDistanceErr <> "\nendDistanceErr: " <> show endDistanceErr <> "\n" <> "startErr:" <> show startErr <> "\n" <> "endErr: " <> show endErr <> "\n" <> "pl2: " <> show pl2 <> "\n"
  | hasIntersection && isNothing foundVal = error "intersection, but cannot cannonicalize."
  | hasIntersection && startDistance <= startFudgeFactor = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= endFudgeFactor = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, mempty, mempty, mempty, mempty)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, mempty, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (pl1Err, pl2Err, mempty, mempty)
  where
    res = plinesIntersectIn pl1 pl2
    startFudgeFactor = realToFrac (ulpVal startDistanceErr) + startErr
    startErr = errAtPPoint pl2 (startPoint l1) (endPoint l1) start
    endFudgeFactor = realToFrac (ulpVal endDistanceErr) + endErr
    endErr = errAtPPoint pl2 (startPoint l1) (endPoint l1) end
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
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, _) (l2, _)
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < start1FudgeFactor + end1FudgeFactor = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstart1FudgeFactor: " <> show start1FudgeFactor <> "\nrawIntersection" <> show rawIntersection
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < start2FudgeFactor + end2FudgeFactor = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nstart2FudgeFactor: " <> show start2FudgeFactor <> "\nrawIntersection" <> show rawIntersection
  | hasIntersection && res == PCollinear = Right PCollinear
  | hasIntersection && res == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && start1Distance <= start1FudgeFactor = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= end1FudgeFactor = Left $ HitEndPoint l1
  | hasIntersection && start2Distance <= start2FudgeFactor = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= end2FudgeFactor = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, mempty, mempty, mempty, mempty)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, mempty, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (pl1Err, pl2Err, mempty, mempty)
  where
    res = plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err)
    start1FudgeFactor = realToFrac (ulpVal start1DistanceErr) + errAtPPoint (pl1,pl1Err) (startPoint l1) (endPoint l1) start1
    end1FudgeFactor = realToFrac (ulpVal end1DistanceErr) + errAtPPoint (pl1,pl2Err) (startPoint l1) (endPoint l1) end1
    start2FudgeFactor = realToFrac (ulpVal start2DistanceErr) + errAtPPoint (pl2,pl1Err) (startPoint l2) (endPoint l2) start2
    end2FudgeFactor = realToFrac (ulpVal end2DistanceErr) + errAtPPoint (pl2,pl2Err) (startPoint l2) (endPoint l2) end2
    (start1Distance, (_,_,_, start1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start1,mempty)
    (start2Distance, (_,_,_, start2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start2,mempty)
    (end1Distance, (_,_,_, end1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end1,mempty)
    (end2Distance, (_,_,_, end2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end2,mempty)
    start1 = eToPPoint2 $ startPoint l1
    end1 = eToPPoint2 $ endPoint l1
    start2 = eToPPoint2 $ startPoint l2
    end2 = eToPPoint2 $ endPoint l2
    (pl1, pl1Err) = eToPLine2WithErr l1
    (pl2, pl2Err) = eToPLine2WithErr l2
    hasIntersection = hasRawIntersection && onSegment l1 (rawIntersection,rawIntersectionErr) && onSegment l2 (rawIntersection,rawIntersectionErr)
    hasRawIntersection = isJust foundVal
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr)) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (rawIntersect, _) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)

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
    (mid, (_, midErr, _, _, _, _)) = pPointBetweenPPointsWithErr (start,mempty) (end,mempty) 0.5 0.5
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac (ulpVal startDistanceErr) + errAtPPoint (eToPLine2WithErr ls) (startPoint ls) (endPoint ls) start
    midFudgeFactor = realToFrac (ulpVal midDistanceErr) + errAtPPoint (eToPLine2WithErr ls) (startPoint ls) (endPoint ls) mid
    endFudgeFactor = realToFrac (ulpVal endDistanceErr) + errAtPPoint (eToPLine2WithErr ls) (startPoint ls) (endPoint ls) end

-- | when given a PLine, and two points guaranteed to be on it, return the maximum distance between a given projective point on the PLine and the 'real' line.
-- FIXME: accept a error on the projectivePoint, and return an error estimate.
errAtPPoint :: (ProjectiveLine, PLine2Err) -> Point2 -> Point2 -> ProjectivePoint -> ℝ
errAtPPoint (inPLine, inPLineErr@(PLine2Err _ _ _ _ (pLineJoinAddErr, pLineJoinMulErr))) (Point2 (x1,y1)) (Point2 (x2,y2)) errPoint
  | x1 == x2 && y1 == y2 = error "0 length line segment."
  -- both intercepts are real. this line is not parallel or collinear to X or Y axises.
  | xInterceptIsRight && yInterceptIsRight && res2Axis < 0.0001 = res2Axis
  | xInterceptIsRight && yInterceptIsRight = error $ dumpInputs
                                             <> "res2Axis: " <> show res2Axis <> "\n"
                                             <> "interceptDistance: " <> show interceptDistance <> "\n"
                                             <> "xInterceptFuzz: " <> show xInterceptFuzz <> "\n"
                                             <> "yInterceptFuzz: " <> show yInterceptFuzz <> "\n"
  -- only the xIntercept is real. this line is parallel to the Y axis.
  | xInterceptIsRight = resYAxis
  -- only the yIntercept is real. this line is parallel to the X axis.
  | yInterceptIsRight = resXAxis
  | otherwise = errT
  where
    dumpInputs = "inPLine: " <> show inPLine <> "\n"
              <> "pline: " <> show pline <> "\n"
              <> "inPLineErr: " <> show inPLineErr <> "\n"
              <> "x1,y1: (" <> show x1 <> ", " <> show y1 <> ")\n"
              <> "x2,y2: (" <> show x2 <> ", " <> show y2 <> ")\n"
              <> "errPoint: " <> show errPoint <> "\n"
              <> "errX: " <> show errX <> "\n"
              <> "errY: " <> show errY <> "\n"
              <> "errT: " <> show errT <> "\n"
    -- this line is parallel to the X axis.
    resXAxis = errT +
               yInterceptFuzz
    -- this line is parallel to the Y axis.
    resYAxis = errT +
               xInterceptFuzz
    -- this line is not parallel to either axis.
    res2Axis = errT +
               case interceptDistance of
                 0 -> originFuzz
                 _ -> xInterceptFuzz + yInterceptFuzz
    originFuzz = ((errX + errY) * ( 1 + fst (distanceBetweenPPointsWithErr (origin, mempty) (errPoint, mempty))))
    origin = eToPPoint2 $ Point2 (0,0)
    interceptDistance = distance (Point2 (fromRight 0 $ fromJust $ xIntercept pline,0))
                                 (Point2 (0,fromRight 0 $ fromJust $ yIntercept pline))
    xInterceptIsRight = isJust (xIntercept pline) && isRight (fromJust $ xIntercept pline)
--    xInterceptAt = eToPPoint2 (Point2 (fromRight 0 (fromJust $ xIntercept pline),0))
    xInterceptFuzz = (errX * (1 + fst (distancePPointToPLineWithErr (errPoint,mempty) xOrigin)))
    xOrigin = (rawXOrigin, rawXOriginErr)
    (rawXOrigin, (_, _, rawXOriginErr)) = join2PPointsWithErr origin (eToPPoint2 $ Point2 (1,0))
    yInterceptIsRight = isJust (yIntercept pline) && isRight (fromJust $ yIntercept pline)
--    yInterceptAt = eToPPoint2 (Point2 (0, fromRight 0 (fromJust $ yIntercept pline)))
    yInterceptFuzz = (errY * (1 + fst (distancePPointToPLineWithErr (errPoint,mempty) yOrigin)))
    yOrigin = (rawYOrigin, rawYOriginErr)
    (rawYOrigin, (_, _, rawYOriginErr)) = join2PPointsWithErr origin (eToPPoint2 $ Point2 (0,1))
    (pline, _) = normalizePLine2WithErr inPLine
    errX, errY, errT :: ℝ
    errX = abs $ realToFrac $ ulpVal $ (eValOf mempty $ getVal [GEPlus 1] pLineJoinAddErr) <> (eValOf mempty $ getVal [GEPlus 1] pLineJoinMulErr)
    errY = abs $ realToFrac $ ulpVal $ (eValOf mempty $ getVal [GEPlus 2] pLineJoinAddErr) <> (eValOf mempty $ getVal [GEPlus 2] pLineJoinMulErr)
    errT = abs $ realToFrac $ ulpVal $ (eValOf mempty $ getVal [GEZero 1] pLineJoinAddErr) <> (eValOf mempty $ getVal [GEZero 1] pLineJoinMulErr)

-- | find the point that a given line crosses the X axis.
-- FIXME: return normErr
xIntercept :: ProjectiveLine -> Maybe (Either ProjectiveLine ℝ)
xIntercept inPLine
  -- handle a line that is parallel to the X axis.
  | isNothing rawX = Nothing
  -- use X and T to calculate our answer
  | isJust rawT = Just $ Right $ negate $ valOf 0 rawT / valOf 0 rawX
  -- is colinear with the X axis.
  | isNothing rawY = Just $ Left inPLine
  -- we have an X and a Y? this line passes through the origin.
  | isNothing rawT = Just $ Right $ 0
  | otherwise = error "we should never get here"
  where
    pLineVals = case inPLine of
                  (NPLine2 (GVec vals)) -> vals
                  pline@(PLine2 _) -> (\(PLine2 (GVec vals)) -> vals) $ fst $ normalizePLine2WithErr pline
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
  | isNothing rawT = Just $ Right $ 0
  | otherwise = error "we should never get here"
  where
    pLineVals = case inPLine of
                  (NPLine2 (GVec vals)) -> vals
                  pline@(PLine2 _) -> (\(PLine2 (GVec vals)) -> vals) $ fst $ normalizePLine2WithErr pline
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
  (==) p1 p2 = canonicalizePPoint2WithErr p1 == canonicalizePPoint2WithErr p2

-- | the error accumulated when calculating a projective point.
data PPoint2Err = PPoint2Err { _pPoint2MeetErr :: ([ErrVal], [ErrVal]), _pPoint2CannonicalizeErr :: UlpSum }
  deriving (Eq, Show)

-- FIXME: make ProjectiveLine type.
data ProjectiveLine =
  PLine2 GVec
  | NPLine2 GVec
  deriving (Generic, NFData, Show)

instance Eq ProjectiveLine where
  (==) (NPLine2 gvec1) (NPLine2 gvec2) = gvec1 == gvec2
  (==) pl1 pl2 = normalizePLine2WithErr pl1 == normalizePLine2WithErr pl2

-- | the two types of error of a projective line.
data PLine2Err = PLine2Err { _pLine2AddErr :: [ErrVal], _pLine2NormErr :: UlpSum, _pLine2AngleErr :: UlpSum, _pLine2TransErr :: UlpSum, _pLine2JoinErr :: ([ErrVal], [ErrVal]) }
  deriving (Eq, Show)

instance Semigroup PLine2Err where
  (<>) (PLine2Err a1 b1 c1 d1 e1) (PLine2Err a2 b2 c2 d2 e2) = PLine2Err (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2)

instance Monoid PLine2Err where
  mempty = PLine2Err mempty mempty mempty mempty mempty

instance Semigroup PPoint2Err where
  (<>) (PPoint2Err a1 b1) (PPoint2Err a2 b2) = PPoint2Err (a1 <> a2) (b1 <> b2)

instance Monoid PPoint2Err where
  mempty = PPoint2Err mempty mempty

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  canPoint :: a -> Bool
  pPointOf :: a -> ProjectivePoint
  ePointOf :: a -> Point2
--  ulpOfPoint :: a -> UlpSum

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> ProjectiveLine
  errOfOut :: a -> PLine2Err

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
(∨+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
(∨+) a b = (dual2DGVec res
           ,(dual2DErrs unlikeAddErr, dual2DErrs unlikeMulErr))
  where
    (res, (unlikeAddErr, unlikeMulErr)) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | a typed join function. join two points, returning a line.
join2PPointsWithErr :: ProjectivePoint -> ProjectivePoint -> (ProjectiveLine, (UlpSum, UlpSum, PLine2Err))
join2PPointsWithErr pp1 pp2 = (PLine2 res,
                               (pv1Ulp, pv2Ulp, PLine2Err mempty mempty mempty transErr resErr))
  where
    (res,resErr@(resAddErr,resMulErr))  = pv1 ∨+ pv2
    transErr = eValOf mempty (getVal [GEZero 1] resAddErr) <> eValOf mempty (getVal [GEZero 1] resMulErr)
    (CPPoint2 pv1) = forceProjectivePointBasis cp1
    (CPPoint2 pv2) = forceProjectivePointBasis cp2
    (cp1, pv1Ulp) = canonicalizePPoint2WithErr pp1
    (cp2, pv2Ulp) = canonicalizePPoint2WithErr pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2PLine2WithErr :: ProjectiveLine -> ProjectiveLine -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
meet2PLine2WithErr pl1 pl2 = (PPoint2 res,
                               (pl1Err,
                                pl2Err,
                                PPoint2Err resUlp mempty))
  where
    (res, resUlp) = pv1 ⎤+ pv2
    (PLine2 pv1) = forcePLine2Basis $ PLine2 plvec1
    (PLine2 pv2) = forcePLine2Basis $ PLine2 plvec2
    (plvec1,pl1Err) = case pl1 of
             (NPLine2 vec) -> (vec,mempty)
             pl@(PLine2 _) -> (\(PLine2 vec,err) -> (vec,err)) $ normalizePLine2WithErr pl
    (plvec2,pl2Err) = case pl2 of
             (NPLine2 vec) -> (vec,mempty)
             pl@(PLine2 _) -> (\(PLine2 vec,err) -> (vec,err)) $ normalizePLine2WithErr pl

eToPPoint2 :: Point2 -> ProjectivePoint
eToPPoint2 (Point2 (x,y)) = PPoint2 res
  where
    CPPoint2 res = makePPoint2 x y

-- | Create a canonical euclidian projective point from the given coordinates.
makePPoint2 :: ℝ -> ℝ -> ProjectivePoint
makePPoint2 x y = pPoint
  where
    pPoint = CPPoint2 $ GVec $ foldl' addVal [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]

-- | Create a euclidian point from a projective point.
pToEPoint2 :: ProjectivePoint -> Point2
pToEPoint2 ppoint
  | isNothing res = error "created an infinite point when trying to convert from a PPoint2 to a Point2"
  | otherwise = fromJust res
  where
    res = projectivePointToPoint2 ppoint

-- | Maybe create a euclidian point from a projective point.
-- FIXME: does negate cause a precision loss?
-- FIXME: canonicalization certainly does...
projectivePointToPoint2 :: ProjectivePoint -> Maybe Point2
projectivePointToPoint2 point
 | e12Val == 0 = Nothing
 | e12Val == 1 = Just $ Point2 (xVal, yVal)
 | otherwise = Just $ Point2 (xVal, yVal)
  where
    vals = case point of
             (CPPoint2 (GVec v)) -> v
             p@(PPoint2 _) -> (\(CPPoint2 (GVec v)) -> v) $ fst $ canonicalizePPoint2WithErr p
    rawVals = case point of
             (CPPoint2 (GVec v)) -> v
             (PPoint2 (GVec v)) -> v
    xVal = negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)

-- | Reverse a vector. Really, take every value in it, and recompute it in the reverse order of the vectors (so instead of e0∧e1, e1∧e0). which has the effect of negating bi and tri-vectors.
reverseGVec :: GVec -> GVec
reverseGVec vec@(GVec vals) = GVec $ foldl' addVal []
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

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DGVec :: GVec -> GVec
dual2DGVec gVec@(GVec vals) = GVec $ foldl' addVal []
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
    realVal     = scalarPart gVec

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DErrs :: [ErrVal] -> [ErrVal]
dual2DErrs vals = filter (\(ErrVal a _) -> a /= mempty)
                 [
                   ErrVal  realVal                                                (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]
  where
    realVal = foldl' (<>) mempty $ filter (\a -> a /= mempty) $ realValue <$> vals
      where
        realValue (ErrVal r gnums) = if gnums == singleton G0 then r else mempty

-- | perform basis coersion. ensure all of the required '0' components exist. required before using basis sensitive raw operators.
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVal (elems needs) has) needs

-- | runtime basis coersion. ensure all of the '0' components exist on a PLine2.
forcePLine2Basis :: ProjectiveLine -> ProjectiveLine
forcePLine2Basis ln
  | hasThreeMembers &&
    fromJust gnum1 == singleton (GEZero 1) &&
    fromJust gnum2 == singleton (GEPlus 1) &&
    fromJust gnum3 == singleton (GEPlus 2)    = ln
  | otherwise                        = case ln of
                                         (PLine2 _) -> PLine2 res
                                         (NPLine2 _) -> NPLine2 res
  where
    res = forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec
    (pvec, hasThreeMembers, gnum1, gnum2, gnum3) = case ln of
                                                     (NPLine2 p@(GVec [GVal _ g1, GVal _ g2, GVal _ g3])) -> (p,True, Just g1,Just g2, Just g3)
                                                     (PLine2 p@(GVec [GVal _ g1, GVal _ g2, GVal _ g3])) -> (p,True, Just g1,Just g2, Just g3)
                                                     (NPLine2 p) -> (p, False, Nothing, Nothing, Nothing)
                                                     (PLine2 p) -> (p, False, Nothing, Nothing, Nothing)

-- | runtime basis coersion. ensure all of the '0' components exist on a Projective Point.
forceProjectivePointBasis :: ProjectivePoint -> ProjectivePoint
forceProjectivePointBasis point
  | hasThreeMembers &&
    fromJust gnum1 == fromList [GEZero 1, GEPlus 1] &&
    fromJust gnum2 == fromList [GEZero 1, GEPlus 2] &&
    fromJust gnum3 == fromList [GEPlus 1, GEPlus 2]    = point
  | otherwise = case point of
                  (PPoint2 _)  -> PPoint2  res
                  (CPPoint2 _) -> CPPoint2 res
  where
    res = forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
    (pvec, hasThreeMembers, gnum1, gnum2, gnum3) = case point of
                                                     (CPPoint2 p@(GVec [GVal _ g1, GVal _ g2, GVal _ g3])) -> (p,True, Just g1,Just g2, Just g3)
                                                     (PPoint2 p@(GVec [GVal _ g1, GVal _ g2, GVal _ g3])) -> (p,True, Just g1,Just g2, Just g3)
                                                     (CPPoint2 p) -> (p, False, Nothing, Nothing, Nothing)
                                                     (PPoint2 p) -> (p, False, Nothing, Nothing, Nothing)

-- | Reverse a line. same line, but pointed in the other direction.
flipPLine2 :: ProjectiveLine -> ProjectiveLine
flipPLine2 pline = res
  where
    (vals,res) = case pline of
                   p@(PLine2 _) -> ((\(NPLine2 (GVec v),_) -> v) $ normalizePLine2WithErr p, NPLine2 rawRes)
                   (NPLine2 (GVec v)) -> (v, NPLine2 rawRes)
    rawRes = GVec $ foldl' addVal []
             [
               GVal (negate $ valOf 0 $ getVal [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 2] vals) (singleton (GEPlus 2))
             ]

eToPLine2WithErr :: LineSeg -> (ProjectiveLine, PLine2Err)
eToPLine2WithErr l1 = (res, resErr)
  where
    (res, (_, _, resErr)) = join2PPointsWithErr (eToPPoint2 $ startPoint l1) (eToPPoint2 $ endPoint l1)

-- | Get the sum of the error involved in storing the values in a given PLine2.
ulpOfPLine2 :: ProjectiveLine -> UlpSum
ulpOfPLine2 pline = res
  where
    res = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVal [GEZero 1] vals
                                   ,getVal [GEPlus 1] vals
                                   ,getVal [GEPlus 2] vals]
    vals = case pline of
             (PLine2 (GVec v)) -> v
             (NPLine2 (GVec v)) -> v

-- | Get the sum of the error involved in storing the values in a given Line Segment.
ulpOfLineSeg :: LineSeg -> UlpSum
ulpOfLineSeg (LineSeg (Point2 (x1,y1)) (Point2 (x2,y2))) = UlpSum $ sum $ abs . realToFrac . doubleUlp <$> [x1, y1, x2, y2]

-- | Get the sum of the error involved in storing the values in a given PPoint2.
ulpOfPPoint2 :: ProjectivePoint -> UlpSum
ulpOfPPoint2 point = res
  where
    res = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
          [getVal [GEZero 1, GEPlus 1] vals
          ,getVal [GEZero 1, GEPlus 2] vals
          ,getVal [GEPlus 1, GEPlus 2] vals]
    vals = case point of
             (PPoint2 (GVec vs)) -> vs
             (CPPoint2 (GVec vs)) -> vs

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalarWithErr. ----
--------------------------------------------------------------

-- | find the idealized norm of a projective point (ideal or not).
idealNormPPoint2WithErr :: ProjectivePoint -> (ℝ, UlpSum)
idealNormPPoint2WithErr point
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
     | e12Val == 0 = ( negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
                     ,          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)
     | otherwise = (\(Point2 (x1,y1)) -> (x1,y1)) $ pToEPoint2 point
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] vals)
    vals = case point of
             (PPoint2 (GVec vs)) -> vs
             (CPPoint2 (GVec vs)) -> vs

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- FIXME: return the error of divVecScalarWithErr
canonicalizePPoint2WithErr :: ProjectivePoint -> (ProjectivePoint, UlpSum)
canonicalizePPoint2WithErr point
  | isNothing foundVal = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = (CPPoint2 (GVec rawVals), mempty)
  | otherwise = (res, ulpSum)
  where
    res = CPPoint2 $ GVec $ foldl' addVal []
          $  ( if isNothing (getVal [GEZero 1, GEPlus 1] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
             )
          <> ( if isNothing (getVal [GEZero 1, GEPlus 2] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
             )
          <> [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    newVec = GVec $ [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])
                    , GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2])]
    (GVec scaledVals, _) = divVecScalarWithErr newVec $ valOf 1 foundVal
    rawVals = case point of
                (PPoint2 (GVec v)) -> v
                (CPPoint2 (GVec v)) -> v
    foundVal = getVal [GEPlus 1, GEPlus 2] rawVals
    ulpSum = if valOf 0 foundVal < 1 && valOf 0 foundVal > -1
             then ulpOfPPoint2 res
             else ulpOfPPoint2 point

-- | Canonicalize the intersection resulting from two PLines.
-- NOTE: Returns nothing when the PLines are (anti)parallel.
canonicalizeIntersectionWithErr :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Maybe (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
  | isNothing foundVal = Nothing
  | otherwise = Just (cpp1, (pl1ResErr, pl2ResErr, PPoint2Err intersectionUnlikeErr canonicalizationErr))
  where
    (cpp1, canonicalizationErr) = canonicalizePPoint2WithErr pp1
    (pp1, (pl1ResErr, pl2ResErr, PPoint2Err intersectionUnlikeErr _)) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) pp1

-- | Normalize a Projective Line.
-- FIXME: return the error of divVecScalarWithErr
normalizePLine2WithErr :: ProjectiveLine -> (ProjectiveLine, PLine2Err)
normalizePLine2WithErr pl = (res, resErr)
  where
    (res, resErr) = case norm of
            1.0 -> (NPLine2 vec, mempty)
            _ -> (NPLine2 scaledVec, PLine2Err mempty normErr mempty mempty mempty)
    (scaledVec, _) = divVecScalarWithErr vec norm
    (norm, normErr) = normOfPLine2WithErr pl
    vec = case pl of
            (PLine2 v) -> v
            (NPLine2 v) -> v

-- | find the norm of a given PLine2
normOfPLine2WithErr :: ProjectiveLine -> (ℝ, UlpSum)
normOfPLine2WithErr pline = (res, resErr)
  where
    (res, resErr) = case sqNormOfPLine2 of
                      1.0 -> (1.0,mempty)
                      _ -> (sqrt sqNormOfPLine2, ulpTotal)
    (sqNormOfPLine2, sqNormErr) = sqNormOfPLine2WithErr pline
    ulpTotal = UlpSum (abs $ realToFrac $ doubleUlp res) <> sqNormErr

-- | find the squared norm of a given PLine2
sqNormOfPLine2WithErr :: ProjectiveLine -> (ℝ, UlpSum)
sqNormOfPLine2WithErr pl = (res, resErr)
  where
    (res, resErr) = case rawRes of
                      1.0 -> (1.0, mempty)
                      _ -> (rawRes, ulpTotal)
    rawRes = a*a+b*b
    a = valOf 0 $ getVal [GEPlus 1] vals
    b = valOf 0 $ getVal [GEPlus 2] vals
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ a*a)
               + abs (realToFrac $ doubleUlp $ b*b)
               + abs (realToFrac $ doubleUlp res)
    vals = case pl of
             (PLine2 (GVec v)) -> v
             (NPLine2 (GVec v)) -> v
