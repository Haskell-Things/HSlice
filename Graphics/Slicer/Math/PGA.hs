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
  pLineFromEndpointsWithErr,
  pLineIntersectionWithErr,
  pLineIsLeft,
  pPointBetweenPPointsWithErr,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEPoint2,
  plinesIntersectIn,
  sameDirection,
  translatePLine2WithErr,
  translateRotatePPoint2,
  ulpOfLineSeg,
  ulpOfPLine2
  ) where

import Prelude (Eq((==),(/=)), Monoid(mempty), Semigroup, Show, Ord, ($), (*), (-), Bool(True, False), (&&), (<$>), otherwise, (>), (>=), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, show, error, sin, cos, realToFrac, fst, sum, (.), realToFrac, signum)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes, fromJust, isNothing, maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, scalePoint, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addVal, addVecPairWithErr, divVecScalar, getVals, mulScalarVec, scalarPart, ulpVal, valOf, vectorPart)

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
  || (idealNorm < realToFrac idnErr
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
  | otherwise                        = IntersectsIn res (pl1Err <> npl1Err, pl2Err <> npl2Err, intersectErr, iaErr, UlpSum idnErr, mempty)
  where
    -- distance within which we  consider parallel lines as the same line.
    parallelFuzziness :: ℝ
    parallelFuzziness = realToFrac $ ulpVal $ (d1Err <> d2Err <> pl1TransErr <> pl2TransErr)
    -- when we're close to parallel or antiparallel, use the distance between the lines to promote to colinear/anticolinear
    (d, (_, _, d1Err, d2Err)) = distanceBetweenPLinesWithErr pl1 pl2
    (idealNorm, UlpSum idnErr) = idealNormPPoint2WithErr intersectPoint
    -- FIXME: how much do the potential normalization errors have an effect on the resultant angle?
    (intersectAngle, (npl1Err, npl2Err, iaErr)) = angleBetweenWithErr pl1 pl2
    (intersectPoint, (intersectErr, _, _)) = pLineIntersectionWithErr pline1 pline2
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (res, resUlp) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pline1 pline2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> Maybe Bool
pLineIsLeft (pl1, pl1Err) (pl2, pl2Err)
  | abs res < realToFrac ulpTotal = Nothing
  | pl1 == pl2                    = Nothing
  | otherwise                     = Just $ res > 0
  where
    (res, UlpSum resErr) = angleCos (npl1, pl1Err) (npl2, pl2Err)
    (npl1, (PLine2Err _ (UlpSum npl1Ulp) _ _ _)) = case pl1 of
                                                     l@(NPLine2 _) -> (l, mempty)
                                                     l@(PLine2 _) -> normalizePLine2WithErr l
    (npl2, (PLine2Err _ (UlpSum npl2Ulp) _ _ _)) = case pl2 of
                                                     l@(NPLine2 _) -> (l, mempty)
                                                     l@(PLine2 _) -> normalizePLine2WithErr l
    -- FIXME: combines normalization error and ... ?
    ulpTotal = npl1Ulp + npl2Ulp + resErr
    -- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
    angleCos :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> (ℝ, UlpSum)
    angleCos (NPLine2 lvec1, lv1Err) (NPLine2 lvec2, lv2Err)
      | isNothing canonicalizedIntersection = (0, mempty)
      | otherwise = (angle, iPointErr)
      where
        angle = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
        (motor,motorErr)          = addVecPairWithErr (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
        (antiMotor,antimotorErr)  = addVecPairWithErr (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
        canonicalizedIntersection = canonicalizeIntersectionWithErr (pline1, lv1Err) (pline2, lv2Err)
        -- safe, because we only accept normalized PLines.
        (CPPoint2 iPointVec,(_,_,(PPoint2Err iPointErr _))) = fromJust canonicalizedIntersection
        -- I, the infinite point.
        gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
        pline1 = PLine2 lvec1
        pline2 = PLine2 lvec2

-- | Find out where two lines intersect, returning a projective point, and the error quotents.
-- Note: this should only be used when you can guarantee these are not collinear, or parallel.
pLineIntersectionWithErr :: (ProjectiveLine, PLine2Err) -> (ProjectiveLine, PLine2Err) -> (ProjectivePoint, (PPoint2Err, PLine2Err, PLine2Err))
pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err) = (res, (resErr, pl1Err <> npl1Err, pl2Err <> npl2Err))
  where
    (res, resErr) = meet2PLine2WithErr npl1 npl2
    (npl1, npl1Err) = case pl1 of
                        l@(NPLine2 _) -> (l, mempty)
                        l@(PLine2 _) -> normalizePLine2WithErr l
    (npl2, npl2Err) = case pl2 of
                        l@(NPLine2 _) -> (l, mempty)
                        l@(PLine2 _) -> normalizePLine2WithErr l

-- | Generate a point between the two given points, where the weights given determine "how far between".
--   If the weights are equal, the distance will be right between the two points.
-- FIXME: how do we account for input error here?
pPointBetweenPPointsWithErr :: ProjectivePoint -> ProjectivePoint -> ℝ -> ℝ -> (ProjectivePoint, (PPoint2Err, PPoint2Err, UlpSum, UlpSum))
pPointBetweenPPointsWithErr start stop weight1 weight2 
  | valOf 0 foundVal == 0 = error "tried to generate an ideal point?"
  | otherwise = (res, resErr)
  where
    (rawStartPoint, cStartErr) = case start of
                                  p@(PPoint2 _) -> (\(CPPoint2 v, e) -> (v,e)) $ canonicalizePPoint2WithErr p
                                  (CPPoint2 v) -> (v, mempty)
    (rawStopPoint, cStopErr) = case stop of
                                 p@(PPoint2 _) -> (\(CPPoint2 v, e) -> (v,e)) $ canonicalizePPoint2WithErr p
                                 (CPPoint2 v) -> (v, mempty)
    resErr = (PPoint2Err mempty cStartErr, PPoint2Err mempty cStopErr, rawResErr, cResErr)
    (res, cResErr) = canonicalizePPoint2WithErr $ PPoint2 rawRes
    (rawRes,rawResErr) = addVecPairWithErr (mulScalarVec weight1 rawStartPoint) (mulScalarVec weight2 rawStopPoint)
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(CPPoint2 (GVec vals)) -> vals) res

-- | Find the distance between a projective point and a projective line.
-- FIXME: use the distance to increase ULP appropriately?
-- FIXME: error attribution here is a mess.
distancePPointToPLineWithErr :: (ProjectivePoint, PPoint2Err) -> (ProjectiveLine, PLine2Err) -> (ℝ, (PPoint2Err, PLine2Err, PPoint2Err, PLine2Err, PLine2Err, PLine2Err, UlpSum))
distancePPointToPLineWithErr (rawPoint,rawPointErr) (rawLine,rawLineErr)
  | valOf 0 foundVal == 0 = error "attempted to get the distance of an ideal point."
  | otherwise = (res, resErr)
  where
    (res, normErr)               = normOfPLine2WithErr newPLine
    (newPLine, newPLineErr)      = join2PPointsWithErr point linePoint
    (perpLine, perpLineRawErr)     = lVec ⨅+ pVec
    (linePoint, lastPointRawErr) = fromJust $ canonicalizeIntersectionWithErr (line, lineErr) (PLine2 perpLine, perpLineInErr)
      where
        perpLineInErr                  = PLine2Err perpLineRawErr mempty mempty mempty mempty
    resErr                      = (pcErr, lineErr, lpErr <> lpcErr, lvErr, plErr, nplErr, normErr)
      where
        (lvErr, plErr, lpErr) = lastPointRawErr
        (pcUlp, lpcUlp, nplErr) = newPLineErr
        pcErr = PPoint2Err mempty pcUlp
        lpcErr = PPoint2Err mempty lpcUlp
    foundVal                       = getVals [GEPlus 1, GEPlus 2] pVals
    point@(CPPoint2 pVec@(GVec pVals)) = forceProjectivePointBasis cpoint
    (cpoint, pointErr) = case rawPoint of
                           p@(CPPoint2 _) -> (p, rawPointErr)
                           p@(PPoint2 _) -> (\(a,b) -> (a, rawPointErr <> PPoint2Err mempty b)) $ canonicalizePPoint2WithErr p
    line@(NPLine2 lVec) = forcePLine2Basis nline
    (nline, lineErr) = case rawLine of
                         l@(NPLine2 _) -> (rawLine, rawLineErr)
                         l@(PLine2 _) -> (\(a,b) -> (a, rawLineErr <> b)) $ normalizePLine2WithErr l

-- | Determine if two points are on the same side of a given line.
-- Returns nothing if one of the points is on the line.
pPointsOnSameSideOfPLine :: ProjectivePoint -> ProjectivePoint -> ProjectiveLine -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  |  abs foundP1 < realToFrac unlikeP1Err ||
     abs foundP2 < realToFrac unlikeP2Err    = Nothing
    | otherwise = Just $ signum foundP1 == signum foundP2
  where
    foundP1 = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1
    foundP2 = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2
    (GVec unlikeP1, UlpSum unlikeP1Err) = pv1 ⎤+ lv1
    (GVec unlikeP2, UlpSum unlikeP2Err) = pv2 ⎤+ lv1
    (PPoint2 pv1) = forceProjectivePointBasis point1
    (PPoint2 pv2) = forceProjectivePointBasis point2
    rlv1 = forcePLine2Basis line
    lv1 =  case rlv1 of
      (PLine2 v) -> v
      (NPLine2 v) -> v

-- | Find the unsigned distance between two projective points.
distanceBetweenPPointsWithErr :: (ProjectivePoint,PPoint2Err) -> (ProjectivePoint,PPoint2Err) -> (ℝ, (PPoint2Err, PPoint2Err, PLine2Err, UlpSum))
distanceBetweenPPointsWithErr (point1,point1Err) (point2,point2Err) = (res, resErr)
  where
    resErr = (PPoint2Err mempty c1Ulp
             ,PPoint2Err mempty c2Ulp
             ,joinErr
             ,normUlp)
    (c1Ulp, c2Ulp, joinErr) = newPLineErr
    (res, normUlp)           = normOfPLine2WithErr newPLine
    (newPLine, newPLineErr) = join2PPointsWithErr point1 point2

-- | Find the unsigned distance between two parallel or antiparallel projective lines.
distanceBetweenPLinesWithErr :: ProjectiveLine -> ProjectiveLine -> (ℝ, (PLine2Err, PLine2Err, UlpSum, UlpSum))
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
-- PROPERTY: any two parallel lines in the same direction have res >= 1.0-2*likeErr
angleBetweenWithErr :: ProjectiveLine -> ProjectiveLine -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
angleBetweenWithErr pl1 pl2 = (scalarPart res, resErr)
  where
    (res, likeErr) = p1 ⎣+ p2
    resErr = (pv1Err, pv2Err, likeErr)
    (PLine2 p1) = forcePLine2Basis $ PLine2 pv1
    (PLine2 p2) = forcePLine2Basis $ PLine2 pv2
    (pv1,pv1Err) = case pl1 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)
    (pv2,pv2Err) = case pl2 of
                     p@(PLine2 _) -> (\(NPLine2 a, b) -> (a,b)) $ normalizePLine2WithErr p
                     (NPLine2 v) -> (v, mempty)

-- | A checker, to ensure two Projective Lines are going the same direction, and are parallel.
sameDirection :: ProjectiveLine -> ProjectiveLine -> Bool
sameDirection a b = res  >= maxAngle
  where
    -- ceiling value. a value bigger than maxAngle is considered to be going the same direction.
    maxAngle :: ℝ
    maxAngle = realToFrac $ 1.0 - 2*ulpVal resErr
    (res, (_,_,resErr)) = angleBetweenWithErr a b

-- | A checker, to ensure two Projective Lines are going the opposite direction, and are parallel..
opposingDirection :: ProjectiveLine -> ProjectiveLine -> Bool
opposingDirection a b = res  <= minAngle
  where
    -- floor value. a value smaller than minAngle is considered to be going the opposite direction.
    minAngle :: ℝ
    minAngle = realToFrac (-1 + 2*ulpVal resErr)
    (res, (_,_,resErr)) = angleBetweenWithErr a b

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: ProjectiveLine -> ProjectivePoint -> ℝ -> (ProjectivePoint, UlpSum)
pPointOnPerpWithErr pline rppoint d = (res,
                                        ulpTotal)
  where
    res = case valOf 0 ( getVals [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) resRaw) of
            1 -> CPPoint2 resRaw
            _ -> PPoint2 resRaw
    resRaw = motor•pvec•reverseGVec motor
    (perpLine,UlpSum perpPLineErr) = lvec ⨅+ pvec
    (PLine2 lvec)                  = forcePLine2Basis $ PLine2 rlvec
    (motor, UlpSum motorErr)       = addVecPairWithErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr :: Rounded 'TowardInf ℝ
    gaIErr = abs $ realToFrac $ doubleUlp $ d/2
    ulpTotal = UlpSum $ gaIErr + perpPLineErr + lErr + motorErr
    pvec = case forceProjectivePointBasis rppoint of
             (PPoint2 a) -> a
             (CPPoint2 a) -> a
    (NPLine2 rlvec,PLine2Err _ (UlpSum lErr) _ _ _) = case pline of
                                                        ln@(NPLine2 _) -> (ln, mempty)
                                                        ln -> normalizePLine2WithErr ln

-- | Translate a line a given distance along it's perpendicular bisector.
-- Uses the property that translation of a line is expressed on the GEZero component.
translatePLine2WithErr :: ProjectiveLine -> ℝ -> (ProjectiveLine, PLine2Err)
translatePLine2WithErr pLine@(PLine2 rawPLine) d = (PLine2 res, PLine2Err mempty mempty mempty (mErr <> m2Err <> nErr) mempty)
  where
    (res,resErr) = addVecPairWithErr m rawPLine
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    m2Err, mErr :: UlpSum
    m2Err = UlpSum $ abs $ realToFrac $ doubleUlp $ tAdd + foundT
    mErr = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    -- the amount to add to GEZero 1 component.
    tAdd = d * n
    (n, nErr) = normOfPLine2WithErr pLine
    foundT = valOf 0 $ getVals [GEZero 1] $ (\(GVec vals) -> vals) rawPLine

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: ProjectivePoint -> ℝ -> ℝ -> ProjectivePoint
translateRotatePPoint2 ppoint d rotation = PPoint2 $ translator•pvec•reverseGVec translator
  where
    (PPoint2 pvec)      = ppoint
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        (PLine2 xLineVec) = forcePLine2Basis $ fst $ pLineFromEndpointsWithErr (Point2 (0,0)) (Point2 (1,0))
    (PLine2 angledLineThroughPPoint2) = forcePLine2Basis $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
      where
        (rotator, rotatorErr) = addVecPairWithErr (mulScalarVec (sin $ rotation/2) pvec) (GVec [GVal (cos $ rotation/2) (singleton G0)])
    (translator, translatorErr) = addVecPairWithErr (angledLineThroughPPoint2 • gaIScaled) (GVec [GVal 1 (singleton G0)])
      where
        -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

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
type SegOrProjectiveLine = Either LineSeg ProjectiveLine

-- FIXME: as long as this is required, we're not accounting for ULP correctly everywhere.
ulpMultiplier :: Rounded 'TowardInf ℝ
ulpMultiplier = 570

-- | Check if/where lines/line segments intersect.
-- entry point usable for all intersection needs.
-- FIXME: take UlpSums here.
intersectsWith :: SegOrProjectiveLine -> SegOrProjectiveLine -> Either Intersection PIntersection
intersectsWith (Left l1)   (Left l2)   =         lineSegIntersectsLineSeg (l1, ulpOfLineSeg l1) (l2, ulpOfLineSeg l2)
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   (pl1, PLine2Err (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) mempty) (pl2, PLine2Err (ulpOfPLine2 pl2) (ulpOfPLine2 pl2) (ulpOfPLine2 pl2) (ulpOfPLine2 pl2) mempty) 
intersectsWith (Left l1)   (Right pl1) =         pLineIntersectsLineSeg (pl1, PLine2Err (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) mempty) (l1, ulpOfLineSeg l1) 0
intersectsWith (Right pl1) (Left l1)   =         pLineIntersectsLineSeg (pl1, PLine2Err (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) (ulpOfPLine2 pl1) mempty) (l1, ulpOfLineSeg l1) 0

-- | Check if/where the arc of a motorcycle, inode, or enode intersect a line segment.
outputIntersectsLineSeg :: (Show a, Arcable a, Pointable a) => a -> (LineSeg, UlpSum) -> Either Intersection PIntersection
outputIntersectsLineSeg source (l1, UlpSum l1Err)
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn (pl1, pl1Err) (pl2, pl2Err)
  | intersectionDistance < foundError = pLineIntersectsLineSeg (pl1, pl1Err) (l1, UlpSum l1Err) 1
  | ulpScale > 100000 = error
                        $ "wtf\n"
                        <> "ulpScale: " <> show ulpScale <> "\n"
                        <> "ulpMultiplier: " <> show ulpMultiplier <> "\n"
                        <> "angle: " <> show angle <> "\n"
                        <> "intersectionDistance: " <> show intersectionDistance <> "\n"
                        <> show source <> "\n"
                        <> show pl2 <> "\n"
                        <> show l1 <> "\n"
                        <> show foundError <> "\n"
  | otherwise = pLineIntersectsLineSeg (pl1, pl1Err) (l1, UlpSum l1Err) ulpScale
  where
    foundError :: ℝ
    foundError = realToFrac $ l1Err + rawPl1TranslateErr + rawPl2TranslateErr + npl1Err + npl2Err + intersectionAddErr + intersectionDistanceUlp + sourceCUlp
    -- | the multiplier used to expand the hitcircle of an endpoint.
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier *  abs (realToFrac angle)
    (angle, _) = angleBetweenWithErr npl1 npl2
    (npl1, PLine2Err _ (UlpSum npl1Err) _ _ _) = normalizePLine2WithErr pl1
    (npl2, PLine2Err _ (UlpSum npl2Err) _ _ _) = normalizePLine2WithErr pl2
    (pl2, pl2Err@(PLine2Err _ _ _ (UlpSum rawPl2TranslateErr) _)) = eToPLine2WithErr l1
    -- the multiplier to account for distance between our Pointable, and where it intersects.
    (pl1, pl1Err@(PLine2Err _ _ _ (UlpSum rawPl1TranslateErr) _))
      | hasArc source = (outOf source, errOfOut source)
      | otherwise = error
                    $ "no arc from source?\n"
                    <> show source <> "\n"
    (rawIntersection, (_, _, rawIntersectionErr@(PPoint2Err (UlpSum intersectionAddErr) _))) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (intersectionDistance, intersectionDistanceErr) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (pPointOf source,mempty)
    (PPoint2Err _ (UlpSum sourceCUlp), _, _, UlpSum intersectionDistanceUlp) = intersectionDistanceErr

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2WithErr = Either (LineSeg, UlpSum) (ProjectiveLine, PLine2Err)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)           (Left l2)                 =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right (pl1,pl1Err))     (Right (pl2,pl2Err)) = Right $ plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err)
intersectsWithErr (Left l1@(rawL1,_)) (Right pl1@(rawPl1,_))    =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle))
    (angle, _) = angleBetweenWithErr pl2 rawPl1
    (pl2, pl2Err) = eToPLine2WithErr rawL1
intersectsWithErr (Right pl1@(rawPl1,_))     (Left l1@(rawL1,_))       =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (abs (realToFrac angle))
    (angle, _) = angleBetweenWithErr rawPl1 pl2
    (pl2, pl2Err) = eToPLine2WithErr rawL1

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (ProjectiveLine, PLine2Err) -> (LineSeg, UlpSum) -> ℝ -> Either Intersection PIntersection
pLineIntersectsLineSeg (pl1, pl1Err@(PLine2Err _ _ pl1AngleErr rawPl1TranslateErr _)) (l1, l1Err) ulpScale
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | res == PCollinear = Right PCollinear
  | res == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (ulpVal $ startDistanceErr <> endDistanceErr <> ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nulpScale: " <> show ulpScale <> "\n"<> dumpULPs
  | hasIntersection && valOf 0 foundVal == 0 = error "intersection, but cannot cannonicalize."
  | hasIntersection && startDistance <= realToFrac (ulpVal ulpStartSum) = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= realToFrac (ulpVal ulpEndSum) = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, mempty, rawPl2TranslateErr, intersectionAddErr, mempty)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, mempty, mempty)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (pl1Err, pl2Err, mempty, mempty)
  where
    res = plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err)
    (startDistance, (_,_,_, startDistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start,mempty)
    (endDistance, (_,_,_, endDistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end,mempty)
    start = eToPPoint2 $ startPoint l1
    end = eToPPoint2 $ endPoint l1
    ulpStartSum = ulpTotal <> startDistanceErr
    ulpEndSum = ulpTotal <> endDistanceErr
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal = rawPl1TranslateErr <> rawPl2TranslateErr <> l1Err <> UlpSum (realToFrac $ realToFrac (ulpVal intersectionAddErr) * ulpScale)
    dumpULPs =  "pl1AngleErr: " <> show pl1AngleErr <> "\n"
             <> "rawPl1TranslateErr: " <> show rawPl1TranslateErr <> "\n"
             <> "pl2AngleErr: " <> show pl2AngleErr <> "\n"
             <> "rawPl2TranslateErr: " <> show rawPl2TranslateErr <> "\n"
             <> "l1Err: " <> show l1Err <> "\n"
             <> "intersectionAddErr: " <> show intersectionAddErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum ulpEndSum
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr@(PPoint2Err intersectionAddErr _))) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (rawIntersect, (PPoint2Err rawIntersectErr _,_,_)) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (pl2, pl2Err@(PLine2Err _ _ pl2AngleErr rawPl2TranslateErr _)) = eToPLine2WithErr l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, l1Err) (l2, ulpL2)
  | res == PParallel = Right PParallel
  | res == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (ulpVal $ start1DistanceErr <> end1DistanceErr <> ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < realToFrac (ulpVal $ start2DistanceErr <> end2DistanceErr <> ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && res == PCollinear = Right PCollinear
  | hasIntersection && res == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && start1Distance <= realToFrac (ulpVal ulpStartSum1) = Left $ HitStartPoint l1
  | hasIntersection && end1Distance <= realToFrac (ulpVal ulpEndSum1) = Left $ HitEndPoint l1
  | hasIntersection && start2Distance <= realToFrac (ulpVal ulpStartSum2) = Left $ HitStartPoint l2
  | hasIntersection && end2Distance <= realToFrac (ulpVal ulpEndSum2) = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (pl1Err, pl2Err, mempty, ulpStartSum2, ulpEndSum2, intersectionAddErr)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (pl1Err, pl2Err, mempty, ulpStartSum2)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (pl1Err, pl2Err, mempty, mempty)
  where
    res = plinesIntersectIn (pl1,pl1Err) (pl2,pl2Err)
    ulpStartSum1, ulpEndSum1, ulpStartSum2, ulpEndSum2 :: UlpSum
    ulpStartSum1 = ulpTotal <> start1DistanceErr
    ulpStartSum2 = ulpTotal <> start2DistanceErr
    ulpEndSum1 = ulpTotal
    ulpEndSum2 = ulpTotal
    (start1Distance, (_,_,_, start1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start1,mempty)
    (start2Distance, (_,_,_, start2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (start2,mempty)
    (end1Distance, (_,_,_, end1DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end1,mempty)
    (end2Distance, (_,_,_, end2DistanceErr)) = distanceBetweenPPointsWithErr (rawIntersection, rawIntersectionErr) (end2,mempty)
    start1 = eToPPoint2 $ startPoint l1
    end1 = eToPPoint2 $ endPoint l1
    start2 = eToPPoint2 $ startPoint l2
    end2 = eToPPoint2 $ endPoint l2
    (pl1, pl1Err@(PLine2Err _ _ pl1AngleErr pl1TranslateErr _)) = eToPLine2WithErr l1
    (pl2, pl2Err@(PLine2Err _ _ pl2AngleErr pl2TranslateErr _)) = eToPLine2WithErr l2
    -- | the sum of all untyped ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal = l1Err <> ulpL2 <> UlpSum ((ulpVal rawIntersectErr) * ulpScale)
    ulpScale = 120 + ulpMultiplier * (realToFrac angle) * (realToFrac angle)
    (angle, _) = angleBetweenWithErr pl1 pl2
    dumpULPs = "pl1AngleErr: " <> show pl1AngleErr <> "\n"
            <> "pl1TranslateErr" <> show pl1TranslateErr <> "\n"
            <> "pl2AngleErr: " <> show pl2AngleErr <> "\n"
            <> "pl2TranslateErr:" <> show pl2TranslateErr <> "\n"
            <> "l1Err: " <> show l1Err <> "\n"
            <> "rawIntersectErr: " <> show rawIntersectErr <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum1 ulpEndSum1 && onSegment l2 rawIntersection ulpStartSum2 ulpEndSum2
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    (rawIntersection, (_, _, rawIntersectionErr@(PPoint2Err intersectionAddErr _))) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    (rawIntersect, (PPoint2Err rawIntersectErr _,_,_)) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> ProjectivePoint -> UlpSum -> UlpSum -> Bool
onSegment ls i startUlp endUlp =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    (startDistance, (_,_,_, startDistanceErr)) = distanceBetweenPPointsWithErr (i,mempty) (start, mempty)
    (midDistance, (_,_,_, UlpSum midDistanceErr)) = distanceBetweenPPointsWithErr (i,mempty) (mid,midErr)
    (endDistance, (_,_,_, endDistanceErr)) = distanceBetweenPPointsWithErr (i,mempty) (end,mempty)
    start = eToPPoint2 $ startPoint ls
    (mid, rawMidErr) = pPointBetweenPPointsWithErr start end 0.5 0.5
    (midErr,midWeight) = (\(PPoint2Err _ a, d@(PPoint2Err _ b), c, e) -> (d,ulpVal $ a <> b <> c <> e)) rawMidErr
    end = eToPPoint2 $ endPoint ls
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ ulpVal $ startUlp <> startDistanceErr
    midFudgeFactor = realToFrac $ abs (realToFrac $ doubleUlp lengthOfSegment) + midDistanceErr + midWeight
    endFudgeFactor = realToFrac $ ulpVal $ endDistanceErr <> endUlp

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
data PPoint2Err = PPoint2Err { _pPoint2UnlikeErr :: UlpSum, _pPoint2CannonicalizeErr :: UlpSum }
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
data PLine2Err = PLine2Err { _pLine2AddErr :: UlpSum, _pLine2NormErr :: UlpSum, _pLine2AngleErr :: UlpSum, _pLine2TransErr :: UlpSum, _pLine2JoinErr :: UlpSum }
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
(∨+) :: GVec -> GVec -> (GVec, UlpSum)
(∨+) a b = (dual2DGVec $ GVec $ foldl' addVal [] res
           , ulpSum)
  where
    (GVec res, ulpSum) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | a typed join function. join two points, returning a line.
join2PPointsWithErr :: ProjectivePoint -> ProjectivePoint -> (ProjectiveLine, (UlpSum, UlpSum, PLine2Err))
join2PPointsWithErr pp1 pp2 = (PLine2 res,
                               (pv1Ulp, pv2Ulp, PLine2Err mempty mempty mempty mempty resUlp))
  where
    (res,resUlp)  = pv1 ∨+ pv2
    (CPPoint2 pv1) = forceProjectivePointBasis cp1
    (CPPoint2 pv2) = forceProjectivePointBasis cp2
    (cp1, pv1Ulp) = canonicalizePPoint2WithErr pp1
    (cp2, pv2Ulp) = canonicalizePPoint2WithErr pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2PLine2WithErr :: ProjectiveLine -> ProjectiveLine -> (ProjectivePoint, PPoint2Err)
meet2PLine2WithErr (NPLine2 plr1) (NPLine2 plr2) = (PPoint2 res,
                                                    PPoint2Err resUlp mempty)
  where
    (res, resUlp) = pv1 ⎤+ pv2
    (PLine2 pv1) = forcePLine2Basis $ PLine2 plr1
    (PLine2 pv2) = forcePLine2Basis $ PLine2 plr2

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
    xVal = negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVals [GEZero 1, GEPlus 1] vals
    e12Val = valOf 0 (getVals [GEPlus 1, GEPlus 2] rawVals)

-- | Reverse a vector. Really, take every value in it, and recompute it in the reverse order of the vectors (so instead of e0∧e1, e1∧e0). which has the effect of negating bi and tri-vectors.
reverseGVec :: GVec -> GVec
reverseGVec vec = GVec $ foldl' addVal []
                  [
                    GVal           realVal                                                (singleton G0)
                  , GVal (         valOf 0 $ getVals [GEZero 1] vals)                     (singleton (GEZero 1))
                  , GVal (         valOf 0 $ getVals [GEPlus 1] vals)                     (singleton (GEPlus 1))
                  , GVal (         valOf 0 $ getVals [GEPlus 2] vals)                     (singleton (GEPlus 2))
                  , GVal (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 1] vals)           (fromList [GEZero 1, GEPlus 1])
                  , GVal (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] vals)           (fromList [GEZero 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVals [GEPlus 1, GEPlus 2] vals)           (fromList [GEPlus 1, GEPlus 2])
                  , GVal (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] vals) (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                  ]
  where
    realVal     = scalarPart vec
    (GVec vals) = vectorPart vec

-- | get the dual of a vector. for a point, find a line, for a line, a point...
dual2DGVec :: GVec -> GVec
dual2DGVec vec = GVec $ foldl' addVal []
                 [
                   GVal           realVal                                                (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVals [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , GVal (negate $ valOf 0 $ getVals [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVals [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , GVal (         valOf 0 $ getVals [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , GVal (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , GVal (         valOf 0 $ getVals [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , GVal (         valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]
  where
    realVal     = scalarPart vec
    (GVec vals) = vectorPart vec

-- | perform basis coersion. ensure all of the required '0' components exist. required before using basis sensitive raw operators.
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVals (elems needs) has) needs

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
               GVal (negate $ valOf 0 $ getVals [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVals [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVals [GEPlus 2] vals) (singleton (GEPlus 2))
             ]

eToPLine2WithErr :: LineSeg -> (ProjectiveLine, PLine2Err)
eToPLine2WithErr l1 = pLineFromEndpointsWithErr (startPoint l1) (endPoint l1)

-- | construct a projective line from a pair of euclidian points.
pLineFromEndpointsWithErr :: Point2 -> Point2 -> (ProjectiveLine, PLine2Err)
pLineFromEndpointsWithErr (Point2 (x1,y1)) (Point2 (x2,y2)) = (PLine2 $ GVec $ foldl' addVal [] [ GVal c (singleton (GEZero 1)), GVal a (singleton (GEPlus 1)), GVal b (singleton (GEPlus 2)) ], pLine2Err)
  where
    a=y2-y1
    b=x1-x2
    cross1 = y1*x2
    cross2 = x1*y2
    c=cross1-cross2
    pLine2Err = PLine2Err
                mempty
                mempty
                (UlpSum $   abs (realToFrac $ doubleUlp a)
                          + abs (realToFrac $ doubleUlp b))
                (UlpSum $   abs (realToFrac $ doubleUlp cross1)
                          + abs (realToFrac $ doubleUlp cross2)
                          + abs (realToFrac $ doubleUlp c))
                mempty

-- | Get the sum of the error involved in storing the values in a given PLine2.
ulpOfPLine2 :: ProjectiveLine -> UlpSum
ulpOfPLine2 pline = res
  where
    res = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVals [GEZero 1] vals
                                   ,getVals [GEPlus 1] vals
                                   ,getVals [GEPlus 2] vals]
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
          [getVals [GEZero 1, GEPlus 1] vals
          ,getVals [GEZero 1, GEPlus 2] vals
          ,getVals [GEPlus 1, GEPlus 2] vals]
    vals = case point of
             (PPoint2 (GVec vs)) -> vs
             (CPPoint2 (GVec vs)) -> vs

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalar. ----
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
     | e12Val == 0 = ( negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] vals
                     ,          valOf 0 $ getVals [GEZero 1, GEPlus 1] vals)
     | otherwise = (\(Point2 (x1,y1)) -> (x1,y1)) $ pToEPoint2 point
    e12Val = valOf 0 (getVals [GEPlus 1, GEPlus 2] vals)
    vals = case point of
             (PPoint2 (GVec vs)) -> vs
             (CPPoint2 (GVec vs)) -> vs

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
canonicalizePPoint2WithErr :: ProjectivePoint -> (ProjectivePoint, UlpSum)
canonicalizePPoint2WithErr point
  | valOf 0 foundVal == 0 = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = (CPPoint2 (GVec rawVals), mempty)
  | otherwise = (res, ulpSum)
  where
    res = CPPoint2 $ GVec $ foldl' addVal []
          $  ( if isNothing (getVals [GEZero 1, GEPlus 1] scaledVals)
               then []
               else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
             )
          <> ( if isNothing (getVals [GEZero 1, GEPlus 2] scaledVals)
               then []
               else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
             )
          <> [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    newVec = GVec $ addVal [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])]
                           (GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2]))
    (GVec scaledVals) = divVecScalar newVec $ valOf 1 foundVal
    rawVals = case point of
                (PPoint2 (GVec v)) -> v
                (CPPoint2 (GVec v)) -> v                
    foundVal = getVals [GEPlus 1, GEPlus 2] rawVals
    ulpSum = if valOf 0 foundVal < 1 && valOf 0 foundVal > -1
             then ulpOfPPoint2 res
             else ulpOfPPoint2 point

-- | Canonicalize the intersection resulting from two PLines.
-- NOTE: Returns nothing when the PLines are (anti)parallel.
canonicalizeIntersectionWithErr :: (ProjectiveLine,PLine2Err) -> (ProjectiveLine,PLine2Err) -> Maybe (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
canonicalizeIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
  | valOf 0 foundVal == 0 = Nothing
  | otherwise = Just (cpp1, (pl1ResErr, pl2ResErr, PPoint2Err intersectionUnlikeErr canonicalizationErr))
  where
    (cpp1, canonicalizationErr) = canonicalizePPoint2WithErr pp1
    (pp1, (PPoint2Err intersectionUnlikeErr _, pl1ResErr, pl2ResErr)) = pLineIntersectionWithErr (pl1,pl1Err) (pl2,pl2Err)
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) pp1

-- | Normalize a Projective Line.
normalizePLine2WithErr :: ProjectiveLine -> (ProjectiveLine, PLine2Err)
normalizePLine2WithErr pl = (res, resErr)
  where
    (res, resErr) = case norm of
            1.0 -> (NPLine2 vec, mempty)
            _ -> (NPLine2 $ divVecScalar vec norm, PLine2Err mempty normErr mempty mempty mempty)
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
    (sqNormOfPLine2, UlpSum sqNormErr) = sqNormOfPLine2WithErr pline
    ulpTotal = UlpSum $ abs (realToFrac $ doubleUlp res) + sqNormErr

-- | find the squared norm of a given PLine2
sqNormOfPLine2WithErr :: ProjectiveLine -> (ℝ, UlpSum)
sqNormOfPLine2WithErr pl = (res, resErr)
  where
    (res, resErr) = case rawRes of
                      1.0 -> (1.0, mempty)
                      _ -> (rawRes, ulpTotal)
    rawRes = a*a+b*b
    a = valOf 0 $ getVals [GEPlus 1] vals
    b = valOf 0 $ getVals [GEPlus 2] vals
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ a*a)
               + abs (realToFrac $ doubleUlp $ b*b)
               + abs (realToFrac $ doubleUlp res)
    vals = case pl of
             (PLine2 (GVec v)) -> v
             (NPLine2 (GVec v)) -> v
