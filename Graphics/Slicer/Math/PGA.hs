{- ORMOLU_DISABLE -}
{-
 - Copyright 2020 Julia Longtin
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

-- | The purpose of this file is to hold projective geometric algebraic arithmatic. It defines a 2D PGA with mixed linear components.

module Graphics.Slicer.Math.PGA(
  Intersection(HitStartPoint, HitEndPoint, NoIntersection),
  NPLine2(NPLine2),
  PIntersection (PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn),
  PLine2(PLine2),
  PPoint2(PPoint2),
  Arcable(hasArc, outOf, ulpOfOut, outUlpMag),
  Pointable(canPoint, pPointOf, ePointOf),
  angleBetweenWithErr,
  combineConsecutiveLineSegs,
  distanceBetweenPLine2sWithErr,
  distanceBetweenPPoints,
  distanceBetweenPPointsWithErr,
  distancePPointToPLine,
  distancePPointToPLineWithErr,
  eToNPLine2,
  eToPLine2,
  eToPPoint2,
  flipPLine2,
  intersectsWith,
  intersectsWithErr,
  join2PPoint2,
  join2PPoint2WithErr,
  lineIsLeft,
  makePPoint2WithErr,
  normalizePLine2,
  normalizePLine2WithErr,
  outputIntersectsLineSeg,
  pLineFromEndpointsWithErr,
  pLineIsLeft,
  pPointBetweenPPoints,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEPoint2,
  plineFromEndpoints,
  plinesIntersectIn,
  pointOnPerp,
  translatePerp,
  translateRotatePPoint2,
  ulpOfLineSeg,
  ulpOfPLine2
  ) where

import Prelude (Eq, Show, Ord, (==), ($), (*), (-), Bool, (&&), (<$>), otherwise, (>), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, show, error, sin, cos, realToFrac, fst, sum, (.))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes, isNothing)

import Data.Set (Set, singleton, fromList, elems)

import Data.Number.BigFloat (BigFloat, PrecPlus20, Eps1)

import Safe (lastMay, initSafe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, startPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addVal, addVecPair, divVecScalar, getVals, mulScalarVec, scalarPart, valOf, vectorPart, hpDivVecScalar)

import Graphics.Slicer.Math.Line (combineLineSegs, endPoint, midPoint)

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
  | IntersectsIn !PPoint2 !(UlpSum, UlpSum, UlpSum, UlpSum, UlpSum, UlpSum)
  deriving (Show, Eq)

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: PLine2 -> PLine2 -> PIntersection
plinesIntersectIn pl1 pl2
  | intersectPoint == PPoint2 (GVec [])
  || (idealNorm < idnUlp
     && (intersectAngle > 1-iaUlp ||
         intersectAngle < -1+iaUlp )) = if intersectAngle > 0
                                         then PCollinear
                                         else PAntiCollinear
  | intersectAngle >  1-iaUlp          = PParallel
  | intersectAngle < -1+iaUlp          = PAntiParallel
  | intersectAngle >  1+iaUlp          = error "too big of an angle?"
  | intersectAngle < -1-iaUlp          = error "too small of an angle?"
  | otherwise                          = IntersectsIn res (resUlp, intersectUlp, npl1Ulp, npl2Ulp, UlpSum iaUlp, UlpSum 0)
  where
    (idealNorm, UlpSum idnUlp) = idealNormPPoint2WithErr intersectPoint
    (intersectAngle, UlpSum iaUlp) = angleBetweenWithErr npl1 npl2
    -- FIXME: how much do the potential normalization errors have an effect on the resultant angle?
    (npl1, npl1Ulp) = normalizePLine2WithErr pl1
    (npl2, npl2Ulp) = normalizePLine2WithErr pl2
    (intersectPoint, intersectUlp) = pLineIntersectionWithErr pl1 pl2
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (res, resUlp) = canonicalizePPoint2WithErr intersectPoint

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: PLine2 -> PLine2 -> Maybe Bool
pLineIsLeft pl1 pl2
  | abs res < ulpSum = Nothing
  | otherwise               = Just $ res > 0
  where
    res = angleCos npl1 npl2
    (npl1, UlpSum npl1Ulp) = normalizePLine2WithErr pl1
    (npl2, UlpSum npl2Ulp) = normalizePLine2WithErr pl2
    ulpSum = npl1Ulp + npl2Ulp

-- | Find out where two lines intersect, returning a projective point, and the error quotent. Note that this should only be used when you can guarantee these are not collinear.
pLineIntersectionWithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
pLineIntersectionWithErr pl1 pl2 = (res, ulpTotal)
  where
    (res, UlpSum resErr) = meet2PLine2WithErr pLine1 pLine2
    (pLine1, UlpSum pl1Err) = normalizePLine2WithErr pl1
    (pLine2, UlpSum pl2Err) = normalizePLine2WithErr pl2
    ulpTotal = UlpSum $ resErr + pl1Err + pl2Err

-- | Find a point somewhere along the line between the two points given.
--  requires two weights. the ratio of these weights determines the position of the found points, E.G: 2/1 is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint.
pPointBetweenPPoints :: PPoint2 -> PPoint2 -> ℝ -> ℝ -> PPoint2
pPointBetweenPPoints (PPoint2 rawStartPoint) (PPoint2 rawStopPoint) weight1 weight2 = PPoint2 $ addVecPair (mulScalarVec weight1 rawStartPoint) (mulScalarVec weight2 rawStopPoint)

-- | Find the unsigned distance between a point and a line.
distancePPointToPLine :: PPoint2 -> PLine2 -> ℝ
distancePPointToPLine point line = fst $ distancePPointToPLineWithErr point line

distancePPointToPLineWithErr :: PPoint2 -> PLine2 -> (ℝ, UlpSum)
distancePPointToPLineWithErr point@(PPoint2 pvec) line = (res, ulpSum)
  where
    (res, UlpSum resErr)           = normOfPLine2WithErr newPLine
    (newPLine, UlpSum newPLineErr) = join2PPoint2WithErr point linePoint
    (NPLine2 lvec, UlpSum normErr) = normalizePLine2WithErr line
    (perpLine, UlpSum perpLineErr) = lvec ⨅+ pvec
    (linePoint, UlpSum lpErr)      = canonicalizeIntersectionWithErr (PLine2 lvec) (PLine2 perpLine)
    ulpSum                         = UlpSum $ lpErr + perpLineErr + normErr + newPLineErr + resErr

-- | Determine if two points are on the same side of a given line.
pPointsOnSameSideOfPLine :: PPoint2 -> PPoint2 -> PLine2 -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  -- Return nothing if one of the points is on the line.
  |  abs foundP1 < unlikeP1Err ||
     abs foundP2 < unlikeP2Err    = Nothing
    | otherwise = Just $ isPositive foundP1 == isPositive foundP2
  where
    foundP1 = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] unlikeP1
    foundP2 = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] unlikeP2
    (GVec unlikeP1, UlpSum unlikeP1Err) = pv1 ⎤+ lv1
    (GVec unlikeP2, UlpSum unlikeP2Err) = pv2 ⎤+ lv1
    (PPoint2 pv1) = forcePPoint2Basis point1
    (PPoint2 pv2) = forcePPoint2Basis point2
    (PLine2 lv1) = forcePLine2Basis line
    isPositive :: ℝ -> Bool
    isPositive i = i > 0

-- | Find the unsigned distance between two projective points.
distanceBetweenPPoints :: PPoint2 -> PPoint2 -> ℝ
distanceBetweenPPoints p1 p2 = fst $ distanceBetweenPPointsWithErr p1 p2

-- | Find the unsigned distance between two projective points, along with the precision of the result.
distanceBetweenPPointsWithErr :: PPoint2 -> PPoint2 -> (ℝ, UlpSum)
distanceBetweenPPointsWithErr point1 point2 = (res, ulpSum)
  where
    (res, UlpSum resErr)           = normOfPLine2WithErr newPLine
    (newPLine, UlpSum newPLineErr) = join2PPoint2WithErr point1 point2
    ulpSum                         = UlpSum $ resErr + newPLineErr

-- | Find the unsigned distance between two parallel or antiparallel projective lines.
-- Same operation as angleBetween, so just a wrapper.
distanceBetweenPLine2sWithErr :: NPLine2 -> NPLine2 -> (ℝ, UlpSum)
distanceBetweenPLine2sWithErr = angleBetweenWithErr

-- | Return the sine of the angle between the two lines, along with the error. results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
-- FIXME: not generating large enough ULPs. why?
angleBetweenWithErr :: NPLine2 -> NPLine2 -> (ℝ, UlpSum)
angleBetweenWithErr (NPLine2 pv1) (NPLine2 pv2) = (scalarPart res
                                                  , ulpSum)
  where
    (res, ulpSum) = pv1 ⎣+ pv2

-- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
angleCos :: NPLine2 -> NPLine2 -> ℝ
angleCos npl1@(NPLine2 lvec1) npl2@(NPLine2 lvec2) = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
  where
    (PPoint2 iPointVec, _)         = canonicalizeIntersectionWithErr pl1 pl2
    motor                          = addVecPair (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
    antiMotor                      = addVecPair (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
    -- I, the infinite point.
    gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    pl1 = (\(NPLine2 a) -> PLine2 a) npl1
    pl2 = (\(NPLine2 a) -> PLine2 a) npl2

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: PLine2 -> PPoint2 -> ℝ -> PPoint2
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: PLine2 -> PPoint2 -> ℝ -> (PPoint2, UlpSum)
pPointOnPerpWithErr pline (PPoint2 pvec) d = (PPoint2 res,
                                               ulpTotal)
  where
    res = motor•pvec•reverseGVec motor
    (NPLine2 lvec,UlpSum lErr)  = normalizePLine2WithErr pline
    (perpLine,UlpSum perpPLineErr) = lvec ⨅+ pvec
    motor = addVecPair (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr = doubleUlp $ d/2
    ulpTotal = UlpSum $ gaIErr + perpPLineErr + lErr

-- | Translate a line a given distance along it's perpendicular bisector.
translatePerp :: PLine2 -> ℝ -> PLine2
translatePerp pLine@(PLine2 rawPLine) d = PLine2 $ addVecPair m rawPLine
  where
    m = GVec [GVal (d*normOfPLine2 pLine) (singleton (GEZero 1))]

-- | Translate a point a given distance away from where it is, rotating it a given amount clockwise (in radians) around it's original location, with 0 degrees being aligned to the X axis.
translateRotatePPoint2 :: PPoint2 -> ℝ -> ℝ -> PPoint2
translateRotatePPoint2 ppoint d rotation = PPoint2 $ translator•pvec•reverseGVec translator
  where
    (PPoint2 pvec)      = ppoint
    xLineThroughPPoint2 = (pvec ⨅ xLineVec) • pvec
      where
        (PLine2 xLineVec) = forcePLine2Basis $ plineFromEndpoints (Point2 (0,0)) (Point2 (1,0))
    (PLine2 angledLineThroughPPoint2) = forcePLine2Basis $ PLine2 $ rotator•xLineThroughPPoint2•reverseGVec rotator
      where
        rotator = addVecPair (mulScalarVec (sin $ rotation/2) pvec) (GVec [GVal (cos $ rotation/2) (singleton G0)])
    translator = addVecPair (angledLineThroughPPoint2 • gaIScaled) (GVec [GVal 1 (singleton G0)])
      where
        -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
        gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

----------------------------------------------------------
-------------- Euclidian Mixed Interface -----------------
----------------------------------------------------------

-- | Intersection events that can only happen with line segments.
data Intersection =
    NoIntersection !PPoint2 !(UlpSum, UlpSum, UlpSum, UlpSum)
  | HitStartPoint !LineSeg !Point2
  | HitEndPoint !LineSeg !Point2
  deriving Show

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2 = Either LineSeg PLine2

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
outputIntersectsLineSeg source (l1, UlpSum ulpL1)
  | intersectionDistance == 0 = pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) 1
  | ulpScale > 100000 = error
                        $ "wtf\n"
                        <> "travelUlpMul: " <> show travelUlpMul <> "\n"
                        <> "ulpMultiplier: " <> show ulpMultiplier <> "\n"
                        <> "angle: " <> show angle <> "\n"
                        <> "angleErr: " <> show angleErr <> "\n"
                        <> "p1Mag: " <> show pl1Mag <> "\n"
                        <> "intersectionDistance: " <> show intersectionDistance <> "\n"
  | otherwise = pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) ulpScale
  where
    -- | the ULP multiplier. used to expand the hitcircle of an endpoint.
    ulpScale = 120 + travelUlpMul * ulpMultiplier * (angle+angleErr) * (angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    npl1 = normalizePLine2 pl1
    npl2 = normalizePLine2 pl2
    pl2 = eToPLine2 l1
    -- the multiplier to account for distance between our Pointable, and where it intersects.
    travelUlpMul
      | canPoint source = pl1Mag / intersectionDistance
      | otherwise = error
                    $ "cannot resolve source to a point?\n"
                    <> show source <> "\n"
    (pl1, UlpSum ulpPL1, pl1Mag)
      | hasArc source = (outOf source, ulpOfOut source, outUlpMag source)
      | otherwise = error
                    $ "no arc from source?\n"
                    <> show source <> "\n"
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, _) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, _) = pLineIntersectionWithErr pl1 pl2
    intersectionDistance = distanceBetweenPPoints (pPointOf source) rawIntersection

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2WithErr = Either (LineSeg, UlpSum) (PLine2,UlpSum)

-- entry point usable for all intersection needs, complete with passed in error values.
intersectsWithErr :: SegOrPLine2WithErr -> SegOrPLine2WithErr -> Either Intersection PIntersection
intersectsWithErr (Left l1)       (Left l2)       =         lineSegIntersectsLineSeg l1 l2
intersectsWithErr (Right (pl1,_)) (Right (pl2,_)) = Right $ plinesIntersectIn pl1 pl2
intersectsWithErr (Left l1@(rawL1,_))       (Right pl1@(rawPL1,_))     =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale = 120 + ulpMultiplier * (angle+angleErr) * (angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr rawPL1
    (npl2, _) = normalizePLine2WithErr pl2
    (pl2, _) = eToPLine2WithErr rawL1
intersectsWithErr (Right pl1@(rawPL1,_))     (Left l1@(rawL1,_))       =         pLineIntersectsLineSeg pl1 l1 ulpScale
  where
    ulpScale = 120 + ulpMultiplier * (angle+angleErr) * (angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr rawPL1
    (npl2, _) = normalizePLine2WithErr pl2
    (pl2, _) = eToPLine2WithErr rawL1
-- FIXME: as long as this is required, we're not accounting for ULP correctly everywhere.
ulpMultiplier :: ℝ
ulpMultiplier = 450

-- | Check if/where a line segment and a PLine intersect.
pLineIntersectsLineSeg :: (PLine2, UlpSum) -> (LineSeg, UlpSum) -> ℝ -> Either Intersection PIntersection
pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) ulpScale
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | distance (startPoint l1) (endPoint l1) < ulpStart+ulpEnd+ulpTotal = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nulpScale: " <> show ulpScale <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && startDistance <= ulpStartSum = Left $ HitStartPoint l1 intersection
  | hasIntersection && endDistance <= ulpEndSum = Left $ HitEndPoint l1 intersection
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum ulpStartSum, UlpSum ulpEndSum, UlpSum ulpPL1, UlpSum ulpPL2, UlpSum ulpI, UlpSum ulpC)
  | otherwise = Left $ NoIntersection rawIntersection (UlpSum ulpStartSum, UlpSum ulpEndSum, UlpSum 0, UlpSum 0)
  where
    (startDistance, UlpSum ulpStart) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ startPoint l1)
    (endDistance, UlpSum ulpEnd) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ endPoint l1)
    ulpStartSum = ulpTotal+ulpStart
    ulpEndSum = ulpTotal+ulpEnd
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal
      | ulpPL1 < 0 || ulpPL2 < 0 || ulpL1 < 0 || ulpI < 0 = error "negative ULP?\n"
      | otherwise = ulpPL1 + ulpPL2 + ulpL1 + ((ulpI + ulpC) * ulpScale)
    dumpULPs = "ulpPL1: " <> show ulpPL1 <> "\nulpPL2: " <> show ulpPL2 <> "\nulpL1: " <> show ulpL1 <> "\nulpI: " <> show ulpI <> "\nulpC: " <> show ulpC <> "\n"
    hasIntersection = onSegment l1 rawIntersection ulpStartSum ulpEndSum
    intersection = pToEPoint2 rawIntersection
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum ulpC) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum ulpI) = pLineIntersectionWithErr pl1 pl2
    (pl2, UlpSum ulpPL2) = eToPLine2WithErr l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, UlpSum ulpL1) (l2, UlpSum ulpL2)
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | distance (startPoint l1) (endPoint l1) < ulpStart1+ulpEnd1+ulpTotal = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | distance (startPoint l2) (endPoint l2) < ulpStart2+ulpEnd2+ulpTotal = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && startDistance1 <= ulpStartSum1 = Left $ HitStartPoint l1 intersection
  | hasIntersection && endDistance1 <= ulpEndSum1 = Left $ HitEndPoint l1 intersection
  | hasIntersection && startDistance2 <= ulpStartSum2 = Left $ HitStartPoint l2 intersection
  | hasIntersection && endDistance2 <= ulpEndSum2 = Left $ HitEndPoint l2 intersection
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum ulpStartSum1, UlpSum ulpEndSum1, UlpSum ulpStartSum2, UlpSum ulpEndSum2, UlpSum ulpTotal, UlpSum ulpI)
  | otherwise = Left $ NoIntersection rawIntersection (UlpSum ulpStartSum1, UlpSum ulpEndSum1, UlpSum ulpStartSum2, UlpSum ulpEndSum2)
  where
    ulpStartSum1 = ulpTotal+ulpStart1
    ulpStartSum2 = ulpTotal+ulpStart2
    ulpEndSum1 = ulpTotal+ulpEnd1
    ulpEndSum2 = ulpTotal+ulpEnd2
    (startDistance1, UlpSum ulpStart1) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ startPoint l1)
    (startDistance2, UlpSum ulpStart2) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ startPoint l2)
    (endDistance1, UlpSum ulpEnd1) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ endPoint l1)
    (endDistance2, UlpSum ulpEnd2) = distanceBetweenPPointsWithErr rawIntersection (eToPPoint2 $ endPoint l2)
    (pl1, UlpSum ulpPL1) = eToPLine2WithErr l1
    (pl2, UlpSum ulpPL2) = eToPLine2WithErr l2
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal
      | ulpPL1 < 0 || ulpPL2 < 0 || ulpL1 < 0 || ulpL2 < 0 || ulpI < 0 || ulpC < 0 = error "negative ULP?\n"
      | otherwise = ulpPL1 + ulpPL2 + ulpL1 + ulpL2 + ((ulpI + ulpC) * ulpScale)
    ulpScale = 120 + ulpMultiplier * (angle+angleErr) * (angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr pl1
    (npl2, _) = normalizePLine2WithErr pl2
    dumpULPs = "ulpPL1: " <> show ulpPL1 <> "\nulpPL2: " <> show ulpPL2 <> "\nulpL1: " <> show ulpL1 <> "\nulpI: " <> show ulpI <> "\nulpC: " <> show ulpC <> "\n"
    hasIntersection = onSegment l1 rawIntersection ulpStartSum1 ulpEndSum1 && onSegment l2 rawIntersection ulpStartSum2 ulpEndSum2
    intersection = pToEPoint2 rawIntersection
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum ulpC) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum ulpI) = pLineIntersectionWithErr pl1 pl2

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> PPoint2 -> ℝ -> ℝ -> Bool
onSegment ls i startUlp endUlp =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    (startDistance, UlpSum startDistanceUlp) = distanceBetweenPPointsWithErr startPPoint i
    (midDistance, UlpSum midDistanceUlp) = distanceBetweenPPointsWithErr midPPoint i
    (endDistance, UlpSum endDistanceUlp) = distanceBetweenPPointsWithErr endPPoint i
    startPPoint = eToPPoint2 $ startPoint ls
    midPPoint = eToPPoint2 $ midPoint ls
    endPPoint = eToPPoint2 $ endPoint ls
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor = startUlp + startDistanceUlp
    midFudgeFactor = abs (doubleUlp lengthOfSegment) + midDistanceUlp
    endFudgeFactor = endUlp + endDistanceUlp

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
lineIsLeft :: LineSeg -> LineSeg -> Maybe Bool
lineIsLeft line1 line2 = pLineIsLeft (eToPLine2 line1) (eToPLine2 line2)

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
        sameLineSeg = plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PCollinear
        sameMiddlePoint = p2 == addPoints p1 s1

pointOnPerp :: LineSeg -> Point2 -> ℝ -> Point2
pointOnPerp line point d = case ppointToPoint2 $ pPointOnPerp (eToPLine2 line) (eToPPoint2 point) d of
                             Nothing -> error $ "generated infinite point trying to travel " <> show d <> "along the line perpendicular to " <> show line <> " at point " <> show point <> "\n"
                             Just v -> v

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

-- | A projective point in 2D space.
newtype PPoint2 = PPoint2 GVec
  deriving (Eq, Ord, Generic, NFData, Show)

-- | A projective line in 2D space.
newtype PLine2 = PLine2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | A normalized projective line in 2D space.
newtype NPLine2 = NPLine2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  canPoint :: a -> Bool
  pPointOf :: a -> PPoint2
  ePointOf :: a -> Point2

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> PLine2
  ulpOfOut :: a -> UlpSum
  outUlpMag :: a -> ℝ

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
(∨+) :: GVec -> GVec -> (GVec, UlpSum)
(∨+) a b = (dual2DGVec $ GVec $ foldl' addVal [] res
           , ulpSum)
  where
    (GVec res, ulpSum) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | a typed join function. join two points, returning a line.
join2PPoint2 :: PPoint2 -> PPoint2 -> PLine2
join2PPoint2 pp1 pp2 = fst $ join2PPoint2WithErr pp1 pp2

-- | a typed join function. join two points, returning a line.
join2PPoint2WithErr :: PPoint2 -> PPoint2 -> (PLine2, UlpSum)
join2PPoint2WithErr pp1 pp2 = (PLine2 res,
                               resUlp)
  where
    (res,resUlp)  = pv1 ∨+ pv2
    (PPoint2 pv1) = forcePPoint2Basis pp1
    (PPoint2 pv2) = forcePPoint2Basis pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2PLine2WithErr :: NPLine2 -> NPLine2 -> (PPoint2, UlpSum)
meet2PLine2WithErr (NPLine2 plr1) (NPLine2 plr2) = (PPoint2 res,
                                                    resUlp)
  where
    (res, resUlp) = pv1 ⎤+ pv2
    (PLine2 pv1) = forcePLine2Basis $ PLine2 plr1
    (PLine2 pv2) = forcePLine2Basis $ PLine2 plr2

-- | Create a projective point from coordinates.
makePPoint2WithErr :: ℝ -> ℝ -> (PPoint2, UlpSum)
makePPoint2WithErr x y = (eToPPoint2 $ Point2 (x,y)
                         , ulpSum)
  where
    ulpSum = UlpSum $ abs (doubleUlp x) + abs (doubleUlp y)

-- | Create a projective point from a euclidian point.
eToPPoint2 :: Point2 -> PPoint2
eToPPoint2 (Point2 (x,y)) = PPoint2 $ GVec $ foldl' addVal [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (-x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]

-- | Create a euclidian point from a projective point.
pToEPoint2 :: PPoint2 -> Point2
pToEPoint2 (PPoint2 (GVec pPoint)) = Point2 (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] pPoint
                                            ,         valOf 0 $ getVals [GEZero 1, GEPlus 1] pPoint)

-- | Create a euclidian point from a projective point.
ppointToPoint2 :: PPoint2 -> Maybe Point2
ppointToPoint2 (PPoint2 (GVec vals)) = if infinitePoint
                                      then Nothing
                                      else Just $ Point2 (xVal, yVal)
  where
    xVal = negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVals [GEZero 1, GEPlus 1] vals
    infinitePoint = 0 == valOf 0 (getVals [GEPlus 1, GEPlus 2] vals)

-- | Create an un-normalized projective line from a euclidian line segment.
eToPLine2 :: LineSeg -> PLine2
eToPLine2 l1 = fst $ eToPLine2WithErr l1

-- | Create a normalized projective line from a euclidian line segment.
eToNPLine2 :: LineSeg -> NPLine2
eToNPLine2 l1 = normalizePLine2 $ fst $ eToPLine2WithErr l1

-- | Create a projective line from a pair of euclidian points.
plineFromEndpoints :: Point2 -> Point2 -> PLine2
plineFromEndpoints point1 point2 = fst $ pLineFromEndpointsWithErr point1 point2

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
forcePLine2Basis :: PLine2 -> PLine2
forcePLine2Basis ln@(PLine2 pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3]))
  | gnum1 == singleton (GEZero 1) &&
    gnum2 == singleton (GEPlus 1) &&
    gnum3 == singleton (GEPlus 2)    = ln
  | otherwise                        = PLine2 $ forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec
forcePLine2Basis (PLine2 pvec)       = PLine2 $ forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] pvec

-- | runtime basis coersion. ensure all of the '0' components exist on a PPoint2.
forcePPoint2Basis :: PPoint2 -> PPoint2
forcePPoint2Basis pt@(PPoint2 pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3]))
  | gnum1 == fromList [GEZero 1, GEPlus 1] &&
    gnum2 == fromList [GEZero 1, GEPlus 2] &&
    gnum3 == fromList [GEPlus 1, GEPlus 2]    = pt
  | otherwise                                 = PPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
forcePPoint2Basis (PPoint2 pvec)              = PPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec

-- | Reverse a line. same line, but pointed in the other direction.
flipPLine2 :: PLine2 -> PLine2
flipPLine2 (PLine2 (GVec vals)) = PLine2 $ GVec $ foldl' addVal []
                                  [
                                    GVal (negate $ valOf 0 $ getVals [GEZero 1] vals) (singleton (GEZero 1))
                                  , GVal (negate $ valOf 0 $ getVals [GEPlus 1] vals) (singleton (GEPlus 1))
                                  , GVal (negate $ valOf 0 $ getVals [GEPlus 2] vals) (singleton (GEPlus 2))
                                  ]

eToPLine2WithErr :: LineSeg -> (PLine2, UlpSum)
eToPLine2WithErr l1 = pLineFromEndpointsWithErr (startPoint l1) (endPoint l1)

pLineFromEndpointsWithErr :: Point2 -> Point2 -> (PLine2, UlpSum)
pLineFromEndpointsWithErr (Point2 (x1,y1)) (Point2 (x2,y2)) = (PLine2 $ GVec $ foldl' addVal [] [ GVal c (singleton (GEZero 1)), GVal a (singleton (GEPlus 1)), GVal b (singleton (GEPlus 2)) ], ulpSum)
  where
    a=y2-y1
    b=x1-x2
    c=y1*x2-x1*y2
    ulpSum = UlpSum
             $ abs (doubleUlp $ y1*x2)
             + abs (doubleUlp $ x1*y2)
             + abs (doubleUlp a)
             + abs (doubleUlp b)
             + abs (doubleUlp c)

-- | Get the sum of the error involved in storing the values in a given PLine2.
ulpOfPLine2 :: PLine2 -> UlpSum
ulpOfPLine2 (PLine2 (GVec vals)) = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVals [GEZero 1] vals
                                   ,getVals [GEPlus 1] vals
                                   ,getVals [GEPlus 2] vals]

-- | Get the sum of the error involved in storing the values in a given Line Segment.
ulpOfLineSeg :: LineSeg -> UlpSum
ulpOfLineSeg (LineSeg (Point2 (x1,y1)) (Point2 (x2,y2))) = UlpSum $ sum $ abs . doubleUlp <$> [x1, y1, x2, y2, x1+x2, y1+y2]

-- | Get the sum of the error involved in storing the values in a given PPoint2.
ulpOfPPoint2 :: PPoint2 -> UlpSum
ulpOfPPoint2 (PPoint2 (GVec vals)) = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                     [getVals [GEZero 1, GEPlus 1] vals
                                     ,getVals [GEZero 1, GEPlus 2] vals
                                     ,getVals [GEPlus 1, GEPlus 2] vals]

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalar. ----
---- Standard precision:                                  ----
--------------------------------------------------------------

-- | Normalize a PLine2.
normalizePLine2 :: PLine2 -> NPLine2
normalizePLine2 pl = fst $ normalizePLine2WithErr pl

-- | find the norm of a given PLine2
normOfPLine2 :: PLine2 -> ℝ
normOfPLine2 pline = fst $ normOfPLine2WithErr pline

-- | find the idealized norm of a projective point.
idealNormPPoint2WithErr :: PPoint2 -> (ℝ, UlpSum)
idealNormPPoint2WithErr ppoint = (res, ulpSum)
  where
    res = sqrt preRes
    preRes = x*x+y*y
    ulpSum = UlpSum $ abs (doubleUlp $ x*x) + abs (doubleUlp $ x*x) + abs (doubleUlp preRes) + abs (doubleUlp res)
    (Point2 (x,y)) = pToEPoint2 ppoint

-- | Normalization of euclidian points is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
canonicalizePPoint2WithErr :: PPoint2 -> (PPoint2, UlpSum)
canonicalizePPoint2WithErr point@(PPoint2 (GVec rawVals))
  | isNothing foundVal = (point, UlpSum 0)
  | otherwise = (res, ulpSum)
  where
    res = PPoint2 $ GVec $ foldl' addVal []
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
    foundVal = getVals [GEPlus 1, GEPlus 2] rawVals
    ulpSum = ulpOfPPoint2 res

-- | Canonicalize the intersection resulting from two PLines.
canonicalizeIntersectionWithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
canonicalizeIntersectionWithErr pl1 pl2 = (cpp1, ulpSum)
  where
    (cpp1, UlpSum canonicalizationErr) = canonicalizePPoint2WithErr pp1
    (pp1, UlpSum intersectionErr) = pLineIntersectionWithErr pl1 pl2
    ulpSum = UlpSum $ intersectionErr + canonicalizationErr

-- | Normalize a PLine2.
normalizePLine2WithErr :: PLine2 -> (NPLine2, UlpSum)
normalizePLine2WithErr pl@(PLine2 vec) = (res, ulpSum)
  where
    res = (\(PLine2 a) -> NPLine2 a) rawRes
    rawRes = PLine2 $ divVecScalar vec normOfMyPLine
    (normOfMyPLine, UlpSum normErr) = normOfPLine2WithErr pl
    ulpSum = UlpSum $ normErr + resErr
    (UlpSum resErr) = ulpOfPLine2 rawRes

-- | find the norm of a given PLine2
normOfPLine2WithErr :: PLine2 -> (ℝ, UlpSum)
normOfPLine2WithErr pline = (res, ulpSum)
  where
    res = sqrt sqNormOfPLine2
    (sqNormOfPLine2, UlpSum sqNormErr) = sqNormOfPLine2WithErr pline
    ulpSum = UlpSum $ abs (doubleUlp res) + sqNormErr

-- | find the squared norm of a given PLine2
sqNormOfPLine2WithErr :: PLine2 -> (ℝ, UlpSum)
sqNormOfPLine2WithErr (PLine2 (GVec vals)) = (res, ulpSum)
  where
    res = a*a+b*b
    a = valOf 0 $ getVals [GEPlus 1] vals
    b = valOf 0 $ getVals [GEPlus 2] vals
    ulpSum = UlpSum $ abs (doubleUlp $ a*a) + abs (doubleUlp $ b*b) + abs (doubleUlp res)

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalar. ----
---- High precision:                                      ----
--------------------------------------------------------------

-- | Normalize a projective point.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
_hpCanonicalizePPoint2 :: PPoint2 -> PPoint2
_hpCanonicalizePPoint2 point@(PPoint2 (GVec rawVals))
  | isNothing foundVal = point
  | otherwise = PPoint2 $ GVec $ foldl' addVal [] $
                ( if isNothing (getVals [GEZero 1, GEPlus 1] scaledVals)
                  then []
                  else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
                ) <>
                ( if isNothing (getVals [GEZero 1, GEPlus 2] scaledVals)
                  then []
                  else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
                ) <>
                [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
  where
    newVec = GVec $ addVal [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])]
                           (GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2]))
    (GVec scaledVals) = hpDivVecScalar newVec $ realToFrac $ valOf 1 foundVal
    foundVal = getVals [GEPlus 1, GEPlus 2] rawVals

-- | find the idealized norm of a projective point.
_hpIdealNormPPoint2 :: PPoint2 -> BigFloat (PrecPlus20 Eps1)
_hpIdealNormPPoint2 ppoint = sqrt (x*x+y*y)
  where
    (Point2 (rawX,rawY)) = pToEPoint2 ppoint
    x,y :: BigFloat (PrecPlus20 Eps1)
    x = realToFrac rawX
    y = realToFrac rawY

-- | Find the norm of a given PLine2
-- NOTE: High precision version.
_hpNormOfPLine2 :: PLine2 -> BigFloat (PrecPlus20 Eps1)
_hpNormOfPLine2 pline = sqrt $ _hpSqNormOfPLine2 pline

-- | Find the squared norm of a given PLine2
-- NOTE: High precision version.
_hpSqNormOfPLine2 :: PLine2 -> BigFloat (PrecPlus20 Eps1)
_hpSqNormOfPLine2 (PLine2 (GVec vals)) = a*a+b*b
  where
    a,b :: BigFloat (PrecPlus20 Eps1)
    a = realToFrac $ valOf 0 $ getVals [GEPlus 1] vals
    b = realToFrac $ valOf 0 $ getVals [GEPlus 2] vals

-- | use the high precision canonicalization function on the intersection resulting from two PLines.
_hpCanonicalizeIntersectionOf :: PLine2 -> PLine2 -> PPoint2
_hpCanonicalizeIntersectionOf pl1 pl2 = _hpCanonicalizePPoint2 $ fst $ pLineIntersectionWithErr pl1 pl2

----------------------------
----- Unused functions -----
----------------------------

-- | Translate a point by another point.
_addPPoint2s :: PPoint2 -> PPoint2 -> PPoint2
_addPPoint2s pPoint1 pPoint2 = PPoint2 $ addVecPair (rawPPoint2 $ _idealPPoint2 pPoint1) (rawPPoint2 $ _idealPPoint2 pPoint2)
  where
    rawPPoint2 (PPoint2 v) = v

-- | Convert from a PPoint2 to it's associated PLine.
_dualPPoint2 :: PPoint2 -> GVec
_dualPPoint2 (PPoint2 vec) = dual2DGVec vec

-- | Convert from a PLine to it's associated projective point.
_dualPLine2 :: PLine2 -> GVec
_dualPLine2 (PLine2 vec) = dual2DGVec vec

-- | Create an ideal point from of a projective point.
-- Note: not yet used.
_idealPPoint2 :: PPoint2 -> PPoint2
_idealPPoint2 (PPoint2 (GVec vals)) = PPoint2 $ GVec $ foldl' addVal []
                                      [
                                        GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] vals) (fromList [GEZero 1, GEPlus 1])
                                      , GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] vals) (fromList [GEZero 1, GEPlus 2])
                                      ]

