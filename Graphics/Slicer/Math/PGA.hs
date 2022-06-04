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
  distanceBetweenPLine2sWithErr,
  distanceBetweenCPPointsWithErr,
  distanceBetweenPPoints,
  distanceBetweenPPointsWithErr,
  distanceCPPointToNPLineWithErr,
  distancePPointToPLine,
  distancePPointToPLineWithErr,
  eToCPPoint2,
  eToNPLine2,
  eToPLine2,
  eToPLine2WithErr,
  eToPPoint2,
  eToPPoint2WithErr,
  flipPLine2,
  intersectsWith,
  intersectsWithErr,
  join2PPoint2,
  join2PPoint2WithErr,
  join2CPPoint2WithErr,
  makeCPPoint2,
  makeCPPoint2WithErr,
  normalizePLine2,
  normalizePLine2WithErr,
  outputIntersectsLineSeg,
  pLineFromEndpointsWithErr,
  pLineIntersectionWithErr,
  pLineIsLeft,
  pPointBetweenPPoints,
  pPointBetweenPPointsWithErr,
  pPointOnPerp,
  pPointOnPerpWithErr,
  pPointsOnSameSideOfPLine,
  pToEPoint2,
  plineFromEndpoints,
  plinesIntersectIn,
  translatePerp,
  translateRotatePPoint2,
  ulpOfLineSeg,
  ulpOfPLine2
  ) where

import Prelude (Eq((==),(/=)), Show, Ord, ($), (*), (-), Bool, (&&), (<$>), otherwise, (>), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, show, error, sin, cos, realToFrac, fst, sum, (.), realToFrac)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes, fromJust, isNothing, maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardNegInf, TowardInf))

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, startPoint, endPoint, distance)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (⨅), (⨅+), (∧), (•), addVal, addVecPair, addVecPairWithErr, divVecScalar, getVals, mulScalarVec, scalarPart, valOf, vectorPart)

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
plinesIntersectIn :: PLine2 -> PLine2 -> PIntersection
plinesIntersectIn pl1 pl2
  | isNothing canonicalizedIntersection
  || (idealNorm < (realToFrac idnUlp)
     && (intersectAngle > maxAngle ||
         intersectAngle < minAngle )) = if intersectAngle > 0
                                        then PCollinear
                                        else PAntiCollinear
  | intersectAngle > maxAngle         = PParallel
  | intersectAngle < minAngle         = PAntiParallel
  | otherwise                         = IntersectsIn res (resUlp, intersectUlp, npl1Ulp, npl2Ulp, UlpSum iaUlp, UlpSum 0)
  where
    -- floor values.
    minAngle, maxAngle :: ℝ
    minAngle = realToFrac $ (-1+realToFrac iaUlp :: Rounded 'TowardNegInf ℝ)
    maxAngle = realToFrac $ 1-iaUlp
    (idealNorm, UlpSum idnUlp) = idealNormPPoint2WithErr intersectPoint
    (intersectAngle, UlpSum iaUlp) = angleBetweenWithErr npl1 npl2
    -- FIXME: how much do the potential normalization errors have an effect on the resultant angle?
    (npl1, npl1Ulp) = normalizePLine2WithErr pl1
    (npl2, npl2Ulp) = normalizePLine2WithErr pl2
    (intersectPoint, intersectUlp) = pLineIntersectionWithErr pl1 pl2
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (res, resUlp) = fromJust canonicalizedIntersection
    canonicalizedIntersection = canonicalizeIntersectionWithErr pl1 pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: PLine2 -> PLine2 -> Maybe Bool
pLineIsLeft pl1 pl2
  | abs res < realToFrac ulpSum = Nothing
  | otherwise                   = Just $ res > 0
  where
    (res, UlpSum resErr)   = angleCos npl1 npl2
    (npl1, UlpSum npl1Ulp) = normalizePLine2WithErr pl1
    (npl2, UlpSum npl2Ulp) = normalizePLine2WithErr pl2
    ulpSum = npl1Ulp + npl2Ulp + resErr
    -- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
    angleCos :: NPLine2 -> NPLine2 -> (ℝ, UlpSum)
    angleCos (NPLine2 lvec1) (NPLine2 lvec2)
      | isNothing canonicalizedIntersection = (0, UlpSum 0)
      | otherwise = (angle, iPointErr)
      where
        angle = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
        (CPPoint2 iPointVec, iPointErr) = fromJust canonicalizedIntersection
        motor                          = addVecPair (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
        antiMotor                      = addVecPair (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
        canonicalizedIntersection      = canonicalizeIntersectionWithErr pline1 pline2
        -- I, the infinite point.
        gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
        pline1 = PLine2 lvec1
        pline2 = PLine2 lvec2

-- | Find out where two lines intersect, returning a projective point, and the error quotent. Note that this should only be used when you can guarantee these are not collinear, or parallel.
pLineIntersectionWithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
pLineIntersectionWithErr pl1 pl2 = (res, ulpTotal)
  where
    (res, UlpSum resErr) = meet2PLine2WithErr pLine1 pLine2
    (pLine1, UlpSum pl1Err) = normalizePLine2WithErr pl1
    (pLine2, UlpSum pl2Err) = normalizePLine2WithErr pl2
    ulpTotal = UlpSum $ resErr + pl1Err + pl2Err

-- | Find a point somewhere along the line between the two points given.
--  requires two weights. the ratio of these weights determines the position of the found points, E.G: (2/3,1/3) is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint. weights can sum to anything.
pPointBetweenPPoints :: PPoint2 -> PPoint2 -> ℝ -> ℝ -> PPoint2
pPointBetweenPPoints startOfSeg stopOfSeg weight1 weight2 = fst $ pPointBetweenPPointsWithErr startOfSeg stopOfSeg weight1 weight2

-- NOTE: returns a canonicalized point.
pPointBetweenPPointsWithErr :: PPoint2 -> PPoint2 -> ℝ -> ℝ -> (PPoint2, UlpSum)
pPointBetweenPPointsWithErr start stop weight1 weight2 = (PPoint2 cRes, UlpSum $ cResErr + cStartErr + cStopErr)
  where
    (CPPoint2 cRes, UlpSum cResErr) = cPPointBetweenCPPointsWithErr cStart cStop weight1 weight2
    (cStart, UlpSum cStartErr) = canonicalizePPoint2WithErr start
    (cStop, UlpSum cStopErr) = canonicalizePPoint2WithErr stop

-- NOTE: returns a canonicalized point.
cPPointBetweenCPPointsWithErr :: CPPoint2 -> CPPoint2 -> ℝ -> ℝ -> (CPPoint2, UlpSum)
cPPointBetweenCPPointsWithErr (CPPoint2 rawStartPoint) (CPPoint2 rawStopPoint) weight1 weight2
  | valOf 0 foundVal == 0 = error "tried to generate an ideal point?"
  | otherwise = canonicalizePPoint2WithErr $ PPoint2 $ res
  where
    res = addVecPair (mulScalarVec weight1 rawStartPoint) (mulScalarVec weight2 rawStopPoint)
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) res

-- | Find the unsigned distance between a point and a line.
distancePPointToPLine :: PPoint2 -> PLine2 -> ℝ
distancePPointToPLine point line = fst $ distancePPointToPLineWithErr point line

distancePPointToPLineWithErr :: PPoint2 -> PLine2 -> (ℝ, UlpSum)
distancePPointToPLineWithErr point line = (res, UlpSum $ resErr + normErr + nPVecErr)
  where
    (res, UlpSum resErr)         = distanceCPPointToNPLineWithErr rnpvec normedLine
    (rnpvec, UlpSum nPVecErr)    = canonicalizePPoint2WithErr point
    (normedLine, UlpSum normErr) = normalizePLine2WithErr line

-- FIXME: use the distance to increase ULP appropriately?
distanceCPPointToNPLineWithErr :: CPPoint2 -> NPLine2 -> (ℝ, UlpSum)
distanceCPPointToNPLineWithErr point (NPLine2 nplvec)
  | valOf 0 foundVal == 0 = error "attempted to get the distance of an ideal point."
  | otherwise = (res, ulpSum)
  where
    (res, UlpSum resErr)           = normOfPLine2WithErr newPLine
    (newPLine, UlpSum newPLineErr) = join2CPPoint2WithErr point linePoint
    (perpLine, UlpSum perpLineErr) = lvec ⨅+ npvec
    (PLine2 lvec)                  = forcePLine2Basis (PLine2 nplvec)
    (CPPoint2 npvec)               = forceCPPoint2Basis point
    (linePoint, UlpSum lpErr)      = fromJust $ canonicalizeIntersectionWithErr (PLine2 lvec) (PLine2 perpLine)
    ulpSum                         = UlpSum $ lpErr + perpLineErr + newPLineErr + resErr
    foundVal                       = getVals [GEPlus 1, GEPlus 2] $ (\(CPPoint2 (GVec vals)) -> vals) point

-- | Determine if two points are on the same side of a given line.
pPointsOnSameSideOfPLine :: PPoint2 -> PPoint2 -> PLine2 -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  -- Return nothing if one of the points is on the line.
  |  abs foundP1 < realToFrac unlikeP1Err ||
     abs foundP2 < realToFrac unlikeP2Err    = Nothing
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
-- FIXME: use CPPoint2
distanceBetweenPPointsWithErr :: PPoint2 -> PPoint2 -> (ℝ, UlpSum)
distanceBetweenPPointsWithErr point1 point2 = (res, UlpSum $ resErr + cpoint1Err + cpoint2Err)
  where
    (res, UlpSum resErr) = distanceBetweenCPPointsWithErr cpoint1 cpoint2
    (cpoint1, UlpSum cpoint1Err) = canonicalizePPoint2WithErr point1
    (cpoint2, UlpSum cpoint2Err) = canonicalizePPoint2WithErr point2

distanceBetweenCPPointsWithErr :: CPPoint2 -> CPPoint2 -> (ℝ, UlpSum)
distanceBetweenCPPointsWithErr cpoint1 cpoint2 = (res, ulpSum)
  where
    (res, UlpSum resErr)           = normOfPLine2WithErr newPLine
    (newPLine, UlpSum newPLineErr) = join2CPPoint2WithErr cpoint1 cpoint2
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
    (res, ulpSum) = p1 ⎣+ p2
    (PLine2 p1) = forcePLine2Basis $ PLine2 pv1
    (PLine2 p2) = forcePLine2Basis $ PLine2 pv2

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: PLine2 -> PPoint2 -> ℝ -> PPoint2
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: PLine2 -> PPoint2 -> ℝ -> (PPoint2, UlpSum)
pPointOnPerpWithErr pline rppoint d = (PPoint2 res,
                                        ulpTotal)
  where
    res = motor•pvec•reverseGVec motor
    (NPLine2 rlvec,UlpSum lErr)    = normalizePLine2WithErr pline
    (perpLine,UlpSum perpPLineErr) = lvec ⨅+ pvec
    (PLine2 lvec)                  = forcePLine2Basis $ PLine2 rlvec
    (PPoint2 pvec)                 = forcePPoint2Basis rppoint
    (motor, UlpSum motorErr) = addVecPairWithErr (perpLine • gaIScaled) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaIScaled = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    gaIErr :: Rounded 'TowardInf ℝ
    gaIErr = abs $ realToFrac $ doubleUlp $ d/2
    ulpTotal = UlpSum $ gaIErr + perpPLineErr + lErr + motorErr

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
outputIntersectsLineSeg source (l1, UlpSum ulpL1)
  -- handle the case where a segment that is an input to the node is checked against.
  | isNothing canonicalizedIntersection = Right $ plinesIntersectIn pl1 pl2
  | intersectionDistance < foundError = pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) 1
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
  | otherwise = pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) ulpScale
  where
    foundError :: ℝ
    foundError = realToFrac $ (ulpL1 + ulpPL1 + ulpPl2 + npl1Err + npl2Err + rawIntersectionErr + intersectionDistanceErr + canonicalizedSourceErr)
    -- | the multiplier used to expand the hitcircle of an endpoint.
    ulpScale :: ℝ
    ulpScale = realToFrac $ ulpMultiplier * (realToFrac travelUlpMul) * (abs (realToFrac angle)+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, UlpSum npl1Err) = normalizePLine2WithErr pl1
    (npl2, UlpSum npl2Err) = normalizePLine2WithErr pl2
    (pl2, UlpSum ulpPl2) = eToPLine2WithErr l1
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
pLineIntersectsLineSeg (pl1, UlpSum ulpPL1) (l1, UlpSum ulpL1) ulpScale
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (startErr + endErr + ulpStart+ulpEnd+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nulpScale: " <> show ulpScale <> "\nrawIntersect" <> show rawIntersect <> dumpULPs
  | hasIntersection && valOf 0 foundVal == 0 = error "intersection, but cannot cannonicalize."
  | hasIntersection && startDistance <= ulpStartSum = Left $ HitStartPoint l1
  | hasIntersection && endDistance <= ulpEndSum = Left $ HitEndPoint l1
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum (realToFrac ulpStartSum), UlpSum (realToFrac ulpEndSum), UlpSum ulpPL1, UlpSum ulpPL2, UlpSum ulpI, UlpSum ulpC)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum (realToFrac ulpStartSum), UlpSum (realToFrac ulpEndSum), UlpSum 0, UlpSum 0)
  | otherwise = Left $ NoIntersection ((\(PPoint2 v) -> CPPoint2 v) rawIntersect) (UlpSum 0, UlpSum 0, UlpSum 0, UlpSum 0)
  where
    (startDistance, UlpSum ulpStart) = distanceBetweenCPPointsWithErr rawIntersection startPPoint
    (endDistance, UlpSum ulpEnd) = distanceBetweenCPPointsWithErr rawIntersection endPPoint
    (startPPoint, PPoint2PosErr startErr) = eToCPPoint2WithErr $ startPoint l1
    (endPPoint, PPoint2PosErr endErr) = eToCPPoint2WithErr $ endPoint l1
    ulpStartSum, ulpEndSum :: ℝ
    ulpStartSum = realToFrac $ ulpTotal+ulpStart
    ulpEndSum = realToFrac $ ulpTotal+ulpEnd
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    -- Note: we do not use ulpC here.
    ulpTotal
      | ulpPL1 < 0 || ulpPL2 < 0 || ulpL1 < 0 || ulpI < 0 = error "negative ULP?\n"
      | otherwise = ulpPL1 + ulpPL2 + ulpL1 + (ulpI * (realToFrac ulpScale))
    dumpULPs = "ulpPL1: " <> show ulpPL1 <> "\nulpPL2: " <> show ulpPL2 <> "\nulpL1: " <> show ulpL1 <> "\nulpI: " <> show ulpI <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum ulpEndSum
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum ulpC) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum ulpI) = pLineIntersectionWithErr pl1 pl2
    (pl2, UlpSum ulpPL2) = eToPLine2WithErr l1

-- | Check if/where two line segments intersect.
lineSegIntersectsLineSeg :: (LineSeg, UlpSum) -> (LineSeg, UlpSum) -> Either Intersection PIntersection
lineSegIntersectsLineSeg (l1, UlpSum ulpL1) (l2, UlpSum ulpL2)
  | plinesIntersectIn pl1 pl2 == PParallel = Right PParallel
  | plinesIntersectIn pl1 pl2 == PAntiParallel = Right PAntiParallel
  | hasRawIntersection && distance (startPoint l1) (endPoint l1) < realToFrac (start1Err + end1Err + ulpStart1+ulpEnd1+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasRawIntersection && distance (startPoint l2) (endPoint l2) < realToFrac (start2Err + end2Err + ulpStart2+ulpEnd2+ulpTotal) = error $ "cannot resolve endpoints of segment: " <> show l1 <> ".\nulpTotal: " <> show ulpTotal <> "\nrawIntersection" <> show rawIntersection <> dumpULPs
  | hasIntersection && plinesIntersectIn pl1 pl2 == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn pl1 pl2 == PAntiCollinear = Right PAntiCollinear
  -- FIXME: why do we return a start/endpoint here?
  | hasIntersection && startDistance1 <= ulpStartSum1 = Left $ HitStartPoint l1
  | hasIntersection && endDistance1 <= ulpEndSum1 = Left $ HitEndPoint l1
  | hasIntersection && startDistance2 <= ulpStartSum2 = Left $ HitStartPoint l2
  | hasIntersection && endDistance2 <= ulpEndSum2 = Left $ HitEndPoint l2
  | hasIntersection = Right $ IntersectsIn rawIntersection (UlpSum $ realToFrac ulpStartSum1, UlpSum $ realToFrac ulpEndSum1, UlpSum $ realToFrac ulpStartSum2, UlpSum $ realToFrac ulpEndSum2, UlpSum ulpTotal, UlpSum ulpI)
  | hasRawIntersection = Left $ NoIntersection rawIntersection (UlpSum $ realToFrac ulpStartSum1, UlpSum $ realToFrac ulpEndSum1, UlpSum $ realToFrac ulpStartSum2, UlpSum $ realToFrac ulpEndSum2)
  | otherwise = Left $ NoIntersection ((\(PPoint2 p) -> CPPoint2 p) rawIntersect) (UlpSum 0, UlpSum 0, UlpSum 0, UlpSum 0)
  where
    ulpStartSum1, ulpEndSum1, ulpStartSum2, ulpEndSum2 :: ℝ
    ulpStartSum1 = realToFrac $ ulpTotal+ulpStart1
    ulpStartSum2 = realToFrac $ ulpTotal+ulpStart2
    ulpEndSum1 = realToFrac $ ulpTotal+ulpEnd1
    ulpEndSum2 = realToFrac $ ulpTotal+ulpEnd2
    (startDistance1, UlpSum ulpStart1) = distanceBetweenCPPointsWithErr rawIntersection start1PPoint
    (startDistance2, UlpSum ulpStart2) = distanceBetweenCPPointsWithErr rawIntersection start2PPoint
    (endDistance1, UlpSum ulpEnd1) = distanceBetweenCPPointsWithErr rawIntersection end1PPoint
    (endDistance2, UlpSum ulpEnd2) = distanceBetweenCPPointsWithErr rawIntersection end2PPoint
    (start1PPoint, PPoint2PosErr start1Err) = eToCPPoint2WithErr $ startPoint l1
    (end1PPoint, PPoint2PosErr end1Err) = eToCPPoint2WithErr $ endPoint l1
    (start2PPoint, PPoint2PosErr start2Err) = eToCPPoint2WithErr $ startPoint l2
    (end2PPoint, PPoint2PosErr end2Err) = eToCPPoint2WithErr $ endPoint l2
    (pl1, UlpSum ulpPL1) = eToPLine2WithErr l1
    (pl2, UlpSum ulpPL2) = eToPLine2WithErr l2
    -- | the sum of all ULPs. used to expand the hitcircle of an endpoint.
    ulpTotal
      | ulpPL1 < 0 || ulpPL2 < 0 || ulpL1 < 0 || ulpL2 < 0 || ulpI < 0 || ulpC < 0 = error "negative ULP?\n"
      | otherwise = ulpPL1 + ulpPL2 + ulpL1 + ulpL2 + (ulpI * ulpScale)
    ulpScale = 120 + ulpMultiplier * (realToFrac angle+angleErr) * (realToFrac angle+angleErr)
    (angle, UlpSum angleErr) = angleBetweenWithErr npl1 npl2
    (npl1, _) = normalizePLine2WithErr pl1
    (npl2, _) = normalizePLine2WithErr pl2
    dumpULPs = "ulpPL1: " <> show ulpPL1 <> "\nulpPL2: " <> show ulpPL2 <> "\nulpL1: " <> show ulpL1 <> "\nulpI: " <> show ulpI <> "\n"
    hasIntersection = hasRawIntersection && onSegment l1 rawIntersection ulpStartSum1 ulpEndSum1 && onSegment l2 rawIntersection ulpStartSum2 ulpEndSum2
    hasRawIntersection = valOf 0 foundVal /= 0
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) rawIntersect
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    (rawIntersection, UlpSum ulpC) = canonicalizePPoint2WithErr rawIntersect
    (rawIntersect, UlpSum ulpI) = pLineIntersectionWithErr pl1 pl2

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> CPPoint2 -> ℝ -> ℝ -> Bool
onSegment ls i startUlp endUlp =
     (startDistance <= startFudgeFactor)
  || (midDistance <= (lengthOfSegment/2) + midFudgeFactor)
  || (endDistance <= endFudgeFactor)
  where
    (startDistance, UlpSum startDistanceUlp) = distanceBetweenCPPointsWithErr startPPoint i
    (midDistance, UlpSum midDistanceUlp) = distanceBetweenCPPointsWithErr midPPoint i
    (endDistance, UlpSum endDistanceUlp) = distanceBetweenCPPointsWithErr endPPoint i
    (startPPoint, PPoint2PosErr startErr) = eToCPPoint2WithErr $ startPoint ls
    (midPPoint, UlpSum midErr) = cPPointBetweenCPPointsWithErr startPPoint endPPoint 0.5 0.5
    (endPPoint, PPoint2PosErr endErr) = eToCPPoint2WithErr $ endPoint ls
    lengthOfSegment = distance (startPoint ls) (endPoint ls)
    startFudgeFactor, midFudgeFactor, endFudgeFactor :: ℝ
    startFudgeFactor = realToFrac $ realToFrac startUlp + startDistanceUlp + startErr
    midFudgeFactor = realToFrac $ abs (realToFrac $ doubleUlp lengthOfSegment) + midDistanceUlp + midErr
    endFudgeFactor = realToFrac $ realToFrac endUlp + endDistanceUlp + endErr

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
        sameLineSeg = plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PCollinear
        sameMiddlePoint = p2 == addPoints p1 s1

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
meet2PLine2WithErr :: NPLine2 -> NPLine2 -> (PPoint2, UlpSum)
meet2PLine2WithErr (NPLine2 plr1) (NPLine2 plr2) = (PPoint2 res,
                                                    resUlp)
  where
    (res, resUlp) = pv1 ⎤+ pv2
    (PLine2 pv1) = forcePLine2Basis $ PLine2 plr1
    (PLine2 pv2) = forcePLine2Basis $ PLine2 plr2

newtype PPoint2PosErr = PPoint2PosErr (Rounded 'TowardInf ℝ)

-- | Create a projective point from a euclidian point.
eToPPoint2 :: Point2 -> PPoint2
eToPPoint2 point = fst $ eToPPoint2WithErr point

eToPPoint2WithErr :: Point2 -> (PPoint2, PPoint2PosErr)
eToPPoint2WithErr (Point2 (x,y)) = (PPoint2 res, resUlp)
  where
    (CPPoint2 res, resUlp) = makeCPPoint2WithErr x y

-- | Create a projective point from a euclidian point.
eToCPPoint2 :: Point2 -> CPPoint2
eToCPPoint2 point = fst $ eToCPPoint2WithErr point

eToCPPoint2WithErr :: Point2 -> (CPPoint2, PPoint2PosErr)
eToCPPoint2WithErr (Point2 (x,y)) = (res, resUlp)
  where
    (res, resUlp) = makeCPPoint2WithErr x y

-- | create a canonical euclidian projective point from the given coordinates.
--   Really, just wraps makeCPPoint2WithErr
makeCPPoint2 :: ℝ -> ℝ -> CPPoint2
makeCPPoint2 x y = fst $ makeCPPoint2WithErr x y

-- | Create a canonical euclidian projective point from the given coordinates, with error.
makeCPPoint2WithErr :: ℝ -> ℝ -> (CPPoint2, PPoint2PosErr)
makeCPPoint2WithErr x y = (pPoint
                         , ulpSum)
  where
    pPoint = CPPoint2 $ GVec $ foldl' addVal [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (negate x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]
    ulpSum = PPoint2PosErr $ abs (realToFrac $ doubleUlp x) + abs (realToFrac $ doubleUlp y)

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
pPointToPoint2 :: PPoint2 -> Maybe Point2
pPointToPoint2 (PPoint2 (GVec vals)) = if infinitePoint
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

-- | runtime basis coersion. ensure all of the '0' components exist on a PPoint2.
forceCPPoint2Basis :: CPPoint2 -> CPPoint2
forceCPPoint2Basis pt@(CPPoint2 pvec@(GVec [GVal _ gnum1, GVal _ gnum2, GVal _ gnum3]))
  | gnum1 == fromList [GEZero 1, GEPlus 1] &&
    gnum2 == fromList [GEZero 1, GEPlus 2] &&
    gnum3 == fromList [GEPlus 1, GEPlus 2]    = pt
  | otherwise                                 = CPPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec
forceCPPoint2Basis (CPPoint2 pvec)            = CPPoint2 $ forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] pvec

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
             $ abs (realToFrac $ doubleUlp $ y1*x2)
             + abs (realToFrac $ doubleUlp $ x1*y2)
             + abs (realToFrac $ doubleUlp a)
             + abs (realToFrac $ doubleUlp b)
             + abs (realToFrac $ doubleUlp c)

-- | Get the sum of the error involved in storing the values in a given PLine2.
ulpOfPLine2 :: PLine2 -> UlpSum
ulpOfPLine2 (PLine2 (GVec vals)) = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVals [GEZero 1] vals
                                   ,getVals [GEPlus 1] vals
                                   ,getVals [GEPlus 2] vals]

-- | Get the sum of the error involved in storing the values in a given Line Segment.
ulpOfLineSeg :: LineSeg -> UlpSum
ulpOfLineSeg (LineSeg (Point2 (x1,y1)) (Point2 (x2,y2))) = UlpSum $ sum $ abs . realToFrac . doubleUlp <$> [x1, y1, x2, y2]

-- | Get the sum of the error involved in storing the values in a given PPoint2.
ulpOfCPPoint2 :: CPPoint2 -> UlpSum
ulpOfCPPoint2 (CPPoint2 (GVec vals)) = UlpSum $ sum $ abs . realToFrac . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                       [getVals [GEZero 1, GEPlus 1] vals
                                       ,getVals [GEZero 1, GEPlus 2] vals
                                       ,getVals [GEPlus 1, GEPlus 2] vals]

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalar. ----
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
    ulpSum = UlpSum $ abs (realToFrac $ doubleUlp $ x*x) + abs (realToFrac $ doubleUlp $ x*x) + abs (realToFrac $ doubleUlp preRes) + abs (realToFrac $ doubleUlp res)
    (Point2 (x,y)) = pToEPoint2 ppoint

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
canonicalizePPoint2WithErr :: PPoint2 -> (CPPoint2, UlpSum)
canonicalizePPoint2WithErr point@(PPoint2 (GVec rawVals))
  | valOf 0 foundVal == 0 = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = ((\(PPoint2 v) -> CPPoint2 v) point, ulpSum)
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
    foundVal = getVals [GEPlus 1, GEPlus 2] rawVals
    ulpSum = ulpOfCPPoint2 res

-- | Canonicalize the intersection resulting from two PLines.
-- NOTE: Returns nothing when the PLines are (anti)parallel.
canonicalizeIntersectionWithErr :: PLine2 -> PLine2 -> Maybe (CPPoint2, UlpSum)
canonicalizeIntersectionWithErr pl1 pl2
  | valOf 0 foundVal == 0 = Nothing
  | otherwise = Just (cpp1, ulpSum)
  where
    (cpp1, UlpSum canonicalizationErr) = canonicalizePPoint2WithErr pp1
    (pp1, UlpSum intersectionErr) = pLineIntersectionWithErr pl1 pl2
    ulpSum = UlpSum $ intersectionErr + canonicalizationErr
    foundVal = getVals [GEPlus 1, GEPlus 2] $ (\(PPoint2 (GVec vals)) -> vals) pp1

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
    ulpSum = UlpSum $ abs (realToFrac $ doubleUlp res) + sqNormErr

-- | find the squared norm of a given PLine2
sqNormOfPLine2WithErr :: PLine2 -> (ℝ, UlpSum)
sqNormOfPLine2WithErr (PLine2 (GVec vals)) = (res, ulpSum)
  where
    res = a*a+b*b
    a = valOf 0 $ getVals [GEPlus 1] vals
    b = valOf 0 $ getVals [GEPlus 2] vals
    ulpSum = UlpSum $ abs (realToFrac $ doubleUlp $ a*a) + abs (realToFrac $ doubleUlp $ b*b) + abs (realToFrac $ doubleUlp res)

