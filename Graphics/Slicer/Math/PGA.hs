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

module Graphics.Slicer.Math.PGA(PPoint2(PPoint2), PLine2(PLine2), Pointable(canPoint, pPointOf, ePointOf), Arcable(hasArc, outOf), addPPoint2s, eToPPoint2, pToEPoint2, canonicalizePPoint2, eToPLine2, combineConsecutiveLineSegs, Intersection(HitStartPoint, HitEndPoint, NoIntersection), dualAngle, pLineIsLeft, lineIntersection, plinesIntersectIn, PIntersection (PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn), dualPPoint2, dualPLine2, dual2DGVec, join2PPoint2, translatePerp, flipPLine2, pointOnPerp, angleBetween, lineIsLeft, distancePPointToPLine, plineFromEndpoints, intersectsWith, SegOrPLine2, pPointsOnSameSideOfPLine, normalizePLine2, distanceBetweenPPoints, distanceBetween2PLine2s, meet2PLine2, distanceBetweenPPointsWithErr, forcePLine2Basis, idealNormPPoint2, idealPPoint2, lineIntersectsPLine, pLineFromEndpointsWithErr, pPointBetweenPPoints, reverseGVec, translateRotatePPoint2, ulpOfLineSeg) where

import Prelude (Eq, Show, Ord, (==), ($), (*), (-), Bool(True), (&&), (<$>), any, otherwise, (>), (>=), (<=), (+), sqrt, negate, (/), (||), (<), (<>), abs, show, error, sin, cos, realToFrac, fst, sum, (.))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList, catMaybes)

import Data.Set (Set, singleton, fromList, elems)

import Data.Number.BigFloat (BigFloat, PrecPlus20, Eps1)

import Safe (lastMay, initSafe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, startPoint, fudgeFactor, distance)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣), (⎤), (⨅), (⨅+), (∧), (•), addVal, addVecPair, divVecScalar, getVals, mulScalarVec, scalarPart, valOf, vectorPart, hpDivVecScalar)

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
  | IntersectsIn !PPoint2
  deriving (Show, Eq)

-- | Translate a point by another point.
addPPoint2s :: PPoint2 -> PPoint2 -> PPoint2
addPPoint2s pPoint1 pPoint2 = PPoint2 $ addVecPair (rawPPoint2 $ idealPPoint2 pPoint1) (rawPPoint2 $ idealPPoint2 pPoint2)
  where
    rawPPoint2 (PPoint2 v) = v

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: PLine2 -> PLine2 -> PIntersection
plinesIntersectIn pl1 pl2
  | intersectPoint == PPoint2 (GVec [])
  || (idealNormPPoint2 intersectPoint < fudgeFactor
     && (intersectAngle >= 1 ||
         intersectAngle <= -1 ))       = if intersectAngle > 0
                                         then PCollinear
                                         else PAntiCollinear
  | intersectAngle >  1-fudgeFactor    = PParallel
  | intersectAngle < -1+fudgeFactor    = PAntiParallel
  | intersectAngle >  1+fudgeFactor    = error "too big of an angle?"
  | intersectAngle < -1-fudgeFactor    = error "too small of an angle?"
  -- FIXME: remove the canonicalization from this function, moving it to the callers.
  | otherwise                                = IntersectsIn res
  where
    intersectAngle = angleBetween pl1 pl2
    (intersectPoint, intersectUlp) = intersectionWithErr pl1 pl2
    (res, resUlp) = canonicalizeIntersectionWithErr pl1 pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: PLine2 -> PLine2 -> Maybe Bool
pLineIsLeft line1 line2
  | dualAngle line1 line2 == 0 = Nothing
  | otherwise                  = Just $ dualAngle line1 line2 > 0

-- | Find out where two lines intersect, returning a projective point. Note that this should only be used when you can guarantee these are not collinear.
intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = meet2PLine2 pl1 pl2

intersectionWithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
intersectionWithErr pl1 pl2 = meet2PLine2WithErr pl1 pl2

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
    (PLine2 lvec, UlpSum normErr)  = normalizePLine2WithErr line
    (perpLine, UlpSum perpLineErr) = lvec ⨅+ pvec
    (linePoint, UlpSum lpErr)      = canonicalizeIntersectionWithErr (PLine2 lvec) (PLine2 perpLine)
    ulpSum                         = UlpSum $ lpErr + perpLineErr + normErr + newPLineErr + resErr

-- | Determine if two points are on the same side of a given line.
pPointsOnSameSideOfPLine :: PPoint2 -> PPoint2 -> PLine2 -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  -- Return nothing if one of the points is on the line.
  |  valOf 0 (getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv1 ⎤ lv1) == 0 ||
     valOf 0 (getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv2 ⎤ lv1) == 0    = Nothing
    | otherwise = Just $ isPositive (valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv1 ⎤ lv1) == isPositive (valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv2 ⎤ lv1)
  where
    (PPoint2 pv1) = forcePPoint2Basis point1
    (PPoint2 pv2) = forcePPoint2Basis point2
    (PLine2 lv1) = forcePLine2Basis line
    gValOf (GVec a) = a
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
distanceBetween2PLine2s :: PLine2 -> PLine2 -> ℝ
distanceBetween2PLine2s = angleBetween

-- | Return the sine of the angle between the two lines. results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
-- NOTE: normalizes inputs.
angleBetween :: PLine2 -> PLine2 -> ℝ
angleBetween pl1 pl2 = scalarPart $ pv1 ⎣ pv2
  where
    (PLine2 pv1) = normalizePLine2 pl1
    (PLine2 pv2) = normalizePLine2 pl2

-- | Find the cosine of the angle between the two lines. results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when "right".
dualAngle :: PLine2 -> PLine2 -> ℝ
dualAngle line1@(PLine2 lvec1) line2 = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
  where
    (PLine2 lvec2)      = normalizePLine2 line2
    (PPoint2 iPointVec) = canonicalizePPoint2 $ meet2PLine2 line1 line2
    motor = addVecPair (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
    antiMotor = addVecPair (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
    -- I, the infinite point.
    gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerp :: PLine2 -> PPoint2 -> ℝ -> PPoint2
pPointOnPerp pline ppoint d = fst $ pPointOnPerpWithErr pline ppoint d

-- | Find a projective point a given distance along a line perpendicularly bisecting the given line at a given point.
pPointOnPerpWithErr :: PLine2 -> PPoint2 -> ℝ -> (PPoint2, UlpSum)
pPointOnPerpWithErr pline (PPoint2 pvec) d = (PPoint2 res,
                                               ulpTotal)
  where
    res = motor•pvec•reverseGVec motor
    (PLine2 lvec,UlpSum lErr)  = normalizePLine2WithErr pline
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
    NoIntersection
  | HitStartPoint !LineSeg !Point2
  | HitEndPoint !LineSeg !Point2
  deriving Show

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2 = Either LineSeg PLine2

-- | Check if/where lines/line segments intersect.
-- entry point usable for all intersection needs, but it's faster to use other functions when you KNOW there MUST be an intersection.
-- FIXME: take UlpSums here.
intersectsWith :: SegOrPLine2 -> SegOrPLine2 -> Either Intersection PIntersection
intersectsWith (Left l1)   (Left l2)   =         lineIntersection    l1  l2
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   pl1 pl2
intersectsWith (Left l1)   (Right pl1) =         lineIntersectsPLine l1  pl1
intersectsWith (Right pl1) (Left l1)   =         lineIntersectsPLine l1  pl1

-- | Check if/where two line segments intersect.
-- FIXME: should we be returning a segment, for PCollinear and PAntiCollinear?
lineIntersection :: LineSeg -> LineSeg -> Either Intersection PIntersection
lineIntersection l1 l2
  | plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PParallel = Right PParallel
  | plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PAntiParallel = Right PAntiParallel
  | hasIntersection && plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn (eToPLine2 l1) (eToPLine2 l2) == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ startPoint l1) < snapFudgeFactor1 = Left $ HitStartPoint l1 intersection
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ endPoint l1) < snapFudgeFactor1 = Left $ HitEndPoint l1 intersection
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ startPoint l2) < snapFudgeFactor2 = Left $ HitStartPoint l2 intersection
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ endPoint l2) < snapFudgeFactor2 = Left $ HitEndPoint l2 intersection
  | hasIntersection = Right $ IntersectsIn rawIntersection
  | otherwise = Left NoIntersection
  where
    snapFudgeFactor1 = fudgeFactor * 15
    snapFudgeFactor2 = fudgeFactor * 15
    hasIntersection = onSegment l1 rawIntersection 0 0 && onSegment l2 rawIntersection 0 0
    intersection = pToEPoint2 rawIntersection
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    rawIntersection = canonicalizePPoint2 $ intersectionOf (eToPLine2 l1) (eToPLine2 l2)

-- | Check if/where a line segment and a PLine intersect.
lineIntersectsPLine :: LineSeg -> PLine2 -> Either Intersection PIntersection
lineIntersectsPLine l1 pl1
  | plinesIntersectIn (eToPLine2 l1) pl1 == PParallel = Right PParallel
  | plinesIntersectIn (eToPLine2 l1) pl1 == PAntiParallel = Right PAntiParallel
  | hasIntersection && plinesIntersectIn (eToPLine2 l1) pl1 == PCollinear = Right PCollinear
  | hasIntersection && plinesIntersectIn (eToPLine2 l1) pl1 == PAntiCollinear = Right PAntiCollinear
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ startPoint l1) < snapFudgeFactor = Left $ HitStartPoint l1 intersection
  | hasIntersection && distanceBetweenPPoints rawIntersection (eToPPoint2 $ endPoint l1) < snapFudgeFactor = Left $ HitEndPoint l1 intersection
  | hasIntersection = Right $ IntersectsIn rawIntersection
  | otherwise = Left NoIntersection
  where
    snapFudgeFactor = fudgeFactor * 15
    hasIntersection = onSegment l1 rawIntersection 0 0
    intersection = pToEPoint2 rawIntersection
    -- FIXME: remove the canonicalization from this function, moving it to the callers.
    rawIntersection = canonicalizePPoint2 $ intersectionOf (normalizePLine2 $ eToPLine2 l1) (normalizePLine2 pl1)

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> PPoint2 -> ℝ -> ℝ -> Bool
onSegment ls i startUlp endUlp =
  any (==True) [ startDistance <= startFudgeFactor
               , midDistance <= (lengthOfSegment/2) + midFudgeFactor
               , endDistance <= endFudgeFactor
               ]
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

-- | Can this node be resolved into a point in 2d space?
class Pointable a where
  canPoint :: a -> Bool
  pPointOf :: a -> PPoint2
  ePointOf :: a -> Point2

-- | does this node have an output (resulting) pLine?
class Arcable a where
  hasArc :: a -> Bool
  outOf :: a -> PLine2

-- | The join operator in 2D PGA, which is just the meet operator operating in the dual space.
(∨) :: GVec -> GVec -> GVec
(∨) a b = dual2DGVec $ GVec $ foldl' addVal [] res
  where
    (GVec res) = dual2DGVec a ⎤  dual2DGVec b
infixl 9 ∨

-- | a typed join function. join two points, returning a line.
join2PPoint2 :: PPoint2 -> PPoint2 -> PLine2
join2PPoint2 pp1 pp2 = fst $ join2PPoint2WithErr pp1 pp2

meet2PLine2 :: PLine2 -> PLine2 -> PPoint2
meet2PLine2 pl1 pl2 = fst $ meet2PLine2WithErr pl1 pl2

-- | a typed join function. join two points, returning a line.
join2PPoint2WithErr :: PPoint2 -> PPoint2 -> (PLine2, UlpSum)
join2PPoint2WithErr pp1 pp2 = (res,
                               ulpSum)
  where
    res = PLine2 $ pv1 ∨ pv2
    ulpSum = UlpSum $ ulpOfPLine2 res
    (PPoint2 pv1) = forcePPoint2Basis pp1
    (PPoint2 pv2) = forcePPoint2Basis pp2

-- | A typed meet function. the meeting of two lines is a point.
meet2PLine2WithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
meet2PLine2WithErr pl1 pl2 = (res,
                              ulpSum)
  where
    res =  PPoint2 $ pv1 ⎤ pv2
    ulpSum = UlpSum $ ulpOfPPoint2 res
    (PLine2 pv1) = forcePLine2Basis pl1
    (PLine2 pv2) = forcePLine2Basis pl2

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

-- | Create an ideal point from of a projective point.
-- Note: not yet used.
idealPPoint2 :: PPoint2 -> PPoint2
idealPPoint2 (PPoint2 (GVec vals)) = PPoint2 $ GVec $ foldl' addVal []
                                     [
                                       GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] vals) (fromList [GEZero 1, GEPlus 1])
                                     , GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] vals) (fromList [GEZero 1, GEPlus 2])
                                     ]

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
-- FIXME: does not generate ULP value.
eToPLine2 :: LineSeg -> PLine2
eToPLine2 l1 = fst $ eToPLine2WithErr l1

-- | Create a projective line from a pair of euclidian points.
plineFromEndpoints :: Point2 -> Point2 -> PLine2
plineFromEndpoints point1 point2 = fst $ pLineFromEndpointsWithErr point1 point2

-- | Convert from a PPoint2 to it's associated PLine.
dualPPoint2 :: PPoint2 -> GVec
dualPPoint2 (PPoint2 vec) = dual2DGVec vec

-- | Convert from a PLine to it's associated projective point.
dualPLine2 :: PLine2 -> GVec
dualPLine2 (PLine2 vec) = dual2DGVec vec

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
ulpOfPLine2 :: PLine2 -> ℝ
ulpOfPLine2 (PLine2 (GVec vals)) = sum $ abs . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                   [getVals [GEZero 1] vals
                                   ,getVals [GEPlus 1] vals
                                   ,getVals [GEPlus 2] vals]

-- | Get the sum of the error involved in storing the values in a given Line Segment.
ulpOfLineSeg :: LineSeg -> ℝ
ulpOfLineSeg (LineSeg (Point2 (x1,y1)) (Point2 (x2,y2))) = sum $ abs . doubleUlp <$> [x1, y1, x2, y2, x1+x2, y1+y2]

-- | Get the sum of the error involved in storing the values in a given PPoint2.
ulpOfPPoint2 :: PPoint2 -> ℝ
ulpOfPPoint2 (PPoint2 (GVec vals)) = sum $ abs . doubleUlp . (\(GVal r _) -> r) <$> catMaybes
                                     [getVals [GEZero 1, GEPlus 1] vals
                                     ,getVals [GEZero 1, GEPlus 2] vals
                                     ,getVals [GEPlus 1, GEPlus 2] vals]

--------------------------------------------------------------
---- Utillity functions that use sqrt(), or divVecScalar. ----
---- Standard precision:                                  ----
--------------------------------------------------------------

-- | find the idealized norm of a projective point.
idealNormPPoint2 :: PPoint2 -> ℝ
idealNormPPoint2 ppoint = sqrt (x*x+y*y)
  where
    (Point2 (x,y)) = pToEPoint2 ppoint

-- | Normalization of euclidian points is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
canonicalizePPoint2 :: PPoint2 -> PPoint2
canonicalizePPoint2 point = fst $ canonicalizePPoint2WithErr point

-- | Normalize a PLine2.
normalizePLine2 :: PLine2 -> PLine2
normalizePLine2 pl = fst $ normalizePLine2WithErr pl

-- | find the norm of a given PLine2
normOfPLine2 :: PLine2 -> ℝ
normOfPLine2 pline = fst $ normOfPLine2WithErr pline

-- | find the squared norm of a given PLine2
sqNormOfPLine2 :: PLine2 -> ℝ
sqNormOfPLine2 pline = fst $ sqNormOfPLine2WithErr pline

-- | Normalization of euclidian points is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
canonicalizePPoint2WithErr :: PPoint2 -> (PPoint2, UlpSum)
canonicalizePPoint2WithErr point@(PPoint2 (GVec rawVals))
  | foundVal == Nothing = (point, UlpSum 0)
  | otherwise = (res, ulpSum)
  where
    res = PPoint2 $ GVec $ foldl' addVal []
          $  ( if getVals [GEZero 1, GEPlus 1] scaledVals == Nothing
               then []
               else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
             )
          <> ( if getVals [GEZero 1, GEPlus 2] scaledVals == Nothing
               then []
               else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
             )
          <> [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    newVec = GVec $ addVal [(GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1]))]
                            (GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2]))
    (GVec scaledVals) = divVecScalar newVec $ valOf 1 $ foundVal
    foundVal = getVals [GEPlus 1, GEPlus 2] rawVals
    ulpSum = UlpSum $ ulpOfPPoint2 res

-- | Canonicalize the intersection resulting from two PLines.
canonicalizeIntersectionWithErr :: PLine2 -> PLine2 -> (PPoint2, UlpSum)
canonicalizeIntersectionWithErr pl1 pl2 = (cpp1, ulpSum)
  where
    (cpp1, (UlpSum canonicalizationErr)) = canonicalizePPoint2WithErr pp1
    (pp1, (UlpSum intersectionErr)) = intersectionWithErr pl1 pl2
    ulpSum = UlpSum $ intersectionErr + canonicalizationErr

-- | Normalize a PLine2.
normalizePLine2WithErr :: PLine2 -> (PLine2, UlpSum)
normalizePLine2WithErr pl@(PLine2 vec) = (res, ulpSum)
  where
    res = PLine2 $ divVecScalar vec $ normOfMyPLine
    (normOfMyPLine, (UlpSum normErr)) = normOfPLine2WithErr pl
    ulpSum = UlpSum $ normOfPLine2 res + normErr

-- | find the norm of a given PLine2
normOfPLine2WithErr :: PLine2 -> (ℝ, UlpSum)
normOfPLine2WithErr pline = (res, ulpSum)
  where
    res = sqrt $ sqNormOfPLine2
    (sqNormOfPLine2, (UlpSum sqNormErr)) = sqNormOfPLine2WithErr pline
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
hpCanonicalizePPoint2 :: PPoint2 -> PPoint2
hpCanonicalizePPoint2 point@(PPoint2 (GVec rawVals))
  | foundVal == Nothing = point
  | otherwise = PPoint2 $ GVec $ foldl' addVal [] $
                ( if getVals [GEZero 1, GEPlus 1] scaledVals == Nothing
                  then []
                  else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
                ) <>
                ( if getVals [GEZero 1, GEPlus 2] scaledVals == Nothing
                  then []
                  else [GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
                ) <>
                [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
  where
    newVec = GVec $ addVal [(GVal (valOf 0 $ getVals [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1]))]
                            (GVal (valOf 0 $ getVals [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2]))
    (GVec scaledVals) = hpDivVecScalar newVec $ realToFrac $ valOf 1 $ foundVal
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
hpCanonicalizeIntersectionOf :: PLine2 -> PLine2 -> PPoint2
hpCanonicalizeIntersectionOf pl1 pl2 = hpCanonicalizePPoint2 $ fst $ intersectionWithErr pl1 pl2
