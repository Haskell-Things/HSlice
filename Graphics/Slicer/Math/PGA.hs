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

module Graphics.Slicer.Math.PGA(PPoint2(PPoint2), PLine2(PLine2), eToPPoint2, pToEPoint2, canonicalizePPoint2, eToPLine2, combineConsecutiveLineSegs, Intersection(HitStartPoint, HitEndPoint, NoIntersection), pLineIsLeft, lineIntersection, plinesIntersectIn, PIntersection (PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn), dualPPoint2, dualPLine2, dual2DGVec, join2PPoint2, translatePerp, flipPLine2, pointOnPerp, angleBetween, lineIsLeft, distancePPointToPLine, plineFromEndpoints, intersectsWith, SegOrPLine2, pPointsOnSameSideOfPLine, normalizePLine2, distanceBetweenPPoints, meet2PLine2, forcePLine2Basis, idealNormPPoint2, lineIntersectsPLine, pPointBetweenPPoints) where

import Prelude (Eq, Show, Ord, (==), ($), (*), (-), Bool, (&&), (<$>), otherwise, (>), (>=), (<=), (+), sqrt, negate, (/), (||), (<), (<>), show, error)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.Ordered (sort, foldt)

import Data.Maybe (Maybe(Just, Nothing), maybeToList)

import Data.Set (Set, singleton, fromList, elems)

import Safe (lastMay, initSafe)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), addPoints, fudgeFactor)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), (⎣), (⎤), (⨅), (∧), (•), addVal, addVecPair, divVecScalar, getVals, mulScalarVec, scalarPart, valOf, vectorPart)

import Graphics.Slicer.Math.Line(combineLineSegs)

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

-- | Determine the intersection point of two projective lines, if applicable. Otherwise, classify the relationship between the two line segments.
plinesIntersectIn :: PLine2 -> PLine2 -> PIntersection
plinesIntersectIn pl1 pl2

  | meet2PLine2 pl1 pl2 == PPoint2 (GVec [])
  || (idealNormPPoint2 (meet2PLine2 pl1 pl2) < fudgeFactor
     && (angleBetween pl1 pl2 >= 1 ||
         angleBetween pl1 pl2 <= -1 ))          = if angleBetween pl1 pl2 > 0
                                                           then PCollinear
                                                           else PAntiCollinear
  | scalarPart (pr1 ⎣ pr2) <   1+fudgeFactor &&
    scalarPart (pr1 ⎣ pr2) >   1-fudgeFactor    = PParallel
  | scalarPart (pr1 ⎣ pr2) <  -1+fudgeFactor &&
    scalarPart (pr1 ⎣ pr2) >  -1-fudgeFactor    = PAntiParallel
  | otherwise                                   = IntersectsIn $ intersectionOf pl1 pl2
  where
    (PLine2 pr1) = normalizePLine2 pl1
    (PLine2 pr2) = normalizePLine2 pl2

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
pLineIsLeft :: PLine2 -> PLine2 -> Maybe Bool
pLineIsLeft line1 line2
  | dualAngle line1 line2 == 0 = Nothing
  | otherwise                  = Just $ dualAngle line1 line2 > 0


-- | Return the cosine of the angle between the two lines. results in a value that is positive when the first line points to the "left" of the second given line, and negative when "right".
dualAngle :: PLine2 -> PLine2 -> ℝ
dualAngle line1@(PLine2 lvec1) line2@(PLine2 lvec2) = valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
  where
    (PPoint2 iPointVec) = canonicalizePPoint2 $ meet2PLine2 line1 line2
    motor = addVecPair (lvec1•gaI) (GVec [GVal 1 (singleton G0)])
    antiMotor = addVecPair (lvec1•gaI) (GVec [GVal (-1) (singleton G0)])
    gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

-- | Find out where two lines intersect, returning a projective point. Note that this should only be used when you can guarantee these are not collinear.
intersectionOf :: PLine2 -> PLine2 -> PPoint2
intersectionOf pl1 pl2 = canonicalizePPoint2 $ meet2PLine2 pl1 pl2

-- | Return the sine of the angle between the two lines. results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
angleBetween :: PLine2 -> PLine2 -> ℝ
angleBetween pl1 pl2 = scalarPart $ pv1 ⎣ pv2
  where
    (PLine2 pv1) = forcePLine2Basis (normalizePLine2 pl1)
    (PLine2 pv2) = forcePLine2Basis (normalizePLine2 pl2)

-- | Translate a line a given distance along it's perpendicular bisector.
--   FIXME: I'm not so sure about the math on this one. test suite, please?
translatePerp :: PLine2 -> ℝ -> PLine2
translatePerp pLine@(PLine2 rawPLine) d = PLine2 $ addVecPair m rawPLine
  where
    m = GVec [GVal (d*normOfPLine2 pLine) (singleton (GEZero 1))]

-- | Find a point somewhere along the line between the two points given.
--  requires two weights. the ratio of these weights determines the position of the found points, E.G: 2/1 is 1/3 the way FROM the stopPoint, and 2/3 the way FROM the startPoint.
pPointBetweenPPoints :: PPoint2 -> PPoint2 -> ℝ -> ℝ -> PPoint2
pPointBetweenPPoints (PPoint2 rawStartPoint) (PPoint2 rawStopPoint) weight1 weight2 = canonicalizePPoint2 $ PPoint2 $ addVecPair (mulScalarVec weight1 rawStartPoint) (mulScalarVec weight2 rawStopPoint)

-- | Find the unsigned distance between a point and a line.
distancePPointToPLine :: PPoint2 -> PLine2 -> ℝ
distancePPointToPLine point line = normOfPLine2 $ join2PPoint2 point linePoint
  where
    (PLine2 lvec)  = normalizePLine2 line
    (PPoint2 pvec) = canonicalizePPoint2 point
    perpLine       = PLine2 $ lvec ⨅ pvec
    linePoint      = meet2PLine2 (PLine2 lvec) perpLine

-- | Determine if two points are on the same side of a given line.
-- FIXME: we now have two implementations. speed test them. the second implementation requires one point that is already on the line.
pPointsOnSameSideOfPLine :: PPoint2 -> PPoint2 -> PLine2 -> Maybe Bool
pPointsOnSameSideOfPLine point1 point2 line
  -- Return nothing if one of the points is on the line.
  |  valOf 0 (getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv1 ⎤ lv1) == 0 ||
     valOf 0 (getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv2 ⎤ lv1) == 0    = Nothing
    | otherwise = Just $ isPositive (valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv1 ⎤ lv1) == isPositive (valOf 0 $ getVals [GEZero 1, GEPlus 1, GEPlus 2] $ gValOf $ pv2 ⎤ lv1)
--  | distancePPointToPLine point1 line == 0 || distancePPointToPLine point2 line == 0 = Nothing
--  | otherwise = Just $ pLineIsLeft (join2PPoint2 linePoint point1) line == pLineIsLeft (join2PPoint2 linePoint point2) line
  where
    (PPoint2 pv1) = forcePPoint2Basis point1
    (PPoint2 pv2) = forcePPoint2Basis point2
    (PLine2 lv1) = forcePLine2Basis line
    gValOf (GVec a) = a
    isPositive :: ℝ -> Bool
    isPositive i = i > 0

-- | Find the unsigned distance between two projective points.
distanceBetweenPPoints :: PPoint2 -> PPoint2 -> ℝ
distanceBetweenPPoints point1 point2 = normOfPLine2 $ join2PPoint2 point1 point2

----------------------------------------------------------
-------------- Euclidian Mixed Interface -----------------
----------------------------------------------------------

-- | Intersection events that can only happen with line segments.
data Intersection =
    NoIntersection
  | HitStartPoint !LineSeg !Point2
  | HitEndPoint !LineSeg !Point2
  deriving (Show)

-- | A type alias, for cases where either input is acceptable.
type SegOrPLine2 = Either LineSeg PLine2

-- | Check if/where lines/line segments intersect.
-- entry point usable for all intersection needs, but it's faster to use other functions when you KNOW there MUST be an intersection.
intersectsWith :: SegOrPLine2 -> SegOrPLine2 -> Either Intersection PIntersection
intersectsWith (Left l1)   (Left l2)   =         lineIntersection    l1  l2
intersectsWith (Right pl1) (Right pl2) = Right $ plinesIntersectIn   pl1 pl2
intersectsWith (Left l1)   (Right pl1) =         lineIntersectsPLine l1  pl1
intersectsWith (Right pl1) (Left l1)   =         lineIntersectsPLine l1  pl1

-- | Check if/where two line segments intersect.
lineIntersection :: LineSeg -> LineSeg -> Either Intersection PIntersection
lineIntersection l1@(LineSeg p1 s1) l2@(LineSeg p2 s2)
  | meet2PLine2 (eToPLine2 l1) (eToPLine2 l2) == PPoint2 (GVec [])         = Right $ if angleBetween (eToPLine2 l1) (eToPLine2 l2) > 0
                                                                                     then PCollinear
                                                                                     else PAntiCollinear
  | hasIntersection && intersection == p1 = Left $ HitStartPoint l1 intersection
  | hasIntersection && intersection == addPoints p1 s1 = Left $ HitEndPoint l1 intersection
  | hasIntersection && intersection == p2 = Left $ HitStartPoint l2 intersection
  | hasIntersection && intersection == addPoints p2 s2 = Left $ HitEndPoint l2 intersection
  | hasIntersection = Right $ IntersectsIn rawIntersection
  | scalarPart (rawPLine (eToPLine2 l1) ⎣ rawPLine (eToPLine2 l2)) ==  1 = Right PParallel
  | scalarPart (rawPLine (eToPLine2 l1) ⎣ rawPLine (eToPLine2 l2)) == -1 = Right PAntiParallel
  | otherwise = Left NoIntersection
  where
    rawPLine (PLine2 a) = a
    hasIntersection = onSegment l1 intersection && onSegment l2 intersection
    intersection = intersectionPoint l1 l2
    rawIntersection = intersectionOf (eToPLine2 l1) (eToPLine2 l2)

-- | Check if/where lines/line segments intersect.
lineIntersectsPLine :: LineSeg -> PLine2 -> Either Intersection PIntersection
lineIntersectsPLine l1@(LineSeg p1 s1) pl1
  | meet2PLine2 (eToPLine2 l1) pl1 == PPoint2 (GVec [])          = Right $ if angleBetween (eToPLine2 l1) pl1 > 0
                                                                           then PCollinear
                                                                           else PAntiCollinear
  | onSegment l1 intersection && intersection == p1              = Left $ HitStartPoint l1 intersection
  | onSegment l1 intersection && intersection == addPoints p1 s1 = Left $ HitEndPoint l1 intersection
  | onSegment l1 intersection = Right $ IntersectsIn rawIntersection
  | scalarPart (rawPLine (eToPLine2 l1) ⎣ rawPLine pl1) ==  1 = Right PParallel
  | scalarPart (rawPLine (eToPLine2 l1) ⎣ rawPLine pl1) == -1 = Right PAntiParallel
  | otherwise = Left NoIntersection
  where
    rawPLine (PLine2 a) = a
    intersection = pToEPoint2 rawIntersection
    rawIntersection = intersectionOf (eToPLine2 l1) pl1

-- | Given the result of intersectionPoint, find out whether this intersection point is on the given segment, or not.
onSegment :: LineSeg -> Point2 -> Bool
onSegment (LineSeg p s) i =
  sqNormOfPLine2 (join2PPoint2 (eToPPoint2 p) (eToPPoint2 i))               <= segmentLength &&
  sqNormOfPLine2 (join2PPoint2 (eToPPoint2 i) (eToPPoint2 (addPoints p s))) <= segmentLength
  where
    segmentLength = sqNormOfPLine2 (join2PPoint2 (eToPPoint2 p) (eToPPoint2 (addPoints p s)))

-- | Find the point where two LineSeg segments (might) intersect.
intersectionPoint :: LineSeg -> LineSeg -> Point2
intersectionPoint l1 l2 = intersectPLines (eToPLine2 l1) (eToPLine2 l2)

-- | Check if the second line's direction is on the 'left' side of the first line, assuming they intersect. If they don't intersect, return Nothing.
lineIsLeft :: LineSeg -> LineSeg -> Maybe Bool
lineIsLeft line1 line2 = pLineIsLeft (eToPLine2 line1) (eToPLine2 line2)

-- | Find out where two lines intersect, returning a linear point.
intersectPLines :: PLine2 -> PLine2 -> Point2
intersectPLines pl1 pl2 = pToEPoint2 res
  where
    res = intersectionOf pl1 pl2

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
        sameLineSeg = meet2PLine2 (eToPLine2 l1) (eToPLine2 l2) == PPoint2 (GVec [])
        sameMiddlePoint = p2 == addPoints p1 s1

-- | find a point a given distance along a line perpendicularly bisecting this line at a given point.
pointOnPerp :: LineSeg -> Point2 -> ℝ -> Point2
pointOnPerp line point d = case ppointToPoint2 (canonicalizePPoint2 $ PPoint2 $ (motor•pvec)•reverse motor) of
                             Nothing -> error $ "generated infinite point trying to travel " <> show d <> "along the line perpendicular to " <> show line <> " at point " <> show point <> "\n"
                             Just v -> v
  where
    (PLine2 lvec)  = normalizePLine2 $ eToPLine2 line
    (PPoint2 pvec) = canonicalizePPoint2 $ eToPPoint2 point
    perpLine       = lvec ⨅ pvec
    motor = addVecPair (perpLine • gaI) (GVec [GVal 1 (singleton G0)])
    -- I, in this geometric algebra system. we multiply it times d/2, to shorten the number of multiples we have to do when creating the motor.
    gaI = GVec [GVal (d/2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

------------------------------------------------
----- And now draw the rest of the algebra -----
------------------------------------------------

-- | A projective point in 2D space.
newtype PPoint2 = PPoint2 GVec
  deriving (Eq, Ord, Generic, NFData, Show)

-- | A projective line in 2D space.
newtype PLine2 = PLine2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | The join operator in 2D PGA, which is just the meet operator operating in the dual space.
(∨) :: GVec -> GVec -> GVec
(∨) a b = dual2DGVec $ GVec $ foldl' addVal [] res
  where
    (GVec res) = dual2DGVec a ⎤  dual2DGVec b
infixl 9 ∨

-- | a typed join function. join two points, returning a line.
join2PPoint2 :: PPoint2 -> PPoint2 -> PLine2
join2PPoint2 pp1 pp2 = PLine2 $ pv1 ∨ pv2
  where
    (PPoint2 pv1) = forcePPoint2Basis pp1
    (PPoint2 pv2) = forcePPoint2Basis pp2

-- | A typed meet function. two lines meet at a point.
meet2PLine2 :: PLine2 -> PLine2 -> PPoint2
meet2PLine2 pl1 pl2 = PPoint2 $ pv1 ⎤ pv2
  where
    (PLine2 pv1) = forcePLine2Basis pl1
    (PLine2 pv2) = forcePLine2Basis pl2

-- | A type stripping meet finction.
_meet2PPoint2 :: PPoint2 -> PPoint2 -> GVec
_meet2PPoint2 pp1 pp2 = pv1 ⎤ pv2
  where
    (PPoint2 pv1) = forcePPoint2Basis pp1
    (PPoint2 pv2) = forcePPoint2Basis pp2

-- | Create a projective point from a euclidian point.
eToPPoint2 :: Point2 -> PPoint2
eToPPoint2 (Point2 (x,y)) = PPoint2 $ GVec $ foldl' addVal [GVal 1 (fromList [GEPlus 1, GEPlus 2])] [ GVal (-x) (fromList [GEZero 1, GEPlus 2]), GVal y (fromList [GEZero 1, GEPlus 1]) ]

-- | Create a euclidian point from a projective point.
pToEPoint2 :: PPoint2 -> Point2
pToEPoint2 (PPoint2 (GVec pPoint)) = Point2 (negate $ valOf 0 $ getVals [GEZero 1, GEPlus 2] pPoint
                                            ,         valOf 0 $ getVals [GEZero 1, GEPlus 1] pPoint)

-- | Create the ideal form of a projective point.
-- Note: not yet used.
_idealPPoint2 :: PPoint2 -> PPoint2
_idealPPoint2 (PPoint2 (GVec vals)) = PPoint2 $ GVec $ foldl' addVal []
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
eToPLine2 :: LineSeg -> PLine2
eToPLine2 (LineSeg startPoint@(Point2 (x1,y1)) (Point2 (x,y))) = plineFromEndpoints startPoint (Point2 (x1+x,y1+y))

-- | Create a projective line from a pair of euclidian points.
plineFromEndpoints :: Point2 -> Point2 -> PLine2
plineFromEndpoints (Point2 (x1,y1)) (Point2 (x2,y2)) = PLine2 $ GVec $ foldl' addVal [] [ GVal c (singleton (GEZero 1)), GVal a (singleton (GEPlus 1)), GVal b (singleton (GEPlus 2)) ]
  where
    a=y2-y1
    b=x1-x2
    c=y1*x2-x1*y2

-- | Convert from a PPoint2 to it's associated PLine.
dualPPoint2 :: PPoint2 -> GVec
dualPPoint2 (PPoint2 vec) = dual2DGVec vec

-- | Convert from a PLine to it's associated projective point.
dualPLine2 :: PLine2 -> GVec
dualPLine2 (PLine2 vec) = dual2DGVec vec

reverse :: GVec -> GVec
reverse vec = GVec $ foldl' addVal []
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

-- | Normalization of euclidian points is really just canonicalization.
canonicalizePPoint2 :: PPoint2 -> PPoint2
canonicalizePPoint2 (PPoint2 vec@(GVec vals)) = PPoint2 $ divVecScalar vec $ valOf 1 $ getVals [GEPlus 1, GEPlus 2] vals

-- | The idealized norm of a projective point.
idealNormPPoint2 :: PPoint2 -> ℝ
idealNormPPoint2 ppoint = sqrt (x*x+y*y)
  where
    (Point2 (x,y)) = pToEPoint2 ppoint

-- | Normalize a PLine2.
normalizePLine2 :: PLine2 -> PLine2
normalizePLine2 pl@(PLine2 vec) = PLine2 $ divVecScalar vec $ normOfPLine2 pl

normOfPLine2 :: PLine2 -> ℝ
normOfPLine2 pline = sqrt $ sqNormOfPLine2 pline

sqNormOfPLine2 :: PLine2 -> ℝ
sqNormOfPLine2 (PLine2 (GVec vals)) = a*a+b*b
  where
    a = valOf 0 $ getVals [GEPlus 1] vals
    b = valOf 0 $ getVals [GEPlus 2] vals

-- | Reverse a line. same line, but pointed in the other direction.
flipPLine2 :: PLine2 -> PLine2
flipPLine2 (PLine2 (GVec vals)) = PLine2 $ GVec $ foldl' addVal []
                                  [
                                    GVal (negate $ valOf 0 $ getVals [GEZero 1] vals) (singleton (GEZero 1))
                                  , GVal (negate $ valOf 0 $ getVals [GEPlus 1] vals) (singleton (GEPlus 1))
                                  , GVal (negate $ valOf 0 $ getVals [GEPlus 2] vals) (singleton (GEPlus 2))
                                  ]

