{- ORMOLU_DISABLE -}
{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- Utility functions for generating random bits of geometry. Used by the test suite. -}

-- For Parallel lists.
{-# LANGUAGE ParallelListComp #-}

-- So we can derive ENum.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- So we can define a Num instance for Positive.
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphics.Slicer.Math.RandomGeometry (
  ListThree(ListThree),
  Radian(Radian),
  cellFrom,
  edgesOf,
  generationsOf,
  justSupported,
  nodeTreesOf,
  oneNodeTreeOf,
  onlyOne,
  onlyOneOf,
  randomConcaveChevronQuad,
  randomConvexBisectableQuad,
  randomConvexSingleRightQuad,
  randomConvexQuad,
  randomDualRightQuad,
  randomENode,
  randomINode,
  randomLineSeg,
  randomLineSegFromOriginNotX1Y1,
  randomLineSegFromPointNotX1Y1,
  randomPLine,
  randomPLineThroughOrigin,
  randomPLineThroughPoint,
  randomPLineWithErr,
  randomRectangle,
  randomSquare,
  randomTriangle,
  randomX1Y1LineSegToOrigin,
  randomX1Y1LineSegToPoint,
  remainderFrom
  ) where

import Prelude (Bool, Enum, Eq, Fractional, Num, Ord, Show, Int, (<>), (<>), (<$>), ($), (.), (==), (+), (-), (*), (<), (/), (>), (<=), (&&), abs, error, fromInteger, fromRational, fst, mempty, mod, not, otherwise, replicate, show, signum, snd)

import Data.Coerce (coerce)

import Data.Either (Either(Left, Right))

import Data.List (sort)

import Data.List.Unique (allUnique)

import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, isJust)

import Math.Tau (tau)

import Numeric (pi)

import Slist.Type (Slist(Slist))

import Slist (len)

import Test.QuickCheck (Arbitrary, Positive(Positive), NonZero(NonZero), arbitrary, shrink, suchThat, vector)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

import Graphics.Slicer.Math.Arcs (getOutsideArc)

import Graphics.Slicer.Math.Contour (makePointContour, maybeFlipContour, minDistanceFromSegMidPoint, mostPerpPointAndLineSeg)

import Graphics.Slicer.Math.ContourIntersections (contourIntersectionCount, getLineContourIntersections)

import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), LineSeg, endPoint, lineSegsOfContour, makeLineSeg, pointBetweenPoints, startPoint)

import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

import Graphics.Slicer.Math.Intersections (isCollinear)

import Graphics.Slicer.Math.Lossy (eToPLine2, pToEPoint2, translateRotatePPoint2)

import Graphics.Slicer.Math.PGA (ProjectiveLine, PLine2Err, ProjectivePoint, eToPL, eToPP, flipL, join2PP, pPointOnPerpWithErr, cPPointOf)

import Graphics.Slicer.Math.Skeleton.Cells (UnsupportedReason)

import Graphics.Slicer.Math.Skeleton.Concave (makeENode)

import Graphics.Slicer.Math.Skeleton.Definitions(ENode, INode, INodeSet(INodeSet), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), getFirstLineSeg, getLastLineSeg, makeINode)

import Graphics.Slicer.Math.Skeleton.Face(Face(Face))

-- moved the below here from the test suite, so that we can drop lines from the test suite into ghci directly.

-- A type for a list of three items. so we can gather a list of exactly three distances / radians.
newtype ListThree a = ListThree {getListThree :: [a]}
  deriving (Show, Ord, Eq)

instance (Arbitrary a) => Arbitrary (ListThree a) where
  arbitrary = ListThree <$> vector 3

-- Radians are always positive, and always between 0 and tau+minfloat.
newtype Radian a = Radian {getRadian :: ℝ}
  deriving (Show, Ord, Eq, Enum)

instance Arbitrary (Radian a) where
  arbitrary = Radian <$> (arbitrary `suchThat` (\a -> a > 0 && a <= tau))
  shrink (Radian x) = [ Radian x' | x' <- shrink x , x' > 0 ]

instance Num (Radian a) where
  (+) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 + r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v <= tau  = v
        | otherwise = wrapIfNeeded $ v - tau
  (-) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 - r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v > 0     = v
        | otherwise = wrapIfNeeded $ v + tau
  (*) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 * r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v <= tau  = v
        | otherwise = wrapIfNeeded $ v - tau
  abs r1 = r1
  fromInteger v = Radian $ fromInteger $ mod v 6
  signum _ = 1

-- | so we can do some arithmatic on positive numbers.
-- FIXME: Yes, this is an orphan instance.
instance (Ord a, Num a) => Num (Positive a) where
  (+) (Positive r1) (Positive r2) = Positive $ r1 + r2
  (-) (Positive r1) (Positive r2)
    | r1 < r2 = Positive $  r1 - r2
    | otherwise = error "tried to produce a negative number."
  (*) (Positive r1) (Positive r2) = Positive $ r1 * r2
  abs r1 = r1
  fromInteger v = Positive $ fromInteger v
  signum _ = 1

instance Fractional (Radian a) where
  (/) (Radian r1) (Radian r2) = Radian $ r1 / r2
  fromRational a = Radian $ fromRational a

instance (Ord a, Num a, Fractional a) => Fractional (Positive a) where
  (/) (Positive r1) (Positive r2) = Positive $ r1 / r2
  fromRational a = Positive $ fromRational a

-- | Generate a random triangle.
randomTriangle :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Contour
randomTriangle centerX centerY rawRadians rawDists = randomStarPoly centerX centerY $ fixCollinear $ makePairs dists radians
  where
    ensureUnique :: [Radian ℝ] -> [Radian ℝ]
    ensureUnique vals
      | allUnique vals = vals
      | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
    fixCollinear :: [(Positive ℝ,Radian ℝ)] -> [(Positive ℝ,Radian ℝ)]
    fixCollinear xs = case xs of
                        [] -> error "impossible, empty set."
                        [_] -> error "impossible, single point."
                        [_,_] -> error "impossible, two points."
                        [a@(ad, aa),b,c] -> if not $ isCollinear (line1, line1Err) (line2, line2Err)
                                            then [a,b,c]
                                            else if not $ isCollinear (line1a, line1aErr) (line2, line2Err)
                                                 then [a2, b, c]
                                                 else [a3, b, c]
                          where
                            (line1, (_,_, line1Err)) = join2PP (pointAroundCenter a) (pointAroundCenter b)
                            (line1a, (_,_, line1aErr)) = join2PP (pointAroundCenter a2) (pointAroundCenter b)
                            (line2, (_,_, line2Err)) = join2PP (pointAroundCenter b) (pointAroundCenter c)
                            a2, a3 :: (Positive ℝ, Radian ℝ)
                            a2 = (ad+0.1, if aa == 0 then 0.1 else aa/2)
                            a3 = (ad+0.2, if aa == 0 then 0.2 else aa/3)
                            pointAroundCenter :: (Positive ℝ, Radian ℝ) -> ProjectivePoint
                            pointAroundCenter (distanceFromPoint, angle) = translateRotatePPoint2 centerPPoint (coerce distanceFromPoint) (coerce angle)
                            centerPPoint      = eToPP $ Point2 (centerX, centerY)
                        _ -> error "too many points."
    radians :: [Radian ℝ]
    radians = ensureUnique $ coerce rawRadians
    dists :: [Positive ℝ]
    dists = coerce rawDists

-- | Generate a random square.
randomSquare :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomSquare centerX centerY tilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
  where
    radians =
      [
        tilt
      , tilt + Radian (tau/4)
      , tilt + Radian (tau/2)
      , tilt + Radian (tau*0.75)
      ]
    distances = replicate 4 distanceToCorner

-- | Generate a random rectangle.
randomRectangle :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomRectangle centerX centerY rawFirstTilt secondTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = replicate 4 distanceToCorner
      firstTilt
        | rawFirstTilt == secondTilt = rawFirstTilt + secondTilt
        | otherwise = rawFirstTilt
      radians = [firstTilt, secondTilt, flipRadian firstTilt, flipRadian secondTilt]

-- | Generate a random convex four sided polygon, with two right angles.
randomDualRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomDualRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = replicate 4 distanceToCorner
      radians = [firstTilt, secondTilt, thirdTilt, flipRadian secondTilt]
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUniqueRadian $ clipRadian <$> rawRadians
      rawRadians = [ rawFirstTilt, rawSecondTilt, rawThirdTilt]

-- | Generate a random convex four sided polygon, with one right angle.
randomConvexSingleRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexSingleRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = firstDistanceToCorner : replicate 3 secondDistanceToCorner
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      radians = [firstTilt, secondTilt, thirdTilt, fourthTilt]
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUniqueRadian $ clipRadian <$> rawRadians
      fourthTilt = flipRadian secondTilt
      rawRadians = [rawFirstTilt, rawSecondTilt, rawThirdTilt]

-- | Generate a random convex four sided polygon, with the property that it can be folded down an axis.
randomConvexBisectableQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, secondDistanceToCorner]
      radians = [firstTilt, secondTilt, thirdTilt, fourthTilt]
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      [firstTilt, secondTilt] = sort $ ensureUniqueRadian $ clipRadian <$> rawRadians
      thirdTilt = secondTilt + (secondTilt - firstTilt)
      fourthTilt = flipRadian secondTilt
      rawRadians = [rawFirstTilt, rawSecondTilt]

-- | Generate a random convex four sided polygon.
randomConvexQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomConvexQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt firstDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = replicate 4 firstDistanceToCorner
      radians = sort $ [firstTilt, secondTilt, thirdTilt, fourthTilt]
      [firstTilt, secondTilt, thirdTilt] = ensureUniqueRadian $ clipRadian <$> rawRadians
      fourthTilt = flipRadian secondTilt
      rawRadians = [rawFirstTilt, rawSecondTilt, rawThirdTilt]

-- | Generate a concave four sided polygon, with the convex motorcycle impacting the opposing bend (a 'dart' per wikipedia. a chevron, or a ^.)
-- Note: the center point is always outside of this polygon.
randomConcaveChevronQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConcaveChevronQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, thirdDistanceToCorner]
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      thirdDistanceToCorner = secondDistanceToCorner / 2
      radians = [firstTilt, secondTilt, flipRadian firstTilt, secondTilt]
      [firstTilt, secondTilt] = sort $ ensureUniqueRadian $ clipRadian <$> rawRadians
      rawRadians = [rawFirstTilt, rawSecondTilt]

-- Workaround: since first and second may be unique, but may not be 0, multiply them!
ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
ensureUniqueDistance vals
  | allUnique vals = vals
  | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3,5,8] | v <- vals]

-- Workaround: since first and second may be unique, but may not be 0, multiply them!
ensureUniqueRadian :: [Radian ℝ] -> [Radian ℝ]
ensureUniqueRadian vals
  | allUnique vals = vals
  | otherwise = ensureUniqueRadian $ sort [v*m | m <- [2,3,5,8] | v <- vals]

flipRadian :: Radian ℝ -> Radian ℝ
flipRadian v
  | v < Radian pi = v + Radian pi
  | otherwise     = v - Radian pi

clipRadian :: Radian ℝ -> Radian ℝ
clipRadian v
  | v > Radian pi = v - Radian pi
  | otherwise = v

-- | generate a random polygon.
-- Idea stolen from: https://stackoverflow.com/questions/8997099/algorithm-to-generate-random-2d-polygon
-- note: the centerPoint is assumed to be inside of the contour.
randomStarPoly :: ℝ -> ℝ -> [(Positive ℝ,Radian ℝ)] -> Contour
randomStarPoly centerX centerY radianDistPairs = fromMaybe dumpError $ maybeFlipContour contour
  where
    contour            = makePointContour points
    points             = pToEPoint2 <$> pointsAroundCenter
    pointsAroundCenter = (\(distanceFromPoint, angle) -> translateRotatePPoint2 centerPPoint (coerce distanceFromPoint) (coerce angle)) <$> radianDistPairs
    centerPPoint       = eToPP $ Point2 (centerX, centerY)
    dumpError          = error $ "failed to flip a contour.\n"
                               <> (dumpGanjas $ [toGanja contour,
                                                 toGanja "Center point", toGanja (Point2 (centerX, centerY)),
                                                 toGanja "perp PLine", toGanja perpPl,
                                                 toGanja "Other PLine", toGanja otherPl,
                                                 toGanja "First MidPoint", toGanja midPoint,
                                                 toGanja "First perpPoint", toGanja perpPoint,
                                                 toGanja "Second otherPoint", toGanja otherPoint,
                                                 toGanja "minDistance", toGanja (show minDistance),
                                                 toGanja "intersection count1", toGanja (show intersectionCount1),
                                                 toGanja "intersection count2", toGanja (show intersectionCount2)
                                                ] <> (toGanja . fst . eToPL <$> lineSegsOfContour contour))<> "\n"
                               <> show (getLineContourIntersections (perpPl, pErr) contour) <> "\n"
                               <> show (getLineContourIntersections (otherPl, oErr) contour) <> "\n"
                               <> show (mostPerpPointAndLineSeg contour) <> "\n"
      where
        (perpPl,(_,_,pErr)) = join2PP perpPoint (eToPP outsidePoint)
        (otherPl,(_,_,oErr)) = join2PP otherPoint (eToPP outsidePoint)
        minDistance    = minDistanceFromSegMidPoint outsidePoint lineSeg
        outsidePoint   = fst $ mostPerpPointAndLineSeg contour
        midPoint       = pointBetweenPoints (startPoint lineSeg) (endPoint lineSeg)
        intersectionCount1 = contourIntersectionCount contour (pToEPoint2 perpPoint, outsidePoint)
        intersectionCount2 = contourIntersectionCount contour (pToEPoint2 otherPoint, outsidePoint)
        perpPoint      = fst $ pPointOnPerpWithErr pl (eToPP midPoint) minDistance
        otherPoint     = fst $ pPointOnPerpWithErr pl (eToPP midPoint) (-minDistance)
        pl             = eToPLine2 lineSeg
        lineSeg        = snd $ mostPerpPointAndLineSeg contour

randomENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> ENode
randomENode x y d1 rawR1 d2 rawR2 = makeENode p1 intersectionPoint p2
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    intersectionPoint = Point2 (x,y)
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    p1 = pToEPoint2 pp1
    p2 = pToEPoint2 pp2
    intersectionPPoint = eToPP intersectionPoint

randomINode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Bool -> INode
randomINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = makeINode [maybeFlippedpl1, maybeFlippedpl2] (Just bisector)
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    (pl1, pl1Err) = eToPL $ getFirstLineSeg eNode
    (pl2, pl2Err) = (flipL ls, lsErr)
      where
        (ls, lsErr) = eToPL $ getLastLineSeg eNode
    intersectionPPoint = cPPointOf eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    maybeFlippedpl1 = (if flipIn1 then flipL pl1 else pl1, pl1Err)
    maybeFlippedpl2 = (if flipIn2 then flipL pl2 else pl2, pl2Err)
    bisector = getOutsideArc (pp1, mempty) maybeFlippedpl1 (pp2, mempty) maybeFlippedpl2

-- | A helper function. constructs a random PLine.
randomPLine :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> ProjectiveLine
randomPLine x y dx dy = fst $ randomPLineWithErr x y dx dy

-- | A helper function. constructs a random PLine.
randomPLineWithErr :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> (ProjectiveLine, PLine2Err)
randomPLineWithErr x y dx dy = eToPL $ makeLineSeg (Point2 (x, y)) (Point2 (x + coerce dx, y + coerce dy))

-- | A helper function. constructs a random LineSeg.
randomLineSeg :: ℝ -> ℝ -> ℝ -> ℝ -> LineSeg
randomLineSeg x y dx dy = makeLineSeg (Point2 (x,y)) (Point2 (dx, dy))

-- | A PLine that does not follow the X = Y line, and does not follow the other given line.
randomPLineThroughOrigin :: ℝ -> ℝ -> (ProjectiveLine, PLine2Err)
randomPLineThroughOrigin x y = eToPL $ makeLineSeg (Point2 (x,y)) (Point2 (0,0))

-- | A PLine that does not follow the X = Y line, and does not follow the other given line.
randomPLineThroughPoint :: ℝ -> ℝ -> ℝ -> (ProjectiveLine, PLine2Err)
randomPLineThroughPoint x y d = eToPL $ makeLineSeg (Point2 (x,y)) (Point2 (d,d))

-- | A line segment ending at the origin. additionally, guaranteed not to be on the X = Y line.
randomLineSegFromPointNotX1Y1 :: ℝ -> ℝ -> ℝ -> LineSeg
randomLineSegFromPointNotX1Y1 rawX rawY d = res
  where
    res = makeLineSeg (Point2 (d, d)) (Point2 (x, y))
    (x, y)
      | rawX == 0 && rawY == 0 = (0,0.1)
      | rawX == rawY = (rawX,0.1)
      | otherwise = (rawX, rawY)

-- | A line segment ending at the origin. additionally, guaranteed not to be on the X = Y line.
randomLineSegFromOriginNotX1Y1 :: ℝ -> ℝ -> LineSeg
randomLineSegFromOriginNotX1Y1 rawX rawY = res
  where
    res = makeLineSeg (Point2 (0, 0)) (Point2 (x, y))
    (x, y)
      | rawX == 0 && rawY == 0 = (0,0.1)
      | rawX == rawY = (rawX,0.1)
      | otherwise = (rawX, rawY)

randomX1Y1LineSegToOrigin :: NonZero ℝ -> LineSeg
randomX1Y1LineSegToOrigin rawD = res
  where
    res = makeLineSeg (Point2 (d,d)) (Point2 (0,0))
    d :: ℝ
    d = coerce rawD

randomX1Y1LineSegToPoint :: NonZero ℝ -> ℝ -> LineSeg
randomX1Y1LineSegToPoint rawD1 d2 = res
  where
    res = makeLineSeg (Point2 (d1,d1)) (Point2 (d2,d2))
    d1 :: ℝ
    d1 = coerce rawD1

-- | combine two lists. for feeding into randomStarPoly.
makePairs :: [a] -> [b] -> [(a,b)]
makePairs [] [] = []
makePairs [] (_:_) = []
makePairs (_:_) [] = error "out of inputs"
makePairs (a:as) (b:bs) = (a,b) : makePairs as bs

-- | Collect just the cell part of ((Cell, Maybe [CellDivide]), Maybe [RemainingContour]), which is the result of findFirstCellOfContour and findNextCell.
cellFrom :: ((a, Maybe b), c) -> a
cellFrom = fst . fst

remainderFrom :: (a,b) -> b
remainderFrom = snd

onlyOne :: [a] -> a
onlyOne as = case as of
               [] -> error "none"
               [a] -> a
               (_:_) -> error "too many"

onlyOneOf :: [a] -> a
onlyOneOf as = case as of
                 [] -> error "none"
                 (a:_) -> a

edgesOf :: Slist Face -> [LineSeg]
edgesOf faces = unwrap <$> (\(Slist a _) -> a) faces
  where
    unwrap :: Face -> LineSeg
    unwrap (Face edge _ _ _) = edge

nodeTreesOf :: Maybe StraightSkeleton -> Int
nodeTreesOf Nothing = 0
nodeTreesOf (Just (StraightSkeleton a@(Slist [_] _) _)) = len a
nodeTreesOf a = error $ "what is this?" <> show a <> "\n"

oneNodeTreeOf :: StraightSkeleton -> NodeTree
oneNodeTreeOf (StraightSkeleton (Slist [] _) _) = error "straight skeleton had no nodeTree?"
oneNodeTreeOf (StraightSkeleton (Slist [[x]] _) _) = x
oneNodeTreeOf (StraightSkeleton (Slist (_) _) _) = error "too many NodeTrees."

generationsOf :: NodeTree -> Int
generationsOf (NodeTree _ maybeINodeSet)
  | isJust maybeINodeSet = (\(INodeSet childGenerations _) -> len childGenerations + 1) $ fromJust maybeINodeSet
  | otherwise = 0

justSupported :: Either UnsupportedReason NodeTree -> NodeTree
justSupported maybeNodeTree = case maybeNodeTree of
                                (Left _) -> error $ "unsupported!\n" <> show maybeNodeTree <> "\n"
                                (Right a) -> a
