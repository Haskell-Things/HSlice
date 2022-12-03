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
  ListThree,
  Radian(Radian),
  cellFrom,
  edgesOf,
  generationsOf,
  onlyOne,
  onlyOneOf,
  randomConcaveChevronQuad,
  randomConvexBisectableQuad,
  randomConvexDualRightQuad,
  randomConvexQuad,
  randomConvexSingleRightQuad,
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

import Prelude (Bool, Enum, Eq, Fractional, Num, Ord, Show, Int, (<>), (<>), (<$>), ($), (==), (+), (-), (*), (<), (/), (>), (<=), (&&), abs, error, fromInteger, fromRational, fst, mempty, mod, otherwise, replicate, show, signum)

import Data.Coerce (coerce)

import Data.List (sort)

import Data.List.Unique (allUnique)

import Data.Maybe (Maybe(Nothing, Just), fromMaybe)

import Math.Tau (tau)

import Numeric (pi)

import Slist.Type (Slist(Slist))

import Slist (len)

import Test.QuickCheck (Arbitrary, Positive(Positive), NonZero(NonZero), arbitrary, shrink, suchThat, vector)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

import Graphics.Slicer.Math.Arcs (getOutsideArc)

import Graphics.Slicer.Math.Contour (makePointContour, maybeFlipContour, firstPointPairOfContour, pointFarOutsideContour)

import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), LineSeg, makeLineSeg)

import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

import Graphics.Slicer.Math.Lossy (eToPLine2, join2PPoints, pPointBetweenPPoints, pToEPoint2, translateRotatePPoint2)

import Graphics.Slicer.Math.PGA (ProjectiveLine, PLine2Err, eToPL, eToPP, flipL, normalizeL, pPointOf)

import Graphics.Slicer.Math.Skeleton.Concave (makeENode)

import Graphics.Slicer.Math.Skeleton.Definitions(ENode, INode, StraightSkeleton(StraightSkeleton), getFirstLineSeg, getLastLineSeg, makeINode)

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
        | v <= tau   = v
        | otherwise = v-tau
  (-) (Radian r1) (Radian r2) = Radian $ wrapIfNeeded $ r1 - r2
    where
      wrapIfNeeded :: ℝ -> ℝ
      wrapIfNeeded v
        | v > 0     = v
        | otherwise = v+tau
  (*) (Radian r1) (Radian r2) = Radian $ recursiveWrap $ r1 * r2
    where
      recursiveWrap :: ℝ -> ℝ
      recursiveWrap v
        | v <= tau   = v
        | otherwise = recursiveWrap $ v-tau
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
-- FIXME: what stops this from trying to generate a triangle with all three points on the same line?
randomTriangle :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Contour
randomTriangle centerX centerY rawRadians rawDists = randomStarPoly centerX centerY $ makePairs dists radians
  where
    radians :: [Radian ℝ]
    radians = coerce rawRadians
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
      , tilt + Radian (pi+(pi/2))
      ]
    distances = replicate 4 distanceToCorner

-- | Generate a random rectangle.
randomRectangle :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomRectangle centerX centerY rawFirstTilt secondTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, add them!
      firstTilt
        | rawFirstTilt == secondTilt = rawFirstTilt + secondTilt
        | otherwise = rawFirstTilt
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , flipRadian firstTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      distances = replicate 4 distanceToCorner

-- | Generate a random convex four sided polygon, with two right angles.
randomConvexDualRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Contour
randomConvexDualRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt distanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = replicate 4 distanceToCorner

-- | Generate a random convex four sided polygon, with one right angle.
randomConvexSingleRightQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexSingleRightQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = firstDistanceToCorner : replicate 3 secondDistanceToCorner

-- | Generate a random convex four sided polygon, with the property that it can be folded down an axis.
randomConvexBisectableQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexBisectableQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3] | v <- vals]
      thirdTilt = secondTilt + (secondTilt - firstTilt)
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, secondDistanceToCorner]

-- | Generate a random convex four sided polygon.
randomConvexQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConvexQuad centerX centerY rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner, thirdDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner, rawThirdDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3,5] | v <- vals]
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt, thirdTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt, rawThirdTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3,5] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , thirdTilt
        , flipRadian secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, thirdDistanceToCorner, secondDistanceToCorner, thirdDistanceToCorner]

-- | Generate a concave four sided polygon, with the convex motorcycle impacting the opposing bend (a 'dart' per wikipedia. a chevron, or a ^.)
-- Note: the center point is always outside of this polygon.
randomConcaveChevronQuad :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Contour
randomConcaveChevronQuad centerX centerY rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = randomStarPoly centerX centerY $ makePairs distances radians
    where
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstDistanceToCorner, secondDistanceToCorner] = sort $ ensureUniqueDistance $ sort [rawFirstDistanceToCorner, rawSecondDistanceToCorner]
      ensureUniqueDistance :: [Positive ℝ] -> [Positive ℝ]
      ensureUniqueDistance vals
        | allUnique vals = vals
        | otherwise = ensureUniqueDistance $ sort [v*m | m <- [2,3] | v <- vals]
      thirdDistanceToCorner = secondDistanceToCorner / 2
      -- Workaround: since first and second may be unique, but may not be 0, multiply them!
      [firstTilt, secondTilt] = sort $ ensureUnique $ clipRadian <$> sort [rawFirstTilt, rawSecondTilt]
      ensureUnique :: [Radian ℝ] -> [Radian ℝ]
      ensureUnique vals
        | allUnique vals = vals
        | otherwise = ensureUnique $ sort [v*m | m <- [2,3] | v <- vals]
      radians :: [Radian ℝ]
      radians =
        [
          firstTilt
        , secondTilt
        , flipRadian firstTilt
        , secondTilt
        ]
      flipRadian :: Radian ℝ -> Radian ℝ
      flipRadian v
        | v < Radian pi = v + Radian pi
        | otherwise     = v - Radian pi
      clipRadian v
        | v > Radian pi = v - Radian pi
        | otherwise = v
      distances = [firstDistanceToCorner, secondDistanceToCorner, firstDistanceToCorner, thirdDistanceToCorner]

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
    dumpError          = error $ "failed to flip a contour:" <> dumpGanjas [toGanja contour, toGanja (Point2 (centerX, centerY)), toGanja outsidePLine] <> "\n"
      where
        outsidePLine   = join2PPoints myMidPoint outsidePoint
        outsidePoint   = eToPP $ pointFarOutsideContour contour
        myMidPoint     = pPointBetweenPPoints (eToPP p1) (eToPP p2) 0.5 0.5
        (p1, p2)       = firstPointPairOfContour contour

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
randomINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = makeINode [maybeFlippedpl1,maybeFlippedpl2] $ Just (bisector1,bisectorErr)
  where
    r1 = rawR1 / 2
    r2 = r1 + (rawR2 / 2)
    pl1 = fst $ normalizeL $ eToPLine2 $ getFirstLineSeg eNode
    pl2 = flipL $ eToPLine2 $ getLastLineSeg eNode
    intersectionPPoint = pPointOf eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    pp1 = translateRotatePPoint2 intersectionPPoint (coerce d1) (coerce r1)
    pp2 = translateRotatePPoint2 intersectionPPoint (coerce d2) (coerce r2)
    maybeFlippedpl1 = if flipIn1 then flipL pl1 else pl1
    maybeFlippedpl2 = if flipIn2 then flipL pl2 else pl2
    (bisector1, bisectorErr) = getOutsideArc (pp1, mempty) (maybeFlippedpl1, mempty) (pp2, mempty) (maybeFlippedpl2, mempty)

-- | A helper function. constructs a random PLine.
randomPLine :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> ProjectiveLine
randomPLine x y dx dy = fst $ randomPLineWithErr x y dx dy

-- | A helper function. constructs a random PLine.
randomPLineWithErr :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> (ProjectiveLine, PLine2Err)
randomPLineWithErr x y dx dy = eToPL $ makeLineSeg (Point2 (x, y)) (Point2 (coerce dx, coerce dy))

-- | A helper function. constructs a random LineSeg.
randomLineSeg :: ℝ -> ℝ -> ℝ -> ℝ -> LineSeg
randomLineSeg x y rawDx rawDy = makeLineSeg (Point2 (x,y)) (Point2 (rawDx, rawDy))

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
makePairs (a:as) (b:bs) = (a,b) : makePairs as bs
makePairs (_:_) [] = error "out of inputs"
makePairs [] (_:_) = []
makePairs [] [] = []

cellFrom :: Maybe (a,b) -> a
cellFrom (Just (v,_)) = v
cellFrom Nothing = error "whoops"

remainderFrom :: Maybe (a,b) -> b
remainderFrom (Just (_,v)) = v
remainderFrom Nothing = error "whoops"

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

generationsOf :: Maybe StraightSkeleton -> Int
generationsOf Nothing = 0
generationsOf (Just (StraightSkeleton (Slist [] _) _)) = 0
generationsOf (Just (StraightSkeleton a@(Slist [_] _) _)) = len a
generationsOf a = error $ "what is this?" <> show a <> "\n"

