{- ORMOLU_DISABLE -}
{- HSlice.
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

-- Shamelessly stolen from ImplicitCAD.

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DataKinds #-}

module Math.PGA (linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec, facetFlakeySpec, facetBrokenSpec, contourSpec, lineSpec) where

-- Be explicit about what we import.
import Prelude (Bool(True, False), ($), (<$>), (==), (>=), error, realToFrac, (/=), (<=), otherwise, (&&), (+), show, length, (<>), fst, not, snd, mempty, pi, (<), (>), (-), (/), (*))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, NonZero(NonZero), Positive(Positive))
import Data.Coerce (coerce)

-- The Either library.
import Data.Either (Either(Left, Right), fromRight, isLeft)

import Data.List (foldl')

import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, Maybe(Just))

import Data.Set (singleton, fromList)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

-- Slists, a form of list with a stated size in the structure.
import Slist (slist)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, getContours, numPointsOfContour, makeLineSegContour, makePointContour, maybeFlipContour, mostPerpPointAndLineSeg)

import Graphics.Slicer.Math.ContourIntersections (getLineContourIntersections)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), Contour(LineSegContour), pointsOfContour, roundPoint2, startPoint, distance, lineSegsOfContour, makeLineSeg, endPoint)

-- Our Geometric Algebra library.
import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(GEZero, GEPlus, G0), GVal(GVal), GVec(GVec), UlpSum(UlpSum), addValPairWithErr, subValPairWithErr, addValWithErr, subVal, addVecPair, subVecPair, mulScalarVecWithErr, divVecScalarWithErr, scalarPart, ulpRaw, ulpVal, vectorPart, (•), (∧), (⋅), (⎣), (⎤))

-- Basic intersection logic.
import Graphics.Slicer.Math.Intersections (isCollinear, outputIntersectsLineSeg)

import Graphics.Slicer.Math.Lossy (canonicalizePPoint2, distanceBetweenPPoints, eToPLine2, getFirstArc, getOutsideArc, pPointOnPerp, translateRotatePPoint2)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (ProjectivePoint2(vecOfP), ProjectiveLine(NPLine2, PLine2), ProjectiveLine2(vecOfL), PLine2Err(PLine2Err), cPPointAndErrOf, distance2PL, distance2PP, distancePPToPL, eToPL, pLineErrAtPPoint, eToPP, join2PP, interpolate2PP, intersect2PL, translateL, flipL, makeCPPoint2, normalizeL, pLineIsLeft, pPointsOnSameSideOfPLine, Intersection(HitStartPoint, HitEndPoint, NoIntersection), PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn), intersectsWithErr, pPointOnPerpWithErr, outOf, combineConsecutiveLineSegs, errOfOut, fuzzinessOfL, onSegment, sameDirection, translateRotatePPoint2WithErr)

import Graphics.Slicer.Math.PGAPrimitives (angleBetween2PL, xIntercept, yIntercept)

-- Our imprecise Contour library.
import Graphics.Slicer.Machine.Contour (shrinkContour, expandContour)

-- Our Infill library.
import Graphics.Slicer.Machine.Infill (InfillType(Horiz, Vert), makeInfill)

-- Our Facet library.
import Graphics.Slicer.Math.Arcs (towardIntersection)
import Graphics.Slicer.Math.Skeleton.Concave (averageNodes, makeENode, makeENodes)
import Graphics.Slicer.Math.Skeleton.Definitions (INode(INode), Motorcycle(Motorcycle), getFirstLineSeg, getLastLineSeg, sortPLinesByReference)
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles)
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), randomENode, randomINode, randomLineSeg, randomPLine, randomPLineWithErr, onlyOne, randomPLineThroughOrigin, randomX1Y1LineSegToOrigin, randomLineSegFromOriginNotX1Y1, randomX1Y1LineSegToPoint, randomLineSegFromPointNotX1Y1, randomPLineThroughPoint)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

-- | simple tests on contours.
contourSpec :: Spec
contourSpec = do
  describe "Contours (math/contour)" $ do
    it "contours made from a list of point pairs retain their order" $
      getContours cl1 --> [c1]
    it "contours made from an out of order list of point pairs is put into order" $
      getContours oocl1 --> [c1]
    it "detects a bigger contour containing a smaller contour" $
      contourContainsContour c1 c2 --> True
    it "ignores a smaller contour contained in a bigger contour" $
      contourContainsContour c2 c1 --> False
    it "ignores two contours that do not contain one another" $
      contourContainsContour c1 c3 --> False
    it "retains order in a contour passed through combineConsecutiveLineSegs" $
      makeLineSegContour (combineConsecutiveLineSegs $ lineSegsOfContour c1) --> makeLineSegContour (lineSegsOfContour c1)
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]
    cl1 = [(Point2 (0,0), Point2 (0,1)), (Point2 (0,1), Point2 (1,1)), (Point2 (1,1), Point2 (1,0)), (Point2 (1,0), Point2 (0,0))]
    oocl1 = [(Point2 (1,0), Point2 (0,0)), (Point2 (0,1), Point2 (1,1)), (Point2 (0,0), Point2 (0,1)), (Point2 (1,1), Point2 (1,0))]
    c1 = makePointContour cp1
    c2 = makePointContour [Point2 (0.75,0.25), Point2 (0.75,0.75), Point2 (0.25,0.75), Point2 (0.25,0.25)]
    c3 = makePointContour [Point2 (3,0), Point2 (3,1), Point2 (2,1), Point2 (2,0)]

lineSpec :: Spec
lineSpec = do
  describe "Contours (math/line)" $ do
    it "contours converted from points to lines then back to points give the input list" $
      pointsOfContour (makePointContour cp1) --> cp1
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]

linearAlgSpec :: Spec
linearAlgSpec = do
  describe "Contours (machine/contour)" $ do
    it "a contour mechanically shrunk has the same amount of points as the input contour" $
      numPointsOfContour (fromMaybe (error "got Nothing") $ shrinkContour 0.1 [] c1) --> numPointsOfContour c1
    it "a contour mechanically shrunk by zero is the same as the input contour" $
      shrinkContour 0 [] c1 --> Just cl1
    it "a contour mechanically expanded has the same amount of points as the input contour" $
      numPointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] c1) --> numPointsOfContour c1
    it "a contour mechanically shrunk and expanded is about equal to where it started" $
      (roundPoint2 <$> pointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] $ fromMaybe (error "got Nothing") $ shrinkContour 0.1 [] c2)) --> roundPoint2 <$> pointsOfContour c2
  describe "Infill (machine/infill)" $ do
    it "infills exactly one line inside of a box big enough for only one line (Horizontal)" $ do
      makeInfill c1 [] 0.5 Horiz --> [[LineSeg (Point2 (0.0,0.5)) (Point2 (1.0,0.5))]]
    it "infills exactly one line inside of a box big enough for only one line (Vertical)" $ do
      makeInfill c1 [] 0.5 Vert --> [[LineSeg (Point2 (0.5,0.0)) (Point2 (0.5,1.0))]]
--  describe "Contours (Skeleton/line)" $ do
--    it "a contour algorithmically shrunk has the same amount of points as the input contour" $
--      numPointsOfContour (justOneContourFrom $ addInset 1 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c1 []) --> numPointsOfContour c1
--    it "a contour algorithmically shrunk and mechanically expanded is about equal to where it started" $
--      roundPoint2 <$> pointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] $ justOneContourFrom $ addInset 1 0.1 $ orderedFacesOf c2l1 $ fromMaybe (error "got Nothing") $ findStraightSkeleton c2 []) --> roundPoint2 <$> pointsOfContour c2
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]
    c1 = makePointContour cp1
    cl1 = makeLineSegContour (lineSegsOfContour c1)
    c2 = makePointContour [Point2 (0.75,0.25), Point2 (0.75,0.75), Point2 (0.25,0.75), Point2 (0.25,0.25)]

geomAlgSpec :: Spec
geomAlgSpec = do
  describe "GVals (Math/GeometricAlgebra)" $ do
    -- 1e1+1e1 = 2e1
    it "adds two values with a common basis vector" $
      addValPairWithErr (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> [(GVal 2 (singleton (GEPlus 1)), ErrVal (UlpSum 4.440892098500626e-16) (singleton (GEPlus 1)))]
    -- 1e1+1e2 = e1+e2
    it "adds two values with different basis vectors" $
      addValPairWithErr (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2))) --> [(GVal 1 (singleton (GEPlus 1)), mempty), (GVal 1 (singleton (GEPlus 2)), mempty)]
    -- 2e1-1e1 = e1
    it "subtracts two values with a common basis vector" $
      subValPairWithErr (GVal 2 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> [(GVal 1 (singleton (GEPlus 1)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1)))]
    -- 1e1-1e2 = e1-e2
    it "subtracts two values with different basis vectors" $
      subValPairWithErr (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2))) --> [(GVal 1 (singleton (GEPlus 1)), mempty), (GVal (-1.0) (singleton (GEPlus 2)),mempty)]
    -- 1e1-1e1 = 0
    it "subtracts two identical values with a common basis vector and gets nothing" $
      subValPairWithErr (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> []
    -- 1e0+1e1+1e2 = e0+e1+e2
    it "adds a value to a list of values" $
      addValWithErr [(GVal 1 (singleton (GEZero 1)),mempty), (GVal 1 (singleton (GEPlus 1)), mempty)] (GVal 1 (singleton (GEPlus 2))) --> [(GVal 1 (singleton (GEZero 1)),mempty), (GVal 1 (singleton (GEPlus 1)),mempty), (GVal 1 (singleton (GEPlus 2)),mempty)]
    -- 2e1+1e2-1e1 = e1+e2
    it "subtracts a value from a list of values" $
      subVal [GVal 2 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] (GVal 1 (singleton (GEPlus 1))) --> [GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))]
    -- 1e1+1e2-1e1 = e2
    it "subtracts a value from a list of values, eliminating an entry with a like basis vector" $
      subVal [GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] (GVal 1 (singleton (GEPlus 1))) --> [GVal 1 (singleton (GEPlus 2))]
  describe "GVecs (Math/GeometricAlgebra)" $ do
    -- 1e1+1e1 = 2e1
    it "adds two (multi)vectors" $
      addVecPair (GVec [GVal 1 (singleton (GEPlus 1))]) (GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec [GVal 2 (singleton (GEPlus 1))]
    -- 1e1-1e1 = 0
    it "subtracts a (multi)vector from another (multi)vector" $
      subVecPair (GVec [GVal 1 (singleton (GEPlus 1))]) (GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec []
    -- 2*1e1 = 2e1
    it "multiplies a (multi)vector by a scalar (mulScalarVecWithErr)" $
      mulScalarVecWithErr 2 (GVec [GVal 1 (singleton (GEPlus 1))]) --> (GVec [GVal 2 (singleton (GEPlus 1))],[ErrVal (UlpSum 4.440892098500626e-16) (singleton (GEPlus 1))])
    it "multiplies a (multi)vector by a scalar (G0)" $
      GVec [GVal 2 (singleton G0)] • GVec [GVal 1 (singleton (GEPlus 1))] --> GVec [GVal 2 (singleton (GEPlus 1))]
    -- 2e1/2 = e1
    it "divides a (multi)vector by a scalar" $
      divVecScalarWithErr (GVec [GVal 2 (singleton (GEPlus 1))]) 2 --> (GVec [GVal 1 (singleton (GEPlus 1))], [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))])
    -- 1e1|1e2 = 0
    it "the dot product of two orthoginal basis vectors is nothing" $
      GVec [GVal 1 (singleton (GEPlus 1))] ⋅ GVec [GVal 1 (singleton (GEPlus 2))] --> GVec []
    it "the like product of two orthoginal basis vectors is nothing" $
      GVec [GVal 1 (singleton (GEPlus 1))] ⎣ GVec [GVal 1 (singleton (GEPlus 2))] --> GVec []
    it "the dot product of two vectors is comutative (a⋅b == b⋅a)" $
      GVec [GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] ⋅ GVec [GVal 2 (singleton (GEPlus 2)), GVal 2 (singleton (GEPlus 2))] -->
      GVec [GVal 2 (singleton (GEPlus 1)), GVal 2 (singleton (GEPlus 2))] ⋅ GVec [GVal 1 (singleton (GEPlus 2)), GVal 1 (singleton (GEPlus 2))]
    it "the like product of two vectors is comutative (a⋅b == b⋅a)" $
      GVec [GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] ⎣ GVec [GVal 2 (singleton (GEPlus 2)), GVal 2 (singleton (GEPlus 2))] -->
      GVec [GVal 2 (singleton (GEPlus 1)), GVal 2 (singleton (GEPlus 2))] ⎣ GVec [GVal 1 (singleton (GEPlus 2)), GVal 1 (singleton (GEPlus 2))]
    -- 2e1|2e1 = 4
    it "the dot product of a vector with itsself is it's magnitude squared" $
      scalarPart (GVec [GVal 2 (singleton (GEPlus 1))] ⋅ GVec [GVal 2 (singleton (GEPlus 1))]) --> 4
    it "the like product of a vector with itsself is it's magnitude squared" $
      scalarPart (GVec [GVal 2 (singleton (GEPlus 1))] ⎣ GVec [GVal 2 (singleton (GEPlus 1))]) --> 4
    -- (2e1^1e2)|(2e1^1e2) = -4
    it "the dot product of a bivector with itsself is the negative of magnitude squared" $
      scalarPart (GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])] ⋅ GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])]) --> (-4)
    it "the like product of a bivector with itsself is the negative of magnitude squared" $
      scalarPart (GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])] ⎣ GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])]) --> (-4)
    -- 1e1^1e1 = 0
    it "the wedge product of two identical vectors is nothing" $
      vectorPart (GVec [GVal 1 (singleton (GEPlus 1))] ∧ GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec []
    it "the unlike product of two identical vectors is nothing" $
      vectorPart (GVec [GVal 1 (singleton (GEPlus 1))] ⎤ GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec []
    it "the wedge product of two vectors is anti-comutative (u∧v == -v∧u)" $
      GVec [GVal 1 (singleton (GEPlus 1))] ∧ GVec [GVal 1 (singleton (GEPlus 2))] -->
      GVec [GVal (-1) (singleton (GEPlus 2))] ∧ GVec [GVal 1 (singleton (GEPlus 1))]
    it "the unlike product of two vectors is anti-comutative (u∧v == -v∧u)" $
      GVec [GVal 1 (singleton (GEPlus 1))] ⎤ GVec [GVal 1 (singleton (GEPlus 2))] -->
      GVec [GVal (-1) (singleton (GEPlus 2))] ⎤ GVec [GVal 1 (singleton (GEPlus 1))]
    it "the result of the like operator on a pair of vectors across common basis vectors is equal to the result of the dot product." $
      GVec [GVal (-0.5) (singleton (GEZero 1)), GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] ⎣ GVec [GVal 2 (singleton (GEPlus 2)), GVal 2 (singleton (GEPlus 2))] -->
      GVec [GVal (-0.5) (singleton (GEZero 1)), GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))] ⋅ GVec [GVal 2 (singleton (GEPlus 2)), GVal 2 (singleton (GEPlus 2))]
  describe "Operators (Math/GeometricAlgebra)" $ do
    it "the multiply operations that should result in nothing all result in nothing" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 ] --> GVec []
    it "the multiply operations that should result in 1 all result in 1" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 , GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 ] --> GVec [GVal 2 (singleton G0)]
    it "the multiply operations that should result in -1 all result in -1" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal (-1) (singleton G0)]
    it "the multiply operations that should result in e0 all result in e0" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 ] --> GVec [GVal 2 (singleton (GEZero 1))]
    it "the multiply operations that should result in e1 all result in e1" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 ] --> GVec [GVal 1 (singleton (GEPlus 1))]
    it "the multiply operations that should result in e2 all result in e2" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal 1 (singleton (GEPlus 2))]
    it "the multiply operations that should result in e01 all result in e01" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 , GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 ] --> GVec [GVal 4 (fromList [GEZero 1, GEPlus 1])]
    it "the multiply operations that should result in e02 all result in e02" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal 2 (fromList [GEZero 1, GEPlus 2])]
    it "the multiply operations that should result in e12 all result in e12" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 ] --> GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    it "the multiply operations that should result in e012 all result in e012" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEZero 1))] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])] • GVec [GVal 1 (singleton (GEPlus 2))]
                                 , GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEZero 1))]
                                 ] --> GVec [GVal 4 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    it "the multiply operations that should result in -e0 all result in -e0" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal (-4) (singleton (GEZero 1))]
    it "the multiply operations that should result in -e1 all result in -e1" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal (-1) (singleton (GEPlus 1))]
    it "the multiply operations that should result in -e2 all result in -e2" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 ] --> GVec [GVal (-1) (singleton (GEPlus 2))]
    it "the multiply operations that should result in -e01 all result in -e01" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
                                 ] --> GVec [GVal (-2) (fromList [GEZero 1, GEPlus 1])]
    it "the multiply operations that should result in -e02 all result in -e02" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
                                 , GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (singleton (GEZero 1))]
                                 , GVec [GVal 1 (fromList [GEPlus 1, GEPlus 2])] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 1])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 ] --> GVec [GVal (-4) (fromList [GEZero 1, GEPlus 2])]
    it "the multiply operations that should result in -e12 all result in -e12" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 2))] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 ] --> GVec [GVal (-1) (fromList [GEPlus 1, GEPlus 2])]
    it "the multiply operations that should result in -e012 all result in -e012" $
      foldl' addVecPair (GVec []) [
                                   GVec [GVal 1 (singleton (GEPlus 1))] • GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])]
                                 , GVec [GVal 1 (fromList [GEZero 1, GEPlus 2])] • GVec [GVal 1 (singleton (GEPlus 1))]
                                 ] --> GVec [GVal (-2) (fromList [GEZero 1, GEPlus 1, GEPlus 2])]

-- | A property test making sure that the scalar part of the little-dot product of two PPoints is always -1.
prop_ScalarDotScalar :: ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_ScalarDotScalar v1 v2 v3 v4 = scalarPart (rawPPoint2 (v1,v2) ⋅ rawPPoint2 (v3,v4)) == (-1)
  where
    rawPPoint2 (x,y) = vecOfP $ eToPP (Point2 (x,y))

-- | A test making sure that the wedge product of two PLines along two different axises is always in e1e2.
prop_TwoAxisAlignedLines :: NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> Expectation
prop_TwoAxisAlignedLines d1 d2 r1 r2 = (\(GVec gVals) -> bases gVals) (vecOfL (eToPLine2 (makeLineSeg (Point2 (coerce d1,0)) (Point2 (coerce d1 - coerce r1,0)))) ∧ vecOfL (eToPLine2 (makeLineSeg (Point2 (0,coerce d2)) (Point2 (0,coerce d2 - coerce r2))))) --> [fromList [GEPlus 1, GEPlus 2]]
  where
    bases gvals = (\(GVal _ base) -> base) <$> gvals

-- | A property test making sure that the scalar part of the big-dot product of two identical PLines is not zero.
prop_TwoOverlappingLinesScalar :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_TwoOverlappingLinesScalar x y dx dy = scalarPart (vecOfL (randomPLine x y dx dy) • vecOfL (randomPLine x y dx dy)) /= 0

-- | A property test for making sure that there is never a vector result of the big-dot product of two identical PLines.
prop_TwoOverlappingLinesVector :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Expectation
prop_TwoOverlappingLinesVector x y dx dy = vectorPart (vecOfL (randomPLine x y dx dy) • vecOfL (randomPLine x y dx dy)) --> GVec []

-- | A property test for: a pline is sameDirection as itsself.
prop_PLineSameDirectionID :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_PLineSameDirectionID x y dx dy = isCollinear pLine pLine
  where
    pLine = randomPLineWithErr x y dx dy

proj2DGeomAlgSpec :: Spec
proj2DGeomAlgSpec = do
  describe "Points (Math/PGA)" $
    -- ((1e0^1e1)+(-1e0^1e2)+(1e1+1e2))|((-1e0^1e1)+(1e0^1e2)+(1e1+1e2)) = -1
    it "the dot product of any two projective points is -1" $
      property prop_ScalarDotScalar
  describe "Lines (Math/PGA)" $ do
    -- (-2e2)*2e1 = 4e12
    it "the intersection of a line along the X axis and a line along the Y axis is the origin point" $
      vecOfL (eToPLine2 (LineSeg (Point2 (-1,0)) (Point2 (1,0)))) ∧ vecOfL (eToPLine2 (LineSeg (Point2 (0,-1)) (Point2 (0,1)))) --> GVec [GVal 4 (fromList [GEPlus 1, GEPlus 2])]
    it "the intersection of two axis aligned lines is a multiple of e1e2" $
      property prop_TwoAxisAlignedLines
    -- (-2e0+1e1)^(2e0-1e2) = -1e01+2e02-e12
    it "the intersection of a line two points above the X axis, and a line two points to the right of the Y axis is at (2,2) in the upper right quadrant" $
      vectorPart (vecOfL (eToPLine2 (LineSeg (Point2 (2,0)) (Point2 (2,1)))) ∧ vecOfL (eToPLine2 (LineSeg (Point2 (0,2)) (Point2 (1,2))))) -->
      GVec [GVal (-2) (fromList [GEZero 1, GEPlus 1]), GVal 2 (fromList [GEZero 1, GEPlus 2]), GVal (-1) (fromList [GEPlus 1, GEPlus 2])]
    it "the geometric product of any two overlapping lines is only a Scalar" $
      property prop_TwoOverlappingLinesScalar
    it "the geometric product of any two overlapping lines does not produce a vector component" $
      property prop_TwoOverlappingLinesVector
    it "A line constructed from a line segment is correct" $
      eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (1,1))) --> pl1
    it "A line constructed from by joining two points is correct" $
      fst (join2PP (eToPP (Point2 (0,0))) (eToPP (Point2 (1,1)))) --> pl1
  where
    pl1 = PLine2 $ GVec [GVal 1 (singleton (GEPlus 1)), GVal (-1) (singleton (GEPlus 2))]

-- | A property test making sure a PPoint projected from an axis-aligned line is along the opposite axis.
prop_AxisProjection :: Positive ℝ -> Bool -> Bool -> Positive ℝ -> Expectation
prop_AxisProjection v xAxis whichDirection dv
  | xAxis = if whichDirection
            then canonicalizePPoint2 (pPointOnPerp (eToPLine2 $ randomLineSeg 0 0 (coerce v) 0) (makeCPPoint2 0 0) (coerce dv)) --> makeCPPoint2 0 (coerce dv)
            else canonicalizePPoint2 (pPointOnPerp (eToPLine2 $ randomLineSeg 0 0 (-(coerce v)) 0) (makeCPPoint2 0 0) (coerce dv)) --> makeCPPoint2 0 (-coerce dv)
  | otherwise = if whichDirection
                then canonicalizePPoint2 (pPointOnPerp (eToPLine2 $ randomLineSeg 0 0 0 (coerce v)) (makeCPPoint2 0 0) (coerce dv)) --> makeCPPoint2 (-coerce dv) 0
                else canonicalizePPoint2 (pPointOnPerp (eToPLine2 $ randomLineSeg 0 0 0 (-(coerce v))) (makeCPPoint2 0 0) (coerce dv)) --> makeCPPoint2 (coerce dv) 0

-- A property test making sure than for any LineSeg, a pointOnPerp is in fact on a 90 degree perpendicular line.
prop_perpAt90Degrees :: ℝ -> ℝ -> Positive ℝ -> ℝ -> NonZero ℝ -> Bool
prop_perpAt90Degrees x y rawX2 y2 rawD
  | angle2 < errTotal4 = True
  | otherwise = error
                $ "wrong angle?\n"
                <> "pline3Err: " <> show pline3Err <> "\n"
                <> "norm3Err: " <> show norm3Err <> "\n"
                <> "pline4Err: " <> show pline4Err <> "\n"
                <> "norm4Err: " <> show norm4Err <> "\n"
                <> "errTotal3: " <> show errTotal3 <> "\n"
                <> "pline3: " <> show pline3 <> "\n"
                <> "pline4: " <> show pline4 <> "\n"
                <> "distance: " <> show d <> "\n"
                <> "angle2: " <> show angle2 <> "\n"
                <> "angle2Err: " <> show angle2Err <> "\n"
  where
    (angle2, angle2Err) = angleBetween2PL normedPLine3 nPLine4
    (rawBisectorStart, _) = interpolate2PP sourceStart sourceEnd 0.5 0.5
    (bisectorEnd, (_,_,_, bisectorEndRawErr)) = pPointOnPerpWithErr nPLine4 rawBisectorStart d
    (pline3, (_,_,pline3Err)) = join2PP rawBisectorStart bisectorEnd
    (normedPLine3, norm3Err) = normalizeL pline3
    sourceStart = makeCPPoint2 x y
    sourceEnd = makeCPPoint2 x2 y2
    (pline4, (_,_,pline4Err)) = join2PP sourceStart sourceEnd
    (nPLine4, norm4Err) = normalizeL pline4
    errTotal3 = ulpVal bisectorEndRawErr
    errTotal4 = ulpVal bisectorEndRawErr
    x2,d :: ℝ
    x2 = coerce rawX2
    d = coerce rawD

-- | A property test making sure two points on the same side of an axis show as being on the same side of the axis.
prop_SameSideOfAxis :: NonZero ℝ -> NonZero ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Bool -> Bool -> Expectation
prop_SameSideOfAxis rawV1 rawV2 rawP1 rawP2 rawMagnitude xAxis positiveSide
  | xAxis = if positiveSide
            then pPointsOnSameSideOfPLine (eToPP $ Point2 (v1,p1)) (eToPP $ Point2 (v2,p2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (mag,0))) --> Just True
            else pPointsOnSameSideOfPLine (eToPP $ Point2 (v1,-p1)) (eToPP $ Point2 (v2,-p2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (mag,0))) --> Just True
  | otherwise = if positiveSide
                then pPointsOnSameSideOfPLine (eToPP $ Point2 (p1,v1)) (eToPP $ Point2 (p2,v2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (0,1))) --> Just True
                else pPointsOnSameSideOfPLine (eToPP $ Point2 (-p1,v1)) (eToPP $ Point2 (-p1,v2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (0,1))) --> Just True
  where
    p1 = coerce rawP1
    p2 = coerce rawP2
    v1 = coerce rawV1
    v2 = coerce rawV2
    mag,p1,p2,v1,v2::ℝ
    mag = coerce rawMagnitude

-- | A property test making sure that two points on opposite sides of an axis show as being on the opposite sides of the axis.
prop_OtherSideOfAxis :: NonZero ℝ -> NonZero ℝ -> Positive ℝ -> Positive ℝ -> Bool -> Bool -> Expectation
prop_OtherSideOfAxis v1 v2 p1 p2 xAxis positive
  | xAxis = if positive
            then pPointsOnSameSideOfPLine (eToPP (Point2 (coerce v1,coerce p1))) (eToPP (Point2 (coerce v2,-(coerce p2)))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (1,0)))) --> Just False
            else pPointsOnSameSideOfPLine (eToPP (Point2 (coerce v1,-(coerce p1)))) (eToPP (Point2 (coerce v2,coerce p2))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (1,0)))) --> Just False
  | otherwise = if positive
                then pPointsOnSameSideOfPLine (eToPP (Point2 (coerce p1,coerce v1))) (eToPP (Point2 (-(coerce p2),coerce v2))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (0,1)))) --> Just False
                else pPointsOnSameSideOfPLine (eToPP (Point2 (-(coerce p1),coerce v1))) (eToPP (Point2 (coerce p1,coerce v2))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (0,1)))) --> Just False

-- | Ensure that a PLine translated, then translated back is approximately the same PLine.
prop_PerpTranslateID :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_PerpTranslateID x y dx dy rawT
  | res <= resErr = res <= resErr
  | otherwise = error
                $ "failed:\n"
                <> "origPLine: " <> show origPLine <> "\n"
                <> "resPLine: " <> show resPLine <> "\n"
                <> "res: " <> show res <> "\n"
  where
    (res, _) = distance2PL resPLine origPLine
    (resPLine, resTransErr) = translateL translatedPLine (-t)
    (translatedPLine, origTransErr) = translateL origPLine t
    (origPLine, origPLineErr) = randomPLineWithErr x y dx dy
    resErr, t :: ℝ
    resErr = ulpVal $ fuzzinessOfL (resPLine, resTransErr <> origTransErr <> origPLineErr)
    t = coerce rawT

pgaSpec :: Spec
pgaSpec = do
  describe "Translation (math/PGA)" $ do
    it "a translated line translated back is the same line" $
     property prop_PerpTranslateID
  describe "Projection (math/PGA)" $ do
    it "a projection on the perpendicular bisector of an axis aligned line is on the other axis" $
      property prop_AxisProjection
--  describe "Distance measurement (math/PGA)" $ do
--    it "the distance between a projective point at (x,y) and an axis is equal to x for the x axis, and y for the y axis" $
--      property prop_DistanceToAxis
  describe "Layout Inspection (math/PGA)" $ do
    it "two projective points on the same side of a line show as being on the same side of the line" $
      property prop_SameSideOfAxis
    it "two projective points on different sides of a line show as being on different sides of a line" $
      property prop_OtherSideOfAxis

-- | ensure that the bisector of a quad crosses the point across the quad from the bisector.
prop_QuadBisectorCrosses :: Positive ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_QuadBisectorCrosses rawX1 rawY1 rawX2 rawY2
  | isEndPoint intersect1 && isStartPoint intersect2 && isEndPoint intersect3 && isEndPoint intersect4 = True
  | otherwise = error $ "missed!\n"
                <> show intersect1 <> "\n"
                <> show intersect2 <> "\n"
                <> show intersect3 <> "\n"
                <> show intersect4 <> "\n"
                <> show lineSeg1 <> "\n"
                <> show lineSeg2 <> "\n"
                <> show bisector <> "\n"
                <> show bisector1 <> "\n"
                <> show eNode <> "\n"
                <> "(" <> show x3 <> "," <> show y3 <> ")\n"
  where
    intersect1 = intersectsWithErr (Right (bisector1, bisector1Err)) (Left lineSeg1 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect2 = intersectsWithErr (Right (bisector1, bisector1Err)) (Left lineSeg2 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (bisector1, bisector1Err) = normalizeL bisector
    (bisector, _) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (x3,y3))
    eNode = makeENode (Point2 (x1,y1)) (Point2 (0,0)) (Point2 (x2,y2))
    -- X1, Y1 and X2 forced uniqueness. additionally, forced "not 180 degree opposition).
    x1,y1,x2,y2 :: ℝ
    x1
     | coerce rawX1 == y1 = coerce rawX1 /2
     | otherwise = coerce rawY1
    y1
     | coerce rawY1 == y2 = 0
     | otherwise = coerce rawY1
    x2
     | coerce rawX2 == y2 = coerce rawX2 + coerce rawX2 / 2
     | otherwise = coerce rawX2
    y2 = coerce rawY2
    -- the point that our bisector is supposed to cross.
    x3 = x1 + x2
    y3 = y1 + y2
    -- the two line segments we should cross.
    lineSeg1 = randomLineSeg x1 y1 x3 y3
    lineSeg2 = randomLineSeg x3 y3 x2 y2
    isEndPoint (Left (HitEndPoint _)) = True
    isEndPoint (Right PCollinear) = True
    isEndPoint (Right PAntiParallel) = True
    isEndPoint _ = False
    isStartPoint (Left (HitStartPoint _)) = True
    isStartPoint (Right PAntiCollinear) = True
    isStartPoint _ = False

-- ensure that the bisector of a quad crosses the point across the quad from the bisector.. and the next N points along that path.
prop_QuadBisectorCrossesMultiple :: Positive ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_QuadBisectorCrossesMultiple rawX1 rawY1 rawX2 rawY2 rawTimes
  | isEndPoint intersect3 && isStartPoint intersect4 = True
  | otherwise = error $ "missed!\n"
                <> show intersect1 <> "\n"
                <> show intersect2 <> "\n"
                <> show intersect3 <> "\n"
                <> show intersect4 <> "\n"
                <> show lineSeg1 <> "\n"
                <> show lineSeg2 <> "\n"
                <> show bisector1 <> "\n"
                <> show eNode <> "\n"
                <> show (angleBetween2PL (outOf eNode) bisector1) <> "\n"
                <> show (errOfOut eNode) <> "\n"
                <> "(" <> show x3 <> "," <> show y3 <> ")\n"
                <> "(" <> show x4 <> "," <> show y4 <> ")\n"
  where
    intersect1 = intersectsWithErr (Right (bisector1, bisector1Err)) (Left lineSeg1 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect2 = intersectsWithErr (Right (bisector1, bisector1Err)) (Left lineSeg2 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (bisector1, bisector1Err) = normalizeL bisector
    (bisector, _) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (x3,y3))
    eNode = makeENode (Point2 (x1,y1)) (Point2 (0,0)) (Point2 (x2,y2))
    -- X1, Y1 and X2 forced uniqueness. additionally, forced "not 180 degree opposition).
    x1,y1,x2,y2,times :: ℝ
    x1
     | coerce rawX1 == y1 = coerce rawX1 /2
     | otherwise = coerce rawY1
    y1
     | coerce rawY1 == y2 = 0
     | otherwise = coerce rawY1
    x2
     | coerce rawX2 == y2 = coerce rawX2 + coerce rawX2 / 2
     | otherwise = coerce rawX2
    y2 = coerce rawY2
    -- the point that our bisector is guaranteed to cross.
    x3 = x1 + x2
    y3 = y1 + y2
    -- the point that our bisector is supposed to cross.
    x4 = x1 * times + x2 * times
    y4 = y1 * times + y2 * times
    times = coerce rawTimes
    -- the two line segments we should cross.
    lineSeg1 = randomLineSeg x1 y1 x4 y4
    lineSeg2 = randomLineSeg x4 y4 x2 y2
    isEndPoint (Left (HitEndPoint _)) = True
    isEndPoint (Right PCollinear) = True
    isEndPoint (Right PAntiParallel) = True
    isEndPoint _ = False
    isStartPoint (Left (HitStartPoint _)) = True
    isStartPoint (Right PAntiCollinear) = True
    isStartPoint _ = False

-- ensure that every intersection with an EndPoint also intersects with a StartPoint.
prop_LineSegIntersectionStableAtOrigin :: NonZero ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_LineSegIntersectionStableAtOrigin d1 x1 y1 rawX2 rawY2
  | isEndPoint res1 &&
    isStartPoint res2 = True
  | otherwise = error
                $ "missed!\n"
                <> "lineSegTo: " <> show x1y1LineSegToOrigin <> "\n"
                <> "lineSegFrom: " <> show lineSegFromOrigin <> "\n"
                <> "pline through origin: " <> show pLineThroughOriginNotX1Y1NotOther <> "\n"
                <> "equivalent LineSeg: " <> show (makeLineSeg (Point2 (x2,y2)) (Point2 (0,0))) <> "\n"
                <> (if isStartPoint res2
                    then "Hit start."
                    else "Missed start.\n"
                         <> show res2 <> "\n"
                         <> show distanceStart
                   ) <> "\n"
                <> (if isEndPoint res1
                    then "Hit end."
                    else "Missed end: " <> show res1
                   ) <> "\n"
                <> "(x2,y2): " <> show (x2,y2) <> "\n"
  where
    res1 = intersectsWithErr (Right pLineThroughOriginNotX1Y1NotOther) (Left x1y1LineSegToOrigin :: Either LineSeg (ProjectiveLine, PLine2Err))
    res2 = intersectsWithErr (Right pLineThroughOriginNotX1Y1NotOther) (Left lineSegFromOrigin :: Either LineSeg (ProjectiveLine, PLine2Err))
    distanceStart = case res2 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (0,0), mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (0,0), mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    pLineThroughOriginNotX1Y1NotOther = randomPLineThroughOrigin x2 y2
    x1y1LineSegToOrigin = randomX1Y1LineSegToOrigin d1
    lineSegFromOrigin = randomLineSegFromOriginNotX1Y1 x1 y1
    isEndPoint (Left (HitEndPoint _)) = True
    isEndPoint (Right PCollinear) = True
    isEndPoint (Right PAntiParallel) = True
    isEndPoint _ = False
    isStartPoint (Left (HitStartPoint _)) = True
    isStartPoint (Right PAntiCollinear) = True
    isStartPoint _ = False
    (x2,y2)
      | rawX2 == 0 && rawY2 == 0 = (1,-1)
      | rawX2 == rawY2 = (x1,y1/2)
      | rawX2 == x1 && rawY2 == y1 = if rawX2/2 == rawY2/3
                           then (rawX2/3,rawY2/4)
                           else (rawX2/2,rawY2/3)
      | otherwise = (rawX2,rawY2)

-- ensure that every intersection with an EndPoint also intersects with a StartPoint.
prop_LineSegIntersectionStableAtX1Y1Point :: NonZero ℝ -> NonZero ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_LineSegIntersectionStableAtX1Y1Point pointD rawD1 x1 y1 rawX2 rawY2
  | isEndPoint res1 && isStartPoint res2 = True
  | otherwise = error
                $ "missed!\n"
                <> "x1y1 Point:" <> show (Point2 (d2,d2)) <> "\n"
                <> "x1y1SegTo: " <> show x1y1LineSegToPoint <> "\n"
                <> "lineSegFrom: " <> show lineSegFromPointNotX1Y1 <> "\n"
                <> "pline through x1y1: " <> show pLineThroughPointNotX1Y1NotOther <> "\n"
                <> "equivalent LineSeg: " <> show (makeLineSeg (Point2 (x2,y2)) (Point2 (d2,d2))) <> "\n"
                <> (if isStartPoint res2
                    then "Hit start."
                    else "Missed start: " <> show res2 <> "\n"
                         <> distanceStart
                   ) <> "\n"
                <> (if isEndPoint res1
                    then "Hit end."
                    else "Missed end: " <> show res1 <> "\n"
                         <> distanceEnd
                   ) <> "\n"
  where
    res1 = intersectsWithErr (Right pLineThroughPointNotX1Y1NotOther) (Left x1y1LineSegToPoint :: Either LineSeg (ProjectiveLine, PLine2Err))
    res2 = intersectsWithErr (Right pLineThroughPointNotX1Y1NotOther) (Left lineSegFromPointNotX1Y1 :: Either LineSeg (ProjectiveLine, PLine2Err))
    distanceStart = case res2 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (d2,d2),mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (d2,d2),mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    distanceEnd = case res1 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (d2,d2),mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (eToPP $ Point2 (d2,d2),mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    pLineThroughPointNotX1Y1NotOther = randomPLineThroughPoint x2 y2 d2
    x1y1LineSegToPoint = randomX1Y1LineSegToPoint d1 d2
    lineSegFromPointNotX1Y1 = randomLineSegFromPointNotX1Y1 x1 y1 d2
    isEndPoint (Left (HitEndPoint _)) = True
    isEndPoint (Right PCollinear) = True
    isEndPoint (Right PAntiCollinear) = True
    isEndPoint (Right PAntiParallel) = True
    isEndPoint _ = False
    isStartPoint (Left (HitStartPoint _)) = True
    isStartPoint (Right PAntiCollinear) = True
    isStartPoint (Right PAntiParallel) = True
    isStartPoint (Right PParallel) = True
    isStartPoint _ = False
    d2 :: ℝ
    d2
      | pointD == rawD1 = coerce rawD1 + coerce pointD
      | otherwise = coerce pointD
    d1 :: NonZero ℝ
    d1 = coerce rawD1
    (x2,y2)
      | rawX2 == 0 && rawY2 == 0 = (1,-1)
      | rawX2 == rawY2 = (x1,y1/2)
      | rawX2 == x1 && rawY2 == y1 = if rawX2/2 == rawY2/3
                           then (rawX2/3,rawY2/4)
                           else (rawX2/2,rawY2/3)
      | otherwise = (rawX2,rawY2)

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAlignedRightAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | xPos             = getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | not xPos && yPos = getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | otherwise        = getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,mag2-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset-mag2))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,mag2-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = getFirstArc (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = getFirstArc (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal  (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = getFirstArc (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = getFirstArc (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
-- NOTE: we use only one magnitude, because getOutsideArc requires normalized inputs.
prop_AxisAlignedRightAnglesOutside :: Bool -> Bool -> ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesOutside xPos yPos offset rawMagnitude
  | xPos && yPos =
    getOutsideArc (eToPP $ Point2 (offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset,offset+mag)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | xPos =
    getOutsideArc (eToPP $ Point2 (offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset,-(offset+mag))) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | not xPos && yPos =
    getOutsideArc (eToPP $ Point2 (-offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (-offset,offset+mag)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | otherwise =
    getOutsideArc (eToPP $ Point2 (-offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-offset,-(offset+mag))) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  where
    mag :: ℝ
    mag = coerce rawMagnitude

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
-- NOTE: we use only one magnitude, because getOutsideArc requires normalized inputs.
-- NOTE: execrises the point-out-point-out path of getOutsideArc.
-- FIXME: expressing this with the second line in each of these pairs the other direction (head->tail vs tail->head) results in falsification?
prop_AxisAligned135DegreeAnglesOutside :: Bool -> Bool -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAnglesOutside xPos yPos rawOffset rawMagnitude
  | xPos && yPos =
    getOutsideArc (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset+mag)) (Point2 (offset,offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | xPos =
    getOutsideArc (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-(offset+mag))) (Point2 (offset,-offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | not xPos && yPos =
    getOutsideArc (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset+mag), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset+mag)) (Point2 (-offset,offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | otherwise =
    getOutsideArc (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-(offset+mag))) (Point2 (-offset,-offset)))
                  `sameDirection`
                   NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  where
    mag,offset :: ℝ
    offset = coerce rawOffset
    mag = coerce rawMagnitude

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAlignedRightAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset))])
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | xPos             = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))])
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset))])
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | otherwise        = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-offset))])
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2))])
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-(offset-mag2)))])
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset-mag2))])
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-(offset-mag2)))])
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (makeENode (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset)))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = outOf (makeENode (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset)))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (makeENode (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset)))
                       `sameDirection`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = outOf (makeENode (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset)))
                       `sameDirection`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

prop_PPointJoinID :: ℝ -> ℝ -> Expectation
prop_PPointJoinID x y = fst (join2PP ppoint ppoint) --> PLine2 (GVec [])
  where
    ppoint = makeCPPoint2 x y

{-
-- | Test of dimensional accuracy.
-- make sure that the measured distance between two points that have been placed as close as possible is less than the amount of error placing both points added to the amount of error of doing a measurement of distance.
prop_PPointWithinErrRange :: ℝ -> ℝ -> Bool
prop_PPointWithinErrRange x y
  | res > realToFrac (p1Err + p2Err + resErr)  = error $ "res too big: " <> show res <> "\nulpSum1: " <> show p1Err <> "\nulpSum2: " <> show p2Err <> "\n"
  | otherwise = (p1 /= p2) || (res == 0 || error "the same, but distance?")
  where
    (res, UlpSum resErr) = distanceBetweenCPPointsWithErr p1 p2
    p1 = makeCPPoint2 x y
    p2 = makeCPPoint2 (x + realToFrac p1Err) (y + realToFrac p1Err)
-}

prop_LineSegWithinErrRange :: ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_LineSegWithinErrRange x1 y1 rawX2 rawY2
  | res1 > 0 = error "too big startPoint"
  | res2 > 0 = error "too big endPoint"
  | otherwise = True
  where
    res1 = distance (startPoint lineSeg) (Point2 (x1,y1))
    res2 = distance (endPoint lineSeg) (Point2 (x2,y2))
    lineSeg = randomLineSeg x1 y1 x2 y2
    (x2,y2)
     | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                    then (1,1)
                                    else (0,0)
     | otherwise = (rawX2, rawY2)

-- | A property test, ensuring that a point that is at least two times the pLine error away from a LineSeg does not show as being 'on' the line segment.
prop_LineSegDistanceAway ::  ℝ -> ℝ -> ℝ -> ℝ -> ℝ -> NonZero ℝ -> Bool -> Bool
prop_LineSegDistanceAway x1 y1 rawX2 rawY2 d1 rawD2 side
  | onSegment lineSeg (projectedPoint, mempty) = error $  "found point on segment:\n"
                                                       <> "Segment: " <> show lineSeg <> "\n"
                                                       <> "Point: " <> show projectedPoint <> "\n"
                                                       <> "Distance Given: " <> show distanceAway <> "\n"
                                                       <> "Distance found: " <> show (distancePPToPL (projectedPoint, mempty) pLine) <> "\n"
  | otherwise = True
  where
    (projectedPoint, _) = pPointOnPerpWithErr pl measuringPoint distanceAway
    distanceAway = (if side then 2 else (-2)) * if errAtMeasuringPoint == 0 then coerce rawD2 else errAtMeasuringPoint
    errAtMeasuringPoint = ulpVal $ fuzzinessOfL pLine <> pLineErrAtPPoint pLine measuringPoint
    (measuringPoint, _) = interpolate2PP (eToPP $ startPoint lineSeg) (eToPP $ endPoint lineSeg) d1 (coerce rawD2)
    pLine@(pl,_) = eToPL lineSeg
    lineSeg = randomLineSeg x1 y1 x2 y2
    (x2,y2)
      | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                     then (1,1)
                                     else (0,0)
      | otherwise = (rawX2, rawY2)

-- | A unit test for a failure of the above property test.
unit_LineSegDistanceAway :: Bool
unit_LineSegDistanceAway
  | onSegment lineSeg (projectedPoint, mempty) = error $  "found point on segment:\n"
                                                       <> "Segment: " <> show lineSeg <> "\n"
                                                       <> "Point: " <> show projectedPoint <> "\n"
                                                       <> "Distance Given: " <> show distanceAway <> "\n"
                                                       <> "Distance found: " <> show (distancePPToPL (projectedPoint, mempty) pLine) <> "\n"
  | otherwise = True
  where
    (projectedPoint, _) = pPointOnPerpWithErr pl measuringPoint distanceAway
    distanceAway = (if side then 2 else (-2)) * if errAtMeasuringPoint == 0 then coerce rawD2 else errAtMeasuringPoint
    errAtMeasuringPoint = ulpVal $ fuzzinessOfL pLine <> pLineErrAtPPoint pLine measuringPoint
    (measuringPoint, _) = interpolate2PP (eToPP $ startPoint lineSeg) (eToPP $ endPoint lineSeg) d1 (coerce rawD2)
    pLine@(pl,_) = eToPL lineSeg
    lineSeg = randomLineSeg x1 y1 x2 y2
    (x2,y2)
      | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                     then (1,1)
                                     else (0,0)
      | otherwise = (rawX2, rawY2)
    x1,y1,rawX2,rawY2,d1 :: ℝ
    x1 = 0.8
    y1 = 0.64
    rawX2 = 0.763
    rawY2 = 0.64
    d1 = 0.0
    rawD2 :: NonZero ℝ
    rawD2 = NonZero (-0.1)
    side = True

prop_obtuseBisectorOnBiggerSide_makeENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Expectation
prop_obtuseBisectorOnBiggerSide_makeENode x y d1 rawR1 d2 rawR2 testFirstLine
  | testFirstLine = pl1 `pLineIsLeft` bisector --> Just True
  | otherwise     = bisector `pLineIsLeft` pl2 --> Just True
  where
    (pl1, _) = eToPL $ getFirstLineSeg eNode
    pl2 = flipL $ fst $ eToPL $ getLastLineSeg eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    bisector = flipL $ outOf eNode

prop_obtuseBisectorOnBiggerSide_makeINode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Bool -> Expectation
prop_obtuseBisectorOnBiggerSide_makeINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = (angleFound >= (1-ulpVal angleErr), angleFound < realToFrac (-1 + (ulpRaw angleErr :: Rounded 'TowardInf ℝ))) --> (True, False)
  where
    (angleFound, (_,_, angleErr)) = angleBetween2PL bisector1 bisector2
    eNode = randomENode x y d1 rawR1 d2 rawR2
    iNode = randomINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2
    bisector1 = outOf iNode
    bisector2 = flipL $ outOf eNode

prop_eNodeTowardIntersection1 :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Expectation
prop_eNodeTowardIntersection1 x y d1 rawR1 d2 rawR2 = l1TowardIntersection --> True
  where
    l1TowardIntersection = towardIntersection (eToPP $ startPoint l1, mempty) pl1 eNodePoint
    eNodePoint = cPPointAndErrOf eNode
    l1 = getFirstLineSeg eNode
    pl1 = eToPL l1
    eNode = randomENode x y d1 rawR1 d2 rawR2

prop_eNodeAwayFromIntersection2 :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Expectation
prop_eNodeAwayFromIntersection2 x y d1 rawR1 d2 rawR2 = l2TowardIntersection --> False
  where
    l2TowardIntersection = towardIntersection (eToPP $ endPoint l2, mempty) pl2 eNodePoint
    eNodePoint = cPPointAndErrOf eNode
    l2 = getLastLineSeg eNode
    pl2 = eToPL l2
    eNode = randomENode x y d1 rawR1 d2 rawR2

prop_translateRotateMoves :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Expectation
prop_translateRotateMoves x y rawD rawR = distanceBetweenPPoints (translateRotatePPoint2 point d r) point /= 0 --> True
  where
    point = makeCPPoint2 x y
    r,d::ℝ
    r = coerce rawR
    d = coerce rawD

-- | translate a point along the X axis.
prop_translateRotateMovesX :: ℝ -> ℝ -> Positive ℝ -> Bool
prop_translateRotateMovesX x y rawD
  | myDistance <= ulpVal myDistanceErr = myDistance <= ulpVal myDistanceErr
  | otherwise = error $ "wtf\n" <> show myDistance <> "\n" <> show myDistanceErr <> "\n" <> show translateErr <> "\n"
  where
    (myDistance, (_,_, myDistanceErr)) = distance2PP (translatedPoint, mempty) (dstPoint, mempty)
    (translatedPoint, translateErr) = translateRotatePPoint2WithErr srcPoint d 0
    srcPoint = makeCPPoint2 x y
    dstPoint = makeCPPoint2 (x+d) y
    d::ℝ
    d = coerce rawD

-- | translate a point along the Y axis. really, translate, and rotate -90 degrees.
prop_translateRotateMovesY :: ℝ -> ℝ -> Positive ℝ -> Bool
prop_translateRotateMovesY x y rawD
  | myDistance <= myDistanceErr = myDistance <= myDistanceErr
  | otherwise = error $ "wtf\n" <> show myDistance <> "\n" <> show myDistanceErr <> "\n" <> show translateErr <> "\n" <> show srcPoint <> "\n" <> show dstPoint <> "\n" <> show translatedPoint <> "\n"
  where
    -- times ten, because.. uh.. epic fail to precision when rotating.
    myDistanceErr = 10 * ulpVal myDistanceErrRaw
    (myDistance, (_,_, myDistanceErrRaw)) = distance2PP (translatedPoint, mempty) (dstPoint, mempty)
    (translatedPoint, translateErr) = translateRotatePPoint2WithErr srcPoint d (-pi/2)
    srcPoint = makeCPPoint2 x y
    dstPoint = makeCPPoint2 x (y+d)
    d::ℝ
    d = coerce rawD

-- | ensure that a random PLine, when normed, is approximately equal to what went in.
prop_NormPLineIsPLine :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_NormPLineIsPLine x y dx dy = fst (normalizeL $ randomPLine x y dx dy)
                                  `sameDirection`
                                  fst (normalizeL (PLine2 $ vecOfL $ fst $ normalizeL $ randomPLine x y dx dy))

prop_PLinesIntersectAtOrigin :: NonZero ℝ -> ℝ -> NonZero ℝ -> ℝ -> Bool
prop_PLinesIntersectAtOrigin rawX y rawX2 rawY2
  | foundDistance <= ulpVal distanceErr = True
  | otherwise = error $ "failed to find intersection at origin for:\n"
                      <> show randomPLine1 <> "\n"
                      <> show randomPLine2 <> "\n"
                      <> show intersectionPPoint2 <> "\n"
  where
    originPPoint2 = makeCPPoint2 0 0
    (foundDistance, (_,_, distanceErr)) = distance2PP (originPPoint2,mempty) (intersectionPPoint2, intersectionErr)
    (intersectionPPoint2, (_, _, intersectionErr)) = intersect2PL pLine1 pLine2
    randomPLine1@(pLine1,_) = randomPLineThroughOrigin x y
    randomPLine2@(pLine2,_) = randomPLineThroughOrigin x2 y2
    x,x2,y2 :: ℝ
    x = coerce rawX
    x2
      | coerce rawX2 == x = if x == 1 then 2 else 1
      | otherwise = coerce rawX2
    y2
      | rawY2 == y = if y == 1 then 2 else 1
      | otherwise = rawY2

prop_PLinesIntersectAtPoint :: NonZero ℝ -> ℝ -> NonZero ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_PLinesIntersectAtPoint rawX y rawX2 rawY2 targetX targetY
  | foundDistance < errSum = True
  | otherwise = error
                $ "wtf\n"
                <> show foundDistance <> "\n"
                <> show errSum <> "\n"
                <> show targetPPoint2 <> "\n"
                <> show foundDistance <> "\n"
                <> show distanceErr <> "\n"
  where
    targetPPoint2 = makeCPPoint2 (coerce targetX) (coerce targetY)
    (foundDistance, (_,_, distanceErr)) = distance2PP (targetPPoint2,mempty) (intersectionPPoint2, intersectionErr)
    (intersectionPPoint2, (_, _, intersectionErr)) = intersect2PL pLine1 pLine2
    randomPLine1@(pLine1, _) = randomPLineWithErr x y targetX targetY
    randomPLine2@(pLine2, _) = randomPLineWithErr x2 y2 targetX targetY
    errSum = ulpVal $ pLineErrAtPPoint randomPLine1 targetPPoint2
                   <> pLineErrAtPPoint randomPLine2 targetPPoint2
                   <> distanceErr
    x,x2,y2 :: ℝ
    x = coerce rawX
    x2
      | coerce rawX2 == x = if x == 1 then 2 else 1
      | otherwise = coerce rawX2
    y2
      | rawY2 == y = if y == 1 then 2 else 1
      | otherwise = rawY2

prop_PLineIntersectsAtXAxis :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_PLineIntersectsAtXAxis x y dx dy m
  -- ignore the case where the random PLine is parallel to the X axis.
  | isNothing axisIntersection = True
  -- Ignore the case where the random PLine is colinear to the X axis.
  | isLeft $ fst $ fromJust axisIntersection = True
  | foundDistance < errSum = True
  | otherwise = error
                $ "wtf\n"
                <> show foundDistance <> "\n"
                <> show errSum <> "\n"
                <> show axisPLine <> "\n"
                <> show foundDistance <> "\n"
                <> show distanceErr <> "\n"
                <> show intersectionErr <> "\n"
  where
    errSum = ulpVal $ axisIntersectionErr <> distanceErr
    (foundDistance, (_,_, distanceErr)) = distance2PP (axisIntersectionPoint,mempty) (intersectionPPoint2, intersectionErr)
    axisIntersectionErr = pLineErrAtPPoint (randomPLine1, pline1Err) axisIntersectionPoint
    axisIntersectionPoint = eToPP $ Point2 (fromRight (error "not right?") $ fst $ fromJust axisIntersection, 0)
    axisIntersection = xIntercept (randomPLine1, pline1Err)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 axisPLine
    (randomPLine1, pline1Err) = randomPLineWithErr x y dx dy
    (axisPLine, _) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (coerce m,0))

prop_PLineIntersectsAtYAxis :: NonZero ℝ -> ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_PLineIntersectsAtYAxis x y x2 rawY2 m
  -- ignore the case where the random PLine is parallel to the X axis.
  | isNothing axisIntersection = True
  -- Ignore the case where the random PLine is colinear to the X axis.
  | isLeft $ fst $ fromJust axisIntersection = True
  | foundDistance < errSum = True
  | otherwise = error
                $ "wtf\n"
                <> show foundDistance <> "\n"
                <> show errSum <> "\n"
                <> show randomPLine1 <> "\n"
                <> show axisPLine <> "\n"
                <> show axisIntersectionPoint <> "\n"
                <> show intersectionPPoint2 <> "\n"
                <> show foundDistance <> "\n"
                <> show distanceErr <> "\n"
                <> show intersectionErr <> "\n"
  where
    errSum = ulpVal $ axisIntersectionErr <> distanceErr
    (foundDistance, (_,_, distanceErr)) = distance2PP (axisIntersectionPoint, mempty) (intersectionPPoint2, intersectionErr)
    axisIntersectionErr = snd $ fromJust axisIntersection
    axisIntersectionPoint = eToPP $ Point2 (0, fromRight (error "not right?") $ fst $ fromJust axisIntersection)
    axisIntersection = yIntercept (randomPLine1, pline1Err)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 axisPLine
    (randomPLine1, pline1Err) = randomPLineWithErr (coerce x) y (coerce x2) (coerce y2)
    (axisPLine, _) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (0,coerce m))
    y2 :: ℝ
    y2 = coerce rawY2

unit_LineContourIntersection1 :: Bool
unit_LineContourIntersection1 = isJust res
  where
    res = getLineContourIntersections line contour
    line = (PLine2 (GVec [GVal 1.9109380841879906 (fromList [GEZero 1]),GVal 1.0 (fromList [GEPlus 1]),GVal (-1.0) (fromList [GEPlus 2])]),
            PLine2Err [] [] (UlpSum 0.0) (UlpSum 0.0) (UlpSum 0.0) ([ErrVal (UlpSum 3.3306690738754696e-16) (fromList [GEPlus 2]),ErrVal (UlpSum 2.220446049250313e-16) (fromList [GEPlus 1]),ErrVal (UlpSum 2.220446049250313e-16) (fromList [GEZero 1])],[ErrVal (UlpSum 2.220446049250313e-16) (fromList [GEPlus 2])])
           )
    contour = LineSegContour (Point2 (95.69999999999982,95.69999999999982)) (Point2 (104.3000000000001,104.3000000000001)) (LineSeg (Point2 (95.69999999999982,95.69999999999982)) (Point2 (104.3000000000001,95.69999999999993))) (LineSeg (Point2 (104.3000000000001,95.69999999999993)) (Point2 (104.3,104.3))) (slist [LineSeg (Point2 (104.3,104.3)) (Point2 (95.69999999999993,104.3000000000001)), LineSeg (Point2 (95.69999999999993,104.3000000000001)) (Point2 (95.69999999999982,95.69999999999982))])

-- | a contour that failed.
unit_ContourFlip1 :: Bool
unit_ContourFlip1
  | isJust res = True
  | otherwise = error $ "fail!\n"
                      <> show (mostPerpPointAndLineSeg contour) <> "\n"
  where
    res = maybeFlipContour contour
    contour = makeLineSegContour
              [
                LineSeg (Point2 (93.5,94.90828649116408)) (Point2 (93.50076817859392,94.96819764062953))
              , LineSeg (Point2 (93.50076817859392,94.96819764062953)) (Point2 (93.5,95.0))
              , LineSeg (Point2 (93.5,95.0)) (Point2 (93.50076817859392,95.03180235937047))
              , LineSeg (Point2 (93.50076817859392,95.03180235937047)) (Point2 (93.5,95.09171350883592))
              , LineSeg (Point2 (93.5,95.09171350883592)) (Point2 (93.50802402291995,95.12805658872026))
              , LineSeg (Point2 (93.50802402291995,95.12805658872026)) (Point2 (93.51042508054853,95.17571341324486))
              , LineSeg (Point2 (93.51042508054853,95.17571341324486)) (Point2 (93.51650085187936,95.20928521609989))
              , LineSeg (Point2 (93.51650085187936,95.20928521609989)) (Point2 (93.53030551178848,95.3))
              , LineSeg (Point2 (93.53030551178848,95.3)) (Point2 (93.5303052036906,95.3))
              , LineSeg (Point2 (93.5303052036906,95.3)) (Point2 (93.54623152772308,95.36137354922205))
              , LineSeg (Point2 (93.54623152772308,95.36137354922205)) (Point2 (93.5543599506901,95.3926959847688))
              , LineSeg (Point2 (93.5543599506901,95.3926959847688)) (Point2 (93.56928030410899,95.45059503353703))
              , LineSeg (Point2 (93.56928030410899,95.45059503353703)) (Point2 (93.59377323794409,95.51632485154659))
              , LineSeg (Point2 (93.59377323794409,95.51632485154659)) (Point2 (93.62517113206894,95.6))
              , LineSeg (Point2 (93.62517113206894,95.6)) (Point2 (93.66980260367518,95.68796916309205))
              , LineSeg (Point2 (93.66980260367518,95.68796916309205)) (Point2 (93.70107642110081,95.75017147975183))
              , LineSeg (Point2 (93.70107642110081,95.75017147975183)) (Point2 (93.75361660957962,95.83016909486017))
              , LineSeg (Point2 (93.75361660957962,95.83016909486017)) (Point2 (93.79991901117867,95.9))
              , LineSeg (Point2 (93.79991901117867,95.9)) (Point2 (93.8,95.90018235516857))
              , LineSeg (Point2 (93.8,95.90018235516857)) (Point2 (93.86807142062428,95.97866928708568))
              , LineSeg (Point2 (93.86807142062428,95.97866928708568)) (Point2 (93.9393399,96.0606601))
              , LineSeg (Point2 (93.9393399,96.0606601)) (Point2 (94.02133071291432,96.13192857937572))
              , LineSeg (Point2 (94.02133071291432,96.13192857937572)) (Point2 (94.09981764483143,96.2))
              , LineSeg (Point2 (94.09981764483143,96.2)) (Point2 (94.1,96.20008098882133))
              , LineSeg (Point2 (94.1,96.20008098882133)) (Point2 (94.16983090513983,96.24638339042038))
              , LineSeg (Point2 (94.16983090513983,96.24638339042038)) (Point2 (94.24982852024817,96.29892357889919))
              , LineSeg (Point2 (94.24982852024817,96.29892357889919)) (Point2 (94.31203083690795,96.33019739632482))
              , LineSeg (Point2 (94.31203083690795,96.33019739632482)) (Point2 (94.4,96.37482886793106))
              , LineSeg (Point2 (94.4,96.37482886793106)) (Point2 (94.48367514845341,96.40622676205591))
              , LineSeg (Point2 (94.48367514845341,96.40622676205591)) (Point2 (94.54940496646297,96.43071969589101))
              , LineSeg (Point2 (94.54940496646297,96.43071969589101)) (Point2 (94.6073040152312,96.4456400493099))
              , LineSeg (Point2 (94.6073040152312,96.4456400493099)) (Point2 (94.7,96.46969448821152))
              , LineSeg (Point2 (94.7,96.46969448821152)) (Point2 (94.79071478390011,96.48349914812064))
              , LineSeg (Point2 (94.79071478390011,96.48349914812064)) (Point2 (94.82428658675514,96.48957491945147))
              , LineSeg (Point2 (94.82428658675514,96.48957491945147)) (Point2 (94.87194341127974,96.49197597708005))
              , LineSeg (Point2 (94.87194341127974,96.49197597708005)) (Point2 (94.90828649116408,96.5))
              , LineSeg (Point2 (94.90828649116408,96.5)) (Point2 (94.96819764062953,96.49923182140608))
              , LineSeg (Point2 (94.96819764062953,96.49923182140608)) (Point2 (95.0,96.5))
              , LineSeg (Point2 (95.0,96.5)) (Point2 (95.03180235937047,96.49923182140608))
              , LineSeg (Point2 (95.03180235937047,96.49923182140608)) (Point2 (95.09171350883592,96.5))
              , LineSeg (Point2 (95.09171350883592,96.5)) (Point2 (95.12805658872026,96.49197597708005))
              , LineSeg (Point2 (95.12805658872026,96.49197597708005)) (Point2 (95.17571341324486,96.48957491945147))
              , LineSeg (Point2 (95.17571341324486,96.48957491945147)) (Point2 (95.20928521609989,96.48349914812064))
              , LineSeg (Point2 (95.20928521609989,96.48349914812064)) (Point2 (95.3,96.46969448821152))
              , LineSeg (Point2 (95.3,96.46969448821152)) (Point2 (95.3926959847688,96.4456400493099))
              , LineSeg (Point2 (95.3926959847688,96.4456400493099)) (Point2 (95.45059503353703,96.43071969589101))
              , LineSeg (Point2 (95.45059503353703,96.43071969589101)) (Point2 (95.51632485154659,96.40622676205591))
              , LineSeg (Point2 (95.51632485154659,96.40622676205591)) (Point2 (95.6,96.37482886793106))
              , LineSeg (Point2 (95.6,96.37482886793106)) (Point2 (95.68796916309205,96.33019739632482))
              , LineSeg (Point2 (95.68796916309205,96.33019739632482)) (Point2 (95.75017147975183,96.29892357889919))
              , LineSeg (Point2 (95.75017147975183,96.29892357889919)) (Point2 (95.83016909486017,96.24638339042038))
              , LineSeg (Point2 (95.83016909486017,96.24638339042038)) (Point2 (95.9,96.20008098882133))
              , LineSeg (Point2 (95.9,96.20008098882133)) (Point2 (95.90018235516857,96.2))
              , LineSeg (Point2 (95.90018235516857,96.2)) (Point2 (95.97866928708568,96.13192857937572))
              , LineSeg (Point2 (95.97866928708568,96.13192857937572)) (Point2 (96.0606601,96.0606601))
              , LineSeg (Point2 (96.0606601,96.0606601)) (Point2 (96.13192857937572,95.97866928708568))
              , LineSeg (Point2 (96.13192857937572,95.97866928708568)) (Point2 (96.2,95.90018235516857))
              , LineSeg (Point2 (96.2,95.90018235516857)) (Point2 (96.20008098882133,95.9))
              , LineSeg (Point2 (96.20008098882133,95.9)) (Point2 (96.24638339042038,95.83016909486017))
              , LineSeg (Point2 (96.24638339042038,95.83016909486017)) (Point2 (96.29892357889919,95.75017147975183))
              , LineSeg (Point2 (96.29892357889919,95.75017147975183)) (Point2 (96.33019739632482,95.68796916309205))
              , LineSeg (Point2 (96.33019739632482,95.68796916309205)) (Point2 (96.37482886793106,95.6))
              , LineSeg (Point2 (96.37482886793106,95.6)) (Point2 (96.40622676205591,95.51632485154659))
              , LineSeg (Point2 (96.40622676205591,95.51632485154659)) (Point2 (96.43071969589101,95.45059503353703))
              , LineSeg (Point2 (96.43071969589101,95.45059503353703)) (Point2 (96.4456400493099,95.3926959847688))
              , LineSeg (Point2 (96.4456400493099,95.3926959847688)) (Point2 (96.46969448821152,95.3))
              , LineSeg (Point2 (96.46969448821152,95.3)) (Point2 (96.4696947963094,95.3))
              , LineSeg (Point2 (96.4696947963094,95.3)) (Point2 (96.47883479278863,95.23993819399247))
              , LineSeg (Point2 (96.47883479278863,95.23993819399247)) (Point2 (96.48349914812064,95.20928521609989))
              , LineSeg (Point2 (96.48349914812064,95.20928521609989)) (Point2 (96.48957491945147,95.17571341324486))
              , LineSeg (Point2 (96.48957491945147,95.17571341324486)) (Point2 (96.49197597708005,95.12805658872026))
              , LineSeg (Point2 (96.49197597708005,95.12805658872026)) (Point2 (96.5,95.09171350883592))
              , LineSeg (Point2 (96.5,95.09171350883592)) (Point2 (96.49923182140608,95.03180235937047))
              , LineSeg (Point2 (96.49923182140608,95.03180235937047)) (Point2 (96.5,95.0))
              , LineSeg (Point2 (96.5,95.0)) (Point2 (96.49923182140608,94.96819764062953))
              , LineSeg (Point2 (96.49923182140608,94.96819764062953)) (Point2 (96.5,94.90828649116408))
              , LineSeg (Point2 (96.5,94.90828649116408)) (Point2 (96.49197597708005,94.87194341127974))
              , LineSeg (Point2 (96.49197597708005,94.87194341127974)) (Point2 (96.48957491945147,94.82428658675514))
              , LineSeg (Point2 (96.48957491945147,94.82428658675514)) (Point2 (96.48349914812064,94.79071478390011))
              , LineSeg (Point2 (96.48349914812064,94.79071478390011)) (Point2 (96.46969448821152,94.7))
              , LineSeg (Point2 (96.46969448821152,94.7)) (Point2 (96.4696947963094,94.7))
              , LineSeg (Point2 (96.4696947963094,94.7)) (Point2 (96.45376847227692,94.63862645077795))
              , LineSeg (Point2 (96.45376847227692,94.63862645077795)) (Point2 (96.4456400493099,94.6073040152312))
              , LineSeg (Point2 (96.4456400493099,94.6073040152312)) (Point2 (96.43071969589101,94.54940496646297))
              , LineSeg (Point2 (96.43071969589101,94.54940496646297)) (Point2 (96.40622676205591,94.48367514845341))
              , LineSeg (Point2 (96.40622676205591,94.48367514845341)) (Point2 (96.37482886793106,94.4))
              , LineSeg (Point2 (96.37482886793106,94.4)) (Point2 (96.33019739632482,94.31203083690795))
              , LineSeg (Point2 (96.33019739632482,94.31203083690795)) (Point2 (96.29892357889919,94.24982852024817))
              , LineSeg (Point2 (96.29892357889919,94.24982852024817)) (Point2 (96.24638339042038,94.16983090513983))
              , LineSeg (Point2 (96.24638339042038,94.16983090513983)) (Point2 (96.20008098882133,94.1))
              , LineSeg (Point2 (96.20008098882133,94.1)) (Point2 (96.2,94.09981764483143))
              , LineSeg (Point2 (96.2,94.09981764483143)) (Point2 (96.13192857937572,94.02133071291432))
              , LineSeg (Point2 (96.13192857937572,94.02133071291432)) (Point2 (96.0606601,93.9393399))
              , LineSeg (Point2 (96.0606601,93.9393399)) (Point2 (95.97866928708568,93.86807142062428))
              , LineSeg (Point2 (95.97866928708568,93.86807142062428)) (Point2 (95.90018235516857,93.8))
              , LineSeg (Point2 (95.90018235516857,93.8)) (Point2 (95.9,93.79991901117867))
              , LineSeg (Point2 (95.9,93.79991901117867)) (Point2 (95.83016909486017,93.75361660957962))
              , LineSeg (Point2 (95.83016909486017,93.75361660957962)) (Point2 (95.75017147975183,93.70107642110081))
              , LineSeg (Point2 (95.75017147975183,93.70107642110081)) (Point2 (95.68796916309205,93.66980260367518))
              , LineSeg (Point2 (95.68796916309205,93.66980260367518)) (Point2 (95.6,93.62517113206894))
              , LineSeg (Point2 (95.6,93.62517113206894)) (Point2 (95.51632485154659,93.59377323794409))
              , LineSeg (Point2 (95.51632485154659,93.59377323794409)) (Point2 (95.45059503353703,93.56928030410899))
              , LineSeg (Point2 (95.45059503353703,93.56928030410899)) (Point2 (95.3926959847688,93.5543599506901))
              , LineSeg (Point2 (95.3926959847688,93.5543599506901)) (Point2 (95.3,93.53030551178848))
              , LineSeg (Point2 (95.3,93.53030551178848)) (Point2 (95.20928521609989,93.51650085187936))
              , LineSeg (Point2 (95.20928521609989,93.51650085187936)) (Point2 (95.17571341324486,93.51042508054853))
              , LineSeg (Point2 (95.17571341324486,93.51042508054853)) (Point2 (95.12805658872026,93.50802402291995))
              , LineSeg (Point2 (95.12805658872026,93.50802402291995)) (Point2 (95.09171350883592,93.5))
              , LineSeg (Point2 (95.09171350883592,93.5)) (Point2 (95.03180235937047,93.50076817859392))
              , LineSeg (Point2 (95.03180235937047,93.50076817859392)) (Point2 (95.0,93.5))
              , LineSeg (Point2 (95.0,93.5)) (Point2 (94.96819764062953,93.50076817859392))
              , LineSeg (Point2 (94.96819764062953,93.50076817859392)) (Point2 (94.90828649116408,93.5))
              , LineSeg (Point2 (94.90828649116408,93.5)) (Point2 (94.87194341127974,93.50802402291995))
              , LineSeg (Point2 (94.87194341127974,93.50802402291995)) (Point2 (94.82428658675514,93.51042508054853))
              , LineSeg (Point2 (94.82428658675514,93.51042508054853)) (Point2 (94.79071478390011,93.51650085187936))
              , LineSeg (Point2 (94.79071478390011,93.51650085187936)) (Point2 (94.7,93.53030551178848))
              , LineSeg (Point2 (94.7,93.53030551178848)) (Point2 (94.7,93.5303052036906))
              , LineSeg (Point2 (94.7,93.5303052036906)) (Point2 (94.63862645077795,93.54623152772308))
              , LineSeg (Point2 (94.63862645077795,93.54623152772308)) (Point2 (94.6073040152312,93.5543599506901))
              , LineSeg (Point2 (94.6073040152312,93.5543599506901)) (Point2 (94.54940496646297,93.56928030410899))
              , LineSeg (Point2 (94.54940496646297,93.56928030410899)) (Point2 (94.48367514845341,93.59377323794409))
              , LineSeg (Point2 (94.48367514845341,93.59377323794409)) (Point2 (94.4,93.62517113206894))
              , LineSeg (Point2 (94.4,93.62517113206894)) (Point2 (94.31203083690795,93.66980260367518))
              , LineSeg (Point2 (94.31203083690795,93.66980260367518)) (Point2 (94.25353875995638,93.69901822960128))
              , LineSeg (Point2 (94.25353875995638,93.69901822960128)) (Point2 (94.19794823502583,93.73505399213997))
              , LineSeg (Point2 (94.19794823502583,93.73505399213997)) (Point2 (94.1,93.8))
              , LineSeg (Point2 (94.1,93.8)) (Point2 (93.99141809848126,93.89417267448127))
              , LineSeg (Point2 (93.99141809848126,93.89417267448127)) (Point2 (93.9393399,93.9393399))
              , LineSeg (Point2 (93.9393399,93.9393399)) (Point2 (93.89417267448127,93.99141809848126))
              , LineSeg (Point2 (93.89417267448127,93.99141809848126)) (Point2 (93.8,94.1))
              , LineSeg (Point2 (93.8,94.1)) (Point2 (93.73505399213997,94.19794823502583))
              , LineSeg (Point2 (93.73505399213997,94.19794823502583)) (Point2 (93.69901822960128,94.25353875995638))
              , LineSeg (Point2 (93.69901822960128,94.25353875995638)) (Point2 (93.66980260367518,94.31203083690795))
              , LineSeg (Point2 (93.66980260367518,94.31203083690795)) (Point2 (93.62517113206894,94.4))
              , LineSeg (Point2 (93.62517113206894,94.4)) (Point2 (93.59377323794409,94.48367514845341))
              , LineSeg (Point2 (93.59377323794409,94.48367514845341)) (Point2 (93.56928030410899,94.54940496646297))
              , LineSeg (Point2 (93.56928030410899,94.54940496646297)) (Point2 (93.5543599506901,94.6073040152312))
              , LineSeg (Point2 (93.5543599506901,94.6073040152312)) (Point2 (93.53030551178848,94.7))
              , LineSeg (Point2 (93.53030551178848,94.7)) (Point2 (93.5303052036906,94.7))
              , LineSeg (Point2 (93.5303052036906,94.7)) (Point2 (93.52116520721137,94.76006180600753))
              , LineSeg (Point2 (93.52116520721137,94.76006180600753)) (Point2 (93.51650085187936,94.79071478390011))
              , LineSeg (Point2 (93.51650085187936,94.79071478390011)) (Point2 (93.51042508054853,94.82428658675514))
              , LineSeg (Point2 (93.51042508054853,94.82428658675514)) (Point2 (93.50802402291995,94.87194341127974))
              , LineSeg (Point2 (93.50802402291995,94.87194341127974)) (Point2 (93.5,94.90828649116408))
              ]

unit_ContourStraightSkeleton :: Bool
unit_ContourStraightSkeleton
  | isJust res = True
  | otherwise = error $ "fail!\n"
                      <> "Straight Skeleton: " <> show res <> "\n"
                      <> "could flip contour? " <> show (isJust $ maybeFlipContour contour) <> "\n"
                      <> "found crash tree: " <> show foundCrashTree <> "\n"
                      <> show (mostPerpPointAndLineSeg contour) <> "\n"
                      <> "Motorcycles: " <> show (length $ convexMotorcycles contour) <> "\n"
                      <> "Sides: " <> show (length $ lineSegsOfContour contour) <> "\n"
  where
    res = findStraightSkeleton contour []
    foundCrashTree = crashMotorcycles contour []
    contour = makePointContour
              [
                  Point2 (90.52523656434744,95.00000174891758)
                , Point2 (90.53142329122582,94.79396066779452)
                , Point2 (90.53329723636156,94.73153680661488)
                , Point2 (90.54033450891663,94.65368440793033)
                , Point2 (90.55756606924172,94.46308666918914)
                , Point2 (90.58651545318247,94.27236502645694)
                , Point2 (90.59831416553266,94.19463047494156)
                , Point2 (90.63427942825298,94.02726792711351)
                , Point2 (90.6565799102664,93.92350075036973)
                , Point2 (90.68091987240932,93.83312630349931)
                , Point2 (90.69161721584192,93.7934078846186)
                , Point2 (90.45798854627252,94.72464821764876)
                , Point2 (90.70851141288935,95.0035666558769)
                , Point2 (90.72470857221676,93.6787962257674)
                , Point2 (90.71686751134479,93.70462952271815)
                , Point2 (90.72952213831952,93.66287058994163)
                , Point2 (90.78070835247406,93.51653297708901)
                , Point2 (90.82520802497912,93.3892688534728)
                , Point2 (90.90624931444934,93.19804504021239)
                , Point2 (90.94150257884705,93.11489841608606)
                , Point2 (90.95572991964394,93.08485212346775)
                , Point2 (90.96313171312099,93.06922211966878)
                , Point2 (90.72652681410516,93.57890934126299)
                , Point2 (90.84262888113695,93.64614576163498)
                , Point2 (91.00457140327511,92.9877165187992)
                , Point2 (90.99729940346542,93.00153612830023)
                , Point2 (91.07310800340393,92.85443085993114)
                , Point2 (91.19549665585357,92.64817812784116)
                , Point2 (91.21433185597525,92.61531992422698)
                , Point2 (91.23159288196383,92.59000000562422)
                , Point2 (91.24249360257754,92.57306689824158)
                , Point2 (91.27416224820777,92.52306138001785)
                , Point2 (91.31292335349919,92.46920879270783)
                , Point2 (91.42314316680815,92.3110058858731)
                , Point2 (91.45203881053632,92.2737028943519)
                , Point2 (91.50856391778473,92.20084333118147)
                , Point2 (91.54860908146797,92.15316473577967)
                , Point2 (91.63803055044139,92.0466706450613)
                , Point2 (91.67743528902548,92.00382438277838)
                , Point2 (91.77893367224766,91.89353345085307)
                , Point2 (91.89347699284706,91.77899013025392)
                , Point2 (91.94074811840437,91.73551406197616)
                , Point2 (92.04675728664768,91.63795609422435)
                , Point2 (92.10101271492351,91.59238851031962)
                , Point2 (92.20068613513772,91.50869425480059)
                , Point2 (92.24723328986644,91.47263118569387)
                , Point2 (92.31116595249517,91.42303164854623)
                , Point2 (92.46920879270783,91.31292335349919)
                , Point2 (92.52306138001785,91.27416224820777)
                , Point2 (92.57306689824158,91.24249360257754)
                , Point2 (92.59000000562422,91.23159288196383)
                , Point2 (92.61531992422698,91.21433185597525)
                , Point2 (92.64817812784116,91.19549665585357)
                , Point2 (92.85443085993114,91.07310800340393)
                , Point2 (93.00153612830023,90.99729940346542)
                , Point2 (92.9877165187992,91.00457140327511)
                , Point2 (93.64614576163498,90.84262888113695)
                , Point2 (93.57890934126299,90.72652681410516)
                , Point2 (93.06921257352505,90.96313614459115)
                , Point2 (93.42500016128389,90.79466573133263)
                , Point2 (93.42500016128389,90.81008228377397)
                , Point2 (93.2553067080979,90.88199893489247)
                , Point2 (93.3893185640321,90.82518875825394)
                , Point2 (93.58014939008443,90.75843963386609)
                , Point2 (93.66283004683464,90.72953442450273)
                , Point2 (93.70462952271815,90.71686751134479)
                , Point2 (93.6787962257674,90.72470857221676)
                , Point2 (95.0035666558769,90.70851141288935)
                , Point2 (94.72464821764876,90.45798854627252)
                , Point2 (93.79340645273629,90.69161757507065)
                , Point2 (94.32499905369538,90.54844719520653)
                , Point2 (94.32499905369538,90.57030039121904)
                , Point2 (94.06540699489828,90.62608525342779)
                , Point2 (94.19463503093796,90.59831336971094)
                , Point2 (94.36182397553434,90.57293598272166)
                , Point2 (94.46308075203177,90.55756688471223)
                , Point2 (94.52907271560787,90.55160185380022)
                , Point2 (94.73154266868215,90.53329695028441)
                , Point2 (94.93394738885749,90.52721941116873)
                , Point2 (94.99999825080576,90.52523656434427)
                , Point2 (95.20603864909656,90.53142327070195)
                , Point2 (95.26845915434926,90.53329711511131)
                , Point2 (95.47092597864834,90.55160173577174)
                , Point2 (95.5369179009237,90.55756676293966)
                , Point2 (95.72763456627625,90.5865153913174)
                , Point2 (95.80536952523627,90.59831416556366)
                , Point2 (95.97273091437924,90.63427917952613)
                , Point2 (95.67500241241035,90.57029469842847)
                , Point2 (95.67500241241035,90.54844759006447)
                , Point2 (95.99233448829969,90.63391257540528)
                , Point2 (96.04267935701206,90.72130322420693)
                , Point2 (96.32120377431346,90.72470857222879)
                , Point2 (96.2953704828859,90.71686751302332)
                , Point2 (96.3371294100193,90.7295221382979)
                , Point2 (96.48346706519771,90.78070836720458)
                , Point2 (96.61073114647759,90.82520802495871)
                , Point2 (96.80195481893021,90.90624925476074)
                , Point2 (96.57500197520388,90.81002371716855)
                , Point2 (96.57500197520388,90.79466674295354)
                , Point2 (96.7505871557286,90.87780878877794)
                , Point2 (96.7808911720591,90.9476598147312)
                , Point2 (97.01228348120087,91.00457140326334)
                , Point2 (96.99846387205933,90.99729940364361)
                , Point2 (97.14556914002745,91.07310800337467)
                , Point2 (97.35182187137244,91.19549665538213)
                , Point2 (97.38468007534733,91.2143318557108)
                , Point2 (97.40999999109025,91.23159287974991)
                , Point2 (97.42693310995041,91.24249360775116)
                , Point2 (97.47693861989968,91.27416224814222)
                , Point2 (97.53079120744584,91.31292335360351)
                , Point2 (97.68899411397008,91.42314316669616)
                , Point2 (97.7262971254248,91.45203882586043)
                , Point2 (97.79915666886333,91.5085639178057)
                , Point2 (97.84683531111433,91.54860912088927)
                , Point2 (97.95332935540218,91.63803055082391)
                , Point2 (97.99617555962381,91.67743523601872)
                , Point2 (98.10646654911997,91.77893367221348)
                , Point2 (98.22100986988748,91.893476992981)
                , Point2 (98.26448598740386,91.94074817209514)
                , Point2 (98.36204390620595,92.04675728712832)
                , Point2 (98.40761130286943,92.10101249256334)
                , Point2 (98.49130574500188,92.20068613491986)
                , Point2 (98.52736879008327,92.2472332586774)
                , Point2 (98.5769683514337,92.31116595246483)
                , Point2 (98.68707664525635,92.46920879092053)
                , Point2 (98.72583775174817,92.52306137989812)
                , Point2 (98.75750639174875,92.57306688923168)
                , Point2 (98.76840712043132,92.5900000091495)
                , Point2 (98.7856681440925,92.61531992433784)
                , Point2 (98.80450334418863,92.64817812790682)
                , Point2 (98.92689199658932,92.8544308599146)
                , Point2 (99.00270059440984,93.0015361241765)
                , Point2 (98.99542859666329,92.9877165185938)
                , Point2 (99.05234018524767,93.21910882794803)
                , Point2 (99.12220223165113,93.24941762536655)
                , Point2 (99.04427031185622,93.08485261291638)
                , Point2 (99.0584797213727,93.11486103656337)
                , Point2 (99.11800101000759,93.25530657805943)
                , Point2 (99.17481124170774,93.3893185639057)
                , Point2 (99.24156033966989,93.58014931441157)
                , Point2 (99.27046557560026,93.66283004715103)
                , Point2 (99.28313248099097,93.70462949739651)
                , Point2 (99.27529142774954,93.67879622559256)
                , Point2 (99.27869677577166,93.9573206429153)
                , Point2 (99.36608892343524,94.00766637510688)
                , Point2 (99.31908204351448,93.833133417053)
                , Point2 (99.34341914371794,93.92349723780309)
                , Point2 (99.37391472738378,94.06540690717972)
                , Point2 (99.4016866303127,94.19463503106583)
                , Point2 (99.42706361269667,94.36182130792311)
                , Point2 (99.44243311530897,94.46308075223536)
                , Point2 (99.44839819551188,94.52907326162845)
                , Point2 (99.46670304970871,94.73154266868238)
                , Point2 (99.47278063225963,94.93394883753771)
                , Point2 (99.47476343565559,94.99999825070864)
                , Point2 (99.46857670970934,95.20603930178119)
                , Point2 (99.46670276362087,95.26846319370384)
                , Point2 (99.45966555325245,95.34631490361156)
                , Point2 (99.44243393075298,95.53691333087573)
                , Point2 (99.41348459608173,95.72763464802651)
                , Point2 (99.40168583447436,95.8053695250555)
                , Point2 (99.36572072216647,95.97273137104125)
                , Point2 (99.34342008973506,96.07649924960621)
                , Point2 (99.31908080382114,96.16687118413628)
                , Point2 (99.30838278414635,96.2065921153522)
                , Point2 (99.54201145370311,95.27535178237046)
                , Point2 (99.29148858710127,94.99643334415902)
                , Point2 (99.27529142777428,96.3212037742354)
                , Point2 (99.2831325042824,96.29537042580031)
                , Point2 (99.27047786178193,96.33712940977867)
                , Point2 (99.21929163217742,96.48346706695861)
                , Point2 (99.17479197502927,96.61073114649997)
                , Point2 (99.09375071189352,96.80195489765781)
                , Point2 (99.05849742129114,96.88510158355312)
                , Point2 (99.04426994211163,96.91514816784013)
                , Point2 (99.03686829621603,96.93077786057788)
                , Point2 (99.27347318599062,96.42109065888862)
                , Point2 (99.23145620824842,96.39675800702065)
                , Point2 (98.9887378945167,97.0255582518726)
                , Point2 (98.92689199658712,97.14556914008277)
                , Point2 (98.80450334376476,97.35182187280202)
                , Point2 (98.78566814411468,97.38468007559177)
                , Point2 (98.76840712264506,97.40999998756496)
                , Point2 (98.75750638475104,97.42693312179145)
                , Point2 (98.72583775177804,97.47693862002909)
                , Point2 (98.68707664602151,97.53079120798513)
                , Point2 (98.57685683330901,97.68899411396428)
                , Point2 (98.5479611708476,97.72629712964388)
                , Point2 (98.4914360824291,97.79915666856763)
                , Point2 (98.45139095863219,97.84683521640524)
                , Point2 (98.36196944949798,97.95332935502714)
                , Point2 (98.32256476526807,97.99617555817369)
                , Point2 (98.22106632740235,98.1064665495116)
                , Point2 (98.10652300728816,98.22100986962587)
                , Point2 (98.05925189022336,98.264485930152)
                , Point2 (97.95324271337233,98.36204390576556)
                , Point2 (97.89898734388426,98.40761144019713)
                , Point2 (97.7993138653753,98.49130574476509)
                , Point2 (97.75276674051467,98.52736879068212)
                , Point2 (97.68883404738446,98.57696835154178)
                , Point2 (97.53079120798513,98.68707664602151)
                , Point2 (97.47693862002909,98.72583775177804)
                , Point2 (97.42693312179145,98.75750638475104)
                , Point2 (97.40999998756496,98.76840712264506)
                , Point2 (97.38468007559177,98.78566814411468)
                , Point2 (97.35182187280202,98.80450334376476)
                , Point2 (97.14556914008277,98.92689199658712)
                , Point2 (97.0255582518726,98.9887378945167)
                , Point2 (96.39675800702065,99.23145620824842)
                , Point2 (96.42109065888862,99.27347318599062)
                , Point2 (96.93078740986465,99.03686386328597)
                , Point2 (96.5749992941212,99.20533452655235)
                , Point2 (96.57499929412121,99.18991794702926)
                , Point2 (96.74469323969993,99.11800108723378)
                , Point2 (96.61068143584744,99.174811241793)
                , Point2 (96.41985067443075,99.2415603436121)
                , Point2 (96.3371699526587,99.27046557566783)
                , Point2 (96.29537042580031,99.2831325042824)
                , Point2 (96.3212037742354,99.27529142777428)
                , Point2 (94.99643334415902,99.29148858710127)
                , Point2 (95.27535178237046,99.54201145370311)
                , Point2 (96.20659354843383,99.30838242461583)
                , Point2 (95.67499388234063,99.45155470728702)
                , Point2 (95.67499388234063,99.429701126789)
                , Point2 (95.9345930669576,99.37391473310454)
                , Point2 (95.80536496906157,99.40168663029196)
                , Point2 (95.63818083325516,99.42706328822015)
                , Point2 (95.53691924734949,99.44243311535939)
                , Point2 (95.47092637362476,99.44839822852755)
                , Point2 (95.26845733130304,99.46670304971644)
                , Point2 (95.06605397617153,99.47278054771773)
                , Point2 (95.00000174901494,99.4747634356524)
                , Point2 (94.79396001510987,99.46857668918547)
                , Point2 (94.73153680663427,99.46670276363139)
                , Point2 (94.65368398104647,99.45966545242192)
                , Point2 (94.4630866691153,99.4424339307494)
                , Point2 (94.27236628806229,99.41348473831613)
                , Point2 (94.19463047496532,99.40168583448144)
                , Point2 (94.02726922936988,99.36572085121198)
                , Point2 (94.32499905369538,99.42970561665034)
                , Point2 (94.32499905369538,99.45155280479676)
                , Point2 (94.00766551163201,99.36608742458083)
                , Point2 (93.9573206429153,99.27869677577166)
                , Point2 (93.67879622559256,99.27529142774954)
                , Point2 (93.70462949739651,99.28313248099097)
                , Point2 (93.6628705901826,99.27047786176048)
                , Point2 (93.5165329755313,99.21929164704339)
                , Point2 (93.3892688534513,99.17479197500914)
                , Point2 (93.19804506646996,99.0937506966894)
                , Point2 (93.4249974802012,99.18997605194838)
                , Point2 (93.4249974802012,99.20533299911274)
                , Point2 (93.24941284425896,99.12219121115578)
                , Point2 (93.23517951491657,99.0893832609998)
                , Point2 (92.97444174818017,98.98873789454744)
                , Point2 (92.8544308599146,98.92689199658932)
                , Point2 (92.64817812790682,98.80450334418863)
                , Point2 (92.61531992433784,98.7856681440925)
                , Point2 (92.5900000091495,98.76840712043132)
                , Point2 (92.57306688923168,98.75750639174875)
                , Point2 (92.52306137989812,98.72583775174817)
                , Point2 (92.46920879092053,98.68707664525635)
                , Point2 (92.3110058858783,98.57685683319652)
                , Point2 (92.2737028901376,98.5479611862004)
                , Point2 (92.20084333147669,98.49143608244958)
                , Point2 (92.15316483055265,98.45139099806059)
                , Point2 (92.04667064499658,98.36196944951091)
                , Point2 (92.00382439167613,98.32256471915899)
                , Point2 (91.89353345084716,98.22106632775365)
                , Point2 (91.77899013010654,98.10652300701301)
                , Point2 (91.73551401344223,98.05925182882105)
                , Point2 (91.63795609367615,97.95324271274465)
                , Point2 (91.59238870718953,97.89898751943518)
                , Point2 (91.50869425483597,97.79931386489767)
                , Point2 (91.47263118510182,97.75276670932834)
                , Point2 (91.42303164843861,97.68883404735429)
                , Point2 (91.31292335360351,97.53079120744584)
                , Point2 (91.27416224814222,97.47693861989968)
                , Point2 (91.24249360775116,97.42693310995041)
                , Point2 (91.23159287974991,97.40999999109025)
                , Point2 (91.2143318557108,97.38468007534733)
                , Point2 (91.19549665538213,97.35182187137244)
                , Point2 (91.07310800337467,97.14556914002745)
                , Point2 (91.01126210549467,97.02555825191367)
                , Point2 (90.91061673901486,96.7648204851061)
                , Point2 (90.87779776835599,96.75058237465284)
                , Point2 (90.95572946023208,96.9151469057644)
                , Point2 (90.94152027883361,96.88513896392662)
                , Point2 (90.88199894680963,96.74469332004125)
                , Point2 (90.8251887582333,96.6106814359307)
                , Point2 (90.75843964697009,96.4198506474299)
                , Point2 (90.72953442443512,96.33716995297458)
                , Point2 (90.71686751302332,96.2953704828859)
                , Point2 (90.72470857222879,96.32120377431346)
                , Point2 (90.72130322420693,96.04267935701206)
                , Point2 (90.63391107647031,95.99233362477842)
                , Point2 (90.68092111210268,96.1668783002161)
                , Point2 (90.65658085625388,96.07650276206564)
                , Point2 (90.62608523245162,95.93459290804716)
                , Point2 (90.59831336970156,95.80536496899053)
                , Point2 (90.57293630616262,95.63817815459794)
                , Point2 (90.55756688466185,95.5369192475533)
                , Point2 (90.55160173577174,95.47092597864834)
                , Point2 (90.53329695028086,95.26845733116474)
                , Point2 (90.52721945817349,95.0660541767252)
                ]

facetBrokenSpec :: Spec
facetBrokenSpec = do
  describe "Geometry (Contour)" $ do
    it "creates a straight skeleton from a contour (unit)"
      unit_ContourStraightSkeleton
  describe "Stability (Lines)" $ do
    it "points that are further than the accumulated error away from a line segment are not onSegment" $
      property unit_LineSegDistanceAway
  -- FIXME: re-establish the sanity of these, and their implementations.
  describe "PGA (Point Stability)" $ do
    it "a line constructed with the midpoint of a segment and a point on the perpendicular bisector is at 90 degrees to the initial segment" $
      property prop_perpAt90Degrees
    it "successfully translates PPoint2s along X" $
      property prop_translateRotateMovesX
    it "successfully translates PPoint2s along Y" $
      property prop_translateRotateMovesY
  describe "PGA (Intersection Stability)" $ do
    it "finds that the intersection of two PLines at an arbitrary point are within the returned UlpSum" $
      property prop_PLinesIntersectAtPoint
    it "finds endpoints and startpoints in equal quantities along the X1Y1 line" $
      property prop_LineSegIntersectionStableAtX1Y1Point
    it "finds an endpoint and a startpoint across a quad from a bisector from the origin" $
      property prop_QuadBisectorCrosses
    it "finds an endpoint and a startpoint the multiple of the discante across a quad from a bisector from the origin" $
      property prop_QuadBisectorCrossesMultiple
  describe "PGA (Output Arcs)" $ do
    it "finds the outside arc of two intersecting lines (inverted makeENode)" $
      property prop_obtuseBisectorOnBiggerSide_makeENode
    it "finds the outside arc of two intersecting lines (makeINode)" $
      property prop_obtuseBisectorOnBiggerSide_makeINode

facetFlakeySpec :: Spec
facetFlakeySpec = do
  describe "Stability (Lines)" $ do
    it "points that are further than the accumulated error away from a line segment are not onSegment" $
      property prop_LineSegDistanceAway
  describe "Stability (Intersections)" $ do
    it "finds that the intersection of two PLines at the origin are within the returned UlpSum" $
      property prop_PLinesIntersectAtOrigin
    it "finds endpoints and startpoints in equal quantities at the origin" $
      property prop_LineSegIntersectionStableAtOrigin

facetSpec :: Spec
facetSpec = do
  describe "Stability (Points)" $ do
    it "the join of two identical points is an empty projective line" $
      property prop_PPointJoinID
  describe "Stability (Lines)" $ do
    it "both ends of a line segment are within UlpSum of the points they were constructed with" $
      property prop_LineSegWithinErrRange
    it "a normalized line normalized again is approximately itsself" $
      property prop_NormPLineIsPLine
    it "a projective line is colinear with itsself" $
      property prop_PLineSameDirectionID
  describe "Stability (Error)" $ do
    it "finds that the X intersection of a random PLine is within the returned UlpSum" $
      property prop_PLineIntersectsAtXAxis
    it "finds that the Y intersection of a random PLine is within the returned UlpSum" $
      property prop_PLineIntersectsAtYAxis
  describe "Arcs (Skeleton/Concave)" $ do
    it "finds the inside arcs of right angles with their sides parallel to the axises (raw)" $
      property prop_AxisAlignedRightAngles
    it "finds the inside arcs of 135 degree angles with one side parallel to an axis (raw)" $
      property prop_AxisAligned135DegreeAngles
    it "finds the inside arcs of 45 degree angles with one side parallel to an axis (raw)" $
      property prop_AxisAligned45DegreeAngles
    it "finds the outside arcs of right angles with their sides parallel to the axises" $
      property prop_AxisAlignedRightAnglesOutside
    it "finds the outside arcs of 135 degree angles with their sides parallel to the axises" $
      property prop_AxisAligned135DegreeAnglesOutside
    it "finds the inside arcs of right angles with their sides parallel to the axises (enode)" $
      property prop_AxisAlignedRightAnglesInENode
    it "finds the inside arcs of 135 degree angles with one side parallel to an axis (enode)" $
      property prop_AxisAligned135DegreeAnglesInENode
    it "finds the inside arcs of 45 degree angles with one side parallel to an axis (enode)" $
      property prop_AxisAligned45DegreeAnglesInENode
    it "sees that the first input line into an ENode is toward the point" $
      property prop_eNodeTowardIntersection1
    it "sees that the second input line into an ENode is away from the point" $
      property prop_eNodeAwayFromIntersection2
    it "successfully translates and rotates PPoint2s" $
      property prop_translateRotateMoves
    it "finds the arc resulting from a node at the intersection of the outArc of two nodes (corner3 and corner4 of c2)" $
      averageNodes c2c3E1 c2c4E1 --> INode (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))], []))
                                           (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 1.7071067811865475 (singleton (GEPlus 2))])
                                           , PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))] [] mempty mempty mempty mempty)
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 1.089790213551637 (singleton (GEPlus 1)), GVal 0.21677275132473928 (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 2.7755575615628911e-17) (singleton (GEPlus 2))] [] mempty mempty mempty mempty))
    it "finds the outside arc of two PLines intersecting at 90 degrees (c2)" $
      averageNodes c2c2E1 c2c3E1 --> INode (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))], []))
                                           (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[]))
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))] [] mempty mempty mempty mempty))

    it "finds the outside arc of two PLines intersecting at 90 degrees (c2)" $
      averageNodes c2c3E1 c2c2E1 --> INode (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))], []))
                                           (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))], []))
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))] [] mempty mempty mempty mempty))
    it "finds the outside arc of two PLines intersecting at 90 degrees (c7)" $
      averageNodes c7c1E1 c7c2E1 --> INode (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 1)), GVal 1.0 (singleton (GEPlus 2))])
                                           , PLine2Err [] [] mempty mempty mempty ([], []))
                                           (PLine2 (GVec [GVal 1.5 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))])
                                           , PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEZero 1))] [] mempty mempty mempty mempty)
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal 1.0606601717798212 (singleton (GEZero 1)), GVal (-1.414213562373095) (singleton (GEPlus 1))]),PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))] [] mempty mempty mempty mempty))
  describe "Motorcycles (Skeleton/Motorcycles)" $ do
    it "finds the motorcycle in our second simple shape" $
      convexMotorcycles c1 --> [Motorcycle (LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,0.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0))) (PLine2 (GVec [GVal 1.414213562373095 (singleton (GEPlus 1))])) (PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))] [] mempty mempty mempty mempty)]
  describe "Cells (Skeleton/Cells)" $ do
    it "finds an infill line (unit)"
      unit_LineContourIntersection1
    it "flips a contour (unit)"
      unit_ContourFlip1
  describe "Sorting (Skeleton)" $ do
    it "sorts PLines (normal ID)" $
      sortPLinesByReference pl1 [pl3, pl2] --> [pl3, pl2]
    it "sorts PLines (normal 1)" $
      sortPLinesByReference pl1 [pl2, pl3] --> [pl3, pl2]
    it "sorts PLines (normal 2)" $
      sortPLinesByReference pl3 [pl2, pl1] --> [pl2, pl1]
    it "sorts PLines (normal 3)" $
      sortPLinesByReference pl2 [pl1, pl3] --> [pl1, pl3]
    it "sorts PLines (anticolinear ID)" $
      sortPLinesByReference pl1 [pl4, pl2] --> [pl4, pl2]
    it "sorts PLines (anticolinear 1)" $
      sortPLinesByReference pl1 [pl2, pl4] --> [pl4, pl2]
    it "sorts PLines (anticolinear 2)" $
      sortPLinesByReference pl4 [pl1, pl2] --> [pl2, pl1]
    it "sorts PLines (anticolinear 3)" $
      sortPLinesByReference pl2 [pl4, pl1] --> [pl1, pl4]
    it "sorts PLines (anticolinear 4)" $
      sortPLinesByReference pl1 [pl2, pl4, pl3] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 4A)" $
      sortPLinesByReference pl1 [pl4, pl3, pl2] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 4B)" $
      sortPLinesByReference pl1 [pl3, pl2, pl4] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 4C)" $
      sortPLinesByReference pl1 [pl3, pl4, pl2] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 4D)" $
      sortPLinesByReference pl1 [pl4, pl2, pl3] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 4E)" $
      sortPLinesByReference pl1 [pl2, pl3, pl4] --> [pl3, pl4, pl2]
    it "sorts PLines (anticolinear 5)" $
      sortPLinesByReference pl3 [pl1, pl2, pl4] --> [pl4, pl2, pl1]
    it "sorts PLines (anticolinear 5A)" $
      sortPLinesByReference pl3 [pl2, pl4, pl1] --> [pl4, pl2, pl1]
    it "sorts PLines (anticolinear 5B)" $
      sortPLinesByReference pl3 [pl4, pl1, pl2] --> [pl4, pl2, pl1]
    it "sorts PLines (anticolinear 6)" $
      sortPLinesByReference pl4 [pl3, pl1, pl2] --> [pl2, pl1, pl3]
    it "sorts PLines (anticolinear 7)" $
      sortPLinesByReference pl2 [pl4, pl3, pl1] --> [pl1, pl3, pl4]
  where
      c1 = makePointContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      -- The next corners are part of a 2x2 square around the origin with a piece missing: (c2 from above)
      --    __  <-- corner 1
      --   | /
      --   | \
      --   ~~~  <-- corner 3
      --   ^-- corner 4
      -- the top and the left side.
      c2c2E1 = makeENode (Point2 (1.0,1.0)) (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
      -- the left and the bottom side.
      c2c3E1 = makeENode (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
      -- the bottom and the entrance to the convex angle.
      c2c4E1 = makeENode (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0)) (Point2 (0.0,0.0))
      -- The next corners are part of a 2x2 square around the origin with a slice and a corner missing: (c7 from above)
      --        v----- corner 2
      --  ┌───┐ ┌─┐<-- corner 1
      --  │    \│ │
      --  └───┐   │
      --      │   │
      --      └───┘
      c7c1E1 = makeENode (Point2 (1.0,-1.0)) (Point2 (1.0,1.0)) (Point2 (0.5,1.0))
      c7c2E1 = makeENode (Point2 (1.0,1.0)) (Point2 (0.5,1.0)) (Point2 (0.5,0.0))
      -- a line to the origin, from the positive Y direction
      pl1 = eToPL $ makeLineSeg (Point2 (0,1)) (Point2 (0,0))
      -- a line to the origin, from the positive X direction
      pl2 = eToPL $ makeLineSeg (Point2 (2,0)) (Point2 (0,0))
      -- a line to the origin, from the -x-y direction.
      pl3 = eToPL $ makeLineSeg (Point2 (-3,-3)) (Point2 (0,0))
      -- a line to the origin, from the negative Y direction
      pl4 = eToPL $ makeLineSeg (Point2 (0,-4)) (Point2 (0,0))
