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

module Math.PGA (linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec, facetFlakeySpec, contourSpec, lineSpec) where

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

import Data.Maybe (fromMaybe, fromJust, isJust, isNothing, Maybe(Just, Nothing))

import Data.Set (singleton, fromList)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

-- Slists, a form of list with a stated size in the structure.
import Slist (slist, len)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, getContours, numPointsOfContour, lineSegsOfContour, makeLineSegContour, makePointContour, maybeFlipContour)

import Graphics.Slicer.Math.ContourIntersections (getLineContourIntersections)

import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg), Contour(LineSegContour), pointsOfContour, roundPoint2, startPoint, distance, makeLineSeg, endPoint)

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
import Graphics.Slicer.Math.Contour (mostPerpPointAndLineSeg)
import Graphics.Slicer.Math.Skeleton.Cells (findFirstCellOfContour, findDivisions, findNextCell)
import Graphics.Slicer.Math.Skeleton.Concave (averageNodes, makeENode, makeENodes)
import Graphics.Slicer.Math.Skeleton.Definitions (Cell(Cell), INode(INode), Motorcycle(Motorcycle), RemainingContour(RemainingContour), getFirstLineSeg, getLastLineSeg)
import Graphics.Slicer.Math.Skeleton.Face (facesOf, orderedFacesOf)
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles, CrashTree(CrashTree))
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), cellFrom, edgesOf, generationsOf, randomConvexQuad, randomConvexBisectableQuad, randomConcaveChevronQuad, randomENode, randomINode, randomLineSeg, randomPLine, randomPLineWithErr, remainderFrom, onlyOne, onlyOneOf, randomPLineThroughOrigin, randomX1Y1LineSegToOrigin, randomLineSegFromOriginNotX1Y1, randomX1Y1LineSegToPoint, randomLineSegFromPointNotX1Y1, randomPLineThroughPoint)

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
      makeLineSegContour (combineConsecutiveLineSegs $ lineSegsOfContour c1) --> (makeLineSegContour $ lineSegsOfContour c1)
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

-- | A property test making sure that the wedge product of two PLines along two different axises is always in e1e2.
prop_TwoAxisAlignedLines :: NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> Expectation
prop_TwoAxisAlignedLines d1 d2 r1 r2 = (\(GVec gVals) -> bases gVals) (vecOfL (eToPLine2 (makeLineSeg (Point2 (coerce d1,0)) (Point2 (coerce r1 - coerce d1,0)))) ∧ vecOfL (eToPLine2 (makeLineSeg (Point2 (0,coerce d2)) (Point2 (0,coerce d2 - coerce r2))))) --> [fromList [GEPlus 1, GEPlus 2]]
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

-- ensure that the bisector of a quad crosses the point across the quad from the bisector.
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
    intersect1 = intersectsWithErr (Right (PLine2 bisector1, bisector1Err)) (Left lineSeg1 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect2 = intersectsWithErr (Right (PLine2 bisector1, bisector1Err)) (Left lineSeg2 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (NPLine2 bisector1, bisector1Err) = normalizeL bisector
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
                <> show (angleBetween2PL (outOf eNode) (PLine2 bisector1)) <> "\n"
                <> show (errOfOut eNode) <> "\n"
                <> "(" <> show x3 <> "," <> show y3 <> ")\n"
                <> "(" <> show x4 <> "," <> show y4 <> ")\n"
  where
    intersect1 = intersectsWithErr (Right (PLine2 bisector1, bisector1Err)) (Left lineSeg1 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect2 = intersectsWithErr (Right (PLine2 bisector1, bisector1Err)) (Left lineSeg2 :: Either LineSeg (ProjectiveLine, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (NPLine2 bisector1, bisector1Err) = normalizeL bisector
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
  | xPos && yPos     = (getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
  | xPos             = (getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
  | not xPos && yPos = (getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  | otherwise        = (getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = (getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | xPos             = (getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,mag2-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | not xPos && yPos = (getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset-mag2)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | otherwise        = (getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,mag2-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = (getFirstArc (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | xPos             = (getFirstArc (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal  (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | not xPos && yPos = (getFirstArc (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | otherwise        = (getFirstArc (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset)))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
-- NOTE: we use only one magnitude, because getOutsideArc requires normalized inputs.
prop_AxisAlignedRightAnglesOutside :: Bool -> Bool -> ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesOutside xPos yPos offset rawMagnitude
  | xPos && yPos = (
    getOutsideArc (eToPP $ Point2 (offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset,offset+mag)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  | xPos = (
    getOutsideArc (eToPP $ Point2 (offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset,-(offset+mag))) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  | not xPos && yPos = (
    getOutsideArc (eToPP $ Point2 (-offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (-offset,offset+mag)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
  | otherwise = (
    getOutsideArc (eToPP $ Point2 (-offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-offset,-(offset+mag))) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
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
  | xPos && yPos = (
    getOutsideArc (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset+mag)) (Point2 (offset,offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | xPos = (
    getOutsideArc (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-(offset+mag))) (Point2 (offset,-offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | not xPos && yPos = (
    getOutsideArc (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset+mag), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset+mag)) (Point2 (-offset,offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | otherwise = (
    getOutsideArc (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-(offset+mag))) (Point2 (-offset,-offset))))
                  `sameDirection`
                   (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  where
    mag,offset :: ℝ
    offset = coerce rawOffset
    mag = coerce rawMagnitude

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAlignedRightAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
  | xPos             = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
  | not xPos && yPos = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  | otherwise        = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-offset))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | xPos             = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-(offset-mag2)))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | not xPos && yPos = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset-mag2))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | otherwise        = (outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-(offset-mag2)))]))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween2PL to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = (outOf (makeENode (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset))))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | xPos             = (outOf (makeENode (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
  | not xPos && yPos = (outOf (makeENode (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset))))
                       `sameDirection`
                       (NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  | otherwise        = (outOf (makeENode (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset))))
                       `sameDirection`
                       (NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

prop_ConvexBisectableQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadNoDivides x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findDivisions convexBisectableQuad (fromMaybe (error $ show convexBisectableQuad) $ crashMotorcycles convexBisectableQuad []) --> []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findStraightSkeleton convexBisectableQuad [] -/> Nothing
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = generationsOf (findStraightSkeleton convexBisectableQuad []) --> 1
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = facesOf (fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) -/> slist []
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> 4
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexBisectableQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexBisectableQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexBisectableQuad) $ findStraightSkeleton convexBisectableQuad []) --> convexBisectableQuadAsSegs
  where
    convexBisectableQuad = randomConvexBisectableQuad x y rawFirstTilt rawSecondTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    convexBisectableQuadAsSegs = lineSegsOfContour convexBisectableQuad
    firstSeg = onlyOneOf convexBisectableQuadAsSegs

prop_ConvexQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadNoDivides x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = findDivisions convexQuad (fromMaybe (error $ show convexQuad) $ crashMotorcycles convexQuad []) --> []
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = findStraightSkeleton convexQuad [] -/> Nothing
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = generationsOf (findStraightSkeleton convexQuad []) --> 1
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = facesOf (fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) -/> slist []
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) --> 4
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner

prop_ConvexQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexQuadFacesInOrder x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexQuad) $ findStraightSkeleton convexQuad []) --> convexQuadAsSegs
  where
    convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt thirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner rawThirdDistanceToCorner
    convexQuadAsSegs = lineSegsOfContour convexQuad
    firstSeg = onlyOneOf convexQuadAsSegs

prop_ConcaveChevronQuadOneDivide :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadOneDivide a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = len (slist $ findDivisions concaveChevronQuad (fromMaybe (error $ show concaveChevronQuad) $ crashMotorcycles concaveChevronQuad [])) --> 1

prop_ConcaveChevronQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasStraightSkeleton a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = findStraightSkeleton concaveChevronQuad [] -/> Nothing

prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = generationsOf (findStraightSkeleton concaveChevronQuad []) --> 1

prop_ConcaveChevronQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadCanPlaceFaces a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = facesOf (fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) -/> slist []

prop_ConcaveChevronQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadHasRightFaceCount a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = length (facesOf $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> 4

prop_ConcaveChevronQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConcaveChevronQuadFacesInOrder a b c d e f = doTest $ randomConcaveChevronQuad a b c d e f
  where
    doTest concaveChevronQuad = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show concaveChevronQuad) $ findStraightSkeleton concaveChevronQuad []) --> concaveChevronQuadAsSegs
      where
        concaveChevronQuadAsSegs = lineSegsOfContour concaveChevronQuad
        firstSeg = onlyOneOf concaveChevronQuadAsSegs

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

prop_obtuseBisectorOnBiggerSide_makeENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Expectation
prop_obtuseBisectorOnBiggerSide_makeENode x y d1 rawR1 d2 rawR2 testFirstLine
  | testFirstLine = pLineIsLeft bisector pl1 --> Just True
  | otherwise     = pLineIsLeft pl2 bisector --> Just True
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
                                  fst (normalizeL ((\(NPLine2 a) -> PLine2 a) $ fst $ normalizeL $ randomPLine x y dx dy))

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

prop_PLineIntersectsAtXAxis :: ℝ -> ℝ -> NonZero ℝ -> ℝ -> NonZero ℝ -> Bool
prop_PLineIntersectsAtXAxis x y rawX2 y2 m
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
    axisIntersectionPoint = eToPP $ Point2 ((fromRight (error "not right?") $ fst $ fromJust axisIntersection), 0)
    axisIntersection = xIntercept (randomPLine1, pline1Err)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 axisPLine
    (randomPLine1, pline1Err) = randomPLineWithErr x y rawX2 (coerce y2)
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
    axisIntersectionPoint = eToPP $ Point2 (0,(fromRight (error "not right?") $ fst $ fromJust axisIntersection))
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

facetFlakeySpec :: Spec
facetFlakeySpec = do
  describe "Stability (Points)" $ do
--    it "both of the points used to construct a PLine2 are within UlpSum of the PLine2" $
--      property prop_PLineWithinErrRange1
    it "a line constructed with the midpoint of a segment and a point on the perpendicular bisector is at 90 degrees to the initial segment" $
      property prop_perpAt90Degrees
    it "successfully translates PPoint2s along X" $
      property prop_translateRotateMovesX
    it "successfully translates PPoint2s along Y" $
      property prop_translateRotateMovesY
  describe "Stability (Intersections)" $ do
    it "finds that the intersection of two PLines at an arbitrary point are within the returned UlpSum" $
      property prop_PLinesIntersectAtPoint
    it "finds endpoints and startpoints in equal quantities along the X1Y1 line" $
      property prop_LineSegIntersectionStableAtX1Y1Point
    it "finds an endpoint and a startpoint across a quad from a bisector from the origin" $
      property prop_QuadBisectorCrosses
    it "finds an endpoint and a startpoint the multiple of the discante across a quad from a bisector from the origin" $
      property prop_QuadBisectorCrossesMultiple
  describe "Faces (Concave Chevron)" $ do
    it "places faces on the straight skeleton of a concave chevron quad" $
      property prop_ConcaveChevronQuadCanPlaceFaces
    it "finds only four faces for any concave chevron quad" $
      property prop_ConcaveChevronQuadHasRightFaceCount
    it "places faces on a concave chevron quad in the order the line segments were given" $
      property prop_ConcaveChevronQuadFacesInOrder
  describe "Arcs (Skeleton/Concave)" $ do
    it "finds the outside arc of two intersecting lines (inverted makeENode)" $
      property prop_obtuseBisectorOnBiggerSide_makeENode
    it "finds the outside arc of two intersecting lines (makeINode)" $
      property prop_obtuseBisectorOnBiggerSide_makeINode

facetSpec :: Spec
facetSpec = do
  describe "Stability (Points)" $ do
    it "the join of two identical points is an empty projective line" $
      property prop_PPointJoinID
--    it "placing a Point within a the error range of another point results in a point a distance away thats less than the sum of the two errors" $
--      property prop_PPointWithinErrRange
--    it "both of the points used to join a PLine2 are within UlpSum of the PLine2" $
--      property prop_PLineWithinErrRange2
--    it "a point on the perpendicular bisector is within distance+UlpSum of a PLine2" $
--      property prop_PPointOnPerpWithinErrRange
  describe "Stability (Lines)" $ do
    it "both ends of a line segment are within UlpSum of the points they were constructed with" $
      property prop_LineSegWithinErrRange
    it "a normalized line normalized again is approximately itsself" $
      property prop_NormPLineIsPLine
    it "points that are further than the accumulated error away from a line segment are not onSegment" $
      property prop_LineSegDistanceAway
    it "a projective line is colinear with itsself" $
      property prop_PLineSameDirectionID
  describe "Stability (Intersections)" $ do
    it "finds that the intersection of two PLines at the origin are within the returned UlpSum" $
      property prop_PLinesIntersectAtOrigin
    it "finds endpoints and startpoints in equal quantities at the origin" $
      property prop_LineSegIntersectionStableAtOrigin
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
    it "finds no divides in a convex bisectable quad" $
      property prop_ConvexBisectableQuadNoDivides
    it "finds the straight skeleton of a convex bisectable quad (property)" $
      property prop_ConvexBisectableQuadHasStraightSkeleton
    it "only generates one generation for a convex bisectable quad" $
      property prop_ConvexBisectableQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a convex bisectable quad" $
      property prop_ConvexBisectableQuadCanPlaceFaces
    it "finds only four faces for any convex bisectable quad" $
      property prop_ConvexBisectableQuadHasRightFaceCount
    it "places faces on a convex bisectable quad in the order the line segments were given" $
      property prop_ConvexBisectableQuadFacesInOrder
    it "finds no divides in a convex quad" $
      property prop_ConvexQuadNoDivides
    it "finds the straight skeleton of a convex quad (property)" $
      property prop_ConvexQuadHasStraightSkeleton
    it "only generates one generation for a convex quad" $
      property prop_ConvexQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a convex quad" $
      property prop_ConvexQuadCanPlaceFaces
    it "finds only four faces for any convex quad" $
      property prop_ConvexQuadHasRightFaceCount
    it "places faces on a convex quad in the order the line segments were given" $
      property prop_ConvexQuadFacesInOrder
    it "finds one divide in a concave chevron quad" $
      property prop_ConcaveChevronQuadOneDivide
    it "finds the straight skeleton of a concave chevron quad (property)" $
      property prop_ConcaveChevronQuadHasStraightSkeleton
    it "only generates one generation for a concave chevron quad" $
      property prop_ConcaveChevronQuadStraightSkeletonHasRightGenerationCount
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
                                           , PLine2Err [] [] mempty mempty mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[]))
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
    it "finds the remains from the first cell of our first simple shape." $
      remainderFrom (findFirstCellOfContour c0 $ findDivisions c0 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c0 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                                            , LineSeg (Point2 (1.0,-1.0)) (Point2 (1.0,1.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the remains from the first cell of our second simple shape." $
      remainderFrom (findFirstCellOfContour c1 $ findDivisions c1 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c1 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,1.0))
                                            , LineSeg (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,0.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the remains from the first cell of our third simple shape." $
      remainderFrom (findFirstCellOfContour c2 $ findDivisions c2 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c2 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                                            , LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,0.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the remains from the first cell of our fourth simple shape." $
      remainderFrom (findFirstCellOfContour c3 $ findDivisions c3 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c3 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,1.0))
                                            , LineSeg (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the remains from the first cell of our fifth simple shape." $
      remainderFrom (findFirstCellOfContour c4 $ findDivisions c4 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c4 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                                            , LineSeg (Point2 (1.0,-1.0)) (Point2 (1.0,1.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the remains from the first cell of our sixth simple shape." $
      remainderFrom (findFirstCellOfContour c5 $ findDivisions c5 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c5 []) -->
      Just [RemainingContour (slist [(slist [
                                              LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))
                                            , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                                            , LineSeg (Point2 (1.0,-1.0)) (Point2 (2.0,0.0))
                                            ]
                                     ,
                                       [])
                                    ])
           ]
    it "finds the second cell of our second simple shape." $
      cellFrom (findNextCell $ onlyOne $ fromMaybe (error "Got Nothing") $ remainderFrom $ findFirstCellOfContour c1 $ findDivisions c1 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c1 []) -->
      Cell (slist [(slist [
                            LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,1.0))
                          , LineSeg (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
                          , LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,0.0))
                          ],Nothing)])
    it "finds the second cell of our fifth simple shape." $
      cellFrom (findNextCell $ onlyOne $ fromMaybe (error "Got Nothing") $ remainderFrom $ findFirstCellOfContour c4 $ findDivisions c4 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c4 []) -->
      Cell (slist [(slist [
                            LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))
                          , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                          , LineSeg (Point2 (1.0,-1.0)) (Point2 (1.0,1.0))
                          ],Nothing)])
    it "finds the second cell of our sixth simple shape." $
      cellFrom (findNextCell $ onlyOne $ fromMaybe (error "Got Nothing") $ remainderFrom $ findFirstCellOfContour c5 $ findDivisions c5 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c5 []) -->
      Cell (slist [(slist [
                            LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))
                          , LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
                          , LineSeg (Point2 (1.0,-1.0)) (Point2 (2.0,0.0))
                          ],Nothing)])
{-  describe "NodeTrees (Skeleton/Cell)" $ do
    it "finds the eNodes of our sixth simple shape." $
      eNodesOfOutsideContour c5 --> [
                                      ENode (Point2 (-1.0,-1.0), Point2 (1.0,-1.0),Point2 (2.0,0.0))
                                            (PLine2 (GVec [GVal (-0.5411961001461969) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                            (UlpSum 6.8833827526759706e-15) 1.7071067811865475
                                    , ENode (Point2 (1.0,-1.0), Point2 (2.0,0.0),Point2 (1.0,1.0))
                                            (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 2))]))
                                            (UlpSum 5.773159728050814e-15) 1.4142135623730951
                                    , ENode (Point2 (2.0,0.0), Point2 (1.0,1.0),Point2 (-1.0,1.0))
                                            (PLine2 (GVec [GVal 0.5411961001461969 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                            (UlpSum 6.8833827526759706e-15) 1.7071067811865475
                                    , ENode (Point2 (1.0,1.0), Point2 (-1.0,1.0), Point2 (0.0,0.0))
                                            (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                            (UlpSum 5.773159728050814e-15) 1.7071067811865475
                                    , ENode (Point2 (0.0,0.0), Point2 (-1.0,-1.0), Point2 (1.0,-1.0))
                                            (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                            (UlpSum 5.773159728050814e-15) 1.7071067811865475]
    it "finds the motorcycle of our sixth simple shape" $
      convexMotorcycles c5 --> [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))])) (UlpSum 2.220446049250313e-16) 2.8284271247461903]
    it "finds the crashtree of our fifth shape." $
      crashMotorcycles c5 [] --> Just (CrashTree (slist [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))])) (UlpSum 2.220446049250313e-16) 2.8284271247461903])
                                                 (slist [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))])) (UlpSum 2.220446049250313e-16) 2.8284271247461903])
                                                 (slist []))
    it "finds the divide of our sixth shape." $
      findDivisions c5 (fromMaybe (error "Got Nothing") $ crashMotorcycles c5 [])
      --> [CellDivide
            (DividingMotorcycles
              (Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))])) (UlpSum 2.220446049250313e-16) 2.8284271247461903)
              (slist []))
            (WithENode $ ENode (Point2 (1.0,-1.0), Point2 (2.0,0.0), Point2 (1.0,1.0)) (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 2))])) (UlpSum 5.773159728050815e-15) 1.4142135623730951)
          ]
    it "finds the motorcycles of our eigth simple shape." $
      convexMotorcycles c7 --> [Motorcycle (LineSeg (Point2 (0.5,1.0)) (Point2 (0.0,-1.0)), LineSeg (Point2 (0.5,0.0)) (Point2 (-0.5,1.0)))
                                           (PLine2 (GVec [GVal 0.9472135954999579 (singleton (GEZero 1)), GVal (-1.8944271909999157) (singleton (GEPlus 1)), GVal (-0.4472135954999579) (singleton (GEPlus 2))]))
                                           (UlpSum 3.885780586188048e-16)
                                           2.118033988749895
                               ,Motorcycle (LineSeg (Point2 (-1.0,0.0)) (Point2 (1.0,0.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (0.0,-1.0)))
                                           (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 1)), GVal (-1.0) (singleton (GEPlus 2))]))
                                           (UlpSum 4.440892098500626e-16)
                                           2.0
                               ]
-}
    it "finds a CrashTree of our eigth simple shape." $
      crashMotorcycles c7 [] -->
        Just (CrashTree (slist $ convexMotorcycles c7)
                        (slist $ convexMotorcycles c7)
                        (slist []))
{-
  describe "insets (Skeleton/Line)" $ do
    it "insets a triangle (unit)" $
      addInset 1 0.25 (facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton triangle [])
      --> ([LineSegContour (Point2 (0.4330127018922193,0.25))
                           (Point2 (1.5669872981077808,1.2320508075688772))
                           (LineSeg (Point2 (0.4330127018922193,0.25)) (Point2 (1.5669872981077808,0.2500000000000001)))
                           (LineSeg (Point2 (1.5669872981077808,0.2500000000000001)) (Point2 (1.0,1.2320508075688771)))
                           (slist [LineSeg (Point2 (1.0,1.2320508075688771)) (Point2 (0.4330127018922193,0.25))])]
          ,[Face (LineSeg (Point2 (2.433012701892219,-0.25)) (Point2 (-0.43301270189221924,-0.25)))
                 (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)),GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
           ,Face (LineSeg (Point2 (1.0,2.232050807568877)) (Point2 (2.4330127018922192,-0.25)))
                 (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                 (slist [])
                 (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
           ,Face (LineSeg (Point2 (-0.43301270189221935,-0.25000000000000006)) (Point2 (1.0,2.232050807568877)))
                 (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
           ])
-}
    it "finds an infill line (unit)" $
      unit_LineContourIntersection1
    it "flips a contour (unit)" $
      unit_ContourFlip1
    where
      -- c0 - c4 are the contours of a square around the origin with a 90 degree chunk missing, rotated 0, 90, 180, 270 and 360 degrees:
      --    __
      --    \ |
      --    /_|
      --
      c0 = makePointContour [Point2 (0,0), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c1 = makePointContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c2 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (0,0), Point2 (1,1), Point2 (-1,1)]
      c3 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (0,0), Point2 (-1,1)]
      c4 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c5 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (2,0), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
--      c6 = makePointContour [Point2 (-1,-1), Point2 (-0.5,-1), Point2 (0,0), Point2 (0.5,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c7 = makePointContour [Point2 (0,-1), Point2 (1,-1), Point2 (1,1), Point2 (0.5,1), Point2 (0.5,0), Point2 (0,1), Point2 (-1,1), Point2 (-1,0), Point2 (0,0)]
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
      -- A simple triangle.
--      triangle = makePointContour [Point2 (2,0), Point2 (1.0,sqrt 3), Point2 (0,0)]
