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

-- So we can add Eq instances here, instead of in the library.
{-# LANGUAGE StandaloneDeriving #-}

-- Ignore the orphan instances we create for testing purposes.
{-# OPTIONS_GHC -Wno-orphans #-}

module Math.PGA (linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec, facetFlakeySpec, contourSpec, lineSpec) where

-- Be explicit about what we import.
import Prelude (($), Bool(True, False), Eq, Show, (<$>), (==), (>=), error, (/=), (<=), mempty, otherwise, abs, (&&), (+), show, length, (<>), fst, not, length, realToFrac, sqrt, (<), (>), (-), (/), (||), (*), snd)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, NonZero(NonZero), Positive(Positive))

import Data.Coerce (coerce)

import Data.Either (Either(Left, Right), fromRight, isLeft)

import Data.List (foldl')

import Data.Maybe (fromJust, fromMaybe, isNothing, Maybe(Just, Nothing))

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf))

import Data.Set (singleton, fromList)

import Slist (slist, len)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- A euclidian point.
import Graphics.Slicer.Math.Definitions(Point2(Point2), Contour(LineSegContour), LineSeg(LineSeg), roundPoint2, startPoint, distance, xOf, yOf, minMaxPoints, makeLineSeg, endPoint)

-- Our Geometric Algebra library.
import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(GEZero, GEPlus, G0), GVal(GVal), GVec(GVec), UlpSum(UlpSum), addValPairWithErr, subValPairWithErr, addValWithErr, ulpRaw, ulpVal, subVal, addVecPair, subVecPair, mulScalarVecWithErr, divVecScalarWithErr, scalarPart, vectorPart, (•), (∧), (⋅), (⎣), (⎤))

import Graphics.Slicer.Math.Intersections (outputIntersectsLineSeg)

import Graphics.Slicer.Math.Lossy (canonicalizePPoint2, distanceBetweenPPoints, distancePPointToPLine, eToPLine2, getFirstArc, join2PPoint2, pPointOnPerp)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (CPPoint2(CPPoint2), NPLine2(NPLine2), PPoint2(PPoint2), PLine2(PLine2), PLine2Err(PLine2Err), ProjectiveLine2, canonicalizeP, distance2PP, distancePPToPL, eToPL, eToPP, eToPP, interpolate2PP, intersect2PL, translateL, translateRotatePPoint2WithErr, angleBetween2PL, flipL, join2PP, makeCPPoint2, normalizeL, pLineIsLeft, pPointsOnSameSideOfPLine, Intersection(HitStartPoint, HitEndPoint, NoIntersection), PIntersection(PCollinear, PAntiCollinear, PParallel, PAntiParallel, IntersectsIn), cPPointAndErrOf, distance2PL, intersectsWithErr, errOfOut, pPointOnPerpWithErr, outOf, vecOfL)


-- The primitives of our PGA only library, and error estimation code.
import Graphics.Slicer.Math.PGAPrimitives (pLineErrAtPPoint, xIntercept, yIntercept)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, getContours, pointsOfContour, numPointsOfContour, justOneContourFrom, lineSegsOfContour, makeLineSegContour, makePointContour, insideIsLeft, innerContourPoint, firstPointPairOfContour, firstLineSegOfContour)

-- Our imprecise Contour library.
import Graphics.Slicer.Machine.Contour (shrinkContour, expandContour)

-- Our Infill library.
import Graphics.Slicer.Machine.Infill (InfillType(Horiz, Vert), makeInfill)

-- Our Facet library.
import Graphics.Slicer.Math.Arcs (getOutsideArc, towardIntersection)
import Graphics.Slicer.Math.Skeleton.Cells (findFirstCellOfContour, findDivisions, findNextCell)
import Graphics.Slicer.Math.Skeleton.Concave (makeENode, makeENodes)
import Graphics.Slicer.Math.Skeleton.Definitions (Motorcycle(Motorcycle), RemainingContour(RemainingContour), Spine(Spine), StraightSkeleton(StraightSkeleton), Cell(Cell), getFirstLineSeg, getLastLineSeg)
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)
import Graphics.Slicer.Math.Skeleton.Line (addInset)

import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles, crashMotorcycles, CrashTree(CrashTree))
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->), (-/>))

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- Our Geometry generation library. For random shaped geometry.
import Graphics.Slicer.Math.RandomGeometry (ListThree, Radian(Radian), cellFrom, edgesOf, generationsOf, randomTriangle, randomRectangle, randomSquare, randomConvexQuad, randomConvexSingleRightQuad, randomConvexDualRightQuad, randomConvexBisectableQuad, randomConcaveChevronQuad, randomENode, randomINode, randomLineSeg, randomPLine, randomPLineWithErr, remainderFrom, onlyOne, onlyOneOf, randomPLineThroughOrigin, randomX1Y1LineSegToOrigin, randomLineSegFromOriginNotX1Y1, randomX1Y1LineSegToPoint, randomLineSegFromPointNotX1Y1, randomPLineThroughPoint)

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

deriving instance Eq RemainingContour
deriving instance Show RemainingContour

deriving instance Eq Cell

deriving instance Eq Spine

deriving instance Eq StraightSkeleton

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
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]
    oocl1 = [(Point2 (1,0), Point2 (0,0)), (Point2 (0,1), Point2 (1,1)), (Point2 (0,0), Point2 (0,1)), (Point2 (1,1), Point2 (1,0))]
    cl1 = [(Point2 (0,0), Point2 (0,1)), (Point2 (0,1), Point2 (1,1)), (Point2 (1,1), Point2 (1,0)), (Point2 (1,0), Point2 (0,0))]
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
  describe "Contours (Skeleton/line)" $ do
    it "a contour algorithmically shrunk has the same amount of points as the input contour" $
      numPointsOfContour (justOneContourFrom $ addInset 1 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c1 []) --> numPointsOfContour c1
    it "a contour algorithmically shrunk and mechanically expanded is about equal to where it started" $
      roundPoint2 <$> pointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] $ justOneContourFrom $ addInset 1 0.1 $ orderedFacesOf c2l1 $ fromMaybe (error "got Nothing") $ findStraightSkeleton c2 []) --> roundPoint2 <$> pointsOfContour c2
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]
    c1 = makePointContour cp1
    cl1 = makeLineSegContour (lineSegsOfContour c1)
    c2 = makePointContour [Point2 (0.75,0.25), Point2 (0.75,0.75), Point2 (0.25,0.75), Point2 (0.25,0.25)]
    c2l1 = firstLineSegOfContour c2

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
      subValPairWithErr (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2))) --> [(GVal 1 (singleton (GEPlus 1)),mempty), (GVal (-1.0) (singleton (GEPlus 2)),mempty)]
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
    it "multiplies a (multi)vector by a scalar (mulScalarVec)" $
      mulScalarVecWithErr 2 (GVec [GVal 1 (singleton (GEPlus 1))]) --> (GVec [GVal 2 (singleton (GEPlus 1))],[ErrVal (UlpSum 4.440892098500626e-16) (singleton (GEPlus 1))])
    it "multiplies a (multi)vector by a scalar (G0)" $
      GVec [GVal 2 (singleton G0)] • GVec [GVal 1 (singleton (GEPlus 1))] --> GVec [GVal 2 (singleton (GEPlus 1))]
    -- 2e1/2 = e1
    it "divides a (multi)vector by a scalar" $
      divVecScalarWithErr (GVec [GVal 2 (singleton (GEPlus 1))]) 2 --> (GVec [GVal 1 (singleton (GEPlus 1))],[ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))])
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
    rawPPoint2 (x,y) = (\(CPPoint2 v) -> v) $ eToPP (Point2 (x,y))

-- | A property test making sure that the wedge product of two PLines along two different axises is always in e1e2.
prop_TwoAxisAlignedLines :: NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> NonZero ℝ -> Expectation
prop_TwoAxisAlignedLines d1 d2 r1 r2 = (\(GVec gVals) -> bases gVals) ((\(PLine2 a) -> a) (eToPLine2 (makeLineSeg (Point2 (coerce d1,0)) (Point2 (coerce r1 - coerce d1,0)))) ∧ (\(PLine2 a) -> a) (eToPLine2 (makeLineSeg (Point2 (0,coerce d2)) (Point2 (0,coerce d2 - coerce r2))))) --> [fromList [GEPlus 1, GEPlus 2]]
  where
    bases gvals = (\(GVal _ base) -> base) <$> gvals

-- | A property test making sure that the scalar part of the big-dot product of two identical PLines is not zero.
prop_TwoOverlappingLinesScalar :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_TwoOverlappingLinesScalar x y dx dy = scalarPart ((\(PLine2 a) -> a) (randomPLine x y dx dy) • (\(PLine2 a) -> a) (randomPLine x y dx dy)) /= 0

-- | A property test for making sure that there is never a vector result of the big-dot product of two identical PLines.
prop_TwoOverlappingLinesVector :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Expectation
prop_TwoOverlappingLinesVector x y dx dy = vectorPart ((\(PLine2 a) -> a) (randomPLine x y dx dy) • (\(PLine2 a) -> a) (randomPLine x y dx dy)) --> GVec []

proj2DGeomAlgSpec :: Spec
proj2DGeomAlgSpec = do
  describe "Points (Math/PGA)" $
    -- ((1e0^1e1)+(-1e0^1e2)+(1e1+1e2))|((-1e0^1e1)+(1e0^1e2)+(1e1+1e2)) = -1
    it "the dot product of any two projective points is -1" $
      property prop_ScalarDotScalar
  describe "Lines (Math/PGA)" $ do
    -- (-2e2)*2e1 = 4e12
    it "the intersection of a line along the X axis and a line along the Y axis is the origin point" $
      (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (-1,0)) (Point2 (1,0)))) ∧ (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (0,-1)) (Point2 (0,1)))) --> GVec [GVal 4 (fromList [GEPlus 1, GEPlus 2])]
    it "the intersection of two axis aligned lines is a multiple of e1e2" $
      property prop_TwoAxisAlignedLines
    -- (-2e0+1e1)^(2e0-1e2) = -1e01+2e02-e12
    it "the intersection of a line two points above the X axis, and a line two points to the right of the Y axis is at (2,2) in the upper right quadrant" $
      vectorPart ((\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (2,0)) (Point2 (2,1)))) ∧ (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (0,2)) (Point2 (1,2))))) -->
      GVec [GVal (-2) (fromList [GEZero 1, GEPlus 1]), GVal 2 (fromList [GEZero 1, GEPlus 2]), GVal (-1) (fromList [GEPlus 1, GEPlus 2])]
    it "the geometric product of any two overlapping lines is only a Scalar" $
      property prop_TwoOverlappingLinesScalar
    it "the geometric product of any two overlapping lines does not have produce a vector component" $
      property prop_TwoOverlappingLinesVector
    it "A line constructed from a line segment is correct" $
      eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (1,1))) --> pl1
    it "A line constructed from by joining two points is correct" $
      join2PPoint2 (eToPP (Point2 (0,0))) (eToPP (Point2 (1,1))) --> pl1
  where
    pl1 = PLine2 $ GVec [GVal 1 (singleton (GEPlus 1)), GVal (-1) (singleton (GEPlus 2))]

-- | A property test making sure a PPoint projected from an axis-aligned line is along the opposite axis.
-- FIXME: use distance here.
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
    (angle2, (_,_, angle2Err)) = angleBetween2PL normedPLine3 normedPLine4
    (PPoint2 rawBisectorStart, _) = interpolate2PP sourceStart sourceEnd 0.5 0.5
    (bisectorEndRaw, _) = pPointOnPerpWithErr pline4 (PPoint2 rawBisectorStart) d
    (bisectorEnd, _) = canonicalizeP bisectorEndRaw
    (pline3, pline3Err) = join2PP (CPPoint2 rawBisectorStart) bisectorEnd
    (normedPLine3, norm3Err) = normalizeL pline3
    sourceStart = makeCPPoint2 x y
    sourceEnd = makeCPPoint2 x2 y2
    (pline4, pline4Err) = join2PP sourceStart sourceEnd
    (normedPLine4, norm4Err) = normalizeL pline4
    errTotal3 = ulpVal $ angle2Err -- <> pline3Err <> pline4Err -- <> bisectorStartErr <> bisectorEndRawErr <> bisectorEndErr
    errTotal4 = ulpVal $ angle2Err -- <> pline3Err <> pline4Err -- <> bisectorStartErr <> bisectorEndRawErr <> bisectorEndErr
    x2 :: ℝ
    x2 = coerce rawX2
    d :: ℝ
    d = coerce rawD

-- | A property test making sure the distance between a point an an axis is equal to the corresponding euclidian component of the point.
prop_DistanceToAxis :: NonZero ℝ -> NonZero ℝ -> Bool -> Expectation
prop_DistanceToAxis v v2 xAxis
  | xAxis = distancePPointToPLine (eToPP $ Point2 (coerce v2,coerce v)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (1,0))) --> abs (coerce v)
  | otherwise = distancePPointToPLine (eToPP $ Point2 (coerce v,coerce v2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (0,1))) --> abs (coerce v)

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
  | res <= ulpVal resErr = res <= ulpVal resErr
  | otherwise = error
                $ "failed:\n"
                <> "origPLine: " <> show origPLine <> "\n"
                <> "resPLine: " <> show resPLine <> "\n"
                <> "res: " <> show res <> "\n"
                <> "resErr: " <> show resErr <> "\n"
  where
    (res, (_,_, resErr)) = distance2PL resPLine origPLine
    (resPLine, _) = translateL translatedPLine (-t)
    (translatedPLine, _) = translateL origPLine t
    (origPLine, _) = randomPLineWithErr x y dx dy
    t :: ℝ
    t = coerce rawT

pgaSpec :: Spec
pgaSpec = do
  describe "Translation (math/PGA)" $ do
    it "a translated line translated back is the same line" $
     property prop_PerpTranslateID
  describe "Projection (math/PGA)" $ do
    it "a projection on the perpendicular bisector of an axis aligned line is on the other axis" $
      property prop_AxisProjection
  describe "Distance measurement (math/PGA)" $ do
    it "the distance between a projective point at (x,y) and an axis is equal to x for the x axis, and y for the y axis" $
      property prop_DistanceToAxis
  describe "Layout Inspection (math/PGA)" $ do
    it "two projective points on the same side of a line show as being on the same side of the line" $
      property prop_SameSideOfAxis
    it "two projective points on different sides of a line show as being on different sides of a line" $
      property prop_OtherSideOfAxis

-- | Ensure that the bisector of a quad crosses the point across the quad from the bisector.
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
                <> show bisector1Err <> "\n"
                <> show bisector2 <> "\n"
                <> show eNode <> "\n"
                <> "(" <> show x3 <> "," <> show y3 <> ")\n"
  where
    intersect1 = intersectsWithErr (Right (bisector1, mempty)) (Left lineSeg1 :: Either LineSeg (PLine2, PLine2Err))
    intersect2 = intersectsWithErr (Right (bisector1, mempty)) (Left lineSeg2 :: Either LineSeg (PLine2, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (bisector, bisectorRawErr) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (x3,y3))
    (bisector1, bisector1NormErr) = normalizeL bisector
    bisector1Err = bisectorRawErr <> bisector1NormErr
    bisector2 = getFirstArc (Point2 (x1,y1)) (Point2 (0,0)) (Point2 (x2,y2))
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
                <> "(" <> show x3 <> "," <> show y3 <> ")\n"
                <> "(" <> show x4 <> "," <> show y4 <> ")\n"
  where
    intersect1 = intersectsWithErr (Right (bisector1, mempty)) (Left lineSeg1 :: Either LineSeg (PLine2, PLine2Err))
    intersect2 = intersectsWithErr (Right (bisector1, mempty)) (Left lineSeg2 :: Either LineSeg (PLine2, PLine2Err))
    intersect3 = outputIntersectsLineSeg eNode lineSeg1
    intersect4 = outputIntersectsLineSeg eNode lineSeg2
    -- note that our bisector always intersects the origin.
    (bisector1, _) = normalizeL bisector
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
    res1 = intersectsWithErr (Right (pLineThroughOriginNotX1Y1NotOther,mempty)) (Left x1y1LineSegToOrigin :: Either LineSeg (PLine2, PLine2Err))
    res2 = intersectsWithErr (Right (pLineThroughOriginNotX1Y1NotOther,mempty)) (Left lineSegFromOrigin :: Either LineSeg (PLine2, PLine2Err))
    distanceStart = case res2 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (makeCPPoint2 0 0, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint,mempty) (makeCPPoint2 0 0, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    (pLineThroughOriginNotX1Y1NotOther,_) = randomPLineThroughOrigin x2 y2
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
    res1 = intersectsWithErr (Right (pLineThroughPointNotX1Y1NotOther,mempty)) (Left x1y1LineSegToPoint :: Either LineSeg (PLine2, PLine2Err))
    res2 = intersectsWithErr (Right (pLineThroughPointNotX1Y1NotOther,mempty)) (Left lineSegFromPointNotX1Y1 :: Either LineSeg (PLine2, PLine2Err))
    distanceStart = case res2 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint, mempty) (makeCPPoint2 d2 d2, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint, mempty) (makeCPPoint2 d2 d2, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    distanceEnd = case res1 of
                      (Left (NoIntersection iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint, mempty) (makeCPPoint2 d2 d2, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      (Right (IntersectsIn iPoint ulpSum)) -> show iPoint <> "\nDistance: " <> show (distance2PP (iPoint, mempty) (makeCPPoint2 d2 d2, mempty)) <> "\nUlpSum:" <> show ulpSum <> "\n"
                      _ -> ""
    (pLineThroughPointNotX1Y1NotOther,_) = randomPLineThroughPoint x2 y2 d2
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

-- | A checker, to ensure an angle is what is expected.
myAngleBetween :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Bool
myAngleBetween a b
  | realToFrac res + (ulpRaw resErr) >= 1.0-(ulpRaw resErr) = True
  | otherwise = error
                $ "angle wrong?\n"
                <> show a <> "\n"
                <> show b <> "\n"
                <> show res <> "\n"
                <> show resErr <> "\n"
  where
    (res, (_,_, resErr)) = angleBetween2PL a b

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
prop_AxisAlignedRightAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = myNorm (getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | xPos             = myNorm (getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | not xPos && yPos = myNorm (getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | otherwise        = myNorm (getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2
    myNorm (PLine2 gvals) = NPLine2 gvals

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = getFirstArc (Point2 (offset,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = getFirstArc (Point2 (offset,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,mag2-offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = getFirstArc (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset-mag2))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = getFirstArc (Point2 (-offset,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,mag2-offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAngles :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAngles xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = getFirstArc (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = getFirstArc (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal  (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = getFirstArc (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = getFirstArc (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
-- NOTE: we use only one magnitude, because getOutsideArc requires normalized inputs.
prop_AxisAlignedRightAnglesOutside :: Bool -> Bool -> ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesOutside xPos yPos offset rawMagnitude
  | xPos && yPos = fst ( 
    getOutsideArc (eToPP $ Point2 (offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset,offset+mag)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | xPos = fst (
    getOutsideArc (eToPP $ Point2 (offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset,-(offset+mag))) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | not xPos && yPos = fst (
    getOutsideArc (eToPP $ Point2 (-offset,offset+mag), mempty) (eToPL $ LineSeg (Point2 (-offset,offset+mag)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | otherwise = fst (
    getOutsideArc (eToPP $ Point2 (-offset,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-offset,-(offset+mag))) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  where
    mag :: ℝ
    mag = coerce rawMagnitude

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
-- NOTE: we use only one magnitude, because getOutsideArc requires normalized inputs.
-- NOTE: execrises the point-out-point-out path of getOutsideArc.
-- FIXME: expressing this with the second line in each of these pairs the other direction (head->tail vs tail->head) results in falsification?
prop_AxisAligned135DegreeAnglesOutside :: Bool -> Bool -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAnglesOutside xPos yPos rawOffset rawMagnitude
  | xPos && yPos = fst (
    getOutsideArc (eToPP $ Point2 (offset+mag,offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset)) (Point2 (offset,offset)))
                  (eToPP $ Point2 (offset+mag,offset+mag), mempty) (eToPL $ LineSeg (Point2 (offset+mag,offset+mag)) (Point2 (offset,offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | xPos = fst (
    getOutsideArc (eToPP $ Point2 (offset+mag,-offset), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-offset)) (Point2 (offset,-offset)))
                  (eToPP $ Point2 (offset+mag,-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (offset+mag,-(offset+mag))) (Point2 (offset,-offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | not xPos && yPos = fst (
    getOutsideArc (eToPP $ Point2 (-(offset+mag),offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset)) (Point2 (-offset,offset)))
                  (eToPP $ Point2 (-(offset+mag),offset+mag), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),offset+mag)) (Point2 (-offset,offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | otherwise = fst (
    getOutsideArc (eToPP $ Point2 (-(offset+mag),-offset), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-offset)) (Point2 (-offset,-offset)))
                  (eToPP $ Point2 (-(offset+mag),-(offset+mag)), mempty) (eToPL $ LineSeg (Point2 (-(offset+mag),-(offset+mag))) (Point2 (-offset,-offset))))
                  `myAngleBetween`
                   NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  where
    mag,offset :: ℝ
    offset = coerce rawOffset
    mag = coerce rawMagnitude

-- | ensure that a right angle with one side parallel with an axis and the other side parallel to the other axis results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
prop_AxisAlignedRightAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAlignedRightAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | xPos             = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  | otherwise        = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-offset))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 135 degree angle with one side parallel with an axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween and >= to filter out minor numerical imprecision.
prop_AxisAligned135DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned135DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,offset+mag1)) (Point2 (offset,offset)),LineSeg (Point2 (offset,offset)) (Point2 (offset+mag2,offset-mag2))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = outOf (onlyOne $ makeENodes [LineSeg (Point2 (offset,-(offset+mag1))) (Point2 (offset,-offset)),LineSeg (Point2 (offset,-offset)) (Point2 (offset+mag2,-(offset-mag2)))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,offset+mag1)) (Point2 (-offset,offset)),LineSeg (Point2 (-offset,offset)) (Point2 (-(offset+mag2),offset-mag2))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = outOf (onlyOne $ makeENodes [LineSeg (Point2 (-offset,-(offset+mag1))) (Point2 (-offset,-offset)),LineSeg (Point2 (-offset,-offset)) (Point2 (-(offset+mag2),-(offset-mag2)))])
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

-- | ensure that a 45 degree angle with one side parallel with the X axis and in the right place results in a line through the origin point.
-- NOTE: hack, using angleBetween to filter out minor numerical imprecision.
prop_AxisAligned45DegreeAnglesInENode :: Bool -> Bool -> ℝ -> Positive ℝ -> Positive ℝ -> Bool
prop_AxisAligned45DegreeAnglesInENode xPos yPos offset rawMagnitude1 rawMagnitude2
  | xPos && yPos     = outOf (makeENode (Point2 (offset+mag1,offset+mag1)) (Point2 (offset,offset)) (Point2 (offset+mag2,offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | xPos             = outOf (makeENode (Point2 (offset+mag1,-offset-mag1)) (Point2 (offset,-offset)) (Point2 (offset+mag2,-offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
  | not xPos && yPos = outOf (makeENode (Point2 (-offset-mag1,offset+mag1)) (Point2 (-offset,offset)) (Point2 (-offset-mag2,offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  | otherwise        = outOf (makeENode (Point2 (-offset-mag1,-offset-mag1)) (Point2 (-offset,-offset)) (Point2 (-offset-mag2,-offset)))
                       `myAngleBetween`
                       NPLine2 (GVec [GVal (-0.3826834323650899) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
  where
    mag1,mag2 :: ℝ
    mag1 = coerce rawMagnitude1
    mag2 = coerce rawMagnitude2

prop_TriangleNoDivides :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleNoDivides centerX centerY rawRadians rawDists = findDivisions triangle (fromMaybe dumpError $ crashMotorcycles triangle []) --> []
  where
    dumpError = error errorString
    dumpError2 = error errorString
    errorString =  dumpGanjas [toGanja triangle, toGanja (Point2 (centerX, centerY)), toGanja (PLine2 pLineToInside), toGanja (PLine2 pLineToOutside)] <> "\n"
                <> show firstSeg <> "\n"
                <> show firstPoints <> "\n"
                <> show (insideIsLeft triangle) <> "\n"
                <> show (pLineIsLeft pLine (PLine2 pLineToInside, mempty)) <> "\n"
    maybeInnerPoint = innerContourPoint triangle
    triangle        = randomTriangle centerX centerY rawRadians rawDists
    firstSeg        = firstLineSegOfContour triangle
    pLine           = eToPL firstSeg
    firstPoints     = firstPointPairOfContour triangle
    (p1, p2)        = firstPointPairOfContour triangle
    (myMidPoint,_)  = interpolate2PP (eToPP p1) (eToPP p2) 0.5 0.5
    pLineToInside = vecOfL $ join2PPoint2 myMidPoint innerPoint
    pLineToOutside = vecOfL $ join2PPoint2 innerPoint $ eToPP outsidePoint
    innerPoint      = fromMaybe (dumpError2) maybeInnerPoint
    minPoint        = fst $ minMaxPoints triangle
    outsidePoint    = Point2 (xOf minPoint - 0.00000001 , yOf minPoint - 0.00000001)

prop_TriangleHasStraightSkeleton :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleHasStraightSkeleton centerX centerY rawRadians rawDists = findStraightSkeleton triangle [] -/> Nothing
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleStraightSkeletonHasRightGenerationCount centerX centerY rawRadians rawDists = generationsOf (findStraightSkeleton triangle []) --> 1
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleCanPlaceFaces :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleCanPlaceFaces centerX centerY rawRadians rawDists = facesOf (fromMaybe (error "Got Nothing") $ findStraightSkeleton triangle []) -/> slist []
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleHasRightFaceCount :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleHasRightFaceCount centerX centerY rawRadians rawDists = length (facesOf $ fromMaybe (error $ show triangle) $ findStraightSkeleton triangle []) --> 3
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists

prop_TriangleFacesInOrder :: ℝ -> ℝ -> ListThree (Radian ℝ) -> ListThree (Positive ℝ) -> Expectation
prop_TriangleFacesInOrder centerX centerY rawRadians rawDists = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show triangle) $ findStraightSkeleton triangle []) --> lineSegsOfContour triangle
  where
    triangle = randomTriangle centerX centerY rawRadians rawDists
    firstSeg = onlyOneOf $ lineSegsOfContour triangle

prop_SquareNoDivides :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareNoDivides x y tilt distanceToCorner = findDivisions square (fromMaybe (error $ show square) $ crashMotorcycles square []) --> []
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareHasStraightSkeleton x y tilt distanceToCorner = findStraightSkeleton square [] -/> Nothing
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareStraightSkeletonHasRightGenerationCount x y tilt distanceToCorner = generationsOf (findStraightSkeleton square []) --> 1
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareCanPlaceFaces x y tilt distanceToCorner = facesOf (fromMaybe (error $ show square) $ findStraightSkeleton square []) -/> slist []
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareHasRightFaceCount x y tilt distanceToCorner = length (facesOf $ fromMaybe (error $ show square) $ findStraightSkeleton square []) --> 4
  where
    square = randomSquare x y tilt distanceToCorner

prop_SquareFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_SquareFacesInOrder x y tilt distanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show square) $ findStraightSkeleton square []) --> squareAsSegs
  where
    square = randomSquare x y tilt distanceToCorner
    squareAsSegs = lineSegsOfContour square
    firstSeg = onlyOneOf squareAsSegs

prop_RectangleNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleNoDivides x y rawFirstTilt rawSecondTilt rawDistanceToCorner = findDivisions rectangle (fromMaybe (error $ show rectangle) $ crashMotorcycles rectangle []) --> []
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawDistanceToCorner = findStraightSkeleton rectangle [] -/> Nothing
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt rawDistanceToCorner = generationsOf (findStraightSkeleton rectangle []) --> 1
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleCanPlaceFaces x y rawFirstTilt rawSecondTilt rawDistanceToCorner = facesOf (fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) -/> slist []
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleHasRightFaceCount x y rawFirstTilt rawSecondTilt rawDistanceToCorner = length (facesOf $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) --> 4
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner

prop_RectangleFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_RectangleFacesInOrder x y rawFirstTilt rawSecondTilt rawDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show rectangle) $ findStraightSkeleton rectangle []) --> rectangleAsSegs
  where
    rectangle = randomRectangle x y rawFirstTilt rawSecondTilt rawDistanceToCorner
    rectangleAsSegs = lineSegsOfContour rectangle
    firstSeg = onlyOneOf rectangleAsSegs

prop_ConvexDualRightQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadNoDivides x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = findDivisions convexDualRightQuad (fromMaybe (error $ show convexDualRightQuad) $ crashMotorcycles convexDualRightQuad []) --> []
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_ConvexDualRightQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = findStraightSkeleton convexDualRightQuad [] -/> Nothing
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_ConvexDualRightQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = generationsOf (findStraightSkeleton convexDualRightQuad []) --> 1
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_ConvexDualRightQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = facesOf (fromMaybe (error $ show convexDualRightQuad) $ findStraightSkeleton convexDualRightQuad []) -/> slist []
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_ConvexDualRightQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexDualRightQuad) $ findStraightSkeleton convexDualRightQuad []) --> 4
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner

prop_ConvexDualRightQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
prop_ConvexDualRightQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexDualRightQuad) $ findStraightSkeleton convexDualRightQuad []) --> convexDualRightQuadAsSegs
  where
    convexDualRightQuad = randomConvexDualRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawDistanceToCorner
    convexDualRightQuadAsSegs = lineSegsOfContour convexDualRightQuad
    firstSeg = onlyOneOf convexDualRightQuadAsSegs

prop_ConvexSingleRightQuadNoDivides :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadNoDivides x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findDivisions convexSingleRightQuad (fromMaybe (error $ show convexSingleRightQuad) $ crashMotorcycles convexSingleRightQuad []) --> []
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexSingleRightQuadHasStraightSkeleton :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadHasStraightSkeleton x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = findStraightSkeleton convexSingleRightQuad [] -/> Nothing
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexSingleRightQuadStraightSkeletonHasRightGenerationCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadStraightSkeletonHasRightGenerationCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = generationsOf (findStraightSkeleton convexSingleRightQuad []) --> 1
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexSingleRightQuadCanPlaceFaces :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadCanPlaceFaces x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = facesOf (fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) -/> slist []
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexSingleRightQuadHasRightFaceCount :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadHasRightFaceCount x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = length (facesOf $ fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) --> 4
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner

prop_ConvexSingleRightQuadFacesInOrder :: ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Positive ℝ -> Expectation
prop_ConvexSingleRightQuadFacesInOrder x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner = edgesOf (orderedFacesOf firstSeg $ fromMaybe (error $ show convexSingleRightQuad) $ findStraightSkeleton convexSingleRightQuad []) --> convexSingleRightQuadAsSegs
  where
    convexSingleRightQuad = randomConvexSingleRightQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner rawSecondDistanceToCorner
    convexSingleRightQuadAsSegs = lineSegsOfContour convexSingleRightQuad
    firstSeg = onlyOneOf convexSingleRightQuadAsSegs

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
    doTest concaveChevronQuad = len ( slist $ findDivisions concaveChevronQuad (fromMaybe (error $ show concaveChevronQuad) $ crashMotorcycles concaveChevronQuad [])) --> 1

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

-- | Test of dimensional accuracy.
-- make sure that the measured distance between two points that have been placed as close as possible is less than the amount of error placing both points added to the amount of error of doing a measurement of distance.
prop_PPointWithinErrRange :: ℝ -> ℝ -> Bool
prop_PPointWithinErrRange x y
  | res > realToFrac (resErr)  = error $ "res too big: " <> show res
  | otherwise = (p1 /= p2) || (res == 0 || error "the same, but distance?")
  where
    (res, (_,_,UlpSum resErr)) = distance2PP (p1,mempty) (p2,mempty)
    p1 = makeCPPoint2 x y
    p2 = makeCPPoint2 x y

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

-- | Check the distance between two points, and a line constructed between them. should be 0 in an ideal world, and should be less than ulpTotals in the floating point world.
-- FIXME: someone is lieing.
prop_PLineWithinErrRange1 :: ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_PLineWithinErrRange1 x1 y1 rawX2 rawY2
  | distance1 > realToFrac ulpTotal1 = error $ "startPoint outside of expected range:\n" <> dumpRes
  | distance2 > realToFrac ulpTotal2 = error $ "endPoint outside of expected range:\n" <> dumpRes
  | otherwise = True
  where
    dumpRes =    "distance1: " <> show distance1 <> "\n"
              <> "distance2: " <> show distance2 <> "\n"
              <> "ulpTotal1: " <> show ulpTotal1 <> "\n"
              <> "ulpTotal2: " <> show ulpTotal2 <> "\n"
              <> "distance1Err: " <> show distance1Err <> "\n"
              <> "distance2Err: " <> show distance2Err <> "\n"
              <> "PLine: " <> show pLine <> "\n"
              <> "PLine1Err: " <> show pLineErr <> "\n"
              <> "NPLine: " <> show nPLine <> "\n"
              <> "PPoint1: " <> show pPoint1 <> "\n"
              <> "PPoint2: " <> show pPoint2 <> "\n"
    -- distance1 and distance2 should be 0, in an ideal world.
    (distance1, (_,_,_,_,_,UlpSum distance1Err)) = distancePPToPL (pPoint1, mempty) (nPLine, nPLineErr)
    (distance2, (_,_,_,_,_,UlpSum distance2Err)) = distancePPToPL (pPoint2, mempty) (nPLine, nPLineErr)
    pPoint1 = makeCPPoint2 x1 y1
    pPoint2 = makeCPPoint2 x2 y2
    (nPLine, nPLineErr) = normalizeL pLine
    (pLine, pLineErr) = eToPL $ makeLineSeg (Point2 (x1,y1)) (Point2 (x2,y2))
    ulpTotal1 = distance1Err
    ulpTotal2 = distance2Err
    -- make sure we do not try to create a 0 length line segment.
    (x2,y2)
     | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                    then (1,1)
                                    else (0,0)
     | otherwise = (rawX2, rawY2)

prop_PLineWithinErrRange2 :: ℝ -> ℝ -> ℝ -> ℝ -> Bool
prop_PLineWithinErrRange2 x1 y1 rawX2 rawY2
  | distance1 > ulpVal ulpTotal1 = error $ "startPoint outside of expected range:\n" <> dumpRes
  | distance2 > ulpVal ulpTotal2 = error $ "endPoint outside of expected range:\n" <> dumpRes
  | otherwise = True
  where
    dumpRes =    "distance1: " <> show distance1 <> "\n"
              <> "distance2: " <> show distance2 <> "\n"
              <> "ulpTotal1: " <> show ulpTotal1 <> "\n"
              <> "ulpTotal2: " <> show ulpTotal2 <> "\n"
              <> "distance1Err: " <> show distance1Err <> "\n"
              <> "distance2Err: " <> show distance2Err <> "\n"
              <> "PLine1: " <> show pLine1 <> "\n"
              <> "PLine1Err: " <> show pLine1Err <> "\n"
              <> "PPoint1: " <> show pPoint1 <> "\n"
              <> "PPoint2: " <> show pPoint2 <> "\n"
              <> "xIntercept(PLine1): " <> show (xIntercept (pLine1,pLine1Err)) <> "\n"
              <> "yIntercept(PLine1): " <> show (yIntercept (pLine1,pLine1Err)) <> "\n"
    -- distance1 and distance2 should be 0, in an ideal world.
    (distance1, (_,_,_,_,_,distance1Err)) = distancePPToPL (pPoint1, mempty) (pLine1, pLine1Err)
    (distance2, (_,_,_,_,_,distance2Err)) = distancePPToPL (pPoint2, mempty) (pLine1, pLine1Err)
    pPoint1 = makeCPPoint2 x1 y1
    pPoint2 = makeCPPoint2 x2 y2
    (pLine1, (_,_,pLine1Err)) = join2PP pPoint1 pPoint2
    ulpTotal1 = distance1Err <> pLineErrAtPPoint (pLine1, pLine1Err) pPoint1
    ulpTotal2 = distance2Err <> pLineErrAtPPoint (pLine1, pLine1Err) pPoint2
    -- make sure we do not try to create a 0 length line segment.
    (x2,y2)
     | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                    then (1,1)
                                    else (0,0)
     | otherwise = (rawX2, rawY2)

prop_PPointOnPerpWithinErrRange :: ℝ -> ℝ -> ℝ -> ℝ -> Positive ℝ -> Bool
prop_PPointOnPerpWithinErrRange x1 y1 rawX2 rawY2 rawD
  | res1 > d + realToFrac ulpTotal1 = error $ "too big startPoint\n" <> dumpRes
  | res2 > d + realToFrac ulpTotal2 = error $ "too big endPoint\n" <> dumpRes
  | otherwise = True
  where
    dumpRes = "PPoint1: " <> show pPoint1 <> "\n"
              <> "PPoint2: " <> show pPoint2 <> "\n"
              <> "distance1: " <> show res1 <> "\n"
              <> "distance2: " <> show res2 <> "\n"
              <> "PLine: " <> show pLine <> "\n"
              <> "ulpTotal1: " <> show ulpTotal1 <> "\n"
              <> "ulpTotal2: " <> show ulpTotal2 <> "\n"
    -- res should be d, in an ideal world.
    (res1,(_,_,_,_,_, UlpSum res1Err)) = distancePPToPL (perp1, mempty) (pLine, mempty)
    (res2,(_,_,_,_,_, UlpSum res2Err)) = distancePPToPL (perp2, mempty) (pLine, mempty)
    (perp1, (_,_,_, UlpSum ulpSumPerp1)) = pPointOnPerpWithErr pLine pPoint1 d
    (perp2, (_,_,_, UlpSum ulpSumPerp2)) = pPointOnPerpWithErr pLine pPoint2 d
    pPoint1 = makeCPPoint2 x1 y1
    pPoint2 = makeCPPoint2 x2 y2
    (pLine, _) = join2PP pPoint1 pPoint2
    ulpTotal1 = res1Err + ulpSumPerp1
    ulpTotal2 = res2Err + ulpSumPerp2
    d :: ℝ
    d = coerce rawD
    -- make sure we do not try to create a 0 length line segment.
    (x2,y2)
     | x1 == rawX2 && y1 == rawY2 = if x1 == 0 && y1 == 0
                                    then (1,1)
                                    else (0,0)
     | otherwise = (rawX2, rawY2)

prop_obtuseBisectorOnBiggerSide_makeENode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Expectation
prop_obtuseBisectorOnBiggerSide_makeENode x y d1 rawR1 d2 rawR2 testFirstLine
  | testFirstLine = pLineIsLeft bisector pl1 --> Just True
  | otherwise     = pLineIsLeft (pl2, mempty) bisector --> Just True
  where
    pl1 = eToPL $ getFirstLineSeg eNode
    pl2 = flipL $ eToPLine2 $ getLastLineSeg eNode
    eNode = randomENode x y d1 rawR1 d2 rawR2
    bisector = (flipL $ outOf eNode, errOfOut eNode)

prop_obtuseBisectorOnBiggerSide_makeINode :: ℝ -> ℝ -> Positive ℝ -> Radian ℝ -> Positive ℝ -> Radian ℝ -> Bool -> Bool -> Expectation
prop_obtuseBisectorOnBiggerSide_makeINode x y d1 rawR1 d2 rawR2 flipIn1 flipIn2 = (angleFound > (1-ulpVal angleErr), angleFound < realToFrac (-1 + (ulpRaw angleErr) :: Rounded 'TowardInf ℝ)) --> (True, False)
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
prop_translateRotateMoves x y rawD rawR = distanceBetweenPPoints (fst $ translateRotatePPoint2WithErr pPoint d r) cPPoint /= 0 --> True
  where
    pPoint = eToPP $ Point2 (x,y)
    cPPoint = makeCPPoint2 x y
    r,d::ℝ
    r = coerce rawR
    d = coerce rawD

-- |ensure that a random PLine, when normed, is approximately equal to what went in.
prop_NormPLineIsPLine :: ℝ -> ℝ -> NonZero ℝ -> NonZero ℝ -> Bool
prop_NormPLineIsPLine x y dx dy = randomPLine x y dx dy
                                  `myAngleBetween`
                                  fst ( normalizeL $ randomPLine x y dx dy)

prop_PLinesIntersectAtOrigin :: NonZero ℝ -> ℝ -> NonZero ℝ -> ℝ -> Bool
prop_PLinesIntersectAtOrigin rawX y rawX2 rawY2
  | foundDistance <= ulpVal distanceErr = True
  | otherwise = error $ "wtf"
                <> show intersectionErr <> "\n"
  where
    originPPoint2 = makeCPPoint2 0 0
    (foundDistance, (_,_,distanceErr)) = distance2PP (originPPoint2, mempty) (intersectionPPoint2, intersectionErr)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 randomPLine2
    (randomPLine1, _) = randomPLineThroughOrigin x y
    (randomPLine2, _) = randomPLineThroughOrigin x2 y2
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
  | foundDistance < realToFrac errSum = True
  | otherwise = error
                $ "wtf\n"
                <> show foundDistance <> "\n"
                <> show errSum <> "\n"
                <> show targetPPoint2 <> "\n"
                <> show foundDistance <> "\n"
                <> show distanceErr <> "\n"
                <> show intersectionErr <> "\n"
  where
    (targetPPoint2) = makeCPPoint2 (coerce targetX) (coerce targetY)
    (foundDistance, (_,_,UlpSum distanceErr)) = distance2PP (targetPPoint2, mempty) (intersectionPPoint2, intersectionErr)
    (intersectionPPoint2, (_,_, intersectionErr)) = intersect2PL randomPLine1 randomPLine2
    (randomPLine1, _) = randomPLineWithErr x y targetX targetY
    (randomPLine2, _) = randomPLineWithErr x2 y2 targetX targetY
    errSum = distanceErr -- + canonicalizationErr
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
    (foundDistance, (_,_,distanceErr)) = distance2PP (axisIntersectionPoint, mempty) (intersectionPPoint2, intersectionErr)
    axisIntersectionErr = snd $ fromJust axisIntersection
    axisIntersectionPoint = eToPP $ Point2 ((fromRight (error "not right?") $ fst $ fromJust axisIntersection), 0)
    axisIntersection = xIntercept (randomPLine1, pline1Err)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 axisPLine
    (randomPLine1, pline1Err) = randomPLineWithErr x y rawX2 (coerce y2)
    -- Error in this line should be in directions that should not matter.
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
    (foundDistance, (_,_,distanceErr)) = distance2PP (axisIntersectionPoint,mempty) (intersectionPPoint2, intersectionErr)
    axisIntersectionErr = snd $ fromJust axisIntersection
    axisIntersectionPoint = eToPP $ Point2 (0,(fromRight (error "not right?") $ fst $ fromJust axisIntersection))
    axisIntersection = yIntercept (randomPLine1, pline1Err)
    (intersectionPPoint2, (_,_,intersectionErr)) = intersect2PL randomPLine1 axisPLine
    (randomPLine1, pline1Err) = randomPLineWithErr (coerce x) y (coerce x2) (coerce y2)
    -- Error in this line should be in directions that should not matter.
    (axisPLine, _) = eToPL $ makeLineSeg (Point2 (0,0)) (Point2 (0,coerce m))
    y2 :: ℝ
    y2 = coerce rawY2

facetFlakeySpec :: Spec
facetFlakeySpec = do
  describe "Stability (Points)" $ do
    it "both of the points used to construct a PLine2 are within UlpSum of the PLine2" $
      property prop_PLineWithinErrRange1
    it "a line constructed with the midpoint of a segment and a point on the perpendicular bisector is at 90 degrees to the initial segment" $
      property prop_perpAt90Degrees
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

facetSpec :: Spec
facetSpec = do
  describe "Stability (Points)" $ do
    it "placing a Point within a the error range of another point results in a point a distance away thats less than the sum of the two errors" $
      property prop_PPointWithinErrRange
    it "both of the points used to join a PLine2 are within UlpSum of the PLine2" $
      property prop_PLineWithinErrRange2
    it "a point on the perpendicular bisector is within distance+UlpSum of a PLine2" $
      property prop_PPointOnPerpWithinErrRange
  describe "Stability (Lines)" $ do
    it "both ends of a line segment are within UlpSum of the points they were constructed with" $
      property prop_LineSegWithinErrRange
    it "a normalized line normalized again is approximately itsself" $
      property prop_NormPLineIsPLine
  describe "Stability (Error)" $ do
    it "finds that the X intersection of a random PLine is within the returned UlpSum" $
      property prop_PLineIntersectsAtXAxis
    it "finds that the Y intersection of a random PLine is within the returned UlpSum" $
      property prop_PLineIntersectsAtYAxis
  describe "Stability (Intersections)" $ do
    it "finds that the intersection of two PLines at the origin are within the returned UlpSum" $
      property prop_PLinesIntersectAtOrigin
    it "finds endpoints and startpoints in equal quantities at the origin" $
      property prop_LineSegIntersectionStableAtOrigin
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
    it "finds no divides in a triangle" $
      property prop_TriangleNoDivides
    it "finds the straight skeleton of a triangle (property)" $
      property prop_TriangleHasStraightSkeleton
    it "only generates one generation for a triangle" $
      property prop_TriangleStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a triangle" $
      property prop_TriangleCanPlaceFaces
    it "places faces on a triangle in the order the line segments were given" $
      property prop_TriangleFacesInOrder
    it "only finds three face triangles" $
      property prop_TriangleHasRightFaceCount
    it "finds no divides in a square" $
      property prop_SquareNoDivides
    it "finds the straight skeleton of a square (property)" $
      property prop_SquareHasStraightSkeleton
    it "only generates one generation for a square" $
      property prop_SquareStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a square" $
      property prop_SquareCanPlaceFaces
    it "only finds four face squares" $
      property prop_SquareHasRightFaceCount
    it "places faces on a square in the order the line segments were given" $
      property prop_SquareFacesInOrder
    it "finds no divides in a rectangle" $
      property prop_RectangleNoDivides
    it "finds the straight skeleton of a rectangle (property)" $
      property prop_RectangleHasStraightSkeleton
    it "only generates one generation for a rectangle" $
      property prop_RectangleStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a rectangle" $
      property prop_RectangleCanPlaceFaces
    it "finds only four faces for any rectangle" $
      property prop_RectangleHasRightFaceCount
    it "places faces on a rectangle in the order the line segments were given" $
      property prop_RectangleFacesInOrder
    it "finds no divides in a convex dual right quad" $
      property prop_ConvexDualRightQuadNoDivides
    it "finds the straight skeleton of a convex dual right quad (property)" $
      property prop_ConvexDualRightQuadHasStraightSkeleton
    it "only generates one generation for a convex dual right quad" $
      property prop_ConvexDualRightQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a convex dual right quad" $
      property prop_ConvexDualRightQuadCanPlaceFaces
    it "finds only four faces for any convex dual right quad" $
      property prop_ConvexDualRightQuadHasRightFaceCount
    it "places faces on a convex dual right quad in the order the line segments were given" $
      property prop_ConvexDualRightQuadFacesInOrder
    it "finds no divides in a convex single right quad" $
      property prop_ConvexSingleRightQuadNoDivides
    it "finds the straight skeleton of a convex single right quad (property)" $
      property prop_ConvexSingleRightQuadHasStraightSkeleton
    it "only generates one generation for a convex single right quad" $
      property prop_ConvexSingleRightQuadStraightSkeletonHasRightGenerationCount
    it "places faces on the straight skeleton of a convex single right quad" $
      property prop_ConvexSingleRightQuadCanPlaceFaces
    it "finds only four faces for any convex single right quad" $
      property prop_ConvexSingleRightQuadHasRightFaceCount
    it "places faces on a convex single right quad in the order the line segments were given" $
      property prop_ConvexSingleRightQuadFacesInOrder
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
    it "finds the outsideArc of two intersecting lines (makeINode)" $
      property prop_obtuseBisectorOnBiggerSide_makeINode
    it "sees that the first input line into an ENode is toward the point" $
      property prop_eNodeTowardIntersection1
    it "sees that the second input line into an ENode is away from the point" $
      property prop_eNodeAwayFromIntersection2
    it "successfully translates and rotates PPoint2s" $
      property prop_translateRotateMoves
{-
    it "finds the arc resulting from a node at the intersection of the outArc of two nodes (corner3 and corner4 of c2)" $
      averageNodes c2c3E1 c2c4E1 --> INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                             PLine2Err mempty [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))] (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001252e-16) mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[]))
                                           (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]),
                                             PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))]
                                                       [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEZero 1)), ErrVal (UlpSum 5.551115123125783e-17) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                       (UlpSum 2.220446049250313e-16) (UlpSum 9.43689570931383e-16) mempty mempty)
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal 0.48706362218573185 (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal 0.19509032201612822 (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 2.7755575615628914e-17) (singleton (GEPlus 2))]
                                                            [ErrVal (UlpSum 5.551115123125783e-17) (singleton (GEZero 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 2.7755575615628914e-17) (singleton (GEPlus 2))]
                                                            (UlpSum 2.220446049250313e-16) (UlpSum 4.510281037539698e-16) mempty mempty)
                                           )
    it "finds the outside arc of two PLines intersecting at 90 degrees (c2)" $
      averageNodes c2c2E1 c2c3E1 --> INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                            PLine2Err []
                                                      [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)),ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[])) 
                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                            PLine2Err []
                                                      [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[]))
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))]
                                                            [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))]
                                                            (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty mempty))
    it "finds the outside arc of two PLines intersecting at 90 degrees (c2)" $
      averageNodes c2c3E1 c2c2E1 --> INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                            PLine2Err []
                                                      [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)),ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[])) 
                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                            PLine2Err []
                                                      [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty ([ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))],[]))
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))]
                                                            [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 2))]
                                                            (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty mempty))
    it "finds the outside arc of two PLines intersecting at 90 degrees (c7)" $
      averageNodes c7c1E1 c7c2E1 --> INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]),
                                            PLine2Err []
                                                      [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty mempty)
                                           (PLine2 (GVec [GVal 1.0606601717798212 (singleton (GEZero 1)), GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]),
                                            PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEZero 1))]
                                                      [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEZero 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 1)), ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEPlus 2))]
                                                      (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty mempty) 
                                           (slist [])
                                           (Just (PLine2 (GVec [GVal 0.75 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]),
                                                  PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))]
                                                            [ErrVal (UlpSum 1.1102230246251565e-16) (singleton (GEZero 1)), ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))]
                                                            (UlpSum 2.220446049250313e-16) (UlpSum 8.881784197001254e-16) mempty mempty))
-}
  describe "Motorcycles (Skeleton/Motorcycles)" $ do
    it "finds the motorcycle in our second simple shape" $
      convexMotorcycles c1 --> [Motorcycle (LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,0.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)))
                                           (PLine2 (GVec [GVal 1.414213562373095 (singleton (GEPlus 1))]))
                                           (PLine2Err [ErrVal (UlpSum 2.220446049250313e-16) (singleton (GEPlus 1))] mempty mempty mempty mempty mempty)
                                           ]
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
                                            (PLine2 (GVec [GVal (-0.5411961001461969) (fromList [GEZero 1]), GVal 0.9238795325112867 (fromList [GEPlus 1]), GVal 0.3826834323650899 (fromList [GEPlus 2])]))
                                            (UlpSum 6.8833827526759706e-15) 1.7071067811865475
                                    , ENode (Point2 (1.0,-1.0), Point2 (2.0,0.0),Point2 (1.0,1.0))
                                            (PLine2 (GVec [GVal 1.0 (fromList [GEPlus 2])]))
                                            (UlpSum 5.773159728050814e-15) 1.4142135623730951
                                    , ENode (Point2 (2.0,0.0), Point2 (1.0,1.0),Point2 (-1.0,1.0))
                                            (PLine2 (GVec [GVal 0.5411961001461969 (fromList [GEZero 1]), GVal (-0.9238795325112867) (fromList [GEPlus 1]), GVal 0.3826834323650899 (fromList [GEPlus 2])]))
                                            (UlpSum 6.8833827526759706e-15) 1.7071067811865475
                                    , ENode (Point2 (1.0,1.0), Point2 (-1.0,1.0), Point2 (0.0,0.0))
                                            (PLine2 (GVec [GVal 0.541196100146197 (fromList [GEZero 1]), GVal (-0.3826834323650897) (fromList [GEPlus 1]), GVal (-0.9238795325112867) (fromList [GEPlus 2])]))
                                            (UlpSum 5.773159728050814e-15) 1.7071067811865475
                                    , ENode (Point2 (0.0,0.0), Point2 (-1.0,-1.0), Point2 (1.0,-1.0))
                                            (PLine2 (GVec [GVal (-0.541196100146197) (fromList [GEZero 1]), GVal 0.3826834323650897 (fromList [GEPlus 1]), GVal (-0.9238795325112867) (fromList [GEPlus 2])]))
                                            (UlpSum 5.773159728050814e-15) 1.7071067811865475]
    it "finds the motorcycle of our sixth simple shape" $
      convexMotorcycles c5 --> [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (singleton (GEPlus 2))])) (UlpSum 2.220446049250313e-16) 2.8284271247461903]
    it "finds the crashtree of our fifth shape." $
      crashMotorcycles c5 [] --> Just (CrashTree (slist [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (fromList [GEPlus 2])])) (UlpSum 2.220446049250313e-16) 2.8284271247461903])
                                                 (slist [Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (fromList [GEPlus 2])])) (UlpSum 2.220446049250313e-16) 2.8284271247461903])
                                                 (slist []))
    it "finds the divide of our sixth shape." $
      findDivisions c5 (fromMaybe (error "Got Nothing") $ crashMotorcycles c5 [])
      --> [CellDivide
            (DividingMotorcycles
              (Motorcycle (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0))) (PLine2 (GVec [GVal (-1.414213562373095) (fromList [GEPlus 2])])) (UlpSum 2.220446049250313e-16) 2.8284271247461903)
              (slist []))
            (WithENode $ ENode (Point2 (1.0,-1.0), Point2 (2.0,0.0), Point2 (1.0,1.0)) (PLine2 (GVec [GVal 1.0 (fromList [GEPlus 2])])) (UlpSum 5.773159728050815e-15) 1.4142135623730951)
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
  describe "insets (Skeleton/Line)" $ do
    it "insets a triangle (unit)" $
      addInset 1 0.25 (facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton triangle [])
      --> ([LineSegContour (Point2 (0.4330127018922193,0.25))
                           (Point2 (1.5669872981077808,1.2320508075688772))
                           (LineSeg (Point2 (0.4330127018922193,0.25)) (Point2 (1.5669872981077808,0.2500000000000001)))
                           (LineSeg (Point2 (1.5669872981077808,0.2500000000000001)) (Point2 (1.0,1.2320508075688771)))
                           (slist [LineSeg (Point2 (1.0,1.2320508075688771)) (Point2 (0.4330127018922193,0.25))])]
          ,[Face (LineSeg (Point2 (2.433012701892219,-0.25)) (Point2 (-0.43301270189221924,-0.25)))
                 (PLine2 (GVec [GVal (-1.7320508075688774) (singleton (GEZero 1)), GVal 0.8660254037844387 (singleton (GEPlus 1)), GVal 1.5 (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 0.8660254037844387 (singleton (GEPlus 1)),GVal (-1.5) (singleton (GEPlus 2))]))
           ,Face (LineSeg (Point2 (1.0,2.232050807568877)) (Point2 (2.4330127018922192,-0.25)))
                 (PLine2 (GVec [GVal 1.7320508075688772 (singleton (GEZero 1)), GVal (-1.7320508075688772) (singleton (GEPlus 1))]))
                 (slist [])
                 (PLine2 (GVec [GVal (-1.7320508075688774) (singleton (GEZero 1)), GVal 0.8660254037844387 (singleton (GEPlus 1)), GVal 1.5 (singleton (GEPlus 2))]))
           ,Face (LineSeg (Point2 (-0.43301270189221935,-0.25000000000000006)) (Point2 (1.0,2.232050807568877)))
                 (PLine2 (GVec [GVal 0.8660254037844387 (singleton (GEPlus 1)), GVal (-1.5) (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 1.7320508075688772 (singleton (GEZero 1)), GVal (-1.7320508075688772) (singleton (GEPlus 1))]))
           ])
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
--      c2c2E1 = makeENode (Point2 (1.0,1.0)) (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0))
      -- the left and the bottom side.
--      c2c3E1 = makeENode (Point2 (-1.0,1.0)) (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0))
      -- the bottom and the entrance to the convex angle.
--      c2c4E1 = makeENode (Point2 (-1.0,-1.0)) (Point2 (1.0,-1.0)) (Point2 (0.0,0.0))
      -- The next corners are part of a 2x2 square around the origin with a slice and a corner missing: (c7 from above)
      --        v----- corner 2
      --  ┌───┐ ┌─┐<-- corner 1
      --  │    \│ │
      --  └───┐   │
      --      │   │
      --      └───┘
--      c7c1E1 = makeENode (Point2 (1.0,-1.0)) (Point2 (1.0,1.0)) (Point2 (0.5,1.0))
--      c7c2E1 = makeENode (Point2 (1.0,1.0)) (Point2 (0.5,1.0)) (Point2 (0.5,0.0))
      -- A simple triangle.
      triangle = makePointContour [Point2 (2,0), Point2 (1.0,sqrt 3), Point2 (0,0)]
