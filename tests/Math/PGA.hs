{- ORMOLU_DISABLE -}
{- HSlice.
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

-- Shamelessly stolen from ImplicitCAD.

module Math.PGA (linearAlgSpec, geomAlgSpec, pgaSpec, proj2DGeomAlgSpec, facetSpec, contourSpec, lineSpec) where

-- Be explicit about what we import.
import Prelude (($), Bool(True, False), (<$>), error, sqrt)

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, pendingWith)

import Data.List (foldl')

import Data.Maybe (fromMaybe, Maybe(Just, Nothing))

import Data.Set (singleton, fromList)

import Slist.Size (Size(Size))

import Slist.Type (Slist(Slist))

import Slist (slist)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- A euclidian point.
import Graphics.Slicer.Math.Definitions(Point2(Point2), Contour(SafeContour), roundPoint2)

-- Our Geometric Algebra library.
import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEZero, GEPlus, G0), GVal(GVal), GVec(GVec), addValPair, subValPair, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, scalarPart, vectorPart, (•), (∧), (⋅), (⎣))

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (PPoint2(PPoint2), PLine2(PLine2), eToPPoint2, eToPLine2, join2PPoint2, translatePerp, pointOnPerp, distancePPointToPLine, pPointsOnSameSideOfPLine)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg))

-- Our Contour library.
import Graphics.Slicer.Math.Contour (contourContainsContour, getContours, pointsOfContour, numPointsOfContour, justOneContourFrom, makeSafeContour)

import Graphics.Slicer.Machine.Contour (shrinkContour, expandContour)

-- Our Infill library.
import Graphics.Slicer.Machine.Infill (InfillType(Horiz, Vert), makeInfill)

-- Our Facet library.
import Graphics.Slicer.Math.Skeleton.Concave (getFirstArc, makeFirstENodes, averageNodes)
import Graphics.Slicer.Math.Skeleton.Definitions (ENode(ENode), Motorcycle(Motorcycle), StraightSkeleton(StraightSkeleton), INode(INode), INodeSet(INodeSet))
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf, orderedFacesOf)
import Graphics.Slicer.Math.Skeleton.Line (addInset)
import Graphics.Slicer.Math.Skeleton.Motorcycles (convexMotorcycles)
import Graphics.Slicer.Math.Skeleton.NodeTrees (makeNodeTree)
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)
import Graphics.Slicer.Math.Skeleton.Tscherne (cellAfter, cellBefore)

-- Our Utility library, for making these tests easier to read.
import Math.Util ((-->))

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

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
    c1 = makeSafeContour cp1
    c2 = makeSafeContour [Point2 (0.75,0.25), Point2 (0.75,0.75), Point2 (0.25,0.75), Point2 (0.25,0.25)]
    c3 = makeSafeContour [Point2 (3,0), Point2 (3,1), Point2 (2,1), Point2 (2,0)]

lineSpec :: Spec
lineSpec = do
  describe "Contours (math/line)" $ do
    it "contours converted from pints to lines then back to points give the input list" $
      pointsOfContour (makeSafeContour cp1) --> cp1
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]

linearAlgSpec :: Spec
linearAlgSpec = do
  describe "Contours (machine/contour)" $ do
    it "a contour mechanically shrunk has the same amount of points as the input contour" $
      numPointsOfContour (fromMaybe (error "got Nothing") $ shrinkContour 0.1 [] c1) --> numPointsOfContour c1
    it "a contour mechanically shrunk by zero is the same as the input contour" $
      shrinkContour 0 [] c1 --> Just c1
    it "a contour mechanically expanded has the same amount of points as the input contour" $
      numPointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] c1) --> numPointsOfContour c1
    it "a contour mechanically shrunk and expanded is about equal to where it started" $
      (roundPoint2 <$> pointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] $ fromMaybe (error "got Nothing") $ shrinkContour 0.1 [] c2)) --> roundPoint2 <$> pointsOfContour c2
  describe "Infill (machine/infill)" $ do
    it "infills exactly one line inside of a box big enough for only one line (Horizontal)" $ do
      pendingWith "https://github.com/julialongtin/hslice/issues/31"
      makeInfill c1 [] 0.5 Horiz --> [[LineSeg (Point2 (0,0.5)) (Point2 (1,0))]]
    it "infills exactly one line inside of a box big enough for only one line (Vertical)" $ do
      pendingWith "https://github.com/julialongtin/hslice/issues/31"
      makeInfill c1 [] 0.5 Vert --> [[LineSeg (Point2 (0.5,0)) (Point2 (0,1))]]
  describe "Contours (Skeleton/line)" $ do
    it "a contour algorithmically shrunk has the same amount of points as the input contour" $
      numPointsOfContour (justOneContourFrom $ addInset 1 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c1 []) --> numPointsOfContour c1
    it "a contour algorithmically shrunk and mechanically expanded is about equal to where it started" $
      roundPoint2 <$> pointsOfContour (fromMaybe (error "got Nothing") $ expandContour 0.1 [] $ justOneContourFrom $ addInset 1 0.1 $ orderedFacesOf c2l1 $ fromMaybe (error "got Nothing") $ findStraightSkeleton c2 []) --> roundPoint2 <$> pointsOfContour c2
  where
    cp1 = [Point2 (1,0), Point2 (1,1), Point2 (0,1), Point2 (0,0)]
    c1 = makeSafeContour cp1
    c2 = makeSafeContour [Point2 (0.75,0.25), Point2 (0.75,0.75), Point2 (0.25,0.75), Point2 (0.25,0.25)]
    c2l1 = LineSeg (Point2 (0.75,0.25)) (Point2 (0,0.5))

geomAlgSpec :: Spec
geomAlgSpec = do
  describe "GVals (Math/GeometricAlgebra)" $ do
    -- 1e1+1e1 = 2e1
    it "adds two values with a common basis vector" $
      addValPair (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> [GVal 2 (singleton (GEPlus 1))]
    -- 1e1+1e2 = e1+e2
    it "adds two values with different basis vectors" $
      addValPair (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2))) --> [GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))]
    -- 2e1-1e1 = e1
    it "subtracts two values with a common basis vector" $
      subValPair (GVal 2 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> [GVal 1 (singleton (GEPlus 1))]
    -- 1e1-1e2 = e1-e2
    it "subtracts two values with different basis vectors" $
      subValPair (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2))) --> [GVal 1 (singleton (GEPlus 1)), GVal (-1) (singleton (GEPlus 2))]
    -- 1e1-1e1 = 0
    it "subtracts two identical values with a common basis vector and gets nothing" $
      subValPair (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 1))) --> []
    -- 1e0+1e1+1e2 = e0+e1+e2
    it "adds a value to a list of values" $
      addVal [GVal 1 (singleton (GEZero 1)), GVal 1 (singleton (GEPlus 1))] (GVal 1 (singleton (GEPlus 2))) --> [GVal 1 (singleton (GEZero 1)), GVal 1 (singleton (GEPlus 1)), GVal 1 (singleton (GEPlus 2))]
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
      mulScalarVec 2 (GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec [GVal 2 (singleton (GEPlus 1))]
    it "multiplies a (multi)vector by a scalar (G0)" $
      (GVec [GVal 2 (singleton G0)]) • (GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec [GVal 2 (singleton (GEPlus 1))]
    -- 2e1/2 = e1
    it "divides a (multi)vector by a scalar" $
      divVecScalar (GVec [GVal 2 (singleton (GEPlus 1))]) 2 --> GVec [GVal 1 (singleton (GEPlus 1))]
    -- 1e1|1e2 = 0
    it "the dot product of two orthoginal basis vectors is nothing" $
      GVec [GVal 1 (singleton (GEPlus 1))] ⋅ GVec [GVal 1 (singleton (GEPlus 2))] --> GVec []
    it "the dot product of two vectors is comutative (a⋅b == b⋅a)" $
      GVec (addValPair (GVal 1 (singleton (GEPlus 1))) (GVal 1 (singleton (GEPlus 2)))) ⋅ GVec (addValPair (GVal 2 (singleton (GEPlus 2))) (GVal 2 (singleton (GEPlus 2)))) -->
      GVec (addValPair (GVal 2 (singleton (GEPlus 1))) (GVal 2 (singleton (GEPlus 2)))) ⋅ GVec (addValPair (GVal 1 (singleton (GEPlus 2))) (GVal 1 (singleton (GEPlus 2))))
    -- 2e1|2e1 = 4
    it "the dot product of a vector with itsself is it's magnitude squared" $
      scalarPart (GVec [GVal 2 (singleton (GEPlus 1))] ⋅ GVec [GVal 2 (singleton (GEPlus 1))]) --> 4
    it "the like product of a vector with itsself is it's magnitude squared" $
      scalarPart (GVec [GVal 2 (singleton (GEPlus 1))] ⎣ GVec [GVal 2 (singleton (GEPlus 1))]) --> 4
    -- (2e1^1e2)|(2e1^1e2) = -4
    it "the dot product of a bivector with itsself is the negative of magnitude squared" $
      scalarPart (GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])] ⋅ GVec [GVal 2 (fromList [GEPlus 1, GEPlus 2])]) --> (-4)
    -- 1e1^1e1 = 0
    it "the wedge product of two identical vectors is nothing" $
      vectorPart (GVec [GVal 1 (singleton (GEPlus 1))] ∧ GVec [GVal 1 (singleton (GEPlus 1))]) --> GVec []
    it "the wedge product of two vectors is anti-comutative (u∧v == -v∧u)" $
      GVec [GVal 1 (singleton (GEPlus 1))] ∧ GVec [GVal 1 (singleton (GEPlus 2))] -->
      GVec [GVal (-1) (singleton (GEPlus 2))] ∧ GVec [GVal 1 (singleton (GEPlus 1))]
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

proj2DGeomAlgSpec :: Spec
proj2DGeomAlgSpec = do
  describe "Points (Math/PGA)" $
    -- ((1e0^1e1)+(-1e0^1e2)+(1e1+1e2))|((-1e0^1e1)+(1e0^1e2)+(1e1+1e2)) = -1
    it "the dot product of any two projective points is -1" $
      scalarPart (rawPPoint2 (1,1) ⋅ rawPPoint2 (-1,-1)) --> (-1)
  describe "Lines (Math/PGA)" $ do
    -- (-2e2)*2e1 = 4e12
    it "the intersection of a line along the X axis and a line along the Y axis is the origin point" $
      (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (-1,0)) (Point2 (2,0)))) ∧ (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (0,-1)) (Point2 (0,2)))) --> GVec [GVal 4 (fromList [GEPlus 1, GEPlus 2])]
    -- (-2e0+1e1)^(2e0-1e2) = -1e01+2e02-e12
    it "the intersection of a line two points above the X axis, and a line two points to the right of the Y axis is at (2,2) in the upper right quadrant" $
      vectorPart ((\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (2,0)) (Point2 (0,1)))) ∧ (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (0,2)) (Point2 (1,0))))) -->
      GVec [GVal (-2) (fromList [GEZero 1, GEPlus 1]), GVal 2 (fromList [GEZero 1, GEPlus 2]), GVal (-1) (fromList [GEPlus 1, GEPlus 2])]
    -- (2e0+1e1-1e2)*(2e0+1e1-1e2) = 2
    it "the geometric product of two overlapping lines is only a Scalar" $
      scalarPart ((\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (-1,1)) (Point2 (1,1)))) • (\(PLine2 a) -> a) (eToPLine2 (LineSeg (Point2 (-1,1)) (Point2 (1,1))))) --> 2.0
    it "A line constructed from a line segment is correct" $
      eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (1,1))) --> pl1
    it "A line constructed from by joining two points is correct" $
      join2PPoint2 (eToPPoint2 (Point2 (0,0))) (eToPPoint2 (Point2 (1,1))) --> pl1
  where
    pl1 = PLine2 $ GVec [GVal 1 (singleton (GEPlus 1)), GVal (-1) (singleton (GEPlus 2))]
    rawPPoint2 (x,y) = (\(PPoint2 v) -> v) $ eToPPoint2 (Point2 (x,y))

pgaSpec :: Spec
pgaSpec = do
  describe "Translation (math/PGA)" $ do
    it "a translated line translated back is the same line" $
      translatePerp (translatePerp (eToPLine2 l1) 1) (-1) --> eToPLine2 l1
  describe "Projection (math/PGA)" $ do
    it "a projection on the perpendicular bisector of an axis aligned line is on the other axis (1 of 2)" $
      pointOnPerp (LineSeg (Point2 (0,0)) (Point2 (0,1))) (Point2 (0,0)) 1 --> Point2 (-1,0)
    it "a projection on the perpendicular bisector of an axis aligned line is on the other axis (2 of 2)" $
      pointOnPerp (LineSeg (Point2 (0,0)) (Point2 (1,0))) (Point2 (0,0)) 1 --> Point2 (0,1)
  describe "Distance measurement (math/PGA)" $ do
    it "the distance between a point at (1,1) and a line on the X axis is 1" $
      distancePPointToPLine (eToPPoint2 $ Point2 (1,1)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (1,0))) --> 1
    it "the distance between a point at (2,2) and a line on the Y axis is 2" $
      distancePPointToPLine (eToPPoint2 $ Point2 (2,2)) (eToPLine2 $ LineSeg (Point2 (0,0)) (Point2 (0,-1))) --> 2
  describe "Layout Inspection (math/PGA)" $ do
    it "two points on the same side of a line show as being on the same side of the line" $
      pPointsOnSameSideOfPLine (eToPPoint2 (Point2 (-1,0))) (eToPPoint2 (Point2 (-1,-1))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (0,1)))) --> Just True
    it "two points on different sides of a line show as being on different sides of a line" $
      pPointsOnSameSideOfPLine (eToPPoint2 (Point2 (-1,0))) (eToPPoint2 (Point2 (1,0))) (eToPLine2 (LineSeg (Point2 (0,0)) (Point2 (0,1)))) --> Just False
  where
    l1 = LineSeg (Point2 (1,1)) (Point2 (2,2))

facetSpec :: Spec
facetSpec = do
  describe "Arcs (Skeleton/Concave)" $ do
    it "finds the inside arc of a right degree angle(to the left)" $
      getFirstArc (LineSeg (Point2 (0,1.0)) (Point2 (0.0,-1.0))) (LineSeg (Point2 (0,0)) (Point2 (1.0,0.0))) --> PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])
    it "finds the inside arc of a right degree angle(to the right)" $
      getFirstArc (LineSeg (Point2 (0,1.0)) (Point2 (0.0,-1.0))) (LineSeg (Point2 (0,0)) (Point2 (-1.0,0.0))) --> PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
    it "finds the inside arc of a 135 degree angle(to the left)" $
      getFirstArc (LineSeg (Point2 (0,1.0)) (Point2 (0.0,-1.0))) (LineSeg (Point2 (0,0)) (Point2 (1.0,-1.0))) --> PLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))])
    it "finds the inside arc of a 135 degree angle(to the right)" $
      getFirstArc (LineSeg (Point2 (0,1.0)) (Point2 (0.0,-1.0))) (LineSeg (Point2 (0,0)) (Point2 (-1.0,-1.0))) --> PLine2 (GVec [GVal 0.3826834323650899 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))])
    it "finds the inside arc of a given pair of line segments (first corner of c2)" $
      makeFirstENodes corner1 --> [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,-1.0)))
                                       (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                 ]
    it "finds the inside arc of a given pair of line segments (second corner of c2)" $
      makeFirstENodes corner2 --> [ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)))
                                       (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))]))
                                 ]
    it "finds the inside arc of a given pair of line segments (third corner of c2)" $
      makeFirstENodes corner3 --> [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0)))
                                       (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                 ]
    it "finds the inside arc of a given pair of line segments (fourth corner of c2)" $
      makeFirstENodes corner4 --> [ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,2.0)))
                                       (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                 ]
    it "finds the arc resulting from a node at the intersection of the outArc of two nodes (corner3 and corner4 of c2)" $
      averageNodes corner3E1 corner4E1 --> INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                 (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                                 (slist [])
                                           (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal 0.19509032201612836 (singleton (GEPlus 2))])))
  describe "Motorcycles (Skeleton/Motorcycles)" $ do
    it "finds one convex motorcycle in a simple shape" $
      convexMotorcycles c1 --> [Motorcycle (LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0))) (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 1))]))]
  describe "Straight Skeletons (skeleton/Tscherne)" $ do
    it "finds the straight skeleton of the left side of our first simple shape." $
      cellAfter c0 c0m1 --> cellAfter c4 c4m1
    it "finds the straight skeleton of the right side of our first simple shape." $
      cellBefore c0 c0m1 --> cellBefore c4 c4m1
    it "finds the straight skeleton of the left side of our first simple shape." $
      cellAfter c1 c1m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0,-2.0)))
                      (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (1.0,1.0)))
                      (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal (-0.3826834323650897) (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal (-0.3826834323650897) (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal 0.19509032201612836 (singleton (GEPlus 1)), GVal (-0.9807852804032305) (singleton (GEPlus 2))])))
                 ] ] 1))
    it "finds the straight skeleton of the right side of our first simple shape." $
      cellBefore c1 c1m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                      (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal 0.3826834323650897 (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                      (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal 0.3826834323650897 (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.19509032201612836 (singleton (GEPlus 1)), GVal 0.9807852804032305 (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the left side of our second simple shape." $
      cellAfter c2 c2m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                      (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (-1.0,1.0)))
                      (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal 0.19509032201612836 (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the right side of our second simple shape." $
      cellBefore c2 c2m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,1.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                      (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                      (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal 0.19509032201612836 (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the left side of our third simple shape." $
      cellAfter c3 c3m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                      (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,-1.0)))
                      (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal 0.3826834323650897 (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal 0.3826834323650897 (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.19509032201612836) (singleton (GEPlus 1)), GVal 0.9807852804032305 (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the right side of our third simple shape." $
      cellBefore c3 c3m1  -->
      makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,1.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                      (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal (-0.3826834323650897) (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                      (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal (-0.3826834323650897) (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal (-0.19509032201612836) (singleton (GEPlus 1)), GVal (-0.9807852804032305) (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the left side of our fourth simple shape." $
      cellAfter c4 c4m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                      (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)))
                      (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])))
                 ]
               ] 1))
    it "finds the straight skeleton of the right side of our fourth simple shape." $
      cellBefore c4 c4m1 -->
      makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                      (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
               ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                      (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
               ]
               (INodeSet (Slist [
                 [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                        (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                        (slist [])
                  (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])))
                 ]
               ] 1))
  describe "Straight Skeleton (Skeleton/Skeleton)" $ do
    it "finds the straight skeleton of our first simple shape." $
      findStraightSkeleton c0 [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)))
                                                                              (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                              (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal  (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)))
                                                                              (PLine2 (GVec [GVal 2.0 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [] 0))
                                                             ]] (Slist [] 0))
  describe "Straight Skeleton (Skeleton/Skeleton)" $ do
    it "finds the straight skeleton of our third simple shape." $
      findStraightSkeleton c2 [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                              (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (-1.0,1.0)))
                                                                              (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal  0.9238795325112867 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal 0.19509032201612836 (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,1.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                                                                              (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal 0.9238795325112867 (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal 0.19509032201612836 (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (-1.0,1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,1.0)))
                                                                              (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [] 0))
                                                             ]] (Slist [] 0))
    it "finds the straight skeleton of our fifth simple shape." $
      findStraightSkeleton c5 [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                              (PLine2 (GVec [GVal 0.5411961001461969 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)))
                                                                              (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal 0.5411961001461969 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal 0.7653668647301793 (singleton (GEZero 1)), GVal (-0.9238795325112867) (singleton (GEPlus 1)), GVal (-0.38268343236508967) (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (1.0,1.0)), LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.0)))
                                                                              (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [] 0))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (1.0,1.0)))
                                                                              (PLine2 (GVec [GVal (-0.5411961001461969) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal (-0.5411961001461969) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal 0.3826834323650899 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal (-0.7653668647301793) (singleton (GEZero 1)), GVal 0.9238795325112867 (singleton (GEPlus 1)), GVal (-0.38268343236508967) (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 1))
                                                             ,makeNodeTree [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)))
                                                                              (PLine2 (GVec [GVal 2.0 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [] 0))
                                                             ]] (Slist [] 0))
    it "finds the straight skeleton of our sixth simple shape." $
      findStraightSkeleton c6 [] --> Just (StraightSkeleton [[ makeNodeTree [ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (0.5,-1.0)), LineSeg (Point2 (0.5,-1.0)) (Point2 (0.5,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.9510565162951536) (singleton (GEZero 1)), GVal 0.8506508083520399 (singleton (GEPlus 1)), GVal (-0.5257311121191337) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (0.5,-1.0)) (Point2 (0.5,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                              (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                              (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal (-0.9510565162951536) (singleton (GEZero 1)), GVal 0.8506508083520399 (singleton (GEPlus 1)), GVal (-0.5257311121191337) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal (-0.606432399999752) (singleton (GEZero 1)), GVal 0.9932897335288758 (singleton (GEPlus 1)), GVal 0.11565251949756605 (singleton (GEPlus 2))])))
                                                                         ]
                                                                       , [INode (PLine2 (GVec [GVal (-0.606432399999752) (singleton (GEZero 1)), GVal 0.9932897335288758 (singleton (GEPlus 1)), GVal 0.11565251949756605 (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal (-0.6961601101968017) (singleton (GEZero 1)), GVal 0.328526568895664 (singleton (GEPlus 1)), GVal 0.9444947292227959 (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 2)),
                                                               makeNodeTree [ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                                                                              (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.5,0.0)))
                                                                              (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                       ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.5,0.0)), LineSeg (Point2 (-0.5,-1.0)) (Point2 (0.5,1.0)))
                                                                              (PLine2 (GVec [GVal 0.9510565162951536 (singleton (GEZero 1)), GVal 0.8506508083520399 (singleton (GEPlus 1)), GVal 0.5257311121191337 (singleton (GEPlus 2))]))
                                                                       ]
                                                                       (INodeSet (Slist [
                                                                         [INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.9510565162951536 (singleton (GEZero 1)), GVal 0.8506508083520399 (singleton (GEPlus 1)), GVal 0.5257311121191337 (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal 0.606432399999752 (singleton (GEZero 1)), GVal 0.9932897335288758 (singleton (GEPlus 1)), GVal (-0.11565251949756605) (singleton (GEPlus 2))])))
                                                                         ],
                                                                         [INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                (PLine2 (GVec [GVal 0.606432399999752 (singleton (GEZero 1)), GVal 0.9932897335288758 (singleton (GEPlus 1)), GVal (-0.11565251949756605) (singleton (GEPlus 2))]))
                                                                                (Slist [] 0)
                                                                          (Just (PLine2 (GVec [GVal 0.6961601101968017 (singleton (GEZero 1)), GVal 0.328526568895664 (singleton (GEPlus 1)), GVal (-0.9444947292227959) (singleton (GEPlus 2))])))
                                                                         ]
                                                                       ] 2)),
                                                              makeNodeTree [ENode (LineSeg (Point2 (-0.5,-1.0)) (Point2 (0.5,1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (0.5,-1.0)))
                                                                              (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 1))]))
                                                                       ]
                                                                       (INodeSet (Slist [] 0))
                                                             ]] (Slist [] 0))
    it "finds the straight skeleton of a triangle." $
      findStraightSkeleton triangle [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.7320508075688772)), LineSeg (Point2 (1.0,1.7320508075688772)) (Point2 (-1.0,-1.7320508075688772)))
                                                                                    (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                             ,ENode (LineSeg (Point2 (1.0,1.7320508075688772)) (Point2 (-1.0,-1.7320508075688772)), LineSeg (Point2 (0.0,0.0)) (Point2 (2.0,0.0)))
                                                                                    (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                             ,ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.7320508075688772)))
                                                                                    (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                                                                             ]
                                                                             (INodeSet (Slist [
                                                                               [INode (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                                      (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)),GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                               (slist [PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)),GVal 0.8660254037844387 (singleton (GEPlus 2))])])
                                                                                      Nothing
                                                                               ]
                                                                             ] 1))
                                                                   ]] (Slist [] 0))
    it "finds the straight skeleton of a square." $
      findStraightSkeleton square [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                                  (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                           ,ENode (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                                  (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                           ,ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                                  (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                           ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                                                                                  (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                           ]
                                                                           (INodeSet (Slist [
                                                                             [INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                    (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                             (slist [PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))])
                                                                                    ,PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))])])
                                                                                    Nothing
                                                                             ]
                                                                           ] 1))
                                                                 ]] (Slist [] 0))
    it "finds the straight skeleton of a rectangle." $
      findStraightSkeleton rectangle [] --> Just (StraightSkeleton [[makeNodeTree [ENode (LineSeg (Point2 (-2.0,1.0)) (Point2 (0.0,-2.0)), LineSeg (Point2 (-2.0,-1.0)) (Point2 (3.0,0.0)))
                                                                                     (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEZero 1)), GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                              ,ENode (LineSeg (Point2 (-2.0,-1.0)) (Point2 (3.0,0.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                                     (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                              ,ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-3.0,0.0)))
                                                                                     (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                              ,ENode (LineSeg (Point2 (1.0,1.0)) (Point2 (-3.0,0.0)), LineSeg (Point2 (-2.0,1.0)) (Point2 (0.0,-2.0)))
                                                                                     (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEZero 1)), GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                              ]
                                                                              (INodeSet (Slist [
                                                                                [INode (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEZero 1)), GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                       (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEZero 1)), GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                       (slist [])
                                                                                       (Just (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))])))
                                                                                ,INode (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                       (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                       (slist [])
                                                                                       (Just (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 2))])))
                                                                                ],
                                                                                 [INode (PLine2 (GVec [GVal 1.0 (singleton (GEPlus 2))]))
                                                                                        (PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))]))
                                                                                        (slist [])
                                                                                        Nothing
                                                                                 ]
                                                                              ] 2))
                                                                    ]] (Slist []0))
  describe "Faces (Skeleton/Face)" $ do
    it "finds faces from a straight skeleton (default order)" $
      facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 []) --> [ Face (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)))
                                                                                           (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                           (slist [PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                           (PLine2 (GVec [GVal 2.0    (singleton (GEPlus 2))]))
                                                                                    , Face (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                    , Face (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                           (slist [PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))]),
                                                                                                   PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                    , Face (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                                           (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                    , Face (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)))
                                                                                           (PLine2 (GVec [GVal 2.0    (singleton (GEPlus 2))]))
                                                                                           (slist [PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                           (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                    ]
    it "finds faces from a straight skeleton (manual order)" $
      orderedFacesOf c0l0 (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 []) --> [ Face (LineSeg (Point2 (0.0,0.0)) (Point2 (-1.0,-1.0)))
                                                                                        (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                        (slist [PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                        (PLine2 (GVec [GVal 2.0    (singleton (GEPlus 2))]))
                                                                                 , Face (LineSeg (Point2 (-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                                        (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                        (slist [])
                                                                                        (PLine2 (GVec [GVal (-0.541196100146197) (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                 , Face (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                                        (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                        (slist [PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))]),
                                                                                                PLine2 (GVec [GVal (-0.4870636221857319) (singleton (GEZero 1)), GVal 0.9807852804032305 (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                        (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                 , Face (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                        (slist [])
                                                                                        (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                 , Face (LineSeg (Point2 (-1.0,1.0)) (Point2 (1.0,-1.0)))
                                                                                        (PLine2 (GVec [GVal 2.0    (singleton (GEPlus 2))]))
                                                                                        (slist [PLine2 (GVec [GVal 0.4870636221857319 (singleton (GEZero 1)), GVal (-0.9807852804032305) (singleton (GEPlus 1)), GVal (-0.19509032201612836) (singleton (GEPlus 2))])])
                                                                                        (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal (-0.3826834323650897) (singleton (GEPlus 1)), GVal (-0.9238795325112867) (singleton (GEPlus 2))]))
                                                                                 ]
    it "finds faces from a triangle (default order)" $
      facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton triangle []) --> [Face (LineSeg (Point2 (1.0,1.73205080756887729)) (Point2 (-1.0,-1.7320508075688772)))
                                                                                             (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                                             (slist [])
                                                                                             (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                                       ,Face (LineSeg (Point2 (0.0,0.0)) (Point2 (2.0,0.0)))
                                                                                             (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                                                                                             (slist [])
                                                                                             (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                                       ,Face (LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.7320508075688772)))
                                                                                             (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                                             (slist [])
                                                                                             (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                                                                                          ]
    it "finds faces from a triangle (manual order)" $
      orderedFacesOf trianglel0 (fromMaybe (error "got Nothing") $ findStraightSkeleton triangle []) --> [Face (LineSeg (Point2 (2.0,0.0)) (Point2 (-1.0,1.7320508075688772)))
                                                                                                               (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                                                               (slist [])
                                                                                                               (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                                                                                                         ,Face (LineSeg (Point2 (1.0,1.73205080756887729)) (Point2 (-1.0,-1.7320508075688772)))
                                                                                                               (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                                                               (slist [])
                                                                                                               (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                                                                                                         ,Face (LineSeg (Point2 (0.0,0.0)) (Point2 (2.0,0.0)))
                                                                                                               (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                                                                                                               (slist [])
                                                                                                               (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                                                                                             ]
    it "finds faces from a square" $
      facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton square []) --> [Face (LineSeg (Point2(-1.0,-1.0)) (Point2 (2.0,0.0)))
                                                                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal  0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                     ,Face (LineSeg (Point2 (1.0,-1.0)) (Point2 (0.0,2.0)))
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475(singleton (GEPlus 2))]))
                                                                                     ,Face (LineSeg (Point2 (1.0,1.0)) (Point2 (-2.0,0.0)))
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                                                                                     ,Face (LineSeg (Point2 (-1.0,1.0)) (Point2 (0.0,-2.0)))
                                                                                           (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                           (slist [])
                                                                                           (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                                                                                ]
    it "finds faces from a rectangle" $
      facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton rectangle [])
      --> [Face (LineSeg (Point2 (-2.0,1.0)) (Point2 (0.0,-2.0)))
                (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEZero 1)), GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                (slist [])
                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEZero 1)), GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
          ,Face (LineSeg (Point2 (-2.0,-1.0)) (Point2 (3.0,0.0)))
                (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                (slist [PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))])])
                (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEZero 1)), GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
          ,Face (LineSeg (Point2(1.0,-1.0)) (Point2 (0.0,2.0)))
                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
                (slist [])
                (PLine2 (GVec [GVal  0.7071067811865475 (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
          ,Face (LineSeg (Point2 (1.0,1.0)) (Point2 (-3.0,0.0)))
                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEZero 1)), GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
                (slist [PLine2 (GVec [GVal (-1.0) (singleton (GEPlus 2))])])
                (PLine2 (GVec [GVal (-0.7071067811865475) (singleton (GEPlus 1)), GVal 0.7071067811865475 (singleton (GEPlus 2))]))
          ]

  describe "insets (Skeleton/Line)" $ do
    it "insets a triangle" $
      addInset 1 0.25 (facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton triangle [])
      --> ([SafeContour (Point2 (0.4330127018922193,0.25))
                        (Point2 (1.5669872981077808,1.2320508075688772))
                        (Point2 (1.0,1.2320508075688772))
                        (Point2 (0.4330127018922193,0.25))
                        (Point2 (1.5669872981077808,0.2500000000000001))
                        (Slist [] (Size 0))]
          ,[Face (LineSeg (Point2 (-0.43301270189221935,-0.25000000000000006)) (Point2 (1.4330127018922194,2.482050807568877)))
                 (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
           ,Face (LineSeg (Point2 (2.433012701892219,-0.25)) (Point2 (-2.8660254037844384,0.0)))
                 (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
                 (slist [])
                 (PLine2 (GVec [GVal 0.5000000000000001 (singleton (GEPlus 1)),GVal (-0.8660254037844387) (singleton (GEPlus 2))]))
           ,Face (LineSeg (Point2 (1.0,2.232050807568877)) (Point2 (1.4330127018922192,-2.482050807568877)))
                 (PLine2 (GVec [GVal 1.0 (singleton (GEZero 1)), GVal (-1.0) (singleton (GEPlus 1))]))
                 (slist [])
                 (PLine2 (GVec [GVal (-1.0000000000000002) (singleton (GEZero 1)), GVal 0.5000000000000001 (singleton (GEPlus 1)), GVal 0.8660254037844387 (singleton (GEPlus 2))]))
           ])
    where
      -- c0 - c4 are the contours of a square around the origin with a 90 degree chunk missing, rotated 0, 90, 180, 270 and 360 degrees:
      --    __
      --    \ |
      --    /_|
      --
      c0 = makeSafeContour [Point2 (0,0), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c0l0 = LineSeg (Point2 (0,0)) (Point2 (-1,-1))
      c0m1 = Motorcycle (LineSeg (Point2 (-1,1)) (Point2 (1,-1)), LineSeg (Point2 (0,0)) (Point2 (-1,-1))) (PLine2 (GVec [GVal 2.0 (singleton (GEPlus 2))]))
      c1 = makeSafeContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c1m1 = Motorcycle (LineSeg (Point2 (-1,-1)) (Point2 (1,1)), LineSeg (Point2 (0,0)) (Point2 (1,-1))) (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 1))]))
      c2 = makeSafeContour [Point2 (-1,-1), Point2 (1,-1), Point2 (0,0), Point2 (1,1), Point2 (-1,1)]
      c2m1 = Motorcycle (LineSeg (Point2 (1,-1)) (Point2 (-1,1)), LineSeg (Point2 (0,0)) (Point2 (1,1))) (PLine2 (GVec [GVal (-2.0) (singleton (GEPlus 2))]))
      c3 = makeSafeContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (0,0), Point2 (-1,1)]
      c3m1 = Motorcycle (LineSeg (Point2 (1,1)) (Point2 (-1,-1)), LineSeg (Point2 (0,0)) (Point2 (-1,1))) (PLine2 (GVec [GVal 2.0 (singleton (GEPlus 1))]))
      c4 = makeSafeContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c4m1 = Motorcycle (LineSeg (Point2 (-1,1)) (Point2 (1,-1)), LineSeg (Point2 (0,0)) (Point2 (-1,-1))) (PLine2 (GVec [GVal 2.0 (singleton (GEPlus 2))]))
      c5 = makeSafeContour [Point2 (-1,-1), Point2 (1,-1), Point2 (2,0), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c6 = makeSafeContour [Point2 (-1,-1), Point2 (-0.5,-1), Point2 (0,0), Point2 (0.5,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      -- The next corners are part of a square around the origin with a piece missing: (think: c2 from above)
      --    __  <-- corner 1
      --   | /
      --   | \
      --   ~~~  <-- corner 3
      --   ^-- corner 4
      -- the top side, and the entry to the convex angle of a 2x2 square around the origin, with a slice missing.
      corner1 = [ LineSeg (Point2 (-1.0,1.0)) (Point2 (2.0,0.0)), LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,-1.0))]
      -- the entry and the exit to the convex angle of a 2x2 square around the origin, with a slice missing.
      corner2 = [ LineSeg (Point2 (1.0,1.0)) (Point2 (-1.0,-1.0)), LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0))]
      -- the exit to the convex angle and the bottom of a 2x2 square around the origin, with a slice missing.
      corner3 = [ LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)), LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0))]
      corner3E1 = ENode (LineSeg (Point2 (0.0,0.0)) (Point2 (1.0,-1.0)) ,LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0))) (PLine2 (GVec [GVal 0.541196100146197 (singleton (GEZero 1)), GVal 0.3826834323650897 (singleton (GEPlus 1)),GVal 0.9238795325112867 (singleton (GEPlus 2))]))
      -- the bottom and the left side of a 2x2 square around the origin, with a slice missing.
      corner4 = [ LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0)), LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,2.0))]
      corner4E1 = ENode (LineSeg (Point2 (1.0,-1.0)) (Point2 (-2.0,0.0)),LineSeg (Point2 (-1.0,-1.0)) (Point2 (0.0,2.0))) (PLine2 (GVec [GVal 0.7071067811865475 (singleton (GEPlus 1)), GVal (-0.7071067811865475) (singleton (GEPlus 2))]))
      -- A simple triangle.
      triangle = makeSafeContour [Point2 (2,0), Point2 (1.0,sqrt 3), Point2 (0,0)]
      trianglel0 = LineSeg (Point2 (2,0)) (Point2 (-1.0,sqrt 3))
      -- A simple square.
      square = makeSafeContour [Point2 (-1,1), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1)]
      -- A simple rectangle.
      rectangle = makeSafeContour [Point2 (-2,1), Point2 (-2,-1), Point2 (1,-1), Point2 (1,1)]
