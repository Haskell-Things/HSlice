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

{- The purpose of this file is to hold projective geometric algebraic arithmatic. -}

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.PGA(PPoint2(PPoint2), eToPPoint2, cannonicalizePPoint2, eToPLine2) where

import Prelude (Eq, Show, error, (==), ($), filter, (*), (-))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions(Point2(Point2), Contour(PointSequence))

import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (∧), (⋅), (•), addValPair, subValPair, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, innerProduct, outerProduct, scalarIze)

-- Our 2D plane coresponds to a Clifford algebra of 2,0,1.

-- | A projective point in 2D space.
data PPoint2 = PPoint2 { _pointInPlane :: GVec}
  deriving (Eq, Generic, NFData, Show)

-- | A projective line in 2D space.
data PLine2 = PLine2 { _lineInPlane :: GVec}
  deriving (Eq, Generic, NFData, Show)

-- | Create a 2D projective point from a 2D euclidian point.
eToPPoint2 :: Point2 -> PPoint2
eToPPoint2 (Point2 (x,y)) = PPoint2  $ GVec [ GVal (-x) [GEZero 1, GEPlus 2], GVal y [GEZero 1, GEPlus 1], GVal 1 [GEPlus 1, GEPlus 2] ]

cannonicalizePPoint2 :: PPoint2 -> PPoint2
cannonicalizePPoint2 (PPoint2 vec@(GVec vals)) = PPoint2 $ divVecScalar vec $ valOf $ getVals [GEPlus 1, GEPlus 2] vals
  where
    getVals num vs = filter (\(GVal _ n) -> n == num) vs
    valOf :: [GVal] -> ℝ
    valOf [] = 1
    valOf [(GVal v _)] = v
    valOf (x:_) = valOf [x]

-- | Create a 2D projective line from a pair of euclidian endpoints.
eToPLine2 :: Point2 -> Point2 -> PLine2
eToPLine2 (Point2 (x1,y1)) (Point2 (x2,y2)) = PLine2 $ GVec [ GVal c [GEZero 1], GVal a [GEPlus 1], GVal b [GEPlus 2] ]
  where
    a=y2-y1
    b=x1-x2
    c=y1*x2-x1*y2

-- | Convert from a PLine to it's associated projective point.
dualLine :: PLine2 -> PPoint2
dualLine (PLine2 inPlane) = PPoint2 inPlane

-- | Convert from a PPoint2 to it's associated projective Line.
dualPoint :: PPoint2 -> PPoint2
dualPoint (PPoint2 inPlane) = PPoint2 $ inPlane ∧ (GVec [GVal 1 [GEZero 1, GEPlus 1, GEPlus 2]])

meet2Point2 :: PPoint2 -> PPoint2 -> PLine2
meet2Point2 (PPoint2 v1) (PPoint2 v2) = PLine2 $ v1 ∧ v2

meet2Line2 :: PLine2 -> PLine2 -> PPoint2
meet2Line2 = error $ "not yet implemented."

-- | Calculate the Point where two lines meet.
{-
meetLines :: PLine2 -> PLine2 -> PPoint2
meetLines l1@(PLine2 (GPoint2 v1) o1) l2@(PLine2 (GPoint2 v2) o2)
  | o1 == o2 = PPoint2 (GPoint2 $ v1 ∧ v2) o1
  | otherwise = error "normalizing origin points not yet implemented."
-}

-- | Calculate the line on which the two points reside.
--joinPoints :: PPoint2 -> PPoint2 -> PLine2
--joinPoints p1 p2 = dualPoint $ meetLines (dualPoint p1) (dualPoint p2)

-- | A contour in 2D projective space. 
data PContour =
  -- For a PLineSequence, the edges of the object are the lines, in order, with the right side of the line pointing toward 'inner' space.
  -- For a PLine parallel with the X axis, we will have to manually test inward/outwardness.
  PLineSequence [PLine2] 
  deriving (Eq, Generic, NFData, Show)

-- | Create a projective contour, from a linear (point based) contour.
-- Use the same center point for all layers to make use of haskell's laziness.
--projectContour :: Contour -> Point2 -> PContour
{-projectContour (PointSequence points) centerPoint = (toProjectivePoint $ toOriginPoint centerPoint) <$> map toGeometricPoint points
  where
    plineFromPoints p1@(Point2 (x1,y1)) p2@(Point2 (x2,y2)) = error "not yet implemented." -- joinPoints (goemetricPoint 
-}

