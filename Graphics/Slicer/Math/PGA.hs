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

 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{- The purpose of this file is to hold projective geometric algebraic arithmatic. -}

-- for adding Generic and NFData to Point.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.PGA(GNum(GEMinus, GEPlus, GEZero, G0), GVal, GVec, addValPair, addVals, addVecs, mulScalarVec, innerProduct, outerProduct, geometricProduct, projectContour) where

import Prelude (Eq, Show, Ord, error, seq, (==), (/=), (+), otherwise, ($), map, (++), head, tail, foldl, filter, not, (>), (*), concatMap, (<$>), null, odd, (<=), fst, snd)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData(rnf))

import Data.List.Ordered(sort, insertSet)

import Graphics.Slicer.Definitions (ℝ, Fastℕ)

import Graphics.Slicer.Math.Definitions(Point2(Point2), Contour(PointSequence))

-- Our 2D plane coresponds to a Clifford algebra of 2,0,1.

-- FIXME: move this to the proper place in ImplicitCAD.
instance NFData Fastℕ where
  rnf a = seq a ()

-- The geometric numbers.
-- We are deriving Ord so we can sort the terms during simplification.
data GNum =
    GEMinus Fastℕ -- squared equal to -1 -- associated with rotation
  | GEZero  Fastℕ -- squared equal to  0 -- associated with translations
  | GEPlus  Fastℕ -- squared equal to +1 -- associated with space/time or hyperbolic rotations
  | G0      -- no imaginary component present
  deriving (Eq, Generic, NFData, Show, Ord)

-- A value in geometric algebra
data GVal = GVal { _real :: ℝ, _basis :: [GNum] }
  deriving (Eq, Generic, NFData, Show, Ord)

-- A (multi)vector in geometric algebra.
newtype GVec = GVec [GVal]
  deriving (Eq, Generic, NFData, Show, Ord)

{-
isVec :: GVec -> Bool
isVec vec =
  where
    combined :: [GNum]
    combined = sort $ (allBasisVectors vec1) ++ (allBasisVectors vec2)
    allBasisVectors :: GVec -> [GNum]
    allBasisVectors (GVec vals) = concatMap (\v -> iOf v) vals
-}

-- | add two geometric values together.
addValPair :: GVal -> GVal -> [GVal]
addValPair v1@(GVal r1 i1) v2@(GVal r2 i2)
  | i1 == i2  = [GVal (r1+r2) i1]
  | otherwise = sort [v1,v2]

-- | Add a geometric value to a list of geometric values. assumes the list of values is in order by basis vector, so we can find items with matching basis vectors easily.
addVals :: [GVal] -> GVal -> [GVal]
addVals dst src
  | not $ null $ sameBasis src dst = insertSet (GVal (foldl (+) (rOf src) (rOf <$> sameBasis src dst)) $ iOf src) $ diffBasis src dst
  | otherwise                      = insertSet src dst
  where
    sameBasis :: GVal -> [GVal] -> [GVal]
    sameBasis val vals = filter (\(GVal _ i) -> i == iOf val) vals
    diffBasis :: GVal -> [GVal] -> [GVal]
    diffBasis val vals = filter (\(GVal _ i) -> i /= iOf val) vals
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- | Add two vectors together.
addVecs :: GVec -> GVec -> GVec
addVecs (GVec vals1) (GVec vals2) = GVec $ foldl addVals vals1 vals2 

-- | multiply a vector by a scalar. arguments are given in this order for maximum readability.
mulScalarVec :: ℝ -> GVec -> GVec
mulScalarVec s (GVec vals) = GVec $ (mulVal s) <$> vals
  where
    mulVal s1 (GVal r i) = GVal (s1*r) i

-- FIXME: implement this:
-- magnitudeVec :: GVec -> GVec -> ℝ

-- | Calculate the dot product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `dotVecPair` gvec2 == gvec2 `dotVecPair` gvec1.
dotVecPair :: GVec -> GVec -> ℝ
dotVecPair a b
  | a > b     = dotVecPair' a b
-- FIXME: two equal vectors == magnitude of the vector, squared.
--  | a = b     =
  | otherwise = dotVecPair' b a

-- Generate the dot product of a vector pair.
dotVecPair' :: GVec -> GVec -> ℝ
dotVecPair' (GVec vals1) (GVec vals2) = foldl (+) 0 $ (mulMatchingBasis vals1) <$> vals2
  where
    mulMatchingBasis vals val
      | not $ null $ sameBasis val vals = foldl (*) (rOf val) (rOf <$> sameBasis val vals)
      | otherwise                       = 0
    sameBasis :: GVal -> [GVal] -> [GVal]
    sameBasis val vals = filter (\(GVal _ i) -> i == iOf val) vals
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- generate the wedge product of a vector pair.
wedgeVecPair :: GVec -> GVec -> GVec
wedgeVecPair vec1 vec2 = GVec $ foldl addVals [(head results)] (tail results) 
  where
    results = wedgeVecPair' (addMissing vec1 combined) (addMissing vec2 combined)
    combined :: [GNum]
    combined = sort $ (allBasisVectors vec1) ++ (allBasisVectors vec2)
    allBasisVectors :: GVec -> [GNum]
    allBasisVectors (GVec vals) = concatMap (\v -> iOf v) vals
    -- Add in missing basis vectors to ensure the given vector has a value in each of the given basis vectors.
    addMissing :: GVec -> [GNum] -> GVec
    addMissing (GVec vals) nums = GVec $ (emptyIfMissing nums) <$> vals
    emptyIfMissing :: [GNum] -> GVal -> GVal
    emptyIfMissing bvecs val@(GVal _ i) = if filter (\v -> [v] == i) bvecs == []
                                         then GVal 0 i
                                         else val
    -- now that we have an equal number of basis vectors, cycle through one list, and generate a pair with the second list when the two basis vectors are not the same.
    wedgeVecPair' :: GVec -> GVec -> [GVal]
    wedgeVecPair' (GVec v1) (GVec v2) = filterZeroes $ concatMap (crossWedgeDiff v1) $ filterZeroes v2
      where
        crossWedgeDiff :: [GVal] -> GVal -> [GVal]
        crossWedgeDiff vals (GVal r1 i1) = filterZeroes $ ( \(GVal r2 i2) -> sortBasis $ GVal (r1*r2) (i2++i1) ) <$> filter (\(GVal _ i2) -> not $ i2 == i1) vals
        filterZeroes = filter (\v -> not $ rOf v == 0)
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- for a multi-basis value where each basis is wedged against one another, sort the basis vectors remembering to invert the value if necessary.
-- really a mutant form of quicksort.
sortBasis :: GVal -> GVal
sortBasis (GVal r i) = if odd flipR then GVal (-r) newI else GVal r newI
  where
    newI :: [GNum]
    (flipR, newI) = sortBasis' (0,i)
sortBasis' :: (Fastℕ, [GNum]) -> (Fastℕ, [GNum])
sortBasis' (_,[])     = (0,[])
sortBasis' (_,[a])    = (0,[a])
sortBasis' (_,x:xs) = if lowerBasis == (0,[]) then ((sumFlips higherBasis), x:(newBasis higherBasis))  else (sumFlips lowerBasis + 1 + sumFlips higherBasis, (newBasis lowerBasis ++ [x] ++ newBasis higherBasis))
  where
    lowerBasis  = sortBasis' (0,[ a | a <- xs, a<=x ])
    higherBasis = sortBasis' (0,[ a | a <- xs, a>x  ])
    sumFlips :: (Fastℕ, [GNum]) -> Fastℕ
    sumFlips flips = fst flips
    newBasis flips = snd flips

-- the dot product is the inner product in geometric algebra terms.
innerProduct :: GVec -> GVec -> ℝ
innerProduct = dotVecPair

-- the outer product always generates a (bi)vector, where the basis vector order is derived from the Ord of GNum of the basis vectors.
outerProduct :: GVec -> GVec -> GVec
outerProduct = wedgeVecPair

data GProduct = GProduct ℝ GVec
  deriving (Eq, Generic, NFData, Show, Ord)

geometricProduct :: GVec -> GVec -> GProduct 
geometricProduct v1 v2 = GProduct (innerProduct v1 v2) (outerProduct v1 v2)

-- | A single Point in 2D geometric space.
newtype GPoint2 = GPoint2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | A single Point in 3D geometric space.
newtype GPoint3 = GPoint3 GVec
  deriving (Eq, Generic, NFData, Show)

-- | A projective point in 2D space.
data PPoint2 = PPoint2 { _pointInPlane :: GPoint2, _pointOrigin :: GPoint3 }
  deriving (Eq, Generic, NFData, Show)

-- | A projective line in 2D space.
data PLine2 = PLine2 { _lineInPlane :: GPoint2, _lineOrigin :: GPoint3 }
  deriving (Eq, Generic, NFData, Show)

-- | Create a 2D geometric point from a linear point.
toGeometricPoint :: Point2 -> GPoint2
toGeometricPoint (Point2 (x,y)) = GPoint2 $ GVec $ [ GVal x $ [GEMinus 1], GVal y $ [GEPlus 1] ]

-- | Create the origin point used when performing projective geometry.
toOriginPoint :: Point2 -> GPoint3
toOriginPoint (Point2 (x,y)) = GPoint3 $ GVec $ [ GVal x $ [GEMinus 1], GVal y $ [GEPlus 1], GVal 1 $ [GEZero 1]]


-- | Create a 2D projective point from a linear point.
toProjectivePoint :: GPoint3 -> GPoint2 -> PPoint2
toProjectivePoint p1 p2 = PPoint2 p2 p1 

-- Calculate the Point where two lines meet.
meetLines :: PLine2 -> PLine2 -> PPoint2
meetLines = error "not yet implemented"

-- Calculate the line on which the two points reside.
joinPoints :: PPoint2 -> PPoint2 -> PLine2
joinPoints p1 p2 = dualPoint $ meetLines (dualPoint p1) (dualPoint p2)

-- Convert from a PLine to it's associated projective point.
dualLine :: PLine2 -> PPoint2
dualLine (PLine2 inPlane origin) = PPoint2 inPlane origin

-- Convert from a PPoint2 to it's associated projective Line.
dualPoint :: PPoint2 -> PLine2
dualPoint (PPoint2 inPlane origin) = PLine2 inPlane origin

-- | A contour in 2D projective space. 
data PContour =
  -- For a PLineSequence, the edges of the object are the lines, in order, with the right side of the line pointing toward 'inner' space.
  -- For a PLine parallel with the X axis, we will have to manually test inward/outwardness.
  PLineSequence [PLine2] 
  deriving (Eq, Generic, NFData, Show)

-- | Create a projective contour, from a linear (point based) contour.
-- Use the same center point for all layers to make use of haskell's laziness.
--projectContour :: Contour -> Point2 -> PContour
projectContour (PointSequence points) centerPoint = (toProjectivePoint $ toOriginPoint centerPoint) <$> map toGeometricPoint points
  where
    plineFromPoints p1@(Point2 (x1,y1)) p2@(Point2 (x2,y2)) = error "not yet implemented." -- joinPoints (goemetricPoint 

-- | Create a line given it's endpoints.
plineFromEndpoints :: PPoint2 -> PPoint2 -> PLine2
plineFromEndpoints = error "not yet implemented"

