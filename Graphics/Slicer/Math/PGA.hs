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

-- for adding Generic and NFData to Point.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.PGA(GNum(GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (∧), addValPair, subValPair, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, innerProduct, outerProduct, geometricProduct, scalarIze, projectContour) where

import Prelude (Eq, Show, Ord(compare), error, seq, (==), (/=), (+), otherwise, ($), map, (++), head, tail, foldl, filter, not, (>), (*), concatMap, (<$>), null, odd, (<=), fst, snd, sum, (&&), any, (/), Bool(True, False))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData(rnf))

import Data.List.Ordered(sort, insertSet)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

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
  | G0            -- A scalar type. short lived.
  deriving (Eq, Generic, NFData, Show, Ord)

-- A value in geometric algebra
data GVal = GVal { _real :: ℝ, _basis :: [GNum] }
  deriving (Eq, Generic, NFData, Show)

instance Ord GVal where
  (GVal r1 i1) `compare` (GVal r2 i2)
    | i1 == i2  = compare r1 r2
    | otherwise = compare i1 i2

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
  | i1 == i2 && r1 == (-r2) = []
  | i1 == i2                = [GVal (r1+r2) i1]
  | otherwise               = sort [v1,v2]

-- | subtract a geometric value from another geometric vaalue.
subValPair :: GVal -> GVal -> [GVal]
subValPair v1@(GVal r1 i1) (GVal r2 i2)
  | i1 == i2 && r1 == r2 = []
  | otherwise            = addValPair v1 $ GVal (-r2) i2

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addVal :: [GVal] -> GVal -> [GVal]
addVal dst src@(GVal r1 _)
  | not $ null $ sameBasis src dst = if sum (rOf <$> sameBasis src dst) == (-r1)
                                     then diffBasis src dst
                                     else insertSet (GVal (r1 + sum (rOf <$> sameBasis src dst)) $ iOf src) $ diffBasis src dst
  | otherwise                      = insertSet src dst
  where
    sameBasis :: GVal -> [GVal] -> [GVal]
    sameBasis val vals = filter (\(GVal _ i) -> i == iOf val) vals
    diffBasis :: GVal -> [GVal] -> [GVal]
    diffBasis val vals = filter (\(GVal _ i) -> i /= iOf val) vals
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- | subtract a geometric value from a list of geometric values.
--   assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
subVal :: [GVal] -> GVal -> [GVal]
subVal dst (GVal r i) = addVal dst $ GVal (-r) i

-- | Add two vectors together.
addVecPair :: GVec -> GVec -> GVec
addVecPair (GVec vals1) (GVec vals2) = GVec $ foldl addVal vals1 vals2

-- | subtract one vector from the other.
subVecPair :: GVec -> GVec -> GVec
subVecPair (GVec vals1) (GVec vals2) = GVec $ foldl subVal vals1 vals2

-- | multiply a vector by a scalar. arguments are given in this order for maximum readability.
mulScalarVec :: ℝ -> GVec -> GVec
mulScalarVec s (GVec vals) = GVec $ mulVal s <$> vals
  where
    mulVal s1 (GVal r i) = GVal (s1*r) i

-- | divide a vector by a scalar. arguments are given in this order for maximum readability.
divVecScalar :: GVec -> ℝ -> GVec
divVecScalar (GVec vals) s = GVec $ divVal s <$> vals
  where
    divVal s1 (GVal r i) = GVal (s1/r) i

-- FIXME: implement this:
-- magnitudeVec :: GVec -> GVec -> ℝ

-- | Calculate the dot product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `dotVecPair` gvec2 == gvec2 `dotVecPair` gvec1.
dotVecPair :: GVec -> GVec -> Maybe GVec
dotVecPair a b
  | a > b     = antiWedgeVecPair a b
-- FIXME: two equal vectors == magnitude of the vector, squared.
--  | a = b     =
  | otherwise = antiWedgeVecPair b a

-- generate the dot product of a vector pair.
antiWedgeVecPair :: GVec -> GVec -> Maybe GVec
antiWedgeVecPair vec1 vec2 = if null results
                         then Nothing
                         else Just $ GVec $ foldl addVal [head results] $ tail results
  where
    results = antiWedgeVecPair' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    antiWedgeVecPair' :: GVec -> GVec -> [GVal]
    antiWedgeVecPair' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [GVal]
        multiplyLike vals (GVal r1 i1) = invert $ filterZeroes $ ( \(GVal r2 i2) -> sortBasis $ GVal (r1*r2) (i2++i1) ) <$> filter (\(GVal _ i2) -> i2 == i1) vals
        filterZeroes = filter (\v -> rOf v /= 0)
        invert xs = map (\(GVal r i) -> GVal (-r) i) xs 
    rOf (GVal r _) = r

-- generate the wedge product of a vector pair.
wedgeVecPair :: GVec -> GVec -> Maybe GVec
wedgeVecPair vec1 vec2 = if null results
                         then Nothing
                         else Just $ GVec $ foldl addVal [head results] $ tail results
  where
    results = wedgeVecPair' (addMissing vec1 combined) (addMissing vec2 combined)
    combined :: [GNum]
    combined = sort $ allBasisVectors vec1 ++ allBasisVectors vec2
    allBasisVectors :: GVec -> [GNum]
    allBasisVectors (GVec vals) = concatMap iOf vals
    -- Add in missing basis vectors to ensure the given vector has a value in each of the given basis vectors.
    addMissing :: GVec -> [GNum] -> GVec
    addMissing (GVec vals) nums = GVec $ emptyIfMissing nums <$> vals
    emptyIfMissing :: [GNum] -> GVal -> GVal
    emptyIfMissing bvecs val@(GVal _ i) = if not (any (\v -> [v] == i) bvecs)
                                         then GVal 0 i
                                         else val
    -- now that we have an equal number of basis vectors, cycle through one list, and generate a pair with the second list when the two basis vectors are not the same.
    wedgeVecPair' :: GVec -> GVec -> [GVal]
    wedgeVecPair' (GVec v1) (GVec v2) = concatMap (crossWedgeDiff v1) $ filterZeroes v2
      where
        crossWedgeDiff :: [GVal] -> GVal -> [GVal]
        crossWedgeDiff vals (GVal r1 i1) = filterZeroes $ ( \(GVal r2 i2) -> sortBasis $ GVal (r1*r2) (i2++i1) ) <$> filter (\(GVal _ i2) -> i2 /= i1) vals
        filterZeroes = filter (\v -> rOf v /= 0)
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
sortBasis' (_,x:xs) = if lowerBasis == (0,[]) then (sumFlips higherBasis, x:newBasis higherBasis)  else (sumFlips lowerBasis + 1 + sumFlips higherBasis, newBasis lowerBasis ++ [x] ++ newBasis higherBasis)
  where
    lowerBasis  = sortBasis' (0,[ a | a <- xs, a<=x ])
--    equalBasis  = sortBasis' (0,[ a | a <- xs, a=x ])
    higherBasis = sortBasis' (0,[ a | a <- xs, a>x ])
    sumFlips :: (Fastℕ, [GNum]) -> Fastℕ
    sumFlips flips = fst flips
    newBasis flips = snd flips

-- the dot product is the inner product in geometric algebra terms.
innerProduct :: GVec -> GVec -> Maybe GVec
innerProduct = dotVecPair

-- the outer product always generates a (bi)vector, where the basis vector order is derived from the Ord of GNum of the basis vectors.
outerProduct :: GVec -> GVec -> Maybe GVec
outerProduct = wedgeVecPair

-- | A wedge operator. not as smart as wedgeVecPair, does not return a Maybe.
(∧) :: GVec -> GVec -> GVec
(∧) vec1 vec2 = fromMaybe (GVec []) $ wedgeVecPair vec1 vec2

-- | A dot operator.
(⋅) :: GVec -> GVec -> GVec
(⋅) vec1 vec2 = fromMaybe (GVec []) $ dotVecPair vec1 vec2

scalarIze :: GVec -> (ℝ, GVec)
scalarIze (GVec gVals) = (scalarPart (stripPairs <$> gVals), GVec $ vectorPart (stripPairs <$> gVals))
  where
    scalarPart vals = sum $ realValue <$> vals
    realValue (GVal v [G0]) = v
    realValue _ = 0
    vectorPart vals = filter (noRealValue) vals
    noRealValue (GVal v [G0]) = False
    noRealValue _ = True

-- | in many situations, we can end up with vectors that have multiple occurances of tthe same basis vector. strip these out, negating as appropriate.
stripPairs :: GVal -> GVal
stripPairs gVal = withoutPairs gVal
  where
    withoutPairs :: GVal -> GVal
    withoutPairs val@(GVal v ((GEPlus a):(GEPlus b):xs))
      | a == b && (not $ null xs) = withoutPairs $ GVal v xs
      | a == b && null xs         = GVal v [G0]
      | a /= b && (not $ null xs) = prependI (GEPlus a) $ withoutPairs $ GVal v (GEPlus b:xs)
      | a /= b && null xs         = val
    withoutPairs val@(GVal v ((GEMinus a):(GEMinus b):xs))
      | a == b && (not $ null xs) = withoutPairs $ GVal (-v) xs
      | a == b && null xs         = GVal (-v) [G0]
      | a /= b && (not $ null xs) = prependI (GEMinus a) $ withoutPairs $ GVal v (GEMinus b:xs)
      | a /= b && null xs         = val
    withoutPairs val@(GVal v ((GEZero a):(GEZero b):xs))
      | a == b                    = GVal 0 [G0]
      | a /= b && (not $ null xs) = prependI (GEZero a) $ withoutPairs $ GVal v (GEMinus b:xs)
      | a /= b && null xs         = val
    prependI :: GNum -> GVal -> GVal
    prependI num (GVal v nums) = GVal v (num:nums)

-- | The geometric product. A real plus a bivector.
data GProduct = GProduct (Maybe GVec) (Maybe GVec)
  deriving (Eq, Generic, NFData, Show, Ord)

-- | Calculate the geometric product of two vectors.
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
toGeometricPoint (Point2 (x,y)) = GPoint2 $ GVec [ GVal x [GEPlus 1], GVal y [GEPlus 2] ]

-- | Create the origin point used when performing projective geometry.
toOriginPoint :: Point2 -> GPoint3
toOriginPoint (Point2 (x,y)) = GPoint3 $ GVec [ GVal x [GEPlus 1], GVal y [GEPlus 2], GVal 1 [GEZero 1]]


-- | Create a 2D projective point from a 2D geometric point, and a 3D geometric origin point.
toProjectivePoint :: GPoint3 -> GPoint2 -> PPoint2
toProjectivePoint p1 p2 = PPoint2 p2 p1

-- | Calculate the Point where two lines meet.
meetLines :: PLine2 -> PLine2 -> PPoint2
meetLines l1@(PLine2 (GPoint2 v1) o1) l2@(PLine2 (GPoint2 v2) o2)
  | o1 == o2 = PPoint2 (GPoint2 $ v1 ∧ v2) o1
  | otherwise = error "normalizing origin points not yet implemented."

-- | Calculate the line on which the two points reside.
--joinPoints :: PPoint2 -> PPoint2 -> PLine2
--joinPoints p1 p2 = dualPoint $ meetLines (dualPoint p1) (dualPoint p2)

-- | Convert from a PLine to it's associated projective point.
dualLine :: PLine2 -> PPoint2
dualLine (PLine2 inPlane origin) = PPoint2 inPlane origin

-- | Convert from a PPoint2 to it's associated projective Line.
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

