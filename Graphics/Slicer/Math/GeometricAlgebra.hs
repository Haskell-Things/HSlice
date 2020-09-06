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

{- The purpose of this file is to hold out geometric algebra library. -}

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.GeometricAlgebra(GNum(G0, GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (∧), (⋅), (•), addValPair, subValPair, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, innerProduct, outerProduct, scalarIze) where

import Prelude (Eq, Show, Ord(compare), seq, (==), (/=), (+), otherwise, ($), map, (++), head, tail, foldl, filter, not, (>), (*), concatMap, (<$>), null, odd, (<=), fst, snd, sum, (&&), (/), Bool(True, False))

import GHC.Generics (Generic)

import Control.DeepSeq (NFData(rnf))

import Data.List.Ordered(sort, insertSet)

import Graphics.Slicer.Definitions (ℝ, Fastℕ)

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
  | r1 == 0 && r2 == 0      = []
  | r1 == 0                 = [v2]
  | r2 == 0                 = [v1]
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
  | r1 == 0 = dst
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
    divVal s1 (GVal r i) = GVal (r/s1) i

-- | Calculate the dot product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `dotVecPair` gvec2 == gvec2 `dotVecPair` gvec1.
dotVecPair :: GVec -> GVec -> GVec
dotVecPair a b
  | a > b     = dotVecPair' a b
  | otherwise = dotVecPair' b a

-- generate the dot product of a vector pair.
dotVecPair' :: GVec -> GVec -> GVec
dotVecPair' vec1 vec2 = if null results
                         then GVec []
                         else GVec $ foldl addVal [head results] $ tail results
  where
    results = dotVecPair'' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    dotVecPair'' :: GVec -> GVec -> [GVal]
    dotVecPair'' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [GVal]
        multiplyLike vals (GVal r1 i1) = invert $ filterZeroes $ ( \(GVal r2 i2) -> sortBasis $ GVal (r1*r2) (i2++i1) ) <$> filter (\(GVal _ i2) -> i2 == i1) vals
        filterZeroes = filter (\v -> rOf v /= 0)
        invert xs = map (\(GVal r i) -> GVal (-r) i) xs 
    rOf (GVal r _) = r
-- generate the wedge product of a vector pair.
wedgeVecPair :: GVec -> GVec -> GVec
wedgeVecPair vec1 vec2 = if null results
                         then GVec []
                         else GVec $ foldl addVal [head results] $ tail results
  where
    results = wedgeVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list when the two basis vectors are not the same.
    wedgeVecPair' :: GVec -> GVec -> [GVal]
    wedgeVecPair' (GVec v1) (GVec v2) = concatMap (crossWedgeDiff v1) $ filterZeroes v2
      where
        crossWedgeDiff :: [GVal] -> GVal -> [GVal]
        crossWedgeDiff vals (GVal r1 i1) = filterZeroes $ ( \(GVal r2 i2) -> sortBasis $ GVal (r1*r2) (i2++i1) ) <$> filter (\(GVal _ i2) -> i2 /= i1) vals
        filterZeroes = filter (\v -> rOf v /= 0)
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

-- | in many situations, we can end up with vectors that have multiple occurances of the same basis vector. strip these out, negating the real part as appropriate.
stripPairs :: GVal -> GVal
stripPairs gVal = withoutPairs gVal
  where
    withoutPairs :: GVal -> GVal
    withoutPairs val@(GVal _ [])  = val
    withoutPairs val@(GVal _ [_])  = val
    withoutPairs val@(GVal r ((GEPlus a):(GEPlus b):xs))
      | a == b && (not $ null xs)  = withoutPairs $ GVal r xs
      | a == b && null xs          = GVal r [G0]
      | a /= b && (not $ null xs)  = prependI (GEPlus a) $ withoutPairs $ GVal r (GEPlus b:xs)
      | a /= b && null xs          = val
    withoutPairs val@(GVal r ((GEMinus a):(GEMinus b):xs))
      | a == b && (not $ null xs)  = withoutPairs $ GVal (-r) xs
      | a == b && null xs          = GVal (-r) [G0]
      | a /= b && (not $ null xs)  = prependI (GEMinus a) $ withoutPairs $ GVal r (GEMinus b:xs)
      | a /= b && null xs          = val
    withoutPairs val@(GVal r ((GEZero a):(GEZero b):xs))
      | a == b                     = GVal 0 [G0]
      | a /= b && (not $ null xs)  = prependI (GEZero a) $ withoutPairs $ GVal r (GEZero b:xs)
      | a /= b && null xs          = val
    withoutPairs (GVal r (a:b:xs)) = prependI (a) $ withoutPairs $ GVal r (b:xs)
    prependI :: GNum -> GVal -> GVal
    prependI num (GVal r nums) = GVal r (num:nums)

-- the dot product is the inner product in geometric algebra terms.
innerProduct :: GVec -> GVec -> GVec
innerProduct = dotVecPair

-- the outer product always generates a (bi)vector, where the basis vector order is derived from the Ord of GNum of the basis vectors.
outerProduct :: GVec -> GVec -> GVec
outerProduct = wedgeVecPair

{-
-- | Calculate the geometric product of two vectors.
geometricProduct :: GVec -> GVec -> GVec
geometricProduct v1 v2 = addVecPair (innerProduct v1 v2) (outerProduct v1 v2)
-}

-- | A wedge operator. 
(∧) :: GVec -> GVec -> GVec
(∧) vec1 vec2 = wedgeVecPair vec1 vec2

-- | A dot operator. gets the dot product of the two arguments
(⋅) :: GVec -> GVec -> GVec
(⋅) vec1 vec2 = dotVecPair vec1 vec2

-- | A big dot operator. Gets the geometric product of the two arguments.
(•) :: GVec -> GVec -> GVec
(•) vec1 vec2 = addVecPair (vec1 ⋅ vec2) (vec1 ∧ vec2)

-- | attempt to extract a scalar value from the vector form of a dot product.
scalarIze :: GVec -> (ℝ, GVec)
scalarIze (GVec gVals) = (scalarPart (stripPairs <$> gVals), GVec $ vectorPart (stripPairs <$> gVals))
  where
    scalarPart vals = sum $ realValue <$> vals
    realValue (GVal r [G0]) = r
    realValue _ = 0
    vectorPart vals = filter (noRealValue) vals
    noRealValue (GVal _ [G0]) = False
    noRealValue _ = True

