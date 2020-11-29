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

{- The purpose of this file is to hold our geometric algebra library. -}

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Graphics.Slicer.Math.GeometricAlgebra(GNum(G0, GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (⎣), (⎤), (⨅), (•), (⋅), (∧), addValPair, subValPair, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, scalarPart, vectorPart, mulVecPair, reduceVecPair, unlikeVecPair) where

import Prelude (Eq, Show, Ord(compare), seq, (==), (/=), (+), otherwise, ($), (++), head, tail, foldl, filter, not, (>), (*), concatMap, (<$>), null, fst, snd, sum, (&&), (/), Bool(True, False), error, flip, (||))

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

-- | Calculate the like product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `likeVecPair` gvec2 == gvec2 `likeVecPair` gvec1.
likeVecPair :: GVec -> GVec -> GVec
likeVecPair a b
  | a > b     = likeVecPair' a b
  | otherwise = likeVecPair' b a

-- | generate the like product of a vector pair.
likeVecPair' :: GVec -> GVec -> GVec
likeVecPair' vec1 vec2 = if null results
                         then GVec []
                         else GVec $ foldl addVal [head results] $ tail results
  where
    results = likeVecPair'' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    likeVecPair'' :: GVec -> GVec -> [GVal]
    likeVecPair'' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [GVal]
        multiplyLike vals val@(GVal _ i1) = mulLikePair val <$> filter (\(GVal _ i2) -> i2 == i1) vals
          where
            mulLikePair (GVal r1 i) (GVal r2 _)
              | i == [G0] = GVal (r1*r2) [G0]
              | otherwise  = sortBasis $ GVal (r1*r2) (i ++ i)

-- | generate the unlike product of a vector pair.
unlikeVecPair :: GVec -> GVec -> GVec
unlikeVecPair vec1 vec2 = if null results
                          then GVec []
                          else GVec $ foldl addVal [head results] $ tail results
  where
    results = unlikeVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list when the two basis vectors are not the same.
    unlikeVecPair' :: GVec -> GVec -> [GVal]
    unlikeVecPair' (GVec v1) (GVec v2) = concatMap (multiplyUnlike v1) v2
      where
        multiplyUnlike :: [GVal] -> GVal -> [GVal]
        multiplyUnlike vals val@(GVal _ i) = mulUnlikePair val <$> filter (\(GVal _ i2) -> i2 /= i) vals
          where
            mulUnlikePair (GVal r1 i1) (GVal r2 i2) = sortBasis $ GVal (r1*r2) (filterG0 i1 ++ filterG0 i2)
              where
                filterG0 xs = filter (/= G0) xs

-- | generate the reductive product of a vector pair.
reduceVecPair :: GVec -> GVec -> GVec
reduceVecPair vec1 vec2 = if null results
                           then GVec []
                           else GVec $ foldl addVal [head results] $ tail results
  where
    results = reduceVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list.
    reduceVecPair' :: GVec -> GVec -> [GVal]
    reduceVecPair' (GVec v1) (GVec v2) = concatMap (multiplyReducing v1) v2
      where
        multiplyReducing :: [GVal] -> GVal -> [GVal]
        multiplyReducing vals val@(GVal _ i) = (flip mulReducingPair) val <$> (filter (\(GVal _ i2) -> i2 `common` i) $ filter (\(GVal _ i2) -> i2 `hasDifferentZeros` i) $ filter (\(GVal _ i2) -> i2 /= i) vals)
          where
            hasDifferentZeros :: [GNum] -> [GNum] -> Bool
            hasDifferentZeros [] _ = error "empty [GNum]"
            hasDifferentZeros (a:[]) nums = containsZero nums a
            hasDifferentZeros nums1 nums2 = null $ filter (\v -> v == False) $ containsZero nums2 <$> filter (isGEZero) nums1
            isGEZero :: GNum -> Bool
            isGEZero (GEZero _) = True
            isGEZero _          = False
            containsZero :: [GNum] -> GNum -> Bool
            containsZero gnums zero = not $ isGEZero zero && (not $ null $ filter (\v -> v == zero) gnums)
            common :: [GNum] -> [GNum] -> Bool
            common a b = contains a b || contains b a
            contains :: [GNum] -> [GNum] -> Bool
            contains []       _    = error "empty [GNum]"
            contains (a:[])   nums = (not $ null $ filter (\v -> v == a) nums)
            contains (a:b:xs) nums = if (not $ null $ filter (\v -> v == a) nums)
                                     then contains (b:xs) nums
                                     else False
            mulReducingPair (GVal r1 i1) (GVal r2 i2) = sortBasis $ GVal (r1*r2) (filterG0 i1 ++ filterG0 i2)
              where
                filterG0 xs = filter (/= G0) xs

-- | generate the geometric product of a vector pair.
mulVecPair :: GVec -> GVec -> GVec
mulVecPair vec1 vec2 = if null results
                         then GVec []
                         else GVec $ foldl addVal [head results] $ tail results
  where
    results = mulVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list.
    mulVecPair' :: GVec -> GVec -> [GVal]
    mulVecPair' (GVec v1) (GVec v2) = concatMap (mulvals v2) v1
      where
        mulvals :: [GVal] -> GVal -> [GVal]
        mulvals vals val = mulValPair val <$> vals
        mulValPair (GVal r1 i1) (GVal r2 i2)
          | i1 == [G0] && i2 == [G0] = GVal (r1*r2) [G0]
          | otherwise                = sortBasis $ GVal (r1*r2) (filterG0 i1 ++ filterG0 i2)
          where
            filterG0 xs = filter (/= G0) xs

-- for a multi-basis value where each basis is wedged against one another, sort the basis vectors remembering to invert the value if necessary.
sortBasis :: GVal -> GVal
sortBasis (GVal r i) = if shouldFlip then GVal (-r) basis else GVal r basis
  where
    (shouldFlip, basis) = sortBasis' i
    -- sort a set of wedged basis vectors. must return an ideal result, along with wehther the associated real value should be flipped or not.
    sortBasis'  :: [GNum] -> (Bool, [GNum])
    sortBasis' thisBasis
      -- If the basis part of calling sortBasis'' once vs calling sortBasis'' twice doesn't change, we are done sorting.
      | basisOf sortOnce == basisOf recurseTwice = sortOnce
      -- If not, recurse.
      | otherwise                                = recurseTwice
      where
        sortOnce = sortBasis'' thisBasis
        recurseTwice = ((flipOf $ sortBasis'' $ basisOf $ sortOnce) /= (flipOf sortOnce), basisOf $ sortBasis'' $ basisOf $ sortOnce)
        basisOf = snd
        flipOf  = fst
        -- sort a set of wedged basis vectors. may not provide an ideal result, but should return a better result, along with whether the associated real value should be flipped or not.
        sortBasis'' :: [GNum] -> (Bool, [GNum])
        sortBasis'' []       = (False,[])
        sortBasis'' [a]      = (False,[a])
        sortBasis'' [a,b]    = if a > b then (True, b:[a]) else (False, a:[b])
        sortBasis'' (a:b:xs) = if a > b
                               then (not $ flipOf $ sortBasis'' (a:xs), b: basisOf (sortBasis'' (a:xs)))
                               else (      flipOf $ sortBasis'' (b:xs), a: basisOf (sortBasis'' (b:xs)))

-- | in many situations, we can end up with vectors that have multiple occurances of the same basis vector. strip these out, negating the real part as appropriate.
stripPairs :: GVal -> GVal
stripPairs = withoutPairs
  where
    withoutPairs :: GVal -> GVal
    withoutPairs val@(GVal _ [])  = val
    withoutPairs val@(GVal _ [_])  = val
    withoutPairs val@(GVal r ((GEPlus a):(GEPlus b):xs))
      | a == b && not (null xs)  = withoutPairs $ GVal r xs
      | a == b && null xs        = GVal r [G0]
      | a /= b && not (null xs)  = prependI (GEPlus a) $ withoutPairs $ GVal r (GEPlus b:xs)
      | a /= b && null xs        = val
    withoutPairs val@(GVal r ((GEMinus a):(GEMinus b):xs))
      | a == b && not (null xs)  = withoutPairs $ GVal (-r) xs
      | a == b && null xs        = GVal (-r) [G0]
      | a /= b && not (null xs)  = prependI (GEMinus a) $ withoutPairs $ GVal r (GEMinus b:xs)
      | a /= b && null xs        = val
    withoutPairs val@(GVal r ((GEZero a):(GEZero b):xs))
      | a == b                   = GVal 0 [G0]
      | a /= b && not (null xs)  = prependI (GEZero a) $ withoutPairs $ GVal r (GEZero b:xs)
      | a /= b && null xs        = val
    withoutPairs (GVal r (a:b:xs)) = prependI a $ withoutPairs $ GVal r (b:xs)
    prependI :: GNum -> GVal -> GVal
    prependI num (GVal r [G0]) = GVal r [num]
    prependI num (GVal r nums) = GVal r (num:nums)

-- | our "like" operator. unicode point u+23a3
(⎣) :: GVec -> GVec -> GVec
(⎣) v1 v2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (likeVecPair v1 v2)

-- | our "unlike" operator. unicode point u+23a4
(⎤) :: GVec -> GVec -> GVec
(⎤) v1 v2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (unlikeVecPair v1 v2)

-- our "reductive" operator.
(⨅) :: GVec -> GVec -> GVec
(⨅) v1 v2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (reduceVecPair v1 v2)

-- | A wedge operator. gets the wedge product of the two arguments
(∧) :: GVec -> GVec -> GVec
(∧) v1 v2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (subVecPair (reduceVecPair v1 v2) (unlikeVecPair v1 v2))

-- | A dot operator. gets the dot product of the two arguments
(⋅) :: GVec -> GVec -> GVec
(⋅) v1 v2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (addVecPair (reduceVecPair v1 v2) (likeVecPair v1 v2))

-- | A geometric product operator. Gets the geometric product of the two arguments.
(•) :: GVec -> GVec -> GVec
(•) vec1 vec2 = GVec $ foldl addVal [] $ stripPairs <$> (\(GVec a) -> a) (mulVecPair vec1 vec2)

-- simplify a GVec, and return any scalar component.
scalarPart :: GVec -> ℝ
scalarPart (GVec gVals) = sum $ realValue <$> vals
  where
    vals = stripPairs <$> gVals
    realValue (GVal r [G0]) = r
    realValue _ = 0

-- simplify a GVec, and return any component that is not a scalar.
vectorPart :: GVec -> GVec
vectorPart (GVec gVals) = GVec $ foldl addVal [] $ filter noRealValue vals
  where
    vals = stripPairs <$> gVals
    noRealValue (GVal _ [G0]) = False
    noRealValue _ = True
