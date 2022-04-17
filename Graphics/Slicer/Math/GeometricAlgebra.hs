{- ORMOLU_DISABLE -}
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

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}

-- | Our geometric algebra library.
module Graphics.Slicer.Math.GeometricAlgebra(GNum(G0, GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (⎣), (⎤+), (⎤), (⨅+), (⨅), (•), (⋅), (∧), addValPair, getVals, subValPair, valOf, addVal, subVal, addVecPair, subVecPair, mulScalarVec, divVecScalar, scalarPart, vectorPart, hpDivVecScalar, reduceVecPair, unlikeVecPair, UlpSum(UlpSum)) where

import Prelude (Eq, Show(show), Ord(compare), (==), (/=), (+), (<>), fst, otherwise, snd, ($), not, (>), (*), concatMap, (<$>), sum, (&&), (/), Bool(True, False), error, flip, (&&), null, realToFrac, abs, (.))

import Prelude as P (filter)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.NonEmpty (NonEmpty((:|)), toList, cons, nonEmpty)

import Data.List.Ordered (sort, insertSet)

import Data.Maybe (Maybe(Just, Nothing))

import Data.Number.BigFloat (BigFloat, PrecPlus20, Eps1)

import Data.Set (Set, singleton, disjoint, elems, size, elemAt, fromAscList)

import Data.Set as S (filter)

import Graphics.Slicer.Definitions (ℝ, Fastℕ)

import Graphics.Slicer.Orphans ()

-- | The geometric numbers.
-- We must derive Ord so we can sort the terms during simplification.
data GNum =
    GEMinus !Fastℕ -- squared equal to -1 -- associated with rotation
  | GEZero  !Fastℕ -- squared equal to  0 -- associated with translations
  | GEPlus  !Fastℕ -- squared equal to +1 -- associated with space/time or hyperbolic rotations
  | G0            -- A scalar type. short lived.
  deriving (Eq, Generic, NFData, Show, Ord)

-- | A value in geometric algebra.
data GVal = GVal { _real :: !ℝ, _basis :: !(Set GNum) }
  deriving (Eq, Generic, NFData, Show)

-- | A value in geometric algebra, in need of reduction. this may have duplicat members, or members out of order.
data GRVal = GRVal { _r :: !ℝ, _i :: !(NonEmpty GNum) }
  deriving (Eq, Generic, NFData, Show)

-- | A constantly increasing sum of error. Used for increasing our error bars proportonally to error from the FPU.
newtype UlpSum = UlpSum ℝ
  deriving (Show, Eq)

-- When sorting gvals, sort the basis, THEN sort the multiplier.
instance Ord GVal where
  (GVal r1 i1) `compare` (GVal r2 i2)
    | i1 == i2  = compare r1 r2
    | otherwise = compare i1 i2

-- | A (multi)vector in geometric algebra.
newtype GVec = GVec [GVal]
  deriving (Eq, Generic, NFData, Show, Ord)

-- | Extract a value from a vector.
getVals :: [GNum] -> [GVal] -> Maybe GVal
getVals nums vs = case matches of
                    [] -> Nothing
                    [oneMatch] -> Just oneMatch
                    multiMatch@(_:_) -> error $ "found multiple candidates" <> show multiMatch <> " when using getVals on " <> show vs <> "when searching for " <> show nums <> "\n"
  where
    matches = P.filter (\(GVal _ n) -> n == fromAscList nums) vs

-- | Return the value of a vector, OR a given value, if the vector requested is not found.
valOf :: ℝ -> Maybe GVal -> ℝ
valOf r Nothing = r
valOf _ (Just (GVal v _)) = v

-- | Add two geometric values together.
addValPair :: GVal -> GVal -> [GVal]
addValPair v1@(GVal r1 i1) v2@(GVal r2 i2)
  | r1 == 0 && r2 == 0      = []
  | r1 == 0                 = [v2]
  | r2 == 0                 = [v1]
  | i1 == i2 && r1 == (-r2) = []
  | i1 == i2                = [GVal (r1+r2) i1]
  | otherwise               = sort [v1,v2]

-- | Subtract a geometric value from another geometric value.
subValPair :: GVal -> GVal -> [GVal]
subValPair v1@(GVal r1 i1) (GVal r2 i2)
  | i1 == i2 && r1 == r2 = []
  | otherwise            = addValPair v1 $ GVal (-r2) i2

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addVal :: [GVal] -> GVal -> [GVal]
addVal dst src@(GVal r1 _)
  | r1 == 0 = dst
  | null dst = [src]
  | otherwise = case sameBasis src dst of
                  [] -> insertSet src dst
                  (_:_) -> if sum (rOf <$> sameBasis src dst) == (-r1)
                           then diffBasis src dst
                           else insertSet (GVal (r1 + sum (rOf <$> sameBasis src dst)) $ iOf src) $ diffBasis src dst
  where
    sameBasis :: GVal -> [GVal] -> [GVal]
    sameBasis val = P.filter (\(GVal _ i) -> i == iOf val)
    diffBasis :: GVal -> [GVal] -> [GVal]
    diffBasis val = P.filter (\(GVal _ i) -> i /= iOf val)
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- | Subtract a geometric value from a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
subVal :: [GVal] -> GVal -> [GVal]
subVal dst (GVal r i) = addVal dst $ GVal (-r) i

-- | Add two vectors together.
addVecPair :: GVec -> GVec -> GVec
addVecPair (GVec vals1) (GVec vals2) = GVec $ foldl' addVal vals1 vals2

-- | Subtract one vector from the other.
subVecPair :: GVec -> GVec -> GVec
subVecPair (GVec vals1) (GVec vals2) = GVec $ foldl' subVal vals1 vals2

-- | Multiply a vector by a scalar. arguments are given in this order for maximum readability.
mulScalarVec :: ℝ -> GVec -> GVec
mulScalarVec s (GVec vals) = GVec $ mulVal s <$> vals
  where
    mulVal s1 (GVal r i) = GVal (s1*r) i

-- | Divide a vector by a scalar. arguments are given in this order for maximum readability.
divVecScalar :: GVec -> ℝ -> GVec
divVecScalar (GVec vals) s = GVec $ divVal s <$> vals
  where
    divVal s1 (GVal r i) = GVal (r/s1) i

-- | Divide a vector by a scalar, high precision (read: slow) version. arguments are given in this order for maximum readability.
hpDivVecScalar :: GVec -> (BigFloat (PrecPlus20 Eps1)) -> GVec
hpDivVecScalar (GVec vals) s = GVec $ divVal s <$> vals
  where
    divVal s1 (GVal r i) = GVal (realToFrac r `hpdiv` realToFrac s1) i
    hpdiv :: (BigFloat (PrecPlus20 Eps1)) -> (BigFloat (PrecPlus20 Eps1)) -> ℝ
    hpdiv a b = realToFrac $ a / b

-- | Calculate the like product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `likeVecPair` gvec2 == gvec2 `likeVecPair` gvec1.
likeVecPair :: GVec -> GVec -> [Either GRVal GVal]
likeVecPair a b
  | a > b     = likeVecPair' a b
  | otherwise = likeVecPair' b a

-- | Generate the like product of a vector pair. multiply only the values in the basis vector sets that are common between the two GVecs.
likeVecPair' :: GVec -> GVec -> [Either GRVal GVal]
likeVecPair' vec1 vec2 = results
  where
    results = likeVecPair'' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    likeVecPair'' :: GVec -> GVec -> [Either GRVal GVal]
    likeVecPair'' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [Either GRVal GVal]
        multiplyLike vals val@(GVal _ i1) = mulLikePair val <$> P.filter (\(GVal _ i2) -> i2 == i1) vals
          where
            mulLikePair (GVal r1 i) (GVal r2 _)
              | size i == 1 = simplifyVal (r1 * r2) (elemAt 0 i)
              | otherwise = case nonEmpty (elems i) of
                              (Just newi) -> Left $ GRVal (r1 * r2) (newi <> newi)
                              Nothing -> error "empty set?"
              where
                simplifyVal v G0 = Right $ GVal v (singleton G0)
                simplifyVal v (GEPlus _) = Right $ GVal v (singleton G0)
                simplifyVal v (GEMinus _) = Right $ GVal (-v) (singleton G0)
                simplifyVal _ (GEZero _) = Right $ GVal 0 (singleton G0)

-- | Generate the unlike product of a vector pair. multiply only the values in the basis vector sets that are not the same between the two GVecs.
unlikeVecPair :: GVec -> GVec -> [Either GRVal GVal]
unlikeVecPair vec1 vec2 = fst <$> unlikeVecPairWithErr vec1 vec2

unlikeVecPairWithErr :: GVec -> GVec -> [(Either GRVal GVal, UlpSum)]
unlikeVecPairWithErr vec1 vec2 = results
  where
    results = unlikeVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list when the two basis vectors are not the same.
    unlikeVecPair' :: GVec -> GVec -> [(Either GRVal GVal, UlpSum)]
    unlikeVecPair' (GVec v1) (GVec v2) = concatMap (multiplyUnlike v1) v2
      where
        multiplyUnlike :: [GVal] -> GVal -> [(Either GRVal GVal, UlpSum)]
        multiplyUnlike vals val@(GVal _ i) = mulUnlikePair val <$> P.filter (\(GVal _ i2) -> i2 /= i) vals
          where
            mulUnlikePair (GVal r1 i1) (GVal r2 i2)
              | i1 == singleton G0 = (Right $ GVal res i2, UlpSum $ abs $ doubleUlp res)
              | i2 == singleton G0 = (Right $ GVal res i1, UlpSum $ abs $ doubleUlp res)
              | otherwise = case nonEmpty (elems i1) of
                              Nothing -> error "empty set?"
                              (Just newI1) -> case nonEmpty (elems i2) of
                                                Nothing -> error "empty set?"
                                                (Just newI2) -> (Left $ GRVal (r1 * r2) (newI1 <> newI2), UlpSum $ abs $ doubleUlp res)
              where
                res = r1*r2

-- | Generate the reductive product of a vector pair. multiply only values where one of the basis vectors is eliminated by the multiplication.
reduceVecPair :: GVec -> GVec -> [GRVal]
reduceVecPair vec1 vec2 = fst <$> reduceVecPairWithErr vec1 vec2

-- | Generate the reductive product of a vector pair. multiply only values where one of the basis vectors is eliminated by the multiplication.
reduceVecPairWithErr :: GVec -> GVec -> [(GRVal, UlpSum)]
reduceVecPairWithErr vec1 vec2 = results
  where
    results = reduceVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list. multiplies only the values where one set has some common vectors with the other set, but they do not have identical sets.
    reduceVecPair' :: GVec -> GVec -> [(GRVal, UlpSum)]
    reduceVecPair' (GVec v1) (GVec v2) = concatMap (multiplyReducing v1) v2
      where
        multiplyReducing :: [GVal] -> GVal -> [(GRVal, UlpSum)]
        multiplyReducing vals val@(GVal _ i) = flip mulReducingPair val <$> P.filter (\(GVal _ i2) -> i2 `common` i) (P.filter (\(GVal _ i2) -> i2 `hasDifferentZeros` i) $ P.filter (\(GVal _ i2) -> i2 /= i) vals)
          where
            hasDifferentZeros :: Set GNum -> Set GNum -> Bool
            hasDifferentZeros nums1 nums2 = disjoint (S.filter isGEZero nums1) (S.filter isGEZero nums2)
            isGEZero :: GNum -> Bool
            isGEZero (GEZero _) = True
            isGEZero _          = False
            common :: Set GNum -> Set GNum -> Bool
            common a b = not $ disjoint a b
            mulReducingPair (GVal r1 i1) (GVal r2 i2) = case nonEmpty (elems i1) of
                                                          Nothing -> error "empty set?"
                                                          (Just newI1) -> case nonEmpty (elems i2) of
                                                                            Nothing -> error "empty set?"
                                                                            (Just newI2) -> (GRVal (res) (newI1 <> newI2), UlpSum $ abs $ doubleUlp res)
                                                                              where
                                                                                res = r1*r2

-- | Generate the geometric product of a vector pair.
mulVecPair :: GVec -> GVec -> [Either GRVal GVal]
mulVecPair vec1 vec2 = results
  where
    results = mulVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list.
    mulVecPair' :: GVec -> GVec -> [Either GRVal GVal]
    mulVecPair' (GVec v1) (GVec v2) = concatMap (mulvals v2) v1
      where
        mulvals :: [GVal] -> GVal -> [Either GRVal GVal]
        mulvals vals val = mulValPair val <$> vals
        mulValPair (GVal r1 i1) (GVal r2 i2)
          | i1 == i2 && size i1 == 1 = simplifyVal (r1*r2) (elemAt 0 i1)
          | i1 == singleton G0       = Right $ GVal (r1*r2) i2
          | i2 == singleton G0       = Right $ GVal (r1*r2) i1
          | otherwise = case nonEmpty (elems i1) of
                          Nothing -> error "empty set?"
                          (Just newI1) -> case nonEmpty (elems i2) of
                                            Nothing -> error "empty set?"
                                            (Just newI2) -> Left $ GRVal (r1 * r2) (newI1 <> newI2)
          where
            simplifyVal v G0 = Right $ GVal v (singleton G0)
            simplifyVal v (GEPlus _) = Right $ GVal v (singleton G0)
            simplifyVal v (GEMinus _) = Right $ GVal (-v) (singleton G0)
            simplifyVal _ (GEZero _) = Right $ GVal 0 (singleton G0)

-- | For a multi-basis value where each basis is wedged against one another, sort the basis vectors remembering to invert the value if necessary.
sortBasis :: GRVal -> GRVal
sortBasis (GRVal r i) = if shouldFlip then GRVal (-r) basis else GRVal r basis
  where
    (shouldFlip, basis) = sortBasis' i
    -- sort a set of wedged basis vectors. must return an ideal result, along with wehther the associated real value should be flipped or not.
    sortBasis'  :: NonEmpty GNum -> (Bool, NonEmpty GNum)
    sortBasis' thisBasis
      -- If the basis part of calling sortBasis'' once vs calling sortBasis'' twice doesn't change, we are done sorting.
      | basisOf sortOnce == basisOf recurseTwice = sortOnce
      -- If not, recurse.
      | otherwise                                = recurseTwice
      where
        sortOnce = sortBasis'' thisBasis
        recurseTwice :: (Bool, NonEmpty GNum)
        recurseTwice = (flipOf (sortBasis'' $ basisOf sortOnce) /= flipOf sortOnce, basisOf $ sortBasis'' $ basisOf sortOnce)
        basisOf = snd
        flipOf  = fst
        -- sort a set of wedged basis vectors. may not provide an ideal result, but should return a better result, along with whether the associated real value should be flipped or not.
        sortBasis'' :: NonEmpty GNum -> (Bool, NonEmpty GNum)
        sortBasis'' (a:|[])     = (False,a:|[])
        sortBasis'' (a:|[b])    = if a > b then (True, b:|[a]) else (False, a:|[b])
        sortBasis'' (a:|(b:xs)) = if a > b
                               then (not $ flipOf $ sortBasis'' (a:|xs), b `cons` basisOf (sortBasis'' (a:|xs)))
                               else (      flipOf $ sortBasis'' (b:|xs), a `cons` basisOf (sortBasis'' (b:|xs)))

-- | for a multi-basis value with each basis wedged against one another, where they are in ascending order, we can end up with vectors that have multiple occurances of the same basis vector. strip these out, negating the real part as appropriate.
stripPairs :: GRVal -> GRVal
stripPairs = withoutPairs
  where
    withoutPairs :: GRVal -> GRVal
    withoutPairs (GRVal r (oneI:|[]))  = GRVal r (oneI:|[])
    withoutPairs (GRVal r is@((GEPlus a):|(GEPlus b):xs))
      | a == b = case nonEmpty xs of
                   Nothing -> GRVal r (G0:|[])
                   (Just vals) -> withoutPairs $ GRVal r vals
      | a /= b = case nonEmpty xs of
                   Nothing -> GRVal r is
                   (Just _) -> prependI (GEPlus a) $ withoutPairs $ GRVal r (GEPlus b:|xs)
    withoutPairs (GRVal r is@((GEMinus a):|(GEMinus b):xs))
      | a == b = case nonEmpty xs of
                   Nothing -> GRVal (-r) (G0:|[])
                   (Just vals) -> withoutPairs $ GRVal (-r) vals
      | a /= b = case nonEmpty xs of
                   Nothing -> GRVal r is
                   (Just _) -> prependI (GEMinus a) $ withoutPairs $ GRVal r (GEMinus b:|xs)
    withoutPairs (GRVal r is@((GEZero a):|(GEZero b):xs))
      | a == b = GRVal 0 (G0:|[])
      | a /= b = case nonEmpty xs of
                   Nothing -> GRVal r is
                   (Just _) -> prependI (GEZero a) $ withoutPairs $ GRVal r (GEZero b:|xs)
    withoutPairs (GRVal r (a:|b:xs)) = prependI a $ withoutPairs $ GRVal r (b:|xs)
    prependI :: GNum -> GRVal -> GRVal
    prependI num (GRVal r nums) = if nums == (G0:|[])
                                  then GRVal r (num:|[])
                                  else GRVal r (num `cons` nums)

-- | a post processor, to clean up a GRVal into a GVal.
postProcess :: GRVal -> GVal
postProcess val = grValToGVal $ stripPairs $ sortBasis val

-- | a post processor, to clean up a GRVal into a GVal. may be given a GVal, in which case it short circuits.
postProcessFilter :: Either GRVal GVal -> GVal
postProcessFilter (Right gval) = gval
postProcessFilter (Left grval) = grValToGVal $ stripPairs $ sortBasis grval

-- Convert a GRval to a GVal. only to be used in postProcess and postProcessFilter.
grValToGVal :: GRVal -> GVal
grValToGVal (GRVal r i) = GVal r (fromAscList (toList i))

-- | Our "like" operator. unicode point u+23a3.
(⎣) :: GVec -> GVec -> GVec
infixl 9 ⎣
(⎣) v1 v2 = GVec $ postProcessFilter <$> likeVecPair v1 v2

-- | Our "unlike" operator. unicode point u+23a4.
(⎤) :: GVec -> GVec -> GVec
infixl 9 ⎤
(⎤) v1 v2 = GVec $ foldl' addVal [] $ postProcessFilter <$> unlikeVecPair v1 v2

-- | Our "unlike" operator. unicode point u+23a4.
(⎤+) :: GVec -> GVec -> (GVec, UlpSum)
infixl 9 ⎤+
(⎤+) v1 v2 = (GVec $ foldl' addVal [] $ postProcessFilter . fst <$> res
             , ulpTotal)
  where
    res = unlikeVecPairWithErr v1 v2
    ulpTotal = foldl' (\(UlpSum a) (UlpSum b) -> UlpSum $ a + b) (UlpSum 0) (snd <$> res)

-- | Our "reductive" operator.
(⨅) :: GVec -> GVec -> GVec
infixl 9 ⨅
(⨅) v1 v2 = fst $ v1 ⨅+ v2

-- | Our "reductive" operator, with attached Error.
(⨅+) :: GVec -> GVec -> (GVec, UlpSum)
infixl 9 ⨅+
(⨅+) v1 v2 = (GVec $ foldl' addVal [] $ postProcess . fst <$> res
             , ulpTotal)
  where
    res = reduceVecPairWithErr v1 v2
    ulpTotal = foldl' (\(UlpSum a) (UlpSum b) -> UlpSum $ a + b) (UlpSum 0) (snd <$> res)

-- | A wedge operator. gets the wedge product of the two arguments. note that wedge = reductive minus unlike.
(∧) :: GVec -> GVec -> GVec
infixl 9 ∧
(∧) v1 v2 = GVec $ foldl' addVal [] $ (\(GVec a) -> a) (subVecPair (GVec $ postProcess <$> reduceVecPair v1 v2) (GVec $ postProcessFilter <$> unlikeVecPair v1 v2))

-- | A dot operator. gets the dot product of the two arguments. note that dot = reductive plus like.
(⋅) :: GVec -> GVec -> GVec
infixl 9 ⋅
(⋅) v1 v2 = GVec $ foldl' addVal (postProcessFilter <$> likeVecPair v1 v2) (postProcess <$> reduceVecPair v1 v2)

-- | A geometric product operator. Gets the geometric product of the two arguments.
(•) :: GVec -> GVec -> GVec
infixl 9 •
(•) vec1 vec2 = GVec $ foldl' addVal [] $ postProcessFilter <$> mulVecPair vec1 vec2

-- | Simplify a GVec, and return any scalar component.
scalarPart :: GVec -> ℝ
scalarPart (GVec gVals) = sum $ realValue <$> vals
  where
    vals = gVals
    realValue (GVal r gnums) = if gnums == singleton G0 then r else 0

-- | Simplify a GVec, and return any component that is not a scalar.
vectorPart :: GVec -> GVec
vectorPart (GVec gVals) = GVec $ foldl' addVal [] $ P.filter noRealValue vals
  where
    vals = gVals
    noRealValue (GVal _ gnums) = gnums /= singleton G0
