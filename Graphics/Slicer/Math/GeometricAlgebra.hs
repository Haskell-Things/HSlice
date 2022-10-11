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

{-# LANGUAGE DataKinds #-}

-- | Our geometric algebra library.
module Graphics.Slicer.Math.GeometricAlgebra(ErrVal(ErrVal), GNum(G0, GEMinus, GEPlus, GEZero), GVal(GVal), GVec(GVec), (⎣+), (⎣), (⎤+), (⎤), (⨅+), (⨅), (•), (⋅), (∧), addValPairWithErr, eValOf, getVal, subValPair, ulpVal, valOf, addVal, subVal, addVecPair, addVecPairWithErr, subVecPair, mulScalarVecWithErr, divVecScalarWithErr, scalarPart, vectorPart, hpDivVecScalar, reduceVecPair, unlikeVecPair, UlpSum(UlpSum)) where

import Prelude (Eq, Monoid(mempty), Ord(compare), Semigroup((<>)), Show(show), (==), (/=), (+), fst, otherwise, snd, ($), not, (>), (*), concatMap, (<$>), sum, (&&), (/), Bool(True, False), error, flip, (&&), null, realToFrac, abs, (.), realToFrac)

import Prelude as P (filter)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl')

import Data.List.NonEmpty (NonEmpty((:|)), toList, cons, nonEmpty)

import Data.List.Ordered (sort, insertSet)

import Data.Maybe (Maybe(Just, Nothing), isJust)

import Data.Number.BigFloat (BigFloat, PrecPlus20, Eps1)

import Data.Set (Set, singleton, disjoint, elems, size, elemAt, fromAscList)

import Data.Set as S (filter)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, ToNearest))

import Safe (headMay)

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

-- | A value in geometric algebra. this will have duplicate members filtered out, and the members will be in order.
data GVal = GVal
  -- _real ::
  !ℝ
  -- _basis ::
  !(Set GNum)
  deriving (Eq, Generic, NFData, Show)

-- | A value in geometric algebra, in need of reduction. this may have duplicate members, or members out of order.
data GRVal = GRVal
  -- real component
  !ℝ
  -- basis vector
  !(NonEmpty GNum)
  deriving (Eq, Generic, NFData, Show)

-- | A constantly increasing sum of error. Used for increasing our error bars proportonally to error collected from the FPU during calculations.
newtype UlpSum = UlpSum { ulpVal :: Rounded 'TowardInf ℝ }
  deriving (Show, Eq, Generic, NFData, Ord)

instance Semigroup UlpSum where
  (<>) (UlpSum sum1) (UlpSum sum2) = UlpSum $ sum1 + sum2

instance Monoid UlpSum where
  mempty = UlpSum 0

data ErrVal = ErrVal
  -- { _ulpVal ::
              !UlpSum
  -- _ulpBasis ::
              !(Set GNum)
  deriving (Eq, Generic, NFData, Show)

data ErrRVal = ErrRVal { _ulpRVal :: !UlpSum, _ulpRBasis :: NonEmpty GNum }
  deriving (Eq, Generic, NFData, Show)

-- Fake instance. do not try to order by ErrVal.
instance Ord ErrVal where
  compare (ErrVal _ a) (ErrVal _ b) = compare a b

instance Semigroup ErrVal where
  (<>) e1@(ErrVal r1 i1) e2@(ErrVal r2 i2)
   | e1 == mempty = e2
   | e2 == mempty = e1
   | i1 == i2 = ErrVal (r1 <> r2) i1
   | otherwise = error $ "tried to <> two ErrVals with different basises.\n" <> show i1 <> "\n" <> show i2 <> "\n"

instance Monoid ErrVal where
  mempty = ErrVal mempty mempty

-- When sorting gvals, sort the basis, THEN sort the multiplier.
instance Ord GVal where
  (GVal r1 i1) `compare` (GVal r2 i2)
    | i1 == i2  = compare r1 r2
    | otherwise = compare i1 i2

-- | A (multi)vector in geometric algebra.
newtype GVec = GVec [GVal]
  deriving (Eq, Generic, NFData, Show, Ord)

-- | a list contains geometric values that can be queried.
class UniqueVals a where
  getVal :: [GNum] -> [a] -> Maybe a

instance UniqueVals GVal where
  -- | Extract a value from a list of values.
  getVal nums vs = case matches of
                      [] -> Nothing
                      [oneMatch@(GVal v _)] -> if v == 0 then Nothing else Just oneMatch
                      multiMatch@(_:_) -> error $ "found multiple candidates:\n" <> show multiMatch <> "\nWas using getVals on:\n" <> show vs <> "\nWas searching for:\n" <> show nums <> "\n"
    where
      matches = P.filter (\(GVal _ n) -> n == fromAscList nums) vs

instance UniqueVals ErrVal where
  -- | Extract a value from a list of values.
  getVal nums vs = case matches of
                      [] -> Nothing
                      [oneMatch@(ErrVal v _)] -> if v == mempty then Nothing else Just oneMatch
                      multiMatch@(_:_) -> error $ "found multiple candidates:\n" <> show multiMatch <> "\nWas using getVals on:\n" <> show vs <> "\nWas searching for:\n" <> show nums <> "\n"
    where
      matches = P.filter (\(ErrVal _ n) -> n == fromAscList nums) vs

-- | Return the value of a (vector, or bivector, or trivector, or...), OR a given value, if the vector requested is not found.
valOf :: ℝ -> Maybe GVal -> ℝ
valOf r Nothing = r
valOf _ (Just (GVal v _)) = v

-- | Return the error component saved from a calculation that produced a vector, OR return a given value, if the error component requested is not found.
eValOf :: UlpSum -> Maybe ErrVal -> UlpSum
eValOf r Nothing = r
eValOf _ (Just (ErrVal v _)) = v

-- | Add two geometric values together.
addValPairWithErr :: GVal -> GVal -> ([GVal], UlpSum)
addValPairWithErr v1@(GVal r1 i1) v2@(GVal r2 i2)
  | r1 == 0 && r2 == 0      = ([],mempty)
  | r1 == 0                 = ([v2],mempty)
  | r2 == 0                 = ([v1],mempty)
  | i1 == i2 && r1 == (-r2) = ([],mempty)
  | i1 == i2                = ([GVal res i1]
                              , UlpSum $ abs $ realToFrac $ doubleUlp res)
  | otherwise               = (sort [v1,v2],mempty)
  where
    res :: ℝ
    res = realToFrac (realToFrac r1 + realToFrac r2 :: Rounded 'ToNearest ℝ)

-- | Subtract a geometric value from another geometric value.
-- FIXME: error component?
subValPair :: GVal -> GVal -> [GVal]
subValPair v1@(GVal r1 i1) (GVal r2 i2)
  | i1 == i2 && r1 == r2 = []
  | otherwise            = fst $ addValPairWithErr v1 $ GVal (-r2) i2

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addVal :: [GVal] -> GVal -> [GVal]
addVal dst src = fst $ addValWithErr (dst, mempty) src

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addValWithErr :: ([GVal], UlpSum) -> GVal -> ([GVal], UlpSum)
addValWithErr dst@(dstVals, dstUlp@(UlpSum dstErr)) src@(GVal r1 _)
  | r1 == 0 = dst
  | null dstVals = ([src], mempty)
  | otherwise = case sameBasis src dstVals of
                  Nothing  -> (insertSet src dstVals, dstUlp)
                  (Just a) -> if rOf a == (-r1)
                              then (diffBasis src dstVals, dstUlp)
                              else (insertSet (GVal newVal $ iOf src) (diffBasis src dstVals)
                                ,UlpSum $ dstErr + abs ( realToFrac $ doubleUlp newVal))
                    where
                      newVal :: ℝ
                      newVal = realToFrac (realToFrac (rOf a) + realToFrac r1 :: Rounded 'ToNearest ℝ)
  where
    sameBasis :: GVal -> [GVal] -> Maybe GVal
    sameBasis val srcVals = headMay $ P.filter (\(GVal _ i) -> i == iOf val) srcVals
    diffBasis :: GVal -> [GVal] -> [GVal]
    diffBasis val = P.filter (\(GVal _ i) -> i /= iOf val)
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

addErr :: [ErrVal] -> ErrVal -> [ErrVal]
addErr dstErrs src@(ErrVal _ i1)
  | src == mempty = dstErrs
  | dstErrs == mempty = [src]
  | otherwise = case sameI i1 dstErrs of
      Nothing -> sort $ dstErrs <> [src]
      Just match -> sort $ diffI i1 dstErrs <> [match <> src]
        where
          diffI :: Set GNum -> [ErrVal] -> [ErrVal]
          diffI i = P.filter (\(ErrVal _ i2) -> i2 /= i)
  where
    sameI :: Set GNum -> [ErrVal] -> Maybe ErrVal
    sameI i errs = headMay $ P.filter (\(ErrVal _ i2) -> i2 == i) errs

-- | Subtract a geometric value from a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
-- FIXME: error component?
subVal :: [GVal] -> GVal -> [GVal]
subVal dst (GVal r i) = addVal dst $ GVal (-r) i

-- | Add two vectors together.
addVecPair :: GVec -> GVec -> GVec
addVecPair vec1 vec2 = fst $ addVecPairWithErr vec1 vec2

-- | Add two vectors together.
addVecPairWithErr :: GVec -> GVec -> (GVec, UlpSum)
addVecPairWithErr (GVec vals1) (GVec vals2) = (GVec res, resUlp)
  where
    (res, resUlp) = foldl' addValWithErr (vals1,mempty) vals2

-- | Subtract one vector from the other.
-- FIXME: error component?
subVecPair :: GVec -> GVec -> GVec
subVecPair (GVec vals1) (GVec vals2) = GVec $ foldl' subVal vals1 vals2

-- | Multiply a vector by a scalar. arguments are given in this order for maximum readability.
mulScalarVecWithErr :: ℝ -> GVec -> (GVec,[ErrVal])
mulScalarVecWithErr s (GVec vals) = (GVec resVals, resErr)
  where
    resVals =  mulVal s <$> vals
    resErr = (\(GVal a b) -> ErrVal (UlpSum $ abs $ realToFrac $ doubleUlp a) b) <$> resVals
    mulVal s1 (GVal r i) = GVal (s1*r) i

-- | Divide a vector by a scalar. arguments are given in this order for maximum readability.
divVecScalarWithErr :: GVec -> ℝ -> (GVec,[ErrVal])
divVecScalarWithErr (GVec vals) s = (GVec resVals, resErr)
  where
    resVals = divVal s <$> vals
    resErr = (\(GVal a b) -> ErrVal (UlpSum $ abs $ realToFrac $ doubleUlp a) b) <$> resVals
    divVal s1 (GVal r i) = GVal (r/s1) i

-- | Divide a vector by a scalar, high precision (read: slow) version. arguments are given in this order for maximum readability.
hpDivVecScalar :: GVec -> BigFloat (PrecPlus20 Eps1) -> GVec
hpDivVecScalar (GVec vals) s = GVec $ divVal s <$> vals
  where
    divVal s1 (GVal r i) = GVal (realToFrac r `hpdiv` realToFrac s1) i
    hpdiv :: BigFloat (PrecPlus20 Eps1) -> BigFloat (PrecPlus20 Eps1) -> ℝ
    hpdiv a b = realToFrac $ a / b

-- | Calculate the like product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `likeVecPair` gvec2 == gvec2 `likeVecPair` gvec1.
likeVecPair :: GVec -> GVec -> [Either GRVal GVal]
likeVecPair a b
  | a > b     = fst <$> likeVecPairWithErr' a b
  | otherwise = fst <$> likeVecPairWithErr' b a

-- | Calculate the like product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `likeVecPair` gvec2 == gvec2 `likeVecPair` gvec1.
likeVecPairWithErr :: GVec -> GVec -> [(Either GRVal GVal, UlpSum)]
likeVecPairWithErr a b
  | a > b     = likeVecPairWithErr' a b
  | otherwise = likeVecPairWithErr' b a

-- | Generate the like product of a vector pair. multiply only the values in the basis vector sets that are common between the two GVecs.
likeVecPairWithErr' :: GVec -> GVec -> [(Either GRVal GVal, UlpSum)]
likeVecPairWithErr' vec1 vec2 = results
  where
    results = likeVecPair'' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    likeVecPair'' :: GVec -> GVec -> [(Either GRVal GVal, UlpSum)]
    likeVecPair'' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [(Either GRVal GVal, UlpSum)]
        multiplyLike vals val@(GVal _ i1) = mulLikePair val <$> P.filter (\(GVal _ i2) -> i2 == i1) vals
          where
            mulLikePair (GVal r1 i) (GVal r2 _)
              | size i == 1 = (simplifyVal res (elemAt 0 i), resUlp)
              | otherwise = case nonEmpty (elems i) of
                              (Just newi) -> (Left $ GRVal res (newi <> newi), resUlp)
                              Nothing -> error "empty set?"
              where
                simplifyVal v G0 = Right $ GVal v (singleton G0)
                simplifyVal v (GEPlus _) = Right $ GVal v (singleton G0)
                simplifyVal v (GEMinus _) = Right $ GVal (-v) (singleton G0)
                simplifyVal _ (GEZero _) = Right $ GVal 0 (singleton G0)
                res :: ℝ
                res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
                resUlp = UlpSum $ abs $ realToFrac $ doubleUlp res

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
              | i1 == singleton G0 = (Right $ GVal res i2, resUlp)
              | i2 == singleton G0 = (Right $ GVal res i1, resUlp)
              | otherwise = case nonEmpty (elems i1) of
                              Nothing -> error "empty set?"
                              (Just newI1) -> case nonEmpty (elems i2) of
                                                Nothing -> error "empty set?"
                                                (Just newI2) -> (Left $ GRVal res (newI1 <> newI2), resUlp)
              where
                res :: ℝ
                res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
                resUlp = UlpSum $ abs $ realToFrac $ doubleUlp res

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
                                                                            (Just newI2) -> (GRVal res (newI1 <> newI2), UlpSum $ abs $ realToFrac $ doubleUlp res)
                                                                              where
                                                                                res :: ℝ
                                                                                res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)

-- | Generate the geometric product of a vector pair.
mulVecPair :: GVec -> GVec -> [Either GRVal GVal]
mulVecPair vec1 vec2 = results
  where
    results = mulVecPair' vec1 vec2
    -- | cycle through one list of vectors, and generate a pair with the second list.
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

-- | For a multi-basis value where each basis vector is wedged against one another, sort the basis vectors remembering to invert the value if necessary.
sortBasis :: GRVal -> GRVal
sortBasis (GRVal r i) = if shouldFlip then GRVal (-r) basis else GRVal r basis
  where
    (shouldFlip, basis) = sortBasis' i

-- | For a multi-basis error quotent where each basis vector is wedged against one another, just sort the basis vectors.
sortErrBasis :: ErrRVal -> ErrRVal
sortErrBasis (ErrRVal r i) = ErrRVal r basis
  where
    (_, basis) = sortBasis' i

-- | Sort a set of basis vectors. Must return an ideal result, along with wehther the associated real value should be flipped or not.
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
    -- | sort a set of wedged basis vectors. may not provide an ideal result, but should return a better result, along with whether the associated real value should be flipped or not.
    sortBasis'' :: NonEmpty GNum -> (Bool, NonEmpty GNum)
    sortBasis'' (a:|[])     = (False,a:|[])
    sortBasis'' (a:|[b])    = if a > b then (True, b:|[a]) else (False, a:|[b])
    sortBasis'' (a:|(b:xs)) = if a > b
                              then (not $ flipOf $ sortBasis'' (a:|xs), b `cons` basisOf (sortBasis'' (a:|xs)))
                              else (      flipOf $ sortBasis'' (b:|xs), a `cons` basisOf (sortBasis'' (b:|xs)))

-- | For a multi-basis value with each basis wedged against one another, where they are in ascending order, we can end up with vectors that have multiple occurances of the same basis vector. strip these out, negating the real part as appropriate.
stripPairs :: GRVal -> GRVal
stripPairs (GRVal real vals)
  | isJust flipRes = if flipRes == Just False
                  then GRVal real res
                  else GRVal (-real) res
  | otherwise = GRVal 0 (G0:|[])
  where
    (flipRes, res) = withoutPairs (Just False, vals)

-- | For a multi-basis value with each basis wedged against one another, where they are in ascending order, we can end up with vectors that have multiple occurances of the same basis vector. strip these out, negating the real part as appropriate.
stripErrPairs :: ErrRVal -> ErrRVal
stripErrPairs (ErrRVal real vals) = ErrRVal real res
  where
    (_, res) = withoutPairs (Just False, vals)

-- | Perform elimination of basis pairs from the given basis set. only works right if the basis set has been sorted first.
-- The maybe bool tracks whether the result should be thrown away (two identical GEZeros), or if the result should have its value inverted.
withoutPairs :: (Maybe Bool, NonEmpty GNum) -> (Maybe Bool, NonEmpty GNum)
withoutPairs (_, oneI:|[]) = (Just False, oneI:|[])
withoutPairs (r, is@((GEPlus a):|(GEPlus b):xs))
  | a == b = case nonEmpty xs of
               Nothing -> (r, G0:|[])
               (Just vs) -> withoutPairs (r, vs)
  | a /= b = case nonEmpty xs of
               Nothing -> (r, is)
               (Just _) -> prependI (GEPlus a) $ withoutPairs (r, GEPlus b:|xs)
withoutPairs (r, is@((GEMinus a):|(GEMinus b):xs))
  | a == b = case nonEmpty xs of
               Nothing -> (maybeNot r,G0:|[])
               (Just vs) -> withoutPairs (maybeNot r,vs)
  | a /= b = case nonEmpty xs of
               Nothing -> (r,is)
               (Just _) -> prependI (GEMinus a) $ withoutPairs (r, GEMinus b:|xs)
  where
    maybeNot :: Maybe Bool -> Maybe Bool
    maybeNot Nothing = Nothing
    maybeNot (Just v) = Just $ not v
withoutPairs (r, is@((GEZero a):|(GEZero b):xs))
  | a == b = (Nothing,G0:|[])
  | a /= b = case nonEmpty xs of
               Nothing -> (r,is)
               (Just _) -> prependI (GEZero a) $ withoutPairs (r, GEZero b:|xs)
withoutPairs (r, a:|b:xs) = prependI a $ withoutPairs (r,b:|xs)

-- | place a term on the front of the basis vector set.
-- if the vector set contains only the scalar vector, eliminate it.
prependI :: GNum -> (a,NonEmpty GNum) -> (a, NonEmpty GNum)
prependI num (r,nums) = (r, newPrependI num nums)
  where
    newPrependI :: GNum -> NonEmpty GNum -> NonEmpty GNum
    newPrependI n ns
      | ns == (G0:|[]) = n:|[]
      | otherwise      = n `cons` ns

-- | A post processor, to convert a GRVal into a GVal. this sorts the basis vectors as part of the conversion.
postProcess :: GRVal -> GVal
postProcess val = grValToGVal $ stripPairs $ sortBasis val

-- | A post processor, to convert a GRVal into a GVal. this sorts the basis vectors as part of the conversion. it may be given a GVal, in which case it short circuits.
postProcessVals :: Either GRVal GVal -> GVal
postProcessVals (Right gval) = gval
postProcessVals (Left grval) = grValToGVal $ stripPairs $ sortBasis grval

-- | A post processor, to convert a GRVal into a GVal, preserving the attached error. this sorts the basis vectors as part of the conversion. it may be given a GVal, in which case it short circuits.
postProcessEitherVals :: Either (GRVal, ErrRVal) (GVal, ErrVal) -> GVal
postProcessEitherVals (Right (v,_)) = v
postProcessEitherVals (Left (v,_)) = grValToGVal $ stripPairs $ sortBasis v

-- | Type Conversion of a GRval to a GVal. only to be used in postProcess, postProcessVals, and postProcessEitherVals.
grValToGVal :: GRVal -> GVal
grValToGVal (GRVal r i) = GVal r (fromAscList (toList i))

-- | A post processor, to convert a ErrRVal into an ErrVal. this sorts the basis vectors as part of the conversion.
postProcessErrs :: ErrRVal -> ErrVal
postProcessErrs val = errRValToErrVal $ stripErrPairs $ sortErrBasis val

-- | A post processor, to convert a ErrRVal into an ErrVal, preserving the attached error. this sorts the basis vectors as part of the conversion. it may be given an ErrVal, in which case it short circuits.
postProcessEitherErrs :: Either (GRVal, ErrRVal) (GVal, ErrVal) -> ErrVal
postProcessEitherErrs (Right (_,v)) = v
postProcessEitherErrs (Left (_,v)) = errRValToErrVal $ stripErrPairs $ sortErrBasis v

-- | Type conversion of an ErrRVal to an ErrVal. only safe to use from postProcessErrs and postProcessEitherErrs.
errRValToErrVal :: ErrRVal -> ErrVal
errRValToErrVal (ErrRVal r i) = ErrVal r (fromAscList (toList i))

-- | Our "like" operator. unicode point u+23a3.
(⎣) :: GVec -> GVec -> GVec
infixl 9 ⎣
(⎣) v1 v2 = fst $ v1 ⎣+ v2
-- | Our "like" operator. unicode point u+23a3.

(⎣+) :: GVec -> GVec -> (GVec, UlpSum)
infixl 9 ⎣+
(⎣+) v1 v2 = (GVec newVals
             , ulpTotal)
  where
    (newVals, addValErr) = foldl' addValWithErr ([], mempty) $ postProcessVals . fst <$> res
    res = likeVecPairWithErr v1 v2
    ulpTotal = foldl' (\(UlpSum a) (UlpSum b) -> UlpSum $ a + b) addValErr (snd <$> res)

-- | Our "unlike" operator. unicode point u+23a4.
(⎤) :: GVec -> GVec -> GVec
infixl 9 ⎤
(⎤) v1 v2 = fst $ v1 ⎤+ v2

-- | Our "unlike" operator. unicode point u+23a4.
(⎤+) :: GVec -> GVec -> (GVec, UlpSum)
infixl 9 ⎤+
(⎤+) v1 v2 = (GVec newVals
             , ulpTotal)
  where
    (newVals, addValErr) = foldl' addValWithErr ([], mempty) $ postProcessVals . fst <$> res
    res = unlikeVecPairWithErr v1 v2
    ulpTotal = foldl' (\(UlpSum a) (UlpSum b) -> UlpSum $ a + b) addValErr (snd <$> res)

-- | Our "reductive" operator.
(⨅) :: GVec -> GVec -> GVec
infixl 9 ⨅
(⨅) v1 v2 = fst $ v1 ⨅+ v2

-- | Our "reductive" operator, with attached Error.
(⨅+) :: GVec -> GVec -> (GVec, UlpSum)
infixl 9 ⨅+
(⨅+) v1 v2 = (GVec newVals
             , ulpTotal)
  where
    (newVals, addValErr) = foldl' addValWithErr ([], mempty) $ postProcess . fst <$> res
    res = reduceVecPairWithErr v1 v2
    ulpTotal = foldl' (\(UlpSum a) (UlpSum b) -> UlpSum $ a + b) addValErr (snd <$> res)

-- | A wedge operator. gets the wedge product of the two arguments. note that wedge = reductive minus unlike.
(∧) :: GVec -> GVec -> GVec
infixl 9 ∧
(∧) v1 v2 = vals
  where
    vals = subVecPair (GVec resReduce) (GVec resUnlike)
    resUnlike = foldl' addVal [] $ postProcessVals <$> unlikeVecPair v1 v2
    resReduce = foldl' addVal [] $ postProcess <$> reduceVecPair v1 v2

-- | A dot operator. gets the dot product of the two arguments. note that dot = reductive plus like.
(⋅) :: GVec -> GVec -> GVec
infixl 9 ⋅
(⋅) v1 v2 = vals
  where
    vals = addVecPair (GVec resLike) (GVec resReduce)
    resLike = foldl' addVal [] $ postProcessVals <$> likeVecPair v1 v2
    resReduce = foldl' addVal [] $ postProcess <$> reduceVecPair v1 v2

-- | A geometric product operator. Gets the geometric product of the two arguments.
(•) :: GVec -> GVec -> GVec
infixl 9 •
(•) v1 v2 = GVec $ foldl' addVal [] $ postProcessVals <$> mulVecPair v1 v2

-- | Return any scalar component of the given GVec.
scalarPart :: GVec -> ℝ
scalarPart (GVec vals) = sum $ realValue <$> vals
  where
    realValue (GVal r gnums) = if gnums == singleton G0 then r else 0

-- | Return any non-scalar component of the given GVec.
vectorPart :: GVec -> GVec
vectorPart (GVec vals) = GVec $ foldl' addVal [] $ P.filter noRealValue vals
  where
    noRealValue (GVal _ gnums) = gnums /= singleton G0
