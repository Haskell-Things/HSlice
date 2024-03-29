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

-- For adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- For TowardInf.
{-# LANGUAGE DataKinds #-}

-- So we can map applying (,mempty) to a list.
{-# LANGUAGE TupleSections #-}

-- For :->:
{-# LANGUAGE TypeOperators, TypeFamilies #-}

-- So we can create an instance of HasTrie for NonEmpty GNum.
{-# LANGUAGE FlexibleInstances #-}

-- | Our geometric algebra library.
module Graphics.Slicer.Math.GeometricAlgebra(
  ErrVal(ErrVal),
  GNum(G0, GEMinus, GEPlus, GEZero),
  GVal(GVal),
  GVec(GVec),
  UlpSum(UlpSum),
  (⎣+),
  (⎣),
  (⎤+),
  (⎤),
  (⨅+),
  (⨅),
  (•+),
  (•),
  (⋅+),
  (⋅),
  (∧+),
  (∧),
  addErr,
  addValPairWithErr,
  addValWithErr,
  addValWithoutErr,
  addVecPair,
  addVecPairWithErr,
  addVecPairWithoutErr,
  divVecScalarWithErr,
  eValOf,
  getVal,
  hpDivVecScalar,
  mulScalarVecWithErr,
  reduceVecPair,
  scalarPart,
  subVal,
  subValPairWithErr,
  subVecPair,
  sumErrVals,
  ulpRaw,
  ulpVal,
  unlikeVecPair,
  valOf,
  vectorPart
  ) where

import Prelude (Eq, Monoid(mempty), Ord(compare), Semigroup((<>)), Show(show), (==), (/=), (+), fst, otherwise, snd, ($), not, (>), (*), concatMap, (<$>), sum, (&&), (/), Bool(True, False), error, flip, (&&), not, null, realToFrac, abs, (.), realToFrac)

import Prelude as P (filter)

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right))

import Data.List (foldl', uncons)

import Data.List.NonEmpty (NonEmpty((:|)), toList, cons, nonEmpty)

import Data.List.Ordered (sort, insertSet)

import Data.Maybe (Maybe(Just, Nothing), isJust)

import Data.MemoTrie (HasTrie(enumerate, trie, untrie), Reg, (:->:), enumerateGeneric, memo, trieGeneric, untrieGeneric)

import Data.Number.BigFloat (BigFloat, PrecPlus20, Eps1)

import Data.Set (Set, singleton, disjoint, elems, size, elemAt, fromAscList)

import Data.Set as S (filter)

import Numeric.Rounded.Hardware (Rounded, RoundingMode(TowardInf, ToNearest), getRounded)

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

instance HasTrie GNum where
  newtype (GNum :->: b) = GNumTrie { unGNumTrie :: Reg GNum :->: b }
  trie = trieGeneric GNumTrie
  untrie = untrieGeneric unGNumTrie
  enumerate = enumerateGeneric unGNumTrie

instance HasTrie (NonEmpty GNum) where
  newtype ((NonEmpty GNum) :->: b) = NEGNumTrie { unNEGNumTrie :: Reg (NonEmpty GNum) :->: b }
  trie = trieGeneric NEGNumTrie
  untrie = untrieGeneric unNEGNumTrie
  enumerate = enumerateGeneric unNEGNumTrie

-- | A value in geometric algebra. this will have duplicate members filtered out, and the members will be in order.
data GVal = GVal
  -- _real ::
  !ℝ
  -- _basis ::
  !(Set GNum)
  deriving (Eq, Generic, NFData, Show)

instance HasTrie GVal where
  newtype (GVal :->: b) = GValTrie { unGValTrie :: Reg GVal :->: b }
  trie = trieGeneric GValTrie
  untrie = untrieGeneric unGValTrie
  enumerate = enumerateGeneric unGValTrie

-- | A value in geometric algebra, in need of reduction. this may have duplicate members, or members out of order.
data GRVal = GRVal
  -- real component
  !ℝ
  -- basis vector
  !(NonEmpty GNum)
  deriving (Eq, Generic, NFData, Show)

-- | A constantly increasing sum of error. Used for increasing our error bars proportonally to error collected from the FPU during calculations.
newtype UlpSum = UlpSum { ulpRaw :: Rounded 'TowardInf ℝ }
  deriving (Show, Eq, Generic, NFData, Ord)

-- | A value extractor directly to ℝ, which is a newtype of Double.
ulpVal :: UlpSum -> ℝ
ulpVal = getRounded . ulpRaw

instance Semigroup UlpSum where
  (<>) (UlpSum sum1) (UlpSum sum2) = UlpSum $ sum1 + sum2

instance Monoid UlpSum where
  mempty = UlpSum 0

instance HasTrie UlpSum where
  newtype (UlpSum :->: b) = UlpSumTrie { unUlpSumTrie :: Reg UlpSum :->: b }
  trie = trieGeneric UlpSumTrie
  untrie = untrieGeneric unUlpSumTrie
  enumerate = enumerateGeneric unUlpSumTrie

data ErrVal = ErrVal
  -- { _ulpVal ::
              !UlpSum
  -- _ulpBasis ::
              !(Set GNum)
  deriving (Eq, Generic, NFData, Show)

data ErrRVal = ErrRVal { _ulpRVal :: !UlpSum, _ulpRBasis :: NonEmpty GNum }
  deriving (Eq, Generic, NFData, Show)

instance HasTrie ErrVal where
  newtype (ErrVal :->: b) = ErrValTrie { unErrValTrie :: Reg ErrVal :->: b }
  trie = trieGeneric ErrValTrie
  untrie = untrieGeneric unErrValTrie
  enumerate = enumerateGeneric unErrValTrie

-- | Fake instance. do not try to order by ErrVal.
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

-- | When sorting gvals, sort the basis, THEN sort the multiplier.
instance Ord GVal where
  (GVal r1 i1) `compare` (GVal r2 i2)
    | i1 == i2  = compare r1 r2
    | otherwise = compare i1 i2

-- | A (multi)vector in geometric algebra.
newtype GVec = GVec [GVal]
  deriving (Eq, Generic, NFData, Show, Ord)

instance HasTrie GVec where
  newtype (GVec :->: b) = GVecTrie { unGVecTrie :: Reg GVec :->: b }
  trie = trieGeneric GVecTrie
  untrie = untrieGeneric unGVecTrie
  enumerate = enumerateGeneric unGVecTrie

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
addValPairWithErr :: GVal -> GVal -> [(GVal, ErrVal)]
addValPairWithErr v1@(GVal r1 i1) v2@(GVal r2 i2)
  | r1 == 0 && r2 == 0      = []
  | r1 == 0                 = [(v2,mempty)]
  | r2 == 0                 = [(v1,mempty)]
  | i1 == i2 && r1 == (-r2) = []
  | i1 == i2                = [(GVal res i1
                              , ErrVal resErr i1)]
  | otherwise               = sort [(v1,mempty),(v2,mempty)]
  where
    res :: ℝ
    res = realToFrac (realToFrac r1 + realToFrac r2 :: Rounded 'ToNearest ℝ)
    resErr = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac (realToFrac r1 + realToFrac r2 :: Rounded 'TowardInf ℝ)

-- | Subtract a geometric value from another geometric value, providing our error quotent.
subValPairWithErr :: GVal -> GVal -> [(GVal, ErrVal)]
subValPairWithErr v1@(GVal r1 i1) (GVal r2 i2)
  | i1 == i2 && r1 == r2 = []
  | otherwise            = addValPairWithErr v1 $ GVal (-r2) i2

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addVal :: [GVal] -> GVal -> [GVal]
addVal dst src = fst <$> addValWithErr ((,mempty) <$> dst) src

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addValWithErr :: [(GVal, ErrVal)] -> GVal -> [(GVal, ErrVal)]
addValWithErr dstVals src@(GVal r1 _)
  | r1 == 0 = dstVals
  | null dstVals = [(src, mempty)]
  | otherwise = case sameI src dstVals of
                  Nothing  -> insertSet (src, mempty) dstVals
                  (Just ((a,e), [])) -> if rOf a == (-r1)
                                        then diffI src dstVals
                                        else insertSet (GVal newVal $ iOf src, newErr) $ diffI src dstVals
                    where
                      newVal :: ℝ
                      newVal = realToFrac (realToFrac (rOf a) + realToFrac r1 :: Rounded 'ToNearest ℝ)
                      newErr = e <> ErrVal (UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac newErrVal) (iOf src)
                      newErrVal = (realToFrac (rOf a) + realToFrac r1 :: Rounded 'TowardInf ℝ)
                  _                  -> error "found too many sameI candidates."
  where
    sameI :: GVal -> [(GVal,ErrVal)] -> Maybe ((GVal,ErrVal), [(GVal, ErrVal)])
    sameI val srcVals = uncons $ P.filter (\(GVal _ i,_) -> i == iOf val) srcVals
    diffI :: GVal -> [(GVal,ErrVal)] -> [(GVal,ErrVal)]
    diffI val = P.filter (\(GVal _ i,_) -> i /= iOf val)
    iOf (GVal _ i) = i
    rOf (GVal r _) = r

-- | Add a geometric value to a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
addValWithoutErr :: [GVal] -> GVal -> [GVal]
addValWithoutErr dstVals src@(GVal r1 _)
  | r1 == 0 = dstVals
  | null dstVals = [src]
  | otherwise = case sameI src dstVals of
                  Nothing  -> insertSet src dstVals
                  (Just a) -> error $ "Condition failed: found two values with the same basis vector to add:\n"
                                   <> show dstVals <> "\n"
                                   <> show a <> "\n"
                                   <> show src <> "\n"
  where
    sameI :: GVal -> [GVal] -> Maybe (GVal, [GVal])
    sameI val srcVals = uncons $ P.filter (\a -> iOf a == iOf val) srcVals
    iOf (GVal _ i) = i

-- | Subtract a geometric value from a list of geometric values.
--   Assumes the list of values is in ascending order by basis vector, so we can find items with matching basis vectors easily.
subVal :: [GVal] -> GVal -> [GVal]
subVal dst src = fst <$> subValWithErr ((,mempty) <$> dst) src

-- | Subtract a geometric value from a list of geometric values.
subValWithErr :: [(GVal, ErrVal)] -> GVal -> [(GVal, ErrVal)]
subValWithErr dst (GVal r i) = addValWithErr dst $ GVal (-r) i

-- | add an error quotent to a list of error quotents.
addErr :: [ErrVal] -> ErrVal -> [ErrVal]
addErr dstErrs src@(ErrVal _ i1)
  | src == mempty = dstErrs
  | dstErrs == mempty = [src]
  | otherwise = case sameI i1 dstErrs of
      Nothing          -> insertSet src dstErrs
      Just (match, []) -> insertSet (src <> match) $ diffI i1 dstErrs
      _                -> error "too many sameI values"
  where
    diffI :: Set GNum -> [ErrVal] -> [ErrVal]
    diffI i = P.filter (\(ErrVal _ i2) -> i2 /= i)
    sameI :: Set GNum -> [ErrVal] -> Maybe (ErrVal, [ErrVal])
    sameI i errs = uncons $ P.filter (\(ErrVal _ i2) -> i2 == i) errs

-- | Add two vectors together.
addVecPair :: GVec -> GVec -> GVec
addVecPair vec1 vec2 = fst $ addVecPairWithErr vec1 vec2

-- | Add two vectors together, preserving the error quotents.
addVecPairWithErr :: GVec -> GVec -> (GVec, [ErrVal])
addVecPairWithErr (GVec vals1) (GVec vals2) = (resVec, resErr)
  where
    resVec = GVec $ fst <$> res
    resErr = P.filter (/= mempty) $ snd <$> res
    res = foldl' addValWithErr ((,mempty) <$> vals1) vals2

-- | Add two vectors together, and ensure that there is not any error produced by the operation
addVecPairWithoutErr :: GVec -> GVec -> GVec
addVecPairWithoutErr (GVec vals1) (GVec vals2) = GVec vals
  where
    vals = foldl' addValWithoutErr vals1 vals2

-- | Subtract one vector from the other.
subVecPair :: GVec -> GVec -> GVec
subVecPair vec1 vec2 = fst $ subVecPairWithErr vec1 vec2

-- | subtract the second vector from the first vector.
subVecPairWithErr :: GVec -> GVec -> (GVec, [ErrVal])
subVecPairWithErr (GVec vals1) (GVec vals2) = (resVec, resErr)
  where
    resVec = GVec $ fst <$> res
    resErr = P.filter (/= mempty) $ snd <$> res
    res = foldl' subValWithErr ((,mempty) <$> vals1) vals2

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
  | a > b     = fstEither <$> likeVecPairWithErr' a b
  | otherwise = fstEither <$> likeVecPairWithErr' b a
  where
    fstEither v = case v of
                    (Left (c,_)) -> Left c
                    (Right (c,_)) -> Right c

-- | Calculate the like product of a vector pair.
-- actually a wrapper to make use of the fact that gvec1 `likeVecPair` gvec2 == gvec2 `likeVecPair` gvec1.
likeVecPairWithErr :: GVec -> GVec -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
likeVecPairWithErr a b
  | a > b     = likeVecPairWithErr' a b
  | otherwise = likeVecPairWithErr' b a

-- | Generate the like product of a vector pair. multiply only the values in the basis vector sets that are common between the two GVecs.
likeVecPairWithErr' :: GVec -> GVec -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
likeVecPairWithErr' vec1 vec2 = results
  where
    results = likeVecPair'' vec1 vec2
    -- cycle through one list, and generate a pair with the second list when the two basis vectors are the same.
    likeVecPair'' :: GVec -> GVec -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
    likeVecPair'' (GVec v1) (GVec v2) = concatMap (multiplyLike v1) v2
      where
        multiplyLike :: [GVal] -> GVal -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
        multiplyLike vals val@(GVal _ i1) = mulLikePair val <$> P.filter (\(GVal _ i2) -> i2 == i1) vals
          where
            mulLikePair :: GVal -> GVal -> Either (GRVal,ErrRVal) (GVal, ErrVal)
            mulLikePair (GVal r1 i) (GVal r2 _)
              | size i == 1 = Right (simplify GVal res (elemAt 0 i), simplifyAbs ErrVal resErr (elemAt 0 i))
              | otherwise = case nonEmpty (elems i) of
                              (Just newi) -> Left (GRVal res (newi <> newi), ErrRVal resErr (newi <> newi))
                              Nothing -> error "empty set?"
              where
                simplify :: (ℝ -> Set GNum -> c) -> ℝ -> GNum -> c
                simplify fn v G0 = fn v (singleton G0)
                simplify fn v (GEPlus _) = fn v (singleton G0)
                simplify fn v (GEMinus _) = fn (-v) (singleton G0)
                simplify fn _ (GEZero _) = fn 0 (singleton G0)
                simplifyAbs :: Monoid c => (a -> Set GNum -> c) -> a -> GNum -> c
                simplifyAbs fn v G0 = fn v (singleton G0)
                simplifyAbs fn v (GEPlus _) = fn v (singleton G0)
                simplifyAbs fn v (GEMinus _) = fn v (singleton G0)
                simplifyAbs _ _ (GEZero _) = mempty
                res :: ℝ
                res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
                resErr = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac resErrRaw
                resErrRaw = realToFrac r1 * realToFrac r2 :: Rounded 'TowardInf ℝ

-- | Generate the unlike product of a vector pair. multiply only the values in the basis vector sets that are not the same between the two GVecs.
unlikeVecPair :: GVec -> GVec -> [Either GRVal GVal]
unlikeVecPair vec1 vec2 = fstEither <$> unlikeVecPairWithErr vec1 vec2
  where
        fstEither v = case v of
                    (Left (c,_)) -> Left c
                    (Right (c,_)) -> Right c

unlikeVecPairWithErr :: GVec -> GVec -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
unlikeVecPairWithErr vec1 vec2 = results
  where
    results = unlikeVecPair' vec1 vec2
    -- cycle through one list of vectors, and generate a pair with the second list when the two basis vectors are not the same.
    unlikeVecPair' :: GVec -> GVec -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
    unlikeVecPair' (GVec v1) (GVec v2) = concatMap (multiplyUnlike v1) v2
      where
        multiplyUnlike :: [GVal] -> GVal -> [Either (GRVal,ErrRVal) (GVal, ErrVal)]
        multiplyUnlike vals val@(GVal _ i) = mulUnlikePair val <$> P.filter (\(GVal _ i2) -> i2 /= i) vals
          where
            mulUnlikePair :: GVal -> GVal -> Either (GRVal, ErrRVal) (GVal, ErrVal)
            mulUnlikePair (GVal r1 i1) (GVal r2 i2)
              | i1 == singleton G0 = Right (GVal res i2, ErrVal resUlp i2)
              | i2 == singleton G0 = Right (GVal res i1, ErrVal resUlp i1)
              | otherwise = case nonEmpty (elems i1) of
                              Nothing -> error "empty set?"
                              (Just newI1) -> case nonEmpty (elems i2) of
                                                Nothing -> error "empty set?"
                                                (Just newI2) -> Left (GRVal res (newI1 <> newI2),
                                                                      ErrRVal resUlp (newI1 <> newI2))
              where
                res :: ℝ
                res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
                resUlp = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac resUlpRaw
                resUlpRaw = realToFrac r1 * realToFrac r2 :: Rounded 'TowardInf ℝ

-- | Generate the reductive product of a vector pair. multiply only values where one of the basis vectors is eliminated by the multiplication.
reduceVecPair :: GVec -> GVec -> [GRVal]
reduceVecPair vec1 vec2 = fst <$> reduceVecPairWithErr vec1 vec2

-- | Generate the reductive product of a vector pair. multiply only values where one of the basis vectors is eliminated by the multiplication.
reduceVecPairWithErr :: GVec -> GVec -> [(GRVal, ErrRVal)]
reduceVecPairWithErr vec1 vec2 = results
  where
    results = reduceVecPair' vec1 vec2
    -- | cycle through one list of vectors, and generate a pair with the second list. multiplies only the values where one set has some common vectors with the other set, but they do not have identical sets.
    reduceVecPair' :: GVec -> GVec -> [(GRVal, ErrRVal)]
    reduceVecPair' (GVec v1) (GVec v2) = concatMap (multiplyReducing v1) v2
      where
        multiplyReducing :: [GVal] -> GVal -> [(GRVal, ErrRVal)]
        multiplyReducing vals val@(GVal _ i) = flip mulReducingPair val <$> P.filter (\(GVal _ i2) -> i2 `common` i) (P.filter (\(GVal _ i2) -> i2 `hasDifferentZeros` i) $ P.filter (\(GVal _ i2) -> i2 /= i) vals)
          where
            common :: Set GNum -> Set GNum -> Bool
            common a b = not $ disjoint a b
            hasDifferentZeros :: Set GNum -> Set GNum -> Bool
            hasDifferentZeros nums1 nums2 = disjoint (S.filter isGEZero nums1) (S.filter isGEZero nums2)
              where
                isGEZero :: GNum -> Bool
                isGEZero (GEZero _) = True
                isGEZero _          = False
            mulReducingPair (GVal r1 i1) (GVal r2 i2) =
              case nonEmpty (elems i1) of
                Nothing -> error "empty set?"
                (Just newI1) ->
                  case nonEmpty (elems i2) of
                    Nothing -> error "empty set?"
                    (Just newI2) -> (GRVal res (newI1 <> newI2), ErrRVal resUlp (newI1 <> newI2))
                      where
                        res :: ℝ
                        res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
                        resUlp = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac resUlpRaw
                        resUlpRaw = (realToFrac r1 * realToFrac r2 :: Rounded 'TowardInf ℝ)

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

-- | Generate the geometric product of a vector pair, preserving the error quotents.
mulVecPairWithErr :: GVec -> GVec -> [Either (GRVal, ErrRVal) (GVal, ErrVal)]
mulVecPairWithErr vec1 vec2 = results
  where
    results = mulVecPairWithErr' vec1 vec2
    -- | cycle through one list of vectors, and generate a pair with the second list.
    mulVecPairWithErr' :: GVec -> GVec -> [Either (GRVal, ErrRVal) (GVal, ErrVal)]
    mulVecPairWithErr' (GVec v1) (GVec v2) = concatMap (mulvals v2) v1
      where
        mulvals :: [GVal] -> GVal -> [Either (GRVal, ErrRVal) (GVal, ErrVal)]
        mulvals vals val = mulValPairWithErr val <$> vals
        mulValPairWithErr :: GVal -> GVal -> Either (GRVal, ErrRVal) (GVal, ErrVal)
        mulValPairWithErr  (GVal r1 i1) (GVal r2 i2)
          | i1 == i2 && size i1 == 1 = Right (simplify GVal res (elemAt 0 i1), simplifyAbs ErrVal resUlp (elemAt 0 i1))
          | i1 == singleton G0       = Right (GVal res i2, ErrVal resUlp i2)
          | i2 == singleton G0       = Right (GVal res i1, ErrVal resUlp i1)
          | otherwise = case nonEmpty (elems i1) of
                          Nothing -> error "empty set?"
                          (Just newI1) -> case nonEmpty (elems i2) of
                                            Nothing -> error "empty set?"
                                            (Just newI2) -> Left (GRVal res (newI1 <> newI2), ErrRVal resUlp (newI1 <> newI2))
          where
            res :: ℝ
            res = realToFrac (realToFrac r1 * realToFrac r2 :: Rounded 'ToNearest ℝ)
            resUlp = UlpSum $ abs $ realToFrac $ doubleUlp $ realToFrac resUlpRaw
            resUlpRaw = realToFrac r1 * realToFrac r2 :: Rounded 'TowardInf ℝ
            simplify :: (ℝ -> Set GNum -> c) -> ℝ -> GNum -> c
            simplify fn v G0 = fn v (singleton G0)
            simplify fn v (GEPlus _) = fn v (singleton G0)
            simplify fn v (GEMinus _) = fn (-v) (singleton G0)
            simplify fn _ (GEZero _) = fn 0 (singleton G0)
            simplifyAbs :: Monoid c => (a -> Set GNum -> c) -> a -> GNum -> c
            simplifyAbs fn v G0 = fn v (singleton G0)
            simplifyAbs fn v (GEPlus _) = fn v (singleton G0)
            simplifyAbs fn v (GEMinus _) = fn v (singleton G0)
            simplifyAbs _ _ (GEZero _) = mempty

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

-- | Wrap sortBasisInternal in a memoization, so it becomes a tree lookup over time.
sortBasis' :: NonEmpty GNum -> (Bool, NonEmpty GNum)
sortBasis' = memo sortBasisInternal

-- | Sort a set of basis vectors. Must return an ideal result, along with wehther the associated real value should be flipped or not.
sortBasisInternal :: NonEmpty GNum -> (Bool, NonEmpty GNum)
sortBasisInternal thisBasis
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

withoutPairs :: (Maybe Bool, NonEmpty GNum) -> (Maybe Bool, NonEmpty GNum)
withoutPairs = memo withoutPairs'

-- | Perform elimination of basis pairs from the given basis set. only works right if the basis set has been sorted first.
-- The maybe bool tracks whether the result should be thrown away (two identical GEZeros), or if the result should have its value inverted.
withoutPairs' :: (Maybe Bool, NonEmpty GNum) -> (Maybe Bool, NonEmpty GNum)
withoutPairs' (_, oneI:|[]) = (Just False, oneI:|[])
withoutPairs' (r, is@((GEPlus a):|(GEPlus b):xs))
  | a == b = case nonEmpty xs of
               Nothing -> (r, G0:|[])
               (Just vs) -> withoutPairs (r, vs)
  | a /= b = case nonEmpty xs of
               Nothing -> (r, is)
               (Just _) -> prependI (GEPlus a) $ withoutPairs (r, GEPlus b:|xs)
withoutPairs' (r, is@((GEMinus a):|(GEMinus b):xs))
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
withoutPairs' (r, is@((GEZero a):|(GEZero b):xs))
  | a == b = (Nothing,G0:|[])
  | a /= b = case nonEmpty xs of
               Nothing -> (r,is)
               (Just _) -> prependI (GEZero a) $ withoutPairs (r, GEZero b:|xs)
withoutPairs' (r, a:|b:xs) = prependI a $ withoutPairs (r,b:|xs)

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

-- | Our "like" operator, returning calculation error. unicode point u+23a3.
(⎣+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
infixl 9 ⎣+
(⎣+) v1 v2 = (GVec vals
             , (mulErrs, addErrs))
  where
    vals = fst <$> likeRes'
    addErrs = P.filter (/= mempty) $ snd <$> likeRes'
    likeRes'= foldl' addValWithErr [] $ postProcessEitherVals <$> likeRes
    mulErrs = foldl' addErr [] $ postProcessEitherErrs <$> likeRes
    likeRes = likeVecPairWithErr v1 v2

-- | Our "unlike" operator. unicode point u+23a4.
(⎤) :: GVec -> GVec -> GVec
infixl 9 ⎤
(⎤) v1 v2 = fst $ v1 ⎤+ v2

-- | Our "unlike" operator, with attached Error bounds. unicode point u+23a4.
(⎤+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
infixl 9 ⎤+
(⎤+) v1 v2 = (GVec vals
             , (mulErrs, addErrs))
  where
    vals = fst <$> unlikeRes'
    addErrs = P.filter (/= mempty) $ snd <$> unlikeRes'
    unlikeRes' = foldl' addValWithErr [] $ postProcessEitherVals <$> unlikeRes
    mulErrs = foldl' addErr [] $ postProcessEitherErrs <$> unlikeRes
    unlikeRes = unlikeVecPairWithErr v1 v2

-- | Our "reductive" operator.
(⨅) :: GVec -> GVec -> GVec
infixl 9 ⨅
(⨅) v1 v2 = fst $ v1 ⨅+ v2

-- | Our "reductive" operator, with attached Error.
(⨅+) :: GVec -> GVec -> (GVec, ([ErrVal],[ErrVal]))
infixl 9 ⨅+
(⨅+) v1 v2 = (GVec vals
             , (mulErrs, addErrs))
  where
    vals = fst <$> res
    addErrs = P.filter (/= mempty) $ snd <$> res
    res = foldl' addValWithErr [] $ postProcess . fst <$> reduceRes
    mulErrs = foldl' addErr [] $ postProcessErrs . snd <$> reduceRes
    reduceRes = reduceVecPairWithErr v1 v2

-- | A wedge operator. gets the wedge product of the two arguments. note that wedge = reductive minus unlike.
(∧) :: GVec -> GVec -> GVec
infixl 9 ∧
(∧) v1 v2 = vec
  where
    vec = subVecPair (GVec resReduce) (GVec resUnlike)
    resUnlike = foldl' addVal [] $ postProcessVals <$> unlikeVecPair v1 v2
    resReduce = foldl' addVal [] $ postProcess <$> reduceVecPair v1 v2

-- | A wedge operator that preserves error. gets the wedge product of the two arguments. note that wedge = reductive minus unlike.
(∧+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal], [ErrVal], [ErrVal], [ErrVal]))
infixl 9 ∧+
(∧+) v1 v2 = (vec, (unlikeMulErrs, unlikeAddErrs, reduceMulErrs, reduceAddErrs, vecSubErrs))
  where
    (vec, vecSubErrs) = subVecPairWithErr (GVec reduceVals) (GVec unlikeVals)
    unlikeVals = fst <$> unlikeRes'
    unlikeAddErrs = P.filter (/= mempty) $ snd <$> unlikeRes'
    unlikeRes' = foldl' addValWithErr [] $ postProcessEitherVals <$> unlikeRes
    unlikeMulErrs =foldl' addErr [] $ postProcessEitherErrs <$> unlikeRes
    unlikeRes = unlikeVecPairWithErr v1 v2
    reduceVals = fst <$> reduceRes'
    reduceAddErrs = P.filter (/= mempty) $ snd <$> reduceRes'
    reduceRes' = foldl' addValWithErr [] $ postProcess . fst <$> reduceRes
    reduceMulErrs = postProcessErrs . snd <$> reduceRes
    reduceRes = reduceVecPairWithErr v1 v2

-- | A dot operator. gets the dot product of the two arguments. note that dot = reductive plus like.
(⋅) :: GVec -> GVec -> GVec
infixl 9 ⋅
(⋅) v1 v2 = vals
  where
    vals = addVecPair (GVec resLike) (GVec resReduce)
    resLike = foldl' addVal [] $ postProcessVals <$> likeVecPair v1 v2
    resReduce = foldl' addVal [] $ postProcess <$> reduceVecPair v1 v2

-- | A dot operator that preserves error. gets the dot product of the two arguments.
-- Note that dot product = reductive product plus like product.
(⋅+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal], [ErrVal], [ErrVal], [ErrVal]))
infixl 9 ⋅+
(⋅+) v1 v2 = (vec
              , (likeMulErrs, reduceMulErrs, likeAddErrs, reduceAddErrs, vecAddErrs))
  where
    vecAddErrs = P.filter (/= mempty) vecAddErrsRaw
    (vec, vecAddErrsRaw) = addVecPairWithErr (GVec reduceVals) (GVec likeVals)
    likeVals = fst <$> likeRes'
    likeAddErrs = P.filter (/= mempty) $ snd <$> likeRes'
    likeMulErrs = foldl' addErr [] $ postProcessEitherErrs <$> likeRes
    likeRes' = foldl' addValWithErr [] $ postProcessEitherVals <$> likeRes
    likeRes = likeVecPairWithErr v1 v2
    reduceVals = fst <$> reduceRes'
    reduceAddErrs = P.filter (/= mempty) $ snd <$> reduceRes'
    reduceRes' = foldl' addValWithErr [] $ postProcess . fst <$> reduceRes
    reduceMulErrs = postProcessErrs . snd <$> reduceRes
    reduceRes = reduceVecPairWithErr v1 v2

-- | A geometric product operator. Gets the geometric product of the two arguments.
(•) :: GVec -> GVec -> GVec
infixl 9 •
(•) v1 v2 = GVec $ foldl' addVal [] $ postProcessVals <$> mulVecPair v1 v2

-- | A geometric product operator. Gets the geometric product of the two arguments.
(•+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
infixl 9 •+
(•+) v1 v2 = (GVec vals, (geomMulErrs, geomAddErrs))
  where
    vals = fst <$> geomRes'
    geomAddErrs = P.filter (/= mempty) $ snd <$> geomRes'
    geomMulErrs = foldl' addErr [] $ postProcessEitherErrs <$> geomRes
    geomRes' = foldl' addValWithErr [] $ postProcessEitherVals <$> geomRes
    geomRes = mulVecPairWithErr v1 v2

-- | Return the scalar component of the given GVec.
scalarPart :: GVec -> ℝ
scalarPart (GVec vals) = sum $ realValue <$> vals
  where
    realValue (GVal r gnums) = if gnums == singleton G0 then r else 0

-- | Return the non-scalar component of the given GVec.
vectorPart :: GVec -> GVec
vectorPart (GVec vals) = GVec $ foldl' addVal [] $ P.filter noRealValue vals
  where
    noRealValue (GVal _ gnums) = gnums /= singleton G0

-- | Temporary hack.
sumErrVals :: [ErrVal] -> UlpSum
sumErrVals errVals = UlpSum $ sum $ ulpRaw . (\(ErrVal a _) -> a) <$> errVals
