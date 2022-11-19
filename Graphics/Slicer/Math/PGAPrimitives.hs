{- ORMOLU_DISABLE -}
{-
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

-- for adding Generic and NFData to our types.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | The purpose of this file is to hold primitive projective geometric algebraic arithmatic.
-- Primitives here are defined as functions that work on types which have an implementation of the ProjectivePoint2 or ProjectiveLine2 typeclasses. Think "Pure 2D PGA functions only".

-- | What we export. Almost everything is part of a typeclass.
module Graphics.Slicer.Math.PGAPrimitives
  (
    PLine2Err(PLine2Err),
    PPoint2Err(PPoint2Err),
    ProjectiveLine(PLine2, NPLine2),
    ProjectiveLine2(
      normalizeL,
      vecOfL
      ),
    ProjectivePoint(PPoint2, CPPoint2),
    ProjectivePoint2(
      canonicalizeP,
      isIdealP,
      vecOfP
      ),
    angleBetween2PL,
    angleCosBetween2PL,
    canonicalizedIntersectionOf2PL,
    distance2PL,
    distance2PP,
    flipL,
    forceBasisOfL,
    forceBasisOfP,
    fuzzinessOfL,
    fuzzinessOfP,
    idealNormOfP,
    interpolate2PP,
    intersect2PL,
    join2PP,
    pLineErrAtPPoint,
    pToEP,
    translateL,
    xIntercept,
    yIntercept
  ) where

import Prelude(Bool(False), Eq((==),(/=)), Monoid(mempty), Ord, Semigroup((<>)), Show(show), (||), (&&), ($), (+), (*), (/), (<$>), abs, error, filter, fst, negate, otherwise, realToFrac, snd, sqrt)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right), fromRight, isRight)

import Data.List (foldl', sort)

import Data.Maybe (Maybe(Just,Nothing), fromJust, isJust, isNothing)

import Data.Set (Set, elems, fromList, singleton)

import GHC.Generics (Generic)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2))

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (∧), (•), addErr, addValWithoutErr, addVecPairWithErr, addVecPairWithoutErr, divVecScalarWithErr, eValOf, getVal, mulScalarVecWithErr, scalarPart, sumErrVals, ulpVal, valOf)

--------------------------------
--- common support functions ---
--------------------------------

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
-- Implemented in this file, because this operator's implementation differs based on the dimensionality of the space we are working in.
-- `A v+ B` returns the join of A and B (and the error quotents).
(∨+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
(∨+) a b = (dual2DGVec res
           ,(dual2DErrs unlikeMulErr, dual2DErrs unlikeAddErr))
  where
    (res, (unlikeMulErr, unlikeAddErr)) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | Get the dual of a vector. for a point, find a line, for a line, a point...
dual2DGVec :: GVec -> GVec
dual2DGVec (GVec vals) = GVec $ foldl' addValWithoutErr []
                 [
                   GVal (         valOf 0 $ getVal [G0] vals)                           (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , GVal (         valOf 0 $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , GVal (negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , GVal (         valOf 0 $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , GVal (         valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]

-- | Get the dual of the error component of a vector. for a point, find a line, for a line, a point...
dual2DErrs :: [ErrVal] -> [ErrVal]
dual2DErrs vals = filter (\(ErrVal a _) -> a /= mempty)
                 [
                   ErrVal (eValOf mempty $ getVal [G0] vals)                           (fromList [GEZero 1, GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1] vals)                     (fromList [GEPlus 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1] vals)                     (fromList [GEZero 1, GEPlus 2])
                 , ErrVal (eValOf mempty $ getVal [GEPlus 2] vals)                     (fromList [GEZero 1, GEPlus 1])
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1] vals)           (singleton (GEPlus 2))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 2] vals)           (singleton (GEPlus 1))
                 , ErrVal (eValOf mempty $ getVal [GEPlus 1, GEPlus 2] vals)           (singleton (GEZero 1))
                 , ErrVal (eValOf mempty $ getVal [GEZero 1, GEPlus 1, GEPlus 2] vals) (singleton G0)
                 ]

-- | Perform basis coersion.
-- Ensure that all of the required '0' components exist. Required before using basis sensitive raw operators directly.
-- TL;DR: before you can use like, unlike, or reductive operators, you have to guarantee all of the basis vectors for your appropriate type are present, even if they are set to 0.
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVal (elems needs) has) needs

-------------------------------
--- Projective Line Support ---
-------------------------------

-- | A projective line in 2D space.
-- NOTE: two constructors. one for normalized lines, one for un-normalized lines.
data ProjectiveLine =
  PLine2 GVec
  | NPLine2 GVec
  deriving (Generic, NFData, Show)

-- | The typeclass definition. functions that must be implemented for any projective line type.
class ProjectiveLine2 a where
  consLikeL :: a -> (GVec -> a)
  normalizeL :: a -> (ProjectiveLine, PLine2Err)
  vecOfL :: a -> GVec

-- | The implementation of typeclass operations.
instance ProjectiveLine2 ProjectiveLine where
  consLikeL l = case l of
                  (NPLine2 _) -> NPLine2
                  (PLine2 _) -> PLine2
  normalizeL l = case l of
                  n@(NPLine2 _) -> (n,mempty)
                  p@(PLine2 _) -> normalizeProjectiveLine p
  vecOfL l = case l of
               (NPLine2 v) -> v
               (PLine2 v) -> v

-- | An equality instance for ProjectiveLine. allows us to skip some normalization.
instance Eq ProjectiveLine where
  (==) (NPLine2 gvec1) (NPLine2 gvec2) = gvec1 == gvec2
  (==) pl1@(PLine2 gvec1) pl2@(PLine2 gvec2) = gvec1 == gvec2 || normalizeL pl1 == normalizeL pl2
  (==) pl1 pl2 = normalizeL pl1 == normalizeL pl2

-- | The types of error of a projective line.
data PLine2Err = PLine2Err
  -- AddErr
    [ErrVal]
  -- NormalizationErr
    [ErrVal]
  -- NormErr
    UlpSum
  -- SqNormErr
    UlpSum
  -- Translation Error. always in GEZero 1.
    UlpSum
  -- Join Error. when a PLine2 is constructed via join.
    ([ErrVal], [ErrVal])
  deriving (Eq, Show)

instance Semigroup PLine2Err where
  (<>) (PLine2Err a1 b1 c1 d1 e1 (f1,g1)) (PLine2Err a2 b2 c2 d2 e2 (f2,g2)) =
    PLine2Err (foldl' addErr a1 a2)
              (foldl' addErr b1 b2)
              (c1 <> c2)
              (d1 <> d2)
              (e1 <> e2)
              (foldl' addErr f1 f2,foldl' addErr g1 g2)

instance Monoid PLine2Err where
  mempty = PLine2Err mempty mempty mempty mempty mempty mempty

-- | Return the sine of the angle between the two lines, along with the error.
-- Results in a value that is ~+1 when a line points in the same direction of the other given line, and ~-1 when pointing backwards.
angleBetweenProjectiveLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal]), UlpSum))
angleBetweenProjectiveLines line1 line2 = (scalarPart likeRes, resErr)
  where
    resErr = (npl1Err, npl2Err, (likeMulErr,likeAddErr), ulpSum)
    -- FIXME: this returned ULPsum is wrong. actually try to interpret it.
    ulpSum = sumErrVals likeMulErr <> sumErrVals likeAddErr
    (likeRes, (likeMulErr, likeAddErr)) = lv1 ⎣+ lv2
    lv1 = vecOfL $ forceBasisOfL npl1
    lv2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeL line1
    (npl2, npl2Err) = normalizeL line2

-- | A wrapper for the above function, that removes error quotents that are not directly related to the input or result.
angleBetween2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
angleBetween2PL l1 l2 = crushErr $ angleBetweenProjectiveLines l1 l2
  where
    crushErr (res, (n1,n2,_,resErr)) = (res, (n1,n2,resErr))

-- | Find the distance between two parallel or antiparallel projective lines.
distanceBetweenProjectiveLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal]), UlpSum))
distanceBetweenProjectiveLines line1 line2 = (res, resErr)
  where
    (res, idealErr) = idealNormOfP $ PPoint2 like
    resErr = (npl1Err, npl2Err, likeErr, idealErr)
    (like, likeErr) = lv1 ⎣+ lv2
    lv1 = vecOfL $ forceBasisOfL npl1
    lv2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeL line1
    (npl2, npl2Err) = normalizeL line2

-- | A wrapper for the above function, that removes error quotents that are not directly related to the input or result.
distance2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
distance2PL l1 l2 = crushErr $ distanceBetweenProjectiveLines l1 l2
  where
    crushErr (res, (n1,n2,_,resErr)) = (res, (n1,n2,resErr))

-- | Reverse a line. same line, but pointed in the other direction.
flipProjectiveLine, flipL :: (ProjectiveLine2 a) => a -> a
-- | Actual implementation.
flipProjectiveLine line = (consLikeL line) rawRes
  where
    rawRes = GVec $ foldl' addValWithoutErr []
             [
               GVal (negate $ valOf 0 $ getVal [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 2] vals) (singleton (GEPlus 2))
             ]
    (GVec vals) = vecOfL line
-- | Wrapper.
flipL l = flipProjectiveLine l

-- | Ensure all of the '0' components exist on a Projective Line.
forceProjectiveLineBasis, forceBasisOfL :: (ProjectiveLine2 a) => a -> a
-- | Actual implementation.
forceProjectiveLineBasis line
  | gnums == Just [singleton (GEZero 1),
                   singleton (GEPlus 1),
                   singleton (GEPlus 2)] = line
  | otherwise = (consLikeL line) res
  where
    res = forceBasis [singleton (GEZero 1), singleton (GEPlus 1), singleton (GEPlus 2)] vec
    gnums = case vals of
              [GVal _ g1, GVal _ g2, GVal _ g3] -> Just [g1,g2,g3]
              _                                 -> Nothing
    vec@(GVec vals) = vecOfL line
-- | Wrapper.
forceBasisOfL l = forceProjectiveLineBasis l

-- | Determine the amount of translation error when trying to resolve a projective line.
-- NOTE: A projective line's error varies depending where on that line you are trying to resolve it.
--       For complete results, combine this with scaling xIntercept and yIntercept.
fuzzinessOfProjectiveLine, fuzzinessOfL :: (ProjectiveLine2 a) => (a, PLine2Err) -> UlpSum
-- | Actual implementation.
fuzzinessOfProjectiveLine (line, lineErr) = tUlp <> joinAddTErr <> joinMulTErr <> normalizeTErr <> additionTErr
  where
    (PLine2Err additionErr normalizeErr _ _ tUlp (joinMulErr, joinAddErr)) = lineErr <> normalizeErrRaw
    additionTErr = eValOf mempty (getVal [GEZero 1] additionErr)
    normalizeTErr = eValOf mempty (getVal [GEZero 1] normalizeErr)
    joinMulTErr = eValOf mempty (getVal [GEZero 1] joinMulErr)
    joinAddTErr = eValOf mempty (getVal [GEZero 1] joinAddErr)
    (_,normalizeErrRaw) = normalizeL line
-- | Wrapper.
fuzzinessOfL l = fuzzinessOfProjectiveLine l

-- | Find out where two lines intersect, returning a projective point, and the error quotents.
-- Note: This should only be used when you can guarantee the input lines are not collinear, or parallel.
intersectionOfProjectiveLines, intersect2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
-- | Actual implementation.
intersectionOfProjectiveLines line1 line2 = (res, (line1Err, line2Err, resErr))
  where
    (res, (line1Err, line2Err, resUnlikeErrs)) = meetOfProjectiveLines line1 line2
    resErr = PPoint2Err resUnlikeErrs mempty mempty mempty mempty iAngleErr iAngleUnlikeErr
    -- Since the angle of intersection has an effect on how well this point was resolved, save it with the point.
    (iAngleErr,(_,_,iAngleUnlikeErr,_)) = angleBetweenProjectiveLines line1 line2
-- | Wrapper.
intersect2PL l1 l2 = intersectionOfProjectiveLines l1 l2

-- | A typed meet function. the meeting of two lines is a point.
-- Kept separate from intersectionOfProjectiveLines for verification reasons.
meetOfProjectiveLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ProjectivePoint, (PLine2Err, PLine2Err, ([ErrVal],[ErrVal])))
meetOfProjectiveLines line1 line2 = (PPoint2 res,
                                            (npl1Err,
                                             npl2Err,
                                             resUnlikeErr))
  where
    (res, resUnlikeErr) = lv1 ⎤+ lv2
    lv1 = vecOfL $ forceBasisOfL npl1
    lv2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeL line1
    (npl2, npl2Err) = normalizeL line2

-- | Normalize a Projective Line.
normalizeProjectiveLine :: (ProjectiveLine2 a) => a -> (ProjectiveLine, PLine2Err)
normalizeProjectiveLine line = (res, resErr)
  where
    (res, resErr) = case norm of
                      1.0 -> (NPLine2 vec      , normErr)
                      _   -> (NPLine2 scaledVec, normErr <> PLine2Err mempty scaledVecErrs mempty mempty mempty mempty)
    (scaledVec, scaledVecErrs) = divVecScalarWithErr vec norm
    (norm, normErr) = normOfL line
    vec = vecOfL line

-- | Find the norm of a given Projective Line.
-- FIXME: should we be placing this error in the PLine2Err? it doesn't effect resolving the line...
normOfProjectiveLine, normOfL :: (ProjectiveLine2 a) => a -> (ℝ, PLine2Err)
-- | Actual implementation.
normOfProjectiveLine line = (res, resErr)
  where
    (res, resErr) = case sqNormOfPLine2 of
                      1.0 -> (1.0                , PLine2Err mempty mempty mempty sqNormUlp mempty mempty)
                      _   -> (sqrt sqNormOfPLine2, PLine2Err mempty mempty rawResUlp sqNormUlp mempty mempty)
    rawRes = sqrt sqNormOfPLine2
    rawResUlp = UlpSum (abs $ realToFrac $ doubleUlp rawRes)
    (sqNormOfPLine2, sqNormUlp) = sqNormOfL line
-- | Wrapper.
normOfL l = normOfProjectiveLine l

-- | Find the squared norm of a given Projective Line.
squaredNormOfProjectiveLine, sqNormOfL :: (ProjectiveLine2 a) => a -> (ℝ, UlpSum)
-- | Actual implementation.
squaredNormOfProjectiveLine line = (res, ulpTotal)
  where
    res = a*a+b*b
    a = valOf 0 $ getVal [GEPlus 1] vals
    b = valOf 0 $ getVal [GEPlus 2] vals
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ a*a)
               + abs (realToFrac $ doubleUlp $ b*b)
               + abs (realToFrac $ doubleUlp res)
    (GVec vals) = vecOfL line
-- | Wrapper.
sqNormOfL l = squaredNormOfProjectiveLine l

-- | Translate a line a given distance along it's perpendicular bisector.
-- Uses the property that translation of a line is expressed on the GEZero component.
translateProjectiveLine, translateL :: (ProjectiveLine2 a) => a -> ℝ -> (ProjectiveLine, PLine2Err)
-- | Actual implementation.
translateProjectiveLine line d = (PLine2 res, normErr <> PLine2Err resErrs mempty mempty mempty tUlp mempty)
  where
    (res, resErrs) = addVecPairWithErr m $ vecOfL line
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    -- the amount to add to the GEZero 1 component.
    tAdd = d * norm
    tUlp = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    (norm, normErr) = normOfL line
-- | Wrapper.
translateL l d = translateProjectiveLine l d

-----------------------------------------
--- Projective Line Error Calculation ---
-----------------------------------------

-- | When given a projective line, return the maximum distance between a projective point known to be on the line and the equivalent point on the 'real' line, which is to say, the projective line without floating point error.
-- FIXME: accept a error on the projectivePoint, and return an error estimate.
pLineErrAtPPoint :: (ProjectiveLine2 a, ProjectivePoint2 b) => (a, PLine2Err) -> b -> UlpSum
pLineErrAtPPoint (line, lineErr) errPoint
  -- Both intercepts are real. This line is not parallel or collinear to X or Y axises, and does not pass through the origin.
  | xInterceptIsRight && yInterceptIsRight = tFuzz <> xInterceptFuzz <> yInterceptFuzz
  -- Only the xIntercept is real. This line is parallel to the Y axis.
  | xInterceptIsRight = tFuzz <> rawXInterceptFuzz
  -- Only the yIntercept is real. This line is parallel to the X axis.
  | yInterceptIsRight = tFuzz <> rawYInterceptFuzz
  -- This line passes through the origin (0,0).
  | otherwise = tFuzz
  where
    xInterceptIsRight = isJust (xIntercept (nPLine, nPLineErr))
                        && isRight (fst $ fromJust $ xIntercept (nPLine, nPLineErr))
                        && fromRight 0 (fst $ fromJust $ xIntercept (nPLine, nPLineErr)) /= 0
    yInterceptIsRight = isJust (yIntercept (nPLine, nPLineErr))
                        && isRight (fst $ fromJust $ yIntercept (nPLine, nPLineErr))
                        && fromRight 0 (fst $ fromJust $ yIntercept (nPLine, nPLineErr)) /= 0
    xInterceptFuzz = UlpSum $ ulpVal rawXInterceptFuzz * ((realToFrac xInterceptDistance + ulpVal rawXInterceptFuzz) / realToFrac yInterceptDistance) * realToFrac (abs xPos)
    yInterceptFuzz = UlpSum $ ulpVal rawYInterceptFuzz * ((realToFrac yInterceptDistance + ulpVal rawYInterceptFuzz) / realToFrac xInterceptDistance) * realToFrac (abs yPos)
    rawYInterceptFuzz = snd $ fromJust $ yIntercept (nPLine, nPLineErr)
    rawXInterceptFuzz = snd $ fromJust $ xIntercept (nPLine, nPLineErr)
    yInterceptDistance = fromRight 0 $ fst $ fromJust $ xIntercept (nPLine, nPLineErr)
    xInterceptDistance = fromRight 0 $ fst $ fromJust $ xIntercept (nPLine, nPLineErr)
    tFuzz = fuzzinessOfL (nPLine, nPLineErr)
    -- FIXME: collect this error, and take it into account.
    (Point2 (xPos,yPos),_) = pToEP errPoint
    nPLineErr = nPLineErrRaw <> lineErr
    (nPLine, nPLineErrRaw) = normalizeL line

-- | Find the point that a given line crosses the X axis.
xIntercept :: (ProjectiveLine2 a) => (a, PLine2Err) -> Maybe (Either a ℝ, UlpSum)
xIntercept (line, lineErr)
  -- Handle a line that is parallel to the X axis.
  | isNothing rawX = Nothing
  -- Use X and T to calculate our answer.
  | isJust rawT = Just (Right xDivRes, xDivErr)
  -- This line is (anti)colinear with the X axis.
  | isNothing rawY = Just (Left line, mempty)
  -- We have an X and a Y, but no T component? This line passes through the origin.
  | isNothing rawT = Just (Right 0, mempty)
  | otherwise = error "totality failure: we should never get here."
  where
    -- negate is required, because the result is inverted.
    xDivRes = negate $ valOf 0 rawT / valOf 0 rawX
    xDivErr = rawXErr <> rawTErr <> UlpSum (abs $ realToFrac $ doubleUlp xDivRes)
    rawTErr = eValOf mempty (getVal [GEZero 1] lAddErr)
           <> eValOf mempty (getVal [GEZero 1] lNormalizeErr)
           <> eValOf mempty (getVal [GEZero 1] lJoinAddErr)
           <> eValOf mempty (getVal [GEZero 1] lJoinMulErr)
    rawXErr = eValOf mempty (getVal [GEPlus 1] lAddErr)
           <> eValOf mempty (getVal [GEPlus 1] lNormalizeErr)
           <> eValOf mempty (getVal [GEPlus 1] lJoinAddErr)
           <> eValOf mempty (getVal [GEPlus 1] lJoinMulErr)
    (PLine2Err lAddErr lNormalizeErr _ _ _ (lJoinMulErr, lJoinAddErr)) = lineErr
    rawT = getVal [GEZero 1] pLineVals
    rawX = getVal [GEPlus 1] pLineVals
    rawY = getVal [GEPlus 2] pLineVals
    (GVec pLineVals) = vecOfL line

-- | Find the point that a given line crosses the Y axis.
yIntercept :: (ProjectiveLine2 a) => (a, PLine2Err) -> Maybe (Either a ℝ, UlpSum)
yIntercept (line, lineErr)
  -- Handle a line that is parallel to the Y axis.
  | isNothing rawY = Nothing
  -- Use Y and T to calculate our answer.
  | isJust rawT = Just $ (Right yDivRes, yDivErr)
  -- This line is (anti)colinear with the Y axis.
  | isNothing rawX = Just (Left line, mempty)
  -- We have an X and a Y, but no T component? This line passes through the origin.
  | isNothing rawT = Just (Right 0, mempty)
  | otherwise = error "totality failure: we should never get here."
  where
    -- negate is required, because the result is inverted.
    yDivRes = negate $ valOf 0 rawT / valOf 0 rawY
    yDivErr = rawYErr <> rawTErr <> UlpSum (abs $ realToFrac $ doubleUlp yDivRes)
    rawTErr = eValOf mempty (getVal [GEZero 1] lAddErr)
           <> eValOf mempty (getVal [GEZero 1] lNormalizeErr)
           <> eValOf mempty (getVal [GEZero 1] lJoinAddErr)
           <> eValOf mempty (getVal [GEZero 1] lJoinMulErr)
    rawYErr = eValOf mempty (getVal [GEPlus 2] lAddErr)
           <> eValOf mempty (getVal [GEPlus 2] lNormalizeErr)
           <> eValOf mempty (getVal [GEPlus 2] lJoinAddErr)
           <> eValOf mempty (getVal [GEPlus 2] lJoinMulErr)
    (PLine2Err lAddErr lNormalizeErr _ _ _ (lJoinMulErr, lJoinAddErr)) = lineErr
    rawT = getVal [GEZero 1] pLineVals
    rawX = getVal [GEPlus 1] pLineVals
    rawY = getVal [GEPlus 2] pLineVals
    (GVec pLineVals) = vecOfL line

---------------------------------------
--- Projective Line Mixed Functions ---
---------------------------------------
{- functions that are part of the projective line API, but use parts of the Projective Point API. -}

-- | Find the cosine of the angle between the two lines, along with the error.
-- Results in a value that is ~+1 when the first line points to the "left" of the second given line, and ~-1 when pointing "right".
angleCosBetweenProjectiveLines, angleCosBetween2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
-- | Actual implementation.
-- FIXME: the sum of iPointErrVals is not +/- radians.
-- FIXME: lots of places for precision related error here, that are not recorded or reported.
-- FIXME: what was the older method we used here? perhaps it has less opportunities for fuzziness?
angleCosBetweenProjectiveLines line1 line2
  | isNothing canonicalizedIntersection = (0, mempty)
  | otherwise = (angle, (npl1Err, npl2Err, sumPPointErrs iPointErrVals))
  where
    angle = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
    (CPPoint2 iPointVec, (npl1Err, npl2Err, PPoint2Err _ iPointErrVals _ _ _ _ _)) = fromJust canonicalizedIntersection
    motor                     = addVecPairWithoutErr (lvec1 • gaI) (GVec [GVal 1 (singleton G0)])
    antiMotor                 = addVecPairWithoutErr (lvec1 • gaI) (GVec [GVal (-1) (singleton G0)])
    canonicalizedIntersection = canonicalizedIntersectionOf2PL line1 line2
    -- I, the infinite point.
    gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    lvec1 = vecOfL $ forceBasisOfL line1
    lvec2 = vecOfL $ forceBasisOfL line2
-- | Wrapper.
angleCosBetween2PL l1 l2 = angleCosBetweenProjectiveLines l1 l2

-- | Get the canonicalized intersection of two lines.
-- NOTE: Returns Nothing when the lines are (anti)parallel.
canonicalizedIntersectionOfProjectiveLines, canonicalizedIntersectionOf2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Maybe (ProjectivePoint, (PLine2Err, PLine2Err, PPoint2Err))
-- | Actual implementation.
canonicalizedIntersectionOfProjectiveLines line1 line2
  -- | Check whether the result of our intersection returns an ideal point. if it does, it means the two lines are (anti)parallel, and we should fail.
  | isIdealP pp1 = Nothing
  | otherwise = Just (cpp1, (l1Err, l2Err, pp1Err <> cpp1Err))
  where
    (cpp1, cpp1Err) = canonicalizeP pp1
    (pp1, (l1Err, l2Err, pp1Err)) = intersect2PL line1 line2
-- | Wrapper.
canonicalizedIntersectionOf2PL l1 l2 = canonicalizedIntersectionOfProjectiveLines l1 l2

--------------------------------
--- Projective Point Support ---
--------------------------------

data ProjectivePoint =
  PPoint2 GVec
  | CPPoint2 GVec
  deriving (Ord, Generic, NFData, Show)

instance Eq ProjectivePoint where
  (==) (CPPoint2 a1) (CPPoint2 a2) = a1 == a2
  (==) p1@(PPoint2 a1) p2@(PPoint2 a2) = a1 == a2 || fst (canonicalizeP p1) == fst (canonicalizeP p2)
  (==) p1 p2 = fst (canonicalizeP p1) == fst (canonicalizeP p2)

-- | The error accumulated when calculating a projective point.
data PPoint2Err =
  PPoint2Err
    -- MeetErr. max error amounts while meeting two PLines to find this point. divided into add err, and multiply err.
    ([ErrVal], [ErrVal])
    -- CanonicalizeErr. error caused by the divide used during canonicalization of a point.
    [ErrVal]
    -- AddErr. error created when adding two points to create a third point.
    [ErrVal]
    -- WeighedStartErr. error created when scaling one of the two input points used to create this point.
    [ErrVal]
    -- WeighedStopErr. error created when scaling one of the two input points used to create this point.
    [ErrVal]
    -- angle between the two input lines, when we generate this point via intersecting two lines.
    ℝ
    -- angleUnlikeErr - the error of the unlike operation that generates the angle in the previous field.
    ([ErrVal], [ErrVal])
  deriving (Eq, Show)

instance Semigroup PPoint2Err where
  (<>) (PPoint2Err (a1,b1) c1 d1 e1 f1 g1 (h1,i1)) (PPoint2Err (a2,b2) c2 d2 e2 f2 g2 (h2,i2)) =
    PPoint2Err
      (foldl' addErr a1 a2, foldl' addErr b1 b2)
      (foldl' addErr c1 c2)
      (foldl' addErr d1 d2)
      (foldl' addErr e1 e2)
      (foldl' addErr f1 f2)
      (g1 <> g2)
      (foldl' addErr h1 h2, foldl' addErr i1 i2)

instance Monoid PPoint2Err where
  mempty = PPoint2Err mempty mempty mempty mempty mempty mempty mempty

class (Show a) => ProjectivePoint2 a where
  canonicalizeP :: a -> (ProjectivePoint, PPoint2Err)
  consLikeP :: a -> (GVec -> a)
  isIdealP :: a -> Bool
  vecOfP :: a -> GVec

instance ProjectivePoint2 ProjectivePoint where
  canonicalizeP p = case p of
                     (CPPoint2 _) -> (p, mempty)
                     _ -> canonicalizeProjectivePoint p
  consLikeP p = case p of
                  (CPPoint2 _) -> CPPoint2
                  (PPoint2 _) -> PPoint2
  isIdealP p = case p of
                 (CPPoint2 _) -> False
                 (PPoint2 _) -> projectivePointIsIdeal p
  vecOfP p = case p of
               (CPPoint2 v) -> v
               (PPoint2 v) -> v

-- | Canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
canonicalizeProjectivePoint :: (ProjectivePoint2 a) => a -> (ProjectivePoint, PPoint2Err)
canonicalizeProjectivePoint point
  | isIdealP point = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- | Handle the ID case. The passed in point is canonicalized already.
  | valOf 1 foundVal == 1 = (CPPoint2 $ GVec rawVals, mempty)
  | otherwise = (res, PPoint2Err mempty scaledErrs mempty mempty mempty mempty mempty)
  where
    res = CPPoint2 $ GVec $ foldl' addValWithoutErr [GVal 1 (fromList [GEPlus 1, GEPlus 2])] scaledVals
    (GVec scaledVals, scaledErrs) = divVecScalarWithErr newVec $ valOf 1 foundVal
    newVec = GVec [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])
                  ,GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2])]
    foundVal = getVal [GEPlus 1, GEPlus 2] rawVals
    (GVec rawVals) = vecOfP point

-- | Find the distance between two projective points, and the error component of the result.
distanceBetweenProjectivePoints :: (ProjectivePoint2 a, ProjectivePoint2 b) => (a, PPoint2Err) -> (b, PPoint2Err) -> (ℝ, (PPoint2Err, PPoint2Err, PLine2Err, UlpSum))
distanceBetweenProjectivePoints (point1, point1Err) (point2, point2Err)
  | cPoint1 == cPoint2 = (0, (cPoint1Err, cPoint2Err, mempty, mempty))
  | otherwise = (res, resErr)
  where
    resErr = (cPoint1Err, cPoint2Err, newPLineErr, ulpSum)
    -- FIXME: missing the component for normErr?
    ulpSum = fuzzinessOfP (cPoint1, point1Err <> cPoint1Err) <> fuzzinessOfP (cPoint2, point2Err <> cPoint2Err) <> fuzzinessOfL (newPLine, newPLineErr)
    -- FIXME: should it be fuzzinessOfL's job to determine how error in normErr effects ... line resolution?
    newPLineErr = newPLineErrRaw <> normErr
    -- FIXME: how does the error in newPLine effect the found norm here?
    (res, normErr) = normOfL newPLine
    (newPLine, (_, _, newPLineErrRaw)) = join2PP cPoint1 cPoint2
    (cPoint1, cPoint1Err) = canonicalizeP point1
    (cPoint2, cPoint2Err) = canonicalizeP point2

-- | A wrapper for the above function, that removes error quotents that are not directly related to the input or result.
distance2PP :: (ProjectivePoint2 a, ProjectivePoint2 b) => (a, PPoint2Err) -> (b, PPoint2Err) -> (ℝ, (PPoint2Err, PPoint2Err, UlpSum))
distance2PP p1 p2 = crushErr $ distanceBetweenProjectivePoints p1 p2
  where
    crushErr (res, (c1, c2, _, resErr)) = (res, (c1, c2, resErr))

-- | Ensure all of the '0' components exist on a Projective Point. This is to ensure like, unlike, and reductive work properly.
forceProjectivePointBasis, forceBasisOfP :: (ProjectivePoint2 a) => a -> a
-- | Actual implementation.
forceProjectivePointBasis point
  | gnums == Just [fromList [GEZero 1, GEPlus 1],
                   fromList [GEZero 1, GEPlus 2],
                   fromList [GEPlus 1, GEPlus 2]] = point
  | otherwise = (consLikeP point) res
  where
    res = forceBasis [fromList [GEZero 1, GEPlus 1], fromList [GEZero 1, GEPlus 2], fromList [GEPlus 1, GEPlus 2]] vec
    gnums = case vals of
              [GVal _ g1, GVal _ g2, GVal _ g3] -> Just [g1,g2,g3]
              _                                 -> Nothing
    vec@(GVec vals) = vecOfP point
-- | Wrapper.
forceBasisOfP p = forceProjectivePointBasis p

-- | Find the idealized norm of a projective point (ideal or not).
idealNormOfProjectivePoint, idealNormOfP :: (ProjectivePoint2 a) => a -> (ℝ, UlpSum)
-- | Actual implementation.
idealNormOfProjectivePoint point
  | preRes == 0 = (0, mempty)
  | otherwise   = (res, ulpTotal)
  where
    res = sqrt preRes
    preRes = x*x+y*y
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ x*x)
               + abs (realToFrac $ doubleUlp $ y*y)
               + abs (realToFrac $ doubleUlp preRes)
               + abs (realToFrac $ doubleUlp res)
    (x,y)
     | e12Val == 0 = ( negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals
                     ,          valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals)
     | otherwise = (\(Point2 (x1,y1),_) -> (x1,y1)) $ pToEP point
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)
    (GVec rawVals) = vecOfP point
-- | Wrapper.
idealNormOfP p = idealNormOfProjectivePoint p

-- | Join two points, returning the line that connects them.
joinOfProjectivePoints, join2PP :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> (ProjectiveLine, (PPoint2Err, PPoint2Err, PLine2Err))
-- | Actual implementation.
joinOfProjectivePoints point1 point2 = (PLine2 res,
                                        (cPoint1Err, cPoint2Err, PLine2Err mempty mempty mempty mempty mempty resUlp))
  where
    -- FIXME: how does error in canonicalization effect the PLine generated here?
    (res, resUlp) = pv1 ∨+ pv2
    pv1 = vecOfP $ forceBasisOfP cPoint1
    pv2 = vecOfP $ forceBasisOfP cPoint2
    (cPoint1, cPoint1Err) = canonicalizeP point1
    (cPoint2, cPoint2Err) = canonicalizeP point2
-- | Wrapper.
join2PP p1 p2 = joinOfProjectivePoints p1 p2

-- | Find a point along the line between the two given points.
-- The position of the found point is determined by the ratio betwenn the two weights supplied.
-- If the weights are equal, the distance will be right between the two points.
projectivePointBetweenProjectivePoints, interpolate2PP :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> (ProjectivePoint, (PPoint2Err, PPoint2Err, PPoint2Err))
-- | Actual implementation.
projectivePointBetweenProjectivePoints startPoint stopPoint weight1 weight2
  | isIdealP res = error "tried to generate an ideal point?"
  | otherwise = (res, resErr)
  where
    res = PPoint2 rawRes
    resErr = (cStartPointErr, cStopPointErr, PPoint2Err mempty mempty rawResErr weighedStartErr weighedStopErr mempty mempty)
    (rawRes, rawResErr) = addVecPairWithErr weighedStart weighedStop
    (weighedStart, weighedStartErr) = mulScalarVecWithErr weight1 rawStartPoint
    (weighedStop, weighedStopErr) = mulScalarVecWithErr weight2 rawStopPoint
    rawStartPoint = vecOfP cStartPoint
    rawStopPoint = vecOfP cStopPoint
    (cStartPoint, cStartPointErr) = canonicalizeP startPoint
    (cStopPoint, cStopPointErr) = canonicalizeP stopPoint
-- | Wrapper.
interpolate2PP p1 p2 = projectivePointBetweenProjectivePoints p1 p2

-- | Determine if a point is an ideal point.
projectivePointIsIdeal :: (ProjectivePoint2 a) => a -> Bool
projectivePointIsIdeal point = isNothing $ getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) $ vecOfP point

-- | Maybe create a euclidian point from a projective point. Will fail if the projective point is ideal.
projectivePointToEuclidianPoint, pToEP :: (ProjectivePoint2 a) => a -> (Point2, PPoint2Err)
-- | Actual implementation.
projectivePointToEuclidianPoint point
 | projectivePointIsIdeal point = error "Attempted to create an infinite point when trying to convert from a Projective Point to a Euclidian Point."
 | otherwise = (res, resErr)
  where
    res = Point2 (xVal, yVal)
    xVal = negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals
    (CPPoint2 (GVec vals), resErr) = canonicalizeP point
-- | Wrapper.
pToEP p = projectivePointToEuclidianPoint p

------------------------------------------
--- Projective Point Error Calculation ---
------------------------------------------

sumPPointErrs :: [ErrVal] -> UlpSum
sumPPointErrs errs = eValOf mempty (getVal [GEZero 1, GEPlus 1] errs)
                  <> eValOf mempty (getVal [GEZero 1, GEPlus 2] errs)
                  <> eValOf mempty (getVal [GEPlus 1, GEPlus 2] errs)

-- | Determine the amount of error in resolving a projective point. Returns the radius of a circle aroind the given point within which the 'correct' answer should be.
-- FIXME: This 1000 here is completely made up BS.
fuzzinessOfProjectivePoint, fuzzinessOfP :: (ProjectivePoint2 a) => (a, PPoint2Err) -> UlpSum
-- | Actual implementation.
fuzzinessOfProjectivePoint (point, pointErr) = UlpSum $ sumTotal * realToFrac (1+(1000*(abs angleIn + realToFrac (ulpVal $ sumPPointErrs angleUnlikeAddErr <> sumPPointErrs angleUnlikeMulErr))))
  where
    sumTotal = ulpVal $ sumPPointErrs pJoinAddErr
                     <> sumPPointErrs pJoinMulErr
                     <> sumPPointErrs pCanonicalizeErr
                     <> sumPPointErrs pAddErr
                     <> sumPPointErrs pIn1MulErr
                     <> sumPPointErrs pIn2MulErr
    (PPoint2Err (pJoinAddErr, pJoinMulErr) pCanonicalizeErr pAddErr pIn1MulErr pIn2MulErr angleIn (angleUnlikeAddErr,angleUnlikeMulErr)) = cPointErr <> pointErr
    (_, cPointErr) = canonicalizeP point
-- | Wrapper.
fuzzinessOfP p = fuzzinessOfProjectivePoint p
