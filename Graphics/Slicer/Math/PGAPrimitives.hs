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
-- Primitives here are defined as functions that work on types which have an implementation of the ProjectivePoint2 or ProjectiveLine2 typeclasses.

module Graphics.Slicer.Math.PGAPrimitives
  (
    Arcable(
      errOfOut,
      hasArc,
      outOf
      ),
    CPPoint2(CPPoint2),
    NPLine2(NPLine2),
    PLine2(PLine2),
    PLine2Err(PLine2Err),
    Pointable(
      canPoint,
      ePointOf,
      pPointOf
      ),
    PPoint2(PPoint2),
    PPoint2Err(PPoint2Err),
    ProjectiveLine2(
      angleBetween2PL,
      angleCosBetween2PL,
      flipL,
      forceBasisOfL,
      fuzzinessOfL,
      intersect2PL,
      normalizeL,
      normOfL,
      sqNormOfL,
      translateL,
      vecOfL
      ),
    ProjectivePoint2(
      canonicalize,
      distance2PP,
      forceBasisOfP,
      fuzzinessOfP,
      idealNormOfP,
      interpolate2PP,
      join2PP,
      pToEP,
      vecOfP
      ),
    canonicalizedIntersectionOf2PL,
    pLineErrAtPPoint,
    xIntercept,
    yIntercept
  ) where

import Prelude(Bool, Eq((==),(/=)), Monoid(mempty), Ord, Semigroup((<>)), Show(show), ($), (+), (*), (/), (<$>), (&&), abs, error, filter, fst, negate, otherwise, realToFrac, snd, sqrt)

import Control.DeepSeq (NFData)

import Data.Bits.Floating.Ulp (doubleUlp)

import Data.Either (Either(Left, Right), fromRight, isRight)

import Data.List (foldl', sort)

import Data.Maybe (Maybe(Just,Nothing), fromJust, fromMaybe, isJust, isNothing)

import Data.Set (Set, elems, fromList, singleton)

import GHC.Generics (Generic)

import Graphics.Slicer.Definitions (ℝ)

import Graphics.Slicer.Math.Definitions (Point2(Point2))

import Graphics.Slicer.Math.GeometricAlgebra (ErrVal(ErrVal), GNum(G0, GEPlus, GEZero), GVal(GVal), GVec(GVec), UlpSum(UlpSum), (⎣+), (⎤+), (∧), (•), addErr, addValWithoutErr, addVecPairWithErr, addVecPairWithoutErr, divVecScalarWithErr, eValOf, getVal, mulScalarVecWithErr, scalarPart, sumErrVals, ulpVal, valOf)

--------------------------------
--- common support functions ---
--------------------------------

-- | The join operator in 2D PGA, which is implemented as the meet operator operating in the dual space.
(∨+) :: GVec -> GVec -> (GVec, ([ErrVal], [ErrVal]))
(∨+) a b = (dual2DGVec res
           ,(dual2DErrs unlikeMulErr, dual2DErrs unlikeAddErr))
  where
    (res, (unlikeMulErr, unlikeAddErr)) = dual2DGVec a ⎤+ dual2DGVec b
infixl 9 ∨+

-- | get the dual of a vector. for a point, find a line, for a line, a point...
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

-- | get the dual of the error component of a vector. for a point, find a line, for a line, a point...
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
forceBasis :: [Set GNum] -> GVec -> GVec
forceBasis numsets (GVec vals) = GVec $ forceVal vals <$> sort numsets
  where
    forceVal :: [GVal] -> Set GNum -> GVal
    forceVal has needs = GVal (valOf 0 $ getVal (elems needs) has) needs

-------------------------------
--- Projective Line Support ---
-------------------------------

-- | A projective line in 2D space.
newtype PLine2 = PLine2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | A normalized projective line in 2D space.
newtype NPLine2 = NPLine2 GVec
  deriving (Eq, Generic, NFData, Show)

class ProjectiveLine2 a where
  angleBetween2PL :: (ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
  angleCosBetween2PL :: (ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
  consLikeL :: a -> (GVec -> a)
  flipL :: a -> a
  forceBasisOfL :: a -> a
  fuzzinessOfL :: (a, PLine2Err) -> UlpSum
  intersect2PL :: (ProjectiveLine2 b) => a -> b -> (PPoint2, (PLine2Err, PLine2Err, PPoint2Err))
  normalizeL :: a -> (NPLine2, PLine2Err)
  normOfL :: a -> (ℝ, PLine2Err)
  sqNormOfL :: a -> (ℝ, UlpSum)
  translateL :: a -> ℝ -> (PLine2, UlpSum)
  vecOfL :: a -> GVec

instance ProjectiveLine2 NPLine2 where
  angleBetween2PL l1 l2 = crushErr $ angleBetweenWithErr l1 l2
    where
      crushErr (res, (n1,n2,_,resErr)) = (res, (n1,n2,resErr))
  angleCosBetween2PL l1 l2 = angleCosBetweenProjectiveLines l1 l2
  consLikeL _ = NPLine2
  flipL l = flipProjectiveLine l
  forceBasisOfL l = forceProjectiveLineBasis l
  fuzzinessOfL l = fuzzinessOfProjectiveLine l
  intersect2PL l1 l2 = intersectionOfProjectiveLinesWithErr l1 l2
  normalizeL l = (l, mempty)
  normOfL l = normOfProjectiveLineWithErr l
  sqNormOfL l = squaredNormOfProjectiveLineWithErr l
  translateL l d = (\(r,(PLine2Err _ _ _ _ t _)) -> (r,t)) $ translateProjectiveLineWithErr l d
  vecOfL (NPLine2 v) = v

instance ProjectiveLine2 PLine2 where
  angleBetween2PL l1 l2 = crushErr $ angleBetweenWithErr l1 l2
    where
      crushErr (res, (n1,n2,_,ulpSum)) = (res, (n1,n2,ulpSum))
  angleCosBetween2PL l1 l2 = angleCosBetweenProjectiveLines l1 l2
  consLikeL _ = PLine2
  flipL l = flipProjectiveLine l
  forceBasisOfL l = forceProjectiveLineBasis l
  fuzzinessOfL l = fuzzinessOfProjectiveLine l
  intersect2PL l1 l2 = intersectionOfProjectiveLinesWithErr l1 l2
  normalizeL l = normalizeProjectiveLineWithErr l
  normOfL l = normOfProjectiveLineWithErr l
  sqNormOfL l = squaredNormOfProjectiveLineWithErr l
  translateL l d = (\(r,(PLine2Err _ _ _ _ t _)) -> (r,t)) $ translateProjectiveLineWithErr l d
  vecOfL (PLine2 v) = v

-- | the two types of error of a projective line.
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
-- FIXME: accept, and do something with input error.
angleBetweenWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal]), UlpSum))
angleBetweenWithErr line1 line2 = (scalarPart likeRes, resErr)
  where
    resErr = (npl1Err, npl2Err, (likeMulErr,likeAddErr), ulpSum)
    -- FIXME: this returned ULPsum is wrong. actually try to interpret it.
    ulpSum = sumErrVals likeMulErr <> sumErrVals likeAddErr
    (likeRes, (likeMulErr, likeAddErr)) = l1 ⎣+ l2
    l1 = vecOfL $ forceBasisOfL npl1
    l2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeProjectiveLineWithErr line1
    (npl2, npl2Err) = normalizeProjectiveLineWithErr line2

-- | Reverse a line. same line, but pointed in the other direction.
flipProjectiveLine :: (ProjectiveLine2 a) => a -> a
flipProjectiveLine line = (consLikeL line) rawRes
  where
    rawRes = GVec $ foldl' addValWithoutErr []
             [
               GVal (negate $ valOf 0 $ getVal [GEZero 1] vals) (singleton (GEZero 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 1] vals) (singleton (GEPlus 1))
             , GVal (negate $ valOf 0 $ getVal [GEPlus 2] vals) (singleton (GEPlus 2))
             ]
    (GVec vals) = vecOfL line

-- | runtime basis coersion. ensure all of the '0' components exist on a Projective Line.
forceProjectiveLineBasis :: (ProjectiveLine2 a) => a -> a
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

-- | determine the amount of error when trying to resolve a projective line.
-- NOTE: a projective line's error varies depending where on that line you are trying to resolve it.
--       for complete results, combine this with scaling xIntercept and yIntercept.
fuzzinessOfProjectiveLine :: (ProjectiveLine2 a) => (a, PLine2Err) -> UlpSum
fuzzinessOfProjectiveLine (inPLine, inPLineErr) = tUlp <> joinAddTErr <> joinMulTErr <> normalizeTErr <> additionTErr
  where
    (PLine2Err additionErr normalizeErr _ _ tUlp (joinMulErr, joinAddErr)) = inPLineErr <> normalizeErrRaw
    additionTErr = eValOf mempty (getVal [GEZero 1] additionErr)
    normalizeTErr = eValOf mempty (getVal [GEZero 1] normalizeErr)
    joinMulTErr = eValOf mempty (getVal [GEZero 1] joinMulErr)
    joinAddTErr = eValOf mempty (getVal [GEZero 1] joinAddErr)
    (_,normalizeErrRaw) = normalizeL inPLine

-- | Find out where two lines intersect, returning a projective point, and the error quotent.
-- Note that this should only be used when you can guarantee these are not collinear, or parallel.
intersectionOfProjectiveLinesWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PPoint2, (PLine2Err, PLine2Err, PPoint2Err))
intersectionOfProjectiveLinesWithErr line1 line2 = (res,
                                                    (pl1ResErr,
                                                     pl2ResErr,
                                                     PPoint2Err resErrs mempty mempty mempty mempty iAngleErr iAngleUnlikeErr))
  where
    -- Since the angle of intersection has an effect on how well this point was resolved, save it with the point.
    (iAngleErr,(_,_,iAngleUnlikeErr,_)) = angleBetweenWithErr line1 line2
    (res, (pl1ResErr, pl2ResErr, resErrs)) = meetOfProjectiveLinesWithErr line1 line2

-- | A typed meet function. the meeting of two lines is a point.
-- kept separate from pLineIntersection for verification reasons.
meetOfProjectiveLinesWithErr :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (PPoint2, (PLine2Err, PLine2Err, ([ErrVal], [ErrVal])))
meetOfProjectiveLinesWithErr line1 line2 = (PPoint2 res,
                                           (npl1Err,
                                            npl2Err,
                                            resUnlikeErr))
  where
    (res, resUnlikeErr) = pv1 ⎤+ pv2
    pv1 = vecOfL $ forceBasisOfL npl1
    pv2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeProjectiveLineWithErr line1
    (npl2, npl2Err) = normalizeProjectiveLineWithErr line2

-- | Normalize a Projective Line.
normalizeProjectiveLineWithErr :: (ProjectiveLine2 a) => a -> (NPLine2, PLine2Err)
normalizeProjectiveLineWithErr line = (res, resErr)
  where
    (res, resErr) = case norm of
                      1.0 -> (NPLine2 vec      , normErr)
                      _   -> (NPLine2 scaledVec, normErr <> PLine2Err mempty scaledVecErrs mempty mempty mempty mempty)
    (scaledVec, scaledVecErrs) = divVecScalarWithErr vec norm
    (norm, normErr) = normOfL line
    vec = vecOfL line

-- | Find the norm of a given Projective Line.
normOfProjectiveLineWithErr :: (ProjectiveLine2 a) => a -> (ℝ, PLine2Err)
normOfProjectiveLineWithErr line = (res, resErr)
  where
    (res, resErr) = case sqNormOfPLine2 of
                      1.0 -> (1.0                , PLine2Err mempty mempty mempty sqNormUlp mempty mempty)
                      _   -> (sqrt sqNormOfPLine2, PLine2Err mempty mempty rawResUlp sqNormUlp mempty mempty)
    rawRes = sqrt sqNormOfPLine2
    rawResUlp = UlpSum (abs $ realToFrac $ doubleUlp rawRes)
    (sqNormOfPLine2, sqNormUlp) = sqNormOfL line

-- | Find the squared norm of a given Projective Line.
squaredNormOfProjectiveLineWithErr :: (ProjectiveLine2 a) => a -> (ℝ, UlpSum)
squaredNormOfProjectiveLineWithErr line = (res, ulpTotal)
  where
    res = a*a+b*b
    a = valOf 0 $ getVal [GEPlus 1] vals
    b = valOf 0 $ getVal [GEPlus 2] vals
    ulpTotal = UlpSum
               $ abs (realToFrac $ doubleUlp $ a*a)
               + abs (realToFrac $ doubleUlp $ b*b)
               + abs (realToFrac $ doubleUlp res)
    (GVec vals) = vecOfL line

-- | Translate a line a given distance along it's perpendicular bisector.
-- Uses the property that translation of a line is expressed on the GEZero component.
translateProjectiveLineWithErr :: (ProjectiveLine2 a) => a -> ℝ -> (PLine2, PLine2Err)
translateProjectiveLineWithErr line d = (PLine2 res, normErr <> PLine2Err resErrs mempty mempty mempty tUlp mempty)
  where
    (res, resErrs) = addVecPairWithErr m $ vecOfL line
    m = GVec [GVal tAdd (singleton (GEZero 1))]
    -- the amount to add to the GEZero 1 component.
    tAdd = d * norm
    tUlp = UlpSum $ abs $ realToFrac $ doubleUlp tAdd
    (norm, normErr) = normOfL line

-----------------------------------------
--- Projective Line Error Calculation ---
-----------------------------------------

-- | when given a PLine, and two points guaranteed to be on it, return the maximum distance between a given projective point known to be on the PLine and the 'real' line.
-- FIXME: accept a error on the projectivePoint, and return an error estimate.
pLineErrAtPPoint :: (ProjectiveLine2 a, ProjectivePoint2 b) => (a, PLine2Err) -> b -> UlpSum
pLineErrAtPPoint (inPLine, inPLineErr) errPoint
  -- both intercepts are real. this line is not parallel or collinear to X or Y axises, and does not pass through the origin.
  | xInterceptIsRight && yInterceptIsRight = tFuzz <> xInterceptFuzz <> yInterceptFuzz
  -- only the xIntercept is real. this line is parallel to the Y axis.
  | xInterceptIsRight = tFuzz <> rawXInterceptFuzz
  -- only the yIntercept is real. this line is parallel to the X axis.
  | yInterceptIsRight = tFuzz <> rawYInterceptFuzz
  -- passes through the origin?
  | otherwise = tFuzz
  where
    xInterceptIsRight =    isJust (xIntercept (nPLine,nPLineErr))
                        && isRight (fst $ fromJust $ xIntercept (nPLine,nPLineErr))
                        && fromRight 0 (fst $ fromJust $ xIntercept (nPLine, nPLineErr)) /= 0
    yInterceptIsRight =    isJust (yIntercept (nPLine,nPLineErr))
                        && isRight (fst $ fromJust $ yIntercept (nPLine,nPLineErr))
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
    nPLineErr = nPLineErrRaw <> inPLineErr
    (nPLine, nPLineErrRaw) = normalizeL inPLine

-- | find the approximate point that a given line crosses the X axis.
xIntercept :: (ProjectiveLine2 a) => (a, PLine2Err) -> Maybe (Either a ℝ, UlpSum)
xIntercept (line, lineErr)
  -- handle a line that is parallel to the X axis.
  | isNothing rawX = Nothing
  -- use X and T to calculate our answer
  | isJust rawT = Just (Right xDivRes, xDivErr)
  -- is colinear with the X axis.
  | isNothing rawY = Just (Left line, mempty)
  -- we have an X and a Y, but no T? this line passes through the origin.
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

-- | find the point that a given line crosses the Y axis.
yIntercept :: (ProjectiveLine2 a) => (a, PLine2Err) -> Maybe (Either a ℝ, UlpSum)
yIntercept (line, lineErr)
  -- handle a line that is parallel to the Y axis.
  | isNothing rawY = Nothing
  -- use Y and T to calculate our answer
  | isJust rawT = Just $ (Right yDivRes, yDivErr)
  -- is along the Y axis.
  | isNothing rawX = Just (Left line, mempty)
  -- we have an X and a Y? this line passes through the origin.
  | isNothing rawT = Just (Right 0, mempty)
  | otherwise = error "we should never get here"
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
-- FIXME: iPointErr is not +/- radians.
-- FIXME: lots of places for precision related error here, that are not recorded or reported.
-- FIXME: does not accept input imprecision.
angleCosBetweenProjectiveLines :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> (ℝ, (PLine2Err, PLine2Err, UlpSum))
angleCosBetweenProjectiveLines line1 line2
  | isNothing canonicalizedIntersection = (0, mempty)
  | otherwise = (angle, (npl1Err, npl2Err, sumPPointErrs iPointErr))
  where
    angle = valOf 0 $ getVal [GEZero 1, GEPlus 1, GEPlus 2] $ (\(GVec a) -> a) $ lvec2 ∧ (motor • iPointVec • antiMotor)
    (CPPoint2 iPointVec, (_,_,PPoint2Err _ iPointErr _ _ _ _ _)) = fromJust canonicalizedIntersection
    motor                     = addVecPairWithoutErr (lvec1 • gaI) (GVec [GVal 1 (singleton G0)])
    antiMotor                 = addVecPairWithoutErr (lvec1 • gaI) (GVec [GVal (-1) (singleton G0)])
    canonicalizedIntersection = canonicalizedIntersectionOf2PL (NPLine2 lvec1) (NPLine2 lvec2)
    -- I, the infinite point.
    gaI = GVec [GVal 1 (fromList [GEZero 1, GEPlus 1, GEPlus 2])]
    lvec1 = vecOfL $ forceBasisOfL npl1
    lvec2 = vecOfL $ forceBasisOfL npl2
    (npl1, npl1Err) = normalizeProjectiveLineWithErr line1
    (npl2, npl2Err) = normalizeProjectiveLineWithErr line2

-- | get the Canonicalized intersection of two lines.
-- NOTE: Returns Nothing when the PLines are (anti)parallel.
canonicalizedIntersectionOf2PL :: (ProjectiveLine2 a, ProjectiveLine2 b) => a -> b -> Maybe (CPPoint2, (PLine2Err, PLine2Err, PPoint2Err))
canonicalizedIntersectionOf2PL l1 l2
  | isNothing foundVal = Nothing
  | otherwise = Just (cpp1, (l1Err, l2Err, pp1Err <> cpp1Err))
  where
    (cpp1, cpp1Err) = canonicalize pp1
    (pp1, (l1Err,l2Err,pp1Err)) = intersect2PL l1 l2
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) $ vecOfP pp1

--------------------------------
--- Projective Point Support ---
--------------------------------

-- | A projective point in 2D space.
newtype PPoint2 = PPoint2 GVec
  deriving (Eq, Ord, Generic, NFData, Show)

-- | A canonicalized projective point in 2D space.
newtype CPPoint2 = CPPoint2 GVec
  deriving (Eq, Generic, NFData, Show)

-- | the error accumulated when calculating a projective point.
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

-- | typeclass for points and projective points.
class Pointable a where
  -- | Can this node be resolved into a point in 2d space?
  canPoint :: a -> Bool
  -- | get a projective representation of this point.
  pPointOf :: a -> PPoint2
  -- | get a euclidian representation of this point.
  ePointOf :: a -> Point2

-- | does this node have an output (resulting) pLine?
class Arcable a where
  errOfOut :: a -> PLine2Err
  hasArc :: a -> Bool
  outOf :: a -> PLine2

class (Show a) => ProjectivePoint2 a where
  canonicalize :: a -> (CPPoint2, PPoint2Err)
  consLikeP :: a -> (GVec -> a)
  distance2PP :: (ProjectivePoint2 b) => a -> b -> (ℝ, UlpSum)
  forceBasisOfP :: a -> a
  fuzzinessOfP :: (a, PPoint2Err) -> UlpSum
  idealNormOfP :: a -> (ℝ, UlpSum)
  interpolate2PP :: (ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> (PPoint2, UlpSum)
  join2PP :: (ProjectivePoint2 b) => a -> b -> (PLine2, PLine2Err)
  pToEP :: a -> (Point2, UlpSum)
  vecOfP :: a -> GVec

instance ProjectivePoint2 PPoint2 where
  canonicalize p = canonicalizePPoint2WithErr p
  consLikeP (PPoint2 _) = PPoint2
  distance2PP p1 p2 = crushErr $ distanceBetweenProjectivePointsWithErr p1 p2
    where
      crushErr (a,(_,_,_,b)) = (a,b)
  forceBasisOfP a = forceProjectivePointBasis a
  fuzzinessOfP a = pPointFuzziness a
  idealNormOfP a = idealNormPPoint2WithErr a
  interpolate2PP p1 p2 = pPointBetweenPPointsWithErr p1 p2
  join2PP a b = crushErr $ join2ProjectivePointsWithErr a b
    where
      crushErr (res, (_,_,resErr)) = (res, resErr)
  pToEP p = crushErr $ fromMaybe (error "Attempted to create an infinite point when trying to convert from a Projective Point to a Euclidian Point.") $ projectivePointToPoint2 p
    where
      crushErr (res, PPoint2Err _ cp1Errs _ _ _ _ _) = (res, sumErrVals cp1Errs)
  vecOfP (PPoint2 a) = a

instance ProjectivePoint2 CPPoint2 where
  canonicalize p = (p, mempty)
  consLikeP (CPPoint2 _) = CPPoint2
  distance2PP p1 p2 = crushErr $ distanceBetweenProjectivePointsWithErr p1 p2
    where
      crushErr (a,(_,_,_,b)) = (a,b)
  forceBasisOfP p = forceProjectivePointBasis p
  fuzzinessOfP a = pPointFuzziness a
  idealNormOfP p = idealNormPPoint2WithErr p
  interpolate2PP p1 p2 = pPointBetweenPPointsWithErr p1 p2
  join2PP a b = crushErr $ join2ProjectivePointsWithErr a b
    where
      crushErr (res, (_,_,resErr)) = (res, resErr)
  pToEP p = crushErr $ fromMaybe (error "Attempted to create an infinite point when trying to convert from a Projective Point to a Euclidian Point.") $ projectivePointToPoint2 p
    where
      crushErr (res, PPoint2Err _ cp1Errs _ _ _ _ _) = (res, sumErrVals cp1Errs)
  vecOfP (CPPoint2 v) = v

-- | canonicalize a euclidian point.
-- Note: Normalization of euclidian points in PGA is really just canonicalization.
-- Note: For precision, we go through some work to not bother dividing the GP1,GP2 component with itsself, and just substitute in the answer, as exactly 1.
canonicalizePPoint2WithErr :: (ProjectivePoint2 a) => a -> (CPPoint2, PPoint2Err)
canonicalizePPoint2WithErr point
  | isNothing foundVal = error $ "tried to canonicalize an ideal point: " <> show point <> "\n"
  -- Handle the ID case.
  | valOf 1 foundVal == 1 = (CPPoint2 $ GVec rawVals, mempty)
  | otherwise = (res, PPoint2Err mempty scaledErrs mempty mempty mempty mempty mempty)
  where
    res = CPPoint2 $ GVec $ foldl' addValWithoutErr []
          $  ( if isNothing (getVal [GEZero 1, GEPlus 1] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] scaledVals) (fromList [GEZero 1, GEPlus 1])]
             )
          <> ( if isNothing (getVal [GEZero 1, GEPlus 2] scaledVals)
               then []
               else [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] scaledVals) (fromList [GEZero 1, GEPlus 2])]
             )
          <> [GVal 1 (fromList [GEPlus 1, GEPlus 2])]
    newVec = GVec [GVal (valOf 0 $ getVal [GEZero 1, GEPlus 1] rawVals) (fromList [GEZero 1, GEPlus 1])
                  ,GVal (valOf 0 $ getVal [GEZero 1, GEPlus 2] rawVals) (fromList [GEZero 1, GEPlus 2])]
    (GVec scaledVals, scaledErrs) = divVecScalarWithErr newVec $ valOf 1 foundVal
    (GVec rawVals) = vecOfP point
    foundVal = getVal [GEPlus 1, GEPlus 2] rawVals

distanceBetweenProjectivePointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> (ℝ, (PPoint2Err, PPoint2Err, PLine2Err, UlpSum))
distanceBetweenProjectivePointsWithErr point1 point2 = (res, (cPoint1Err, cPoint2Err, resErr <> newPLineErr, ulpTotal))
  where
    ulpTotal = pLineErrAtPPoint (newPLine, newPLineErr) cPoint1
    (res, resErr) = normOfL newPLine
    (newPLine, newPLineErr) = join2PP cPoint1 cPoint2
    (cPoint1, cPoint1Err) = canonicalizePPoint2WithErr point1
    (cPoint2, cPoint2Err) = canonicalizePPoint2WithErr point2

-- | runtime basis coersion. ensure all of the '0' components exist on a Projective Point.
forceProjectivePointBasis :: (ProjectivePoint2 a) => a -> a
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

-- | find the idealized norm of a projective point (ideal or not).
idealNormPPoint2WithErr :: (ProjectivePoint2 a) => a -> (ℝ, UlpSum)
idealNormPPoint2WithErr ppoint
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
     | otherwise = (\(Point2 (x1,y1),_) -> (x1,y1)) $ pToEP ppoint
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)
    (GVec rawVals) = vecOfP ppoint

-- | a typed join function. join two points, returning a line.
join2ProjectivePointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> (PLine2, (PPoint2Err, PPoint2Err, PLine2Err))
join2ProjectivePointsWithErr pp1 pp2 = (PLine2 res,
                                        (cp1Errs, cp2Errs, PLine2Err mempty mempty mempty mempty mempty resUlp))
  where
    (res,resUlp)  = pv1 ∨+ pv2
    pv1 = vecOfP $ forceBasisOfP cp1
    pv2 = vecOfP $ forceBasisOfP cp2
    (cp1, cp1Errs) = canonicalizePPoint2WithErr pp1
    (cp2, cp2Errs) = canonicalizePPoint2WithErr pp2

-- FIXME: automatically raise addVecRes to a CPPoint2 if it turns out to be canonical?
pPointBetweenPPointsWithErr :: (ProjectivePoint2 a, ProjectivePoint2 b) => a -> b -> ℝ -> ℝ -> (PPoint2, UlpSum)
pPointBetweenPPointsWithErr startP stopP weight1 weight2
  | isNothing foundVal = error "tried to generate an ideal point?"
  | otherwise = (PPoint2 addVecRes, ulpSum)
  where
    ulpSum = sumErrVals addVecResErr <> sumErrVals weighedStartErr <> sumErrVals weighedStopErr
    foundVal = getVal [GEPlus 1, GEPlus 2] $ (\(GVec vals) -> vals) addVecRes
    (addVecRes, addVecResErr) = addVecPairWithErr weighedStart weighedStop
    (weighedStart, weighedStartErr) = mulScalarVecWithErr weight1 rawStartPoint
    (weighedStop, weighedStopErr) = mulScalarVecWithErr weight2 rawStopPoint
    rawStartPoint = vecOfP startP'
    rawStopPoint = vecOfP stopP'
    (startP', _) = canonicalize startP
    (stopP', _) = canonicalize stopP

-- | Maybe create a euclidian point from a projective point. Will fail if the projective point is ideal.
projectivePointToPoint2 :: (ProjectivePoint2 a) => a -> Maybe (Point2, PPoint2Err)
projectivePointToPoint2 ppoint
 | e12Val == 0 = Nothing
 | otherwise = Just (Point2 (xVal, yVal), cpErrs)
  where
    (CPPoint2 (GVec vals), cpErrs) = canonicalizePPoint2WithErr ppoint
    xVal = negate $ valOf 0 $ getVal [GEZero 1, GEPlus 2] vals
    yVal =          valOf 0 $ getVal [GEZero 1, GEPlus 1] vals
    e12Val = valOf 0 (getVal [GEPlus 1, GEPlus 2] rawVals)
    (GVec rawVals) = vecOfP ppoint

------------------------------------------
--- Projective Point Error Calculation ---
------------------------------------------

sumPPointErrs :: [ErrVal] -> UlpSum
sumPPointErrs errs = eValOf mempty (getVal [GEZero 1, GEPlus 1] errs)
                     <> eValOf mempty (getVal [GEZero 1, GEPlus 2] errs)
                     <> eValOf mempty (getVal [GEPlus 1, GEPlus 2] errs)

-- | determine the amount of error in resolving a projective point.
-- FIXME: this 1000 is completely made up BS.
pPointFuzziness :: (ProjectivePoint2 a) => (a, PPoint2Err) -> UlpSum
pPointFuzziness (inPPoint, inErr) = UlpSum $ sumTotal * realToFrac (1+(1000*(abs angleIn + realToFrac (ulpVal $ sumPPointErrs angleUnlikeAddErr <> sumPPointErrs angleUnlikeMulErr))))
  where
    sumTotal = ulpVal $ sumPPointErrs pJoinAddErr
                     <> sumPPointErrs pJoinMulErr
                     <> sumPPointErrs pCanonicalizeErr
                     <> sumPPointErrs pAddErr
                     <> sumPPointErrs pIn1MulErr
                     <> sumPPointErrs pIn2MulErr
    (PPoint2Err (pJoinAddErr, pJoinMulErr) pCanonicalizeErr pAddErr pIn1MulErr pIn2MulErr angleIn (angleUnlikeAddErr,angleUnlikeMulErr)) = cpErr <> inErr
    (_, cpErr) = canonicalize inPPoint
