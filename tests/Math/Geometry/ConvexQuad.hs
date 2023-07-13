{- ORMOLU_DISABLE -}
{- HSlice.
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

{- tests for the properties of a DualRightQuad, or a four sided figgure with two right angles. -}

module Math.Geometry.ConvexQuad (
  convexQuadBrokenSpec,
  convexQuadSpec
  ) where

import Prelude (Bool(True), ($), (.), (<>), (==), (<$>), all, error, otherwise, show)

-- The List library.
import Data.List (concatMap)

-- The Maybe library.
import Data.Maybe (Maybe(Just), fromMaybe)

-- Slists, a form of list with a stated size in the structure.
import Slist.Type (Slist(Slist))

-- Hspec, for writing specs.
import Test.Hspec (describe, Spec, it, Expectation)

-- QuickCheck, for writing properties.
import Test.QuickCheck (property, Positive)

-- The numeric type in HSlice.
import Graphics.Slicer (ℝ)

-- Basic definitions, used in multiple places in the math library.
import Graphics.Slicer.Math.Definitions (Contour, mapWithFollower)

-- Our debugging library, for making the below simpler to read, and drop into command lines.
import Graphics.Slicer.Math.Ganja (dumpGanjas, toGanja)

-- Our 2D Projective Geometric Algebra library.
import Graphics.Slicer.Math.PGA (eToPL, pLineIsLeft)

-- The functions for generating random geometry, for testing purposes.
import Graphics.Slicer.Math.RandomGeometry (Radian(Radian), randomConvexQuad)

-- The part of our library that puts faces onto a contour. faces have one exterior side, and a number of internal sides (defined by Arcs).
import Graphics.Slicer.Math.Skeleton.Face (Face(Face), facesOf)

-- The entry point for getting the straight skeleton of a contour.
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Shared tests, between different geometry.
import Math.Geometry.CommonTests (prop_CanPlaceFaces, prop_FacesHaveThreeToFiveSides, prop_FacesAllWoundLeft, prop_FacesInOrder, prop_HasFourFaces, prop_HasAStraightSkeleton, prop_NodeTreeHasFewerThanFourGenerations, prop_NoDivides, prop_NoMotorcycles, prop_StraightSkeletonHasOneNodeTree)

-- | generates a malformed (concave!) contour.
unit_ConvexQuadFacesAllWoundLeft :: Bool
unit_ConvexQuadFacesAllWoundLeft
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> concatMap (show . faceLefts) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
                     <> dumpGanjas ([toGanja contour]
                                    <> (toGanja <$> (\(Slist a _) -> a) faces))

  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ eToPL edge : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomConvexQuad x y tilt1 tilt2 tilt3 distance1
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 2.0
    tilt2 = Radian 2.0
    tilt3 = Radian 3.2
    distance1 :: Positive ℝ
    distance1 = 1.0

-- | was a misfolded skeleton. fixed, but now generating a miswound face.
{-
       miswound face found:
       [Just True,Just True,Just True][Just True,Just False,Just False,Just True][Just True,Just True,Just True][Just True,Just False,Just False,Just True]
-}
unit_ConvexQuadFacesAllWoundLeft_2 :: Bool
unit_ConvexQuadFacesAllWoundLeft_2
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> concatMap (show . faceLefts) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
                     <> dumpGanjas ([toGanja contour]
                                    <> (toGanja <$> (\(Slist a _) -> a) faces))

  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ eToPL edge : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomConvexQuad x y tilt1 tilt2 tilt3 distance1
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 1.0e-2
    tilt2 = Radian 1.0
    tilt3 = Radian 0.1
    distance1 :: Positive ℝ
    distance1 = 0.1

-- | mis-folded skeleton. fixed.
{-
      ENodes outs should be: [PLine2 (GVec [GVal 0.7321551936758581 (fromList [GEZero (Fastℕ 1)]),GVal 0.9917735953323468 (fromList [GEPlus (Fastℕ 1)]),GVal (-0.18549674514320513) (fromList [GEPlus (Fastℕ 2)])]),PLine2 (GVec [GVal 2.1285450743080947 (fromList [GEZero (Fastℕ 1)]),GVal (-0.2335910588939532) (fromList [GEPlus (Fastℕ 1)]),GVal 1.9360745608276422 (fromList [GEPlus (Fastℕ 2)])]),PLine2 (GVec [GVal 1.4344853529650106e-3 (fromList [GEZero (Fastℕ 1)]),GVal (-7.165585212050712e-3) (fromList [GEPlus (Fastℕ 1)]),GVal 7.19994904783583e-4 (fromList [GEPlus (Fastℕ 2)])]),PLine2 (GVec [GVal (-2.862134753336918) (fromList [GEZero (Fastℕ 1)]),GVal (-0.7510169512263428) (fromList [GEPlus (Fastℕ 1)]),GVal (-1.7512978105892207) (fromList [GEPlus (Fastℕ 2)])])]
       ENode outs are:       [PLine2 (GVec [GVal 0.7321551936758581 (fromList [GEZero (Fastℕ 1)]),GVal 0.9917735953323468 (fromList [GEPlus (Fastℕ 1)]),GVal (-0.18549674514320513) (fromList [GEPlus (Fastℕ 2)])]),PLine2 (GVec [GVal 2.1285450743080947 (fromList [GEZero (Fastℕ 1)]),GVal (-0.2335910588939532) (fromList [GEPlus (Fastℕ 1)]),GVal 1.9360745608276422 (fromList [GEPlus (Fastℕ 2)])]),PLine2 (GVec [GVal (-2.862134753336918) (fromList [GEZero (Fastℕ 1)]),GVal (-0.7510169512263428) (fromList [GEPlus (Fastℕ 1)]),GVal (-1.7512978105892207) (fromList [GEPlus (Fastℕ 2)])])]
-}
unit_ConvexQuadFacesAllWoundLeft_3 :: Bool
unit_ConvexQuadFacesAllWoundLeft_3
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> concatMap (show . faceLefts) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
                     <> dumpGanjas ([toGanja contour]
                                    <> (toGanja <$> (\(Slist a _) -> a) faces))

  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ eToPL edge : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomConvexQuad x y tilt1 tilt2 tilt3 distance1
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 1.0e-2
    tilt2 = Radian 0.1
    tilt3 = Radian 2.0
    distance1 :: Positive ℝ
    distance1 = 1.0

-- | miswound face found.
unit_ConvexQuadFacesAllWoundLeft_4 :: Bool
unit_ConvexQuadFacesAllWoundLeft_4
  | allIsLeft = True
  | otherwise = error $ "miswound face found:\n"
                     <> concatMap (show . faceLefts) faces <> "\n"
                     <> show skeleton <> "\n"
                     <> show faces <> "\n"
                     <> dumpGanjas ([toGanja contour]
                                    <> (toGanja <$> (\(Slist a _) -> a) faces))
                     <> dumpGanjas [toGanja contour, toGanja skeleton]

  where
    allIsLeft = all faceAllIsLeft faces
    faceAllIsLeft face = all (== Just True) $ faceLefts face
    faceLefts (Face edge firstArc (Slist midArcs _) lastArc) = mapWithFollower (\(pl1, _) (pl2, _) -> pLineIsLeft pl1 pl2)  $ eToPL edge : firstArc : midArcs <> [lastArc]
    faces = facesOf skeleton
    skeleton = fromMaybe (error $ show contour) $ findStraightSkeleton contour []
    contour = randomConvexQuad x y tilt1 tilt2 tilt3 distance1
    x,y :: ℝ
    x = 0
    y = 0
    tilt1 = Radian 0.1
    tilt2 = Radian 2.0
    tilt3 = Radian 5.0
    distance1 :: Positive ℝ
    distance1 = 1.0

convexQuadBrokenSpec :: Spec
convexQuadBrokenSpec = do
  describe "Convex Quads" $
    it "each face is wound to the left (unit)"
      unit_ConvexQuadFacesAllWoundLeft

convexQuadSpec :: Spec
convexQuadSpec = do
  describe "Convex Quads" $ do
    it "finds no convex motorcycles" $
      property (expectationFromConvexQuad prop_NoMotorcycles)
    it "finds no divides" $
      property (expectationFromConvexQuad prop_NoDivides)
    it "finds a straight skeleton" $
      property (expectationFromConvexQuad prop_HasAStraightSkeleton)
    it "only finds one nodetree in the straight skeleton" $
      property (expectationFromConvexQuad prop_StraightSkeletonHasOneNodeTree)
    it "generates one, two, or three generations of INodes" $
      property (boolFromConvexQuad prop_NodeTreeHasFewerThanFourGenerations)
    it "can place faces on the straight skeleton" $
      property (expectationFromConvexQuad prop_CanPlaceFaces)
    it "only places four faces" $
      property (expectationFromConvexQuad prop_HasFourFaces)
    it "faces have between three and five sides" $
      property (boolFromConvexQuad prop_FacesHaveThreeToFiveSides)
    it "places faces in the order the line segments were given in" $
      property (expectationFromConvexQuad prop_FacesInOrder)
    it "each face is wound to the left" $
      property (boolFromConvexQuad prop_FacesAllWoundLeft)
    it "each face is wound to the left (unit) (2)"
      unit_ConvexQuadFacesAllWoundLeft_2
    it "each face is wound to the left (unit) (3)"
      unit_ConvexQuadFacesAllWoundLeft_3
    it "each face is wound to the left (unit) (4)"
      unit_ConvexQuadFacesAllWoundLeft_4
  where
    boolFromConvexQuad :: (Contour -> Bool) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Bool
    boolFromConvexQuad f x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner = f convexQuad
      where
        convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner
    expectationFromConvexQuad :: (Contour -> Expectation) -> ℝ -> ℝ -> Radian ℝ -> Radian ℝ -> Radian ℝ -> Positive ℝ -> Expectation
    expectationFromConvexQuad f x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner = f convexQuad
      where
        convexQuad = randomConvexQuad x y rawFirstTilt rawSecondTilt rawThirdTilt rawFirstDistanceToCorner
