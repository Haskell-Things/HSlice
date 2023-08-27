{- ORMOLU_DISABLE -}
{-
 - Copyright 2020 Julia Longtin, Sandy McGuire
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

module GoldenSpec.Spec (goldenSpec) where

import Prelude (($), (<>), (<$>), error, fst, head, last, sqrt)

import Data.List (concat)

import Data.Maybe (fromMaybe, fromJust)

import GoldenSpec.Util (golden, goldens)

import Test.Hspec (describe, Spec)

import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (makePointContour)

-- A euclidian point.
import Graphics.Slicer.Math.Definitions (Point2(Point2), LineSeg(LineSeg))

import Graphics.Slicer.Math.Ganja (toGanja)

import Graphics.Slicer.Math.RandomGeometry (cellFrom, contoursFrom, remainderFrom, onlyOne)

import Graphics.Slicer.Math.Skeleton.Cells (addNodeTreesAlongDivide, findFirstCellOfContour, findNextCell, findDivisions, getRawNodeTreeOfCell)

import Graphics.Slicer.Math.Skeleton.MotorcycleCells (allMotorcycleCells)

import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

import Graphics.Slicer.Math.Skeleton.Face (facesOf, orderedFacesOf)

import Graphics.Slicer.Math.Skeleton.Line (insetBy, infiniteInset)

goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
  golden "C0-Cell1" $ cellFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c0 []
  golden "C0-Cell1-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-Cell2" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c0 []
  golden "C0-Cell2-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-MotorcycleCell1" $ head $ allMotorcycleCells c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-MotorcycleCell2" $ last $ allMotorcycleCells c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c0 []
  golden "C0-NodeTree" $ addNodeTreesAlongDivide
      (fst $ getRawNodeTreeOfCell (cellFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []))
      (fst $ getRawNodeTreeOfCell (cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c0 $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []))
      (onlyOne $ findDivisions c0 $ fromJust $ crashMotorcycles c0 [])
  golden "C0-Divide" $ onlyOne $ findDivisions c0 $ fromJust $ crashMotorcycles c0 []
  golden "C0-Faces-Default" $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c0 []
  golden "C0-Faces-Ordered" $ orderedFacesOf c0l0 (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 [])
  golden "C1-Cell1" $ cellFrom $ findFirstCellOfContour c1 $ findDivisions c1 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c1 []
  golden "C1-Cell1-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findFirstCellOfContour c1 $ findDivisions c1 $ fromJust $ crashMotorcycles c1 []
  golden "C1-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c1 $ findDivisions c1 $ fromJust $ crashMotorcycles c1 []
  golden "C1-Cell2" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c1 $ findDivisions c1 $ fromJust $ crashMotorcycles c1 []
  golden "C1-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c1 []
  golden "C2-Cell1" $ cellFrom $ findFirstCellOfContour c2 $ findDivisions c2 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c2 []
  golden "C2-Cell1-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findFirstCellOfContour c2 $ findDivisions c2 $ fromJust $ crashMotorcycles c2 []
  golden "C2-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c2 $ findDivisions c2 $ fromJust $ crashMotorcycles c2 []
  golden "C2-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c2 []
  golden "C3-Divide" $ onlyOne $ findDivisions c3 $ fromJust $ crashMotorcycles c3 []
  golden "C3-Cell1" $ cellFrom $ findFirstCellOfContour c3 $ findDivisions c3 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c3 []
  golden "C3-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c3 $ findDivisions c3 $ fromJust $ crashMotorcycles c3 []
  golden "C3-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c3 []
  goldens "C3-Straight_Skeleton_And_Insets" ([ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c3 []] <>
                                              (concat $ (\a -> toGanja <$> a) <$> (infiniteInset 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c3 [])))
  golden "C4-Cell1" $ cellFrom $ findFirstCellOfContour c4 $ findDivisions c4 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c4 []
  golden "C4-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c4 $ findDivisions c4 $ fromJust $ crashMotorcycles c4 []
  golden "C4-Cell2" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c4 $ findDivisions c4 $ fromJust $ crashMotorcycles c4 []
  golden "C4-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c4 []
  golden "C4-Faces-Default" $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c4 []
  golden "C5-Cell1" $ cellFrom $ findFirstCellOfContour c5 $ findDivisions c5 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c5 []
  golden "C5-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c5 $ findDivisions c5 $ fromJust $ crashMotorcycles c5 []
  golden "C5-Cell2" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c5 $ findDivisions c5 $ fromJust $ crashMotorcycles c5 []
  golden "C5-Divide" $ onlyOne $ findDivisions c5 $ fromJust $ crashMotorcycles c5 []
  golden "C5-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c5 []
--  golden "C5-Faces-Default" $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c5 []
  golden "C6-Divide" $ onlyOne $ findDivisions c6 $ fromJust $ crashMotorcycles c6 []
  golden "C6-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c6 []
--  golden "C6-Faces-Default" $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c6 []
  golden "C7-Cell1" $ cellFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Remainder1" $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Cell1-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Divide1" $ head $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Cell2" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Remainder2" $ onlyOne $ remainderFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Cell2-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Cell3" $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Cell3-NodeTree" $ fst $ getRawNodeTreeOfCell $ cellFrom $ findNextCell $ onlyOne $ remainderFrom $ findNextCell $ onlyOne $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromJust $ crashMotorcycles c7 []
  golden "C7-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c7 []
--  golden "C7-Faces-Default" $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton c7 []
  golden "triangle-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton triangle []
  goldens "triangle-Straight_Skeleton_And_Inset" [ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton triangle []
                                                 , toGanja $ onlyOne $ contoursFrom $ insetBy 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton triangle []]
  goldens "triangle-Straight_Skeleton_And_Insets" ([ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton triangle []] <>
                                                  (concat $ (\a -> toGanja <$> a) <$> (infiniteInset 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton triangle [])))
  golden "triangle-Faces-Default" $ facesOf $ fromMaybe (error "no skeleton?") $ findStraightSkeleton triangle []
  golden "square-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton square []
  goldens "square-Straight_Skeleton_And_Inset" [ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton square []
                                                 , toGanja $ onlyOne $ contoursFrom $ insetBy 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton square []]
  goldens "square-Straight_Skeleton_And_Insets" ([ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton square []] <>
                                                  (concat $ (\a -> toGanja <$> a) <$> (infiniteInset 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton square [])))
  golden "square-Faces-Default" $ facesOf $ fromMaybe (error "no skeleton?") $ findStraightSkeleton square []
  golden "rectangle-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton rectangle []
  goldens "rectangle-Straight_Skeleton_And_Inset" [ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton rectangle []
                                                 , toGanja $ onlyOne $ contoursFrom $ insetBy 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton rectangle []]
  goldens "rectangle-Straight_Skeleton_And_Insets" ([ toGanja $ fromMaybe (error "no skeleton?") $ findStraightSkeleton rectangle []] <>
                                                  (concat $ (\a -> toGanja <$> a) <$> (infiniteInset 0.1 $ facesOf $ fromMaybe (error "got Nothing") $ findStraightSkeleton rectangle [])))
  golden "rectangle-Faces-Default" $ facesOf $ fromMaybe (error "no skeleton?") $ findStraightSkeleton rectangle []
    where
      c0 = makePointContour [Point2 (0,0), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c0l0 = LineSeg (Point2 (0,0)) (Point2 (-1,-1))
      c1 = makePointContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c2 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (0,0), Point2 (1,1), Point2 (-1,1)]
      c3 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (0,0), Point2 (-1,1)]
      c4 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c5 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (2,0), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c6 = makePointContour [Point2 (-1,-1), Point2 (-0.5,-1), Point2 (0,0), Point2 (0.5,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c7 = makePointContour [Point2 (0,-1), Point2 (1,-1), Point2 (1,1), Point2 (0.5,1), Point2 (0.5,0), Point2 (0,1), Point2 (-1,1), Point2 (-1,0), Point2 (0,0)]
      -- A simple triangle.
      triangle = makePointContour [Point2 (2,0), Point2 (1.0,sqrt 3), Point2 (0,0)]
      -- A simple square.
      square = makePointContour [Point2 (-1,1), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1)]
      -- A simple rectangle.
      rectangle = makePointContour [Point2 (-1.5,1), Point2 (-1.5,-1), Point2 (1.5,-1), Point2 (1.5,1)]
