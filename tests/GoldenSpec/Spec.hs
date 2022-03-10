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
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-type-defaults        #-}

module GoldenSpec.Spec (spec) where

import Prelude (($), error, (<>), show)

import Data.Either (Either(Left, Right))

import Data.Maybe (fromMaybe)

import GoldenSpec.Util (golden)

import Test.Hspec (describe, Spec)

import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

-- Our Contour library.
import Graphics.Slicer.Math.Contour (makePointContour)

-- A euclidian point.
import Graphics.Slicer.Math.Definitions(Point2(Point2), LineSeg(LineSeg))

import Graphics.Slicer.Math.Ganja(cellFrom, remainderFrom, onlyOne)

import Graphics.Slicer.Math.Skeleton.Cells (UnsupportedReason, findFirstCellOfContour, findNextCell, findDivisions, getNodeTreeOfCell)

import Graphics.Slicer.Math.Skeleton.Definitions (NodeTree)

import Graphics.Slicer.Math.Skeleton.Motorcycles (crashMotorcycles)

import Graphics.Slicer.Math.Skeleton.Face (facesOf, orderedFacesOf)

spec :: Spec
spec = describe "golden tests" $ do
  golden "C0-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c0 []
  golden "C1-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c1 []
  golden "C2-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c2 []
  golden "C3-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c3 []
  golden "C4-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c4 []
  golden "C5-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c5 []
  golden "C6-Straight_Skeleton" $ fromMaybe (error "no skeleton?") $ findStraightSkeleton c6 []
  golden "C7-Cell1" $ cellFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c7 []
  golden "C7-Remainder1" $ onlyOne $ fromMaybe (error "no remainder?") $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c7 []
  golden "C7-Cell2" $ cellFrom $ findNextCell $ onlyOne $ fromMaybe (error "no remainder?") $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c7 []
  golden "C7-Remainder2" $ onlyOne $ fromMaybe (error "no remainder?") $ remainderFrom $ findNextCell $ onlyOne $ fromMaybe (error "no remainder?") $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c7 []
  golden "C7-Cell3-NodeTree" $ justSupported $ getNodeTreeOfCell $ cellFrom $ findNextCell $ onlyOne $ fromMaybe (error "Got Nothing") $ remainderFrom $ findNextCell $ onlyOne $ fromMaybe (error "Got Nothing") $ remainderFrom $ findFirstCellOfContour c7 $ findDivisions c7 $ fromMaybe (error "Got Nothing") $ crashMotorcycles c7 []
  golden "C0-Faces-Default" $ facesOf (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 [])
  golden "C0-Faces-Ordered" $ orderedFacesOf c0l0 (fromMaybe (error "got Nothing") $ findStraightSkeleton c0 [])
    where
      justSupported :: Either UnsupportedReason NodeTree -> NodeTree
      justSupported maybeNodeTree = case maybeNodeTree of
                                      (Left _) -> error $ "unsupported!\n" <> show maybeNodeTree <> "\n"
                                      (Right a) -> a
      c0 = makePointContour [Point2 (0,0), Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c0l0 = LineSeg (Point2 (0,0)) (Point2 (-1,-1))
      c1 = makePointContour [Point2 (-1,-1), Point2 (0,0), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c2 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (0,0), Point2 (1,1), Point2 (-1,1)]
      c3 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (0,0), Point2 (-1,1)]
      c4 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c5 = makePointContour [Point2 (-1,-1), Point2 (1,-1), Point2 (2,0), Point2 (1,1), Point2 (-1,1), Point2 (0,0)]
      c6 = makePointContour [Point2 (-1,-1), Point2 (-0.5,-1), Point2 (0,0), Point2 (0.5,-1), Point2 (1,-1), Point2 (1,1), Point2 (-1,1)]
      c7 = makePointContour [Point2 (0,-1), Point2 (1,-1), Point2 (1,1), Point2 (0.5,1), Point2 (0.5,0), Point2 (0,1), Point2 (-1,1), Point2 (-1,0), Point2 (0,0)]