{- ORMOLU_DISABLE -}
{- HSlice.
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

-- Shamelessly stolen from ImplicitCAD.

-- So we can add instances of basic typeclasses (Eq, Show, ...) here, instead of in the library.
{-# LANGUAGE StandaloneDeriving #-}

-- Ignore the orphan instances we create for testing purposes.
{-# OPTIONS_GHC -Wno-orphans #-}

-- Utilities
module Math.Util
         ( (-->), (-/>)
         ) where

-- be explicit about where we get things from.
import Prelude (Eq, Show)

import Test.Hspec (Expectation, shouldBe, shouldNotBe)

import Graphics.Slicer.Math.Skeleton.Definitions (Cell(Cell), RemainingContour(RemainingContour), Spine(Spine), StraightSkeleton(StraightSkeleton))

-- operators for expressions for "result of the left side should evaluate to the right side."

infixr 1 -->
-- Expectation operator for comparing values.
(-->) :: (Eq a,Show a) => a -> a -> Expectation
(-->) res exp =
  res `shouldBe` exp

infixr 1 -/>
-- Expectation operator for comparing values.
(-/>) :: (Eq a,Show a) => a -> a -> Expectation
(-/>) res exp =
  res `shouldNotBe` exp

-- add some instances that are required for testing our types.
deriving instance Eq Cell

deriving instance Show RemainingContour

deriving instance Eq RemainingContour

deriving instance Eq StraightSkeleton

deriving instance Eq Spine

