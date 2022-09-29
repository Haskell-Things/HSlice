{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
 - Copyright 2016 Noah Halford and Catherine Moresco
 - Copyright 2019 Julia Longtin
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

-- | container for orphan instances. these should go in their appropriate upstreams.
module Graphics.Slicer.Orphans () where

import Control.DeepSeq (NFData (rnf))
import Graphics.Slicer.Definitions (Fastℕ,ℝ)
import Slist.Size (Size (Infinity, Size))
import Slist.Type (Slist (Slist))
import Prelude (Monoid(mempty), Semigroup((<>)), seq, (+))

instance NFData a => NFData (Slist a) where
  rnf (Slist vals n) = rnf vals `seq` rnf n

instance NFData Size where
  rnf Infinity = ()
  rnf (Size n) = seq n ()

-- | FIXME: move this to the proper place in ImplicitCAD.
instance NFData Fastℕ where
  rnf a = seq a ()

instance Semigroup ℝ where
  (<>) a b = a + b
  
instance Monoid ℝ where
  mempty = 0

