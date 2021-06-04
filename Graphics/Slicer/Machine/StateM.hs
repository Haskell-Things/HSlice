{- ORMOLU_DISABLE -}
-- Slicer.
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

-- The purpose of this file is to contain the definition of the machine's presumed state during a print.

module Graphics.Slicer.Machine.StateM (getEPos, setEPos, EPos(EPos), StateM, MachineState(MachineState)) where

import Prelude (Rational, Show(show), ($), fromRational, pure)

import Data.Functor.Identity (Identity)

import Control.Monad.State (StateT, get, put)

import Graphics.Slicer.Math.Definitions (roundToFifth)

-- The always increasing amount of filament extruded during this print.
newtype EPos = EPos Rational

instance Show EPos where
  show (EPos v) = show $ roundToFifth $ fromRational v

-- | This is the state of a 3D printer. track just the amount of material extruded by the print so far, for now.
-- FIXME: support multiple extruders.
newtype MachineState = MachineState EPos

type StateM = StateT MachineState Identity

getEPos :: StateM Rational
getEPos = do
  (MachineState (EPos position)) <- get
  pure position

setEPos :: Rational -> StateM ()
setEPos val = put (MachineState (EPos val))
