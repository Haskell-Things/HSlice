-- STL Linter.
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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-
http://www.varlog.com/admesh-htm/ADMeshThesis.zip
-}

-- FIXME: Force compilation.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types #-}

-- To treat literal strings as Text
{-# LANGUAGE OverloadedStrings #-}

-- For matching our OpenScad variable types.
{-# LANGUAGE ViewPatterns #-}

import Prelude ((<>))

import Control.Applicative ((<*>), (<$>))

import Data.Function (($))

import Data.ByteString (readFile)

import Data.ByteString.Builder (toLazyByteString)

import Data.ByteString.Lazy (writeFile)

import Data.String (String)

import Control.Monad ((>>=))

import Data.Maybe (Maybe, fromMaybe)

import System.IO (IO)

import Options.Applicative (fullDesc, progDesc, header, info, helper, help, long, short, metavar, execParser, Parser, optional, strOption)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ)

import Graphics.Slicer.Formats.STL.Definitions (facetsFromSTL, buildAsciiSTL)

default (ℕ, Fastℕ, ℝ)

------------------------ STEAL ME ---------------------------
{-
https://crypto.stanford.edu/~blynn/haskell/
https://hackaday.com/2020/05/17/look-ma-no-support-for-my-floating-holes/
https://homes.cs.washington.edu/~ztatlock/pubs/reincarnate-nandi-icfp18.pdf
https://github.com/uwplse/reincarnate-aec
https://github.com/Zip-o-mat/Slic3r/tree/nonplanar
-}

----------------------------------------------------------
------------------------ OPTIONS -------------------------
----------------------------------------------------------

-- Container for the global scope.
-- FIXME: extruders and STLs are supposed to be different scopes.
data ExtAdMeshRootOpts =
  ExtAdMeshRootOpts
    {
      outputFileOpt           :: Maybe String
    , inputFileOpt            :: Maybe String
    }

-- | A parser for admesh style command line arguments.
extAdMeshOpts :: Parser ExtAdMeshRootOpts
extAdMeshOpts = ExtAdMeshRootOpts <$>
      optional (
  strOption
    (    short 'a'
      <> long "write-ascii-stl"
      <> metavar "OUTPUTFILE"
      <> help "write an ASCII formatted STL file"
      )
    )
  <*> optional (
  strOption
    (    short 'l'
      <> long "load"
      <> metavar "INPUTFILE"
      <> help "load an ASCII formatted STL file"
      )
    )

-----------------------------------------------------------------------
--------------------------- Main --------------------------------------
-----------------------------------------------------------------------

run :: ExtAdMeshRootOpts -> IO ()
run rawArgs = do
    let
      args = rawArgs
      inFile = fromMaybe "in.stl" $ inputFileOpt args
    facets <- readFile inFile
    let
      outFile = fromMaybe "out.stl" $ outputFileOpt args
      stlOut = buildAsciiSTL 1 (facetsFromSTL 1 facets)
    writeFile outFile $ toLazyByteString stlOut

-- | The entry point. Use the option parser then run the slicer.
main :: IO ()
main = execParser opts >>= run
    where
      opts= info (helper <*> extAdMeshOpts)
            ( fullDesc
              <> progDesc "HSlice: STL Lint Checker."
              <> header "extadmesh - Extended AdMesh"
            )

