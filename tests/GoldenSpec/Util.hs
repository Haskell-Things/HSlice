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
------------------------------------------------------------------------------
-- | Construct a golden test for rendering the given object to javascript
-- suitable for dropping into https://enkimute.github.io/ganja.js/
--
-- On the first run of this test, it will render the object and cache the
-- results. Subsequent test runs will compare their result to the cached one.
-- This is valuable for ensuring representations of structures don't break
-- across commits.
--
-- The objects are cached under @tests/golden/@, with the given name. Deleting
-- this file is sufficient to update the test if changs in the structures are
-- intended.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module GoldenSpec.Util (golden, goldens) where

import Prelude (IO, FilePath, Bool (True, False), String, pure, (==), readFile, writeFile, (>>=), (<>), ($))

import Control.Monad.IO.Class (liftIO)

import System.Directory (getTemporaryDirectory, doesFileExist)

import System.IO (hClose, openTempFile)

import Test.Hspec (it, shouldBe, SpecWith)

import Graphics.Slicer.Math.Ganja(GanjaAble, dumpGanja, dumpGanjas)

golden :: (GanjaAble a) => String -> a -> SpecWith ()
golden name object = it (name <> " (golden)") $ do
  (res, cached) <- liftIO $ do
    temp_fp <- getTemporaryFilePath "ganja.js"
    -- Output the rendered mesh
    writeFile temp_fp $ dumpGanja object
    !res <- readFile temp_fp
    let golden_fp = "./tests/golden/" <> name <> ".ganja.js"
    -- Check if the cached results already exist.
    doesFileExist golden_fp >>= \case
      True  -> pure ()
      -- If not, save the mesh we just created in the cache.
      False -> writeFile golden_fp res
    !cached <- readFile golden_fp
    pure (res, cached)
  -- Finally, check if the two meshes are equal.
  if res == cached
    then pure ()
    else False `shouldBe` True

goldens :: String -> [String -> (String, String)] -> SpecWith ()
goldens name objects = it (name <> " (golden)") $ do
  (res, cached) <- liftIO $ do
    temp_fp <- getTemporaryFilePath "ganja.js"
    -- Output the rendered mesh
    writeFile temp_fp $ dumpGanjas objects
    !res <- readFile temp_fp
    let golden_fp = "./tests/golden/" <> name <> ".ganja.js"
    -- Check if the cached results already exist.
    doesFileExist golden_fp >>= \case
      True  -> pure ()
      -- If not, save the mesh we just created in the cache.
      False -> writeFile golden_fp res
    !cached <- readFile golden_fp
    pure (res, cached)
  -- Finally, check if the two meshes are equal.
  if res == cached
    then pure ()
    else False `shouldBe` True

------------------------------------------------------------------------------
-- | Get a temporary filepath with the desired extension. On unix systems, this
-- is a file under @/tmp@. Useful for tests that need to write files.
getTemporaryFilePath
    :: String  -- ^ File extension
    -> IO FilePath
getTemporaryFilePath ext = do
  tempdir <- getTemporaryDirectory
  -- The only means available to us for getting a temporary filename also opens
  -- its file handle. Because the 'writeSTL' function opens the file handle
  -- itself, we must first close our handle.
  (fp, h) <- openTempFile tempdir "implicit-golden"
  hClose h
  pure $ fp <> "." <> ext

