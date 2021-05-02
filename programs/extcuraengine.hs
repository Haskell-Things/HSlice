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
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

-- FIXME: Force compilation.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types #-}

-- To treat literal strings as Text
{-# LANGUAGE OverloadedStrings #-}

-- For matching our OpenScad variable types.
{-# LANGUAGE ViewPatterns #-}

import Prelude ((*), (/), (+), (-), odd, mod, round, floor, foldMap, (<>), FilePath, fromInteger, init, error, div, reverse, fst, filter, (<=), (&&), Either(Right))

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($))

import Data.ByteString.UTF8 (fromString)

import Data.String (String)

import Data.Bool(Bool(True, False), otherwise, not)

import Data.List (length, zip, tail, head, zipWith, maximum, minimum, last, concat, null)

import Control.Monad ((>>=))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, mapMaybe, isJust, fromJust)

import Text.Show(show)

import System.IO (IO)

import Data.ByteString (readFile, writeFile, ByteString)

import Data.ByteString.Char8 (unlines)

import Control.Monad.State(runState)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch, hsubparser, command, many)

import Control.Parallel.Strategies (using, rdeepseq, rseq, parListChunk)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(ONum, OString, OBool), lookupVarIn)

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ(Fastℕ), fromFastℕ, fromFastℕtoℝ)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea, CylinderArea), Contour(PointSequence), getContours, Extruder(Extruder), nozzleDiameter, EPos(EPos), StateM, MachineState(MachineState), makeContourTree, ContourTree(ContourTree))

import Graphics.Slicer.Formats.STL.Definitions (trianglesFromSTL)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), xOf, yOf, zOf)

import Graphics.Slicer.Math.Tri (Tri, sidesOf, shiftTri, triIntersects)

import Graphics.Slicer.Math.Line (flipLineSeg, LineSeg(LineSeg), endpoint, lineSegFromEndpoints)

import Graphics.Slicer.Math.Skeleton.Face (facesFromStraightSkeleton, addLineSegsToFace)
import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

import Graphics.Slicer.Machine.Infill (makeInfill, InfillType(Diag1, Diag2, Horiz, Vert))

import Graphics.Slicer.Machine.Contour (cleanContour, shrinkContour, expandContour)

import Graphics.Slicer.Machine.GCode (GCode(GCMarkOuterWallStart, GCMarkInnerWallStart, GCMarkInfillStart, GCMarkLayerStart, GCMarkSupportStart), cookExtrusions, make3DTravelGCode, make2DTravelGCode, addFeedRate, gcodeForContour, gcodeForInfill, gcodeToText)

default (ℕ, Fastℕ, ℝ)

-------------------- TOTAL HACK -----------------------
threads :: Fastℕ
threads = 32

------------------------ STEAL ME ---------------------------
{-
https://crypto.stanford.edu/~blynn/haskell/
https://hackaday.com/2020/05/17/look-ma-no-support-for-my-floating-holes/
https://homes.cs.washington.edu/~ztatlock/pubs/reincarnate-nandi-icfp18.pdf
https://github.com/uwplse/reincarnate-aec
https://github.com/Zip-o-mat/Slic3r/tree/nonplanar
-}

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- Center triangles relative to the center of the build area.
-- FIXME: assumes the origin is at the front left corner.
centeredTrisFromSTL :: BuildArea -> ByteString -> [Tri]
centeredTrisFromSTL (RectArea (bedX,bedY,_)) stl = shiftedTris
    where
      centerPoint = Point3 (dx,dy,dz)
      shiftedTris = [shiftTri centerPoint tri | tri <- tris] `using` parListChunk (div (length tris) (fromFastℕ threads)) rseq
      tris = trianglesFromSTL threads stl
      (dx,dy,dz) = (bedX/2-x0, bedY/2-y0, -zMin)
      xMin = minimum $ xOf.fst <$> foldMap sidesOf tris
      yMin = minimum $ yOf.fst <$> foldMap sidesOf tris
      zMin = minimum $ zOf.fst <$> foldMap sidesOf tris
      xMax = maximum $ xOf.fst <$> foldMap sidesOf tris
      yMax = maximum $ yOf.fst <$> foldMap sidesOf tris
      (x0,y0) = ((xMax+xMin)/2-xMin, (yMax+yMin)/2-yMin)

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- | Create contours from a list of facets.
-- Note that instead of cutting at the top of the layer, we slice in the middle.
layers :: Print -> [Tri] -> [[Contour]]
layers print fs = catMaybes <$> rawContours
  where
    rawContours = [cleanContour <$> getContours (allIntersections (currentLayer-(lh/2))) | currentLayer <- [lh,lh*2..zmax] ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    allIntersections :: ℝ -> [(Point2,Point2)]
    allIntersections zLayer = catMaybes $ triIntersects zLayer <$> fs
    zs = [zOf . fst <$> triPoints | triPoints <- sidesOf <$> fs ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    zmax :: ℝ
    zmax = maximum $ concat zs
    lh = layerHeight print

-- | Get the appropriate InfillType to use when generating infill for the given layer.
-- FIXME: handle the tops and bottoms of surfaces
-- FIXME: handle and the top N layers of the object?
getInfillType :: Print -> Fastℕ -> InfillType
getInfillType print layerNo
  | layerNo <= surfaceLayers = if layerNo `mod` 2 == 0 then Horiz else Vert
  | layerNo `mod` 2 == 0 = Diag1
  | otherwise            = Diag2
 where
   surfaceLayers :: Fastℕ
   surfaceLayers = round $ topBottomThickness print / layerHeight print

----------------------------------------------------------------------
---------------------------- MISC ------------------------------------
----------------------------------------------------------------------

-- Map a function to every other value in a list. This is used for infill generation.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f xs = zipWith (\x v -> if odd v then f x else x) xs [0::Fastℕ,1..]
--------------------------------------------------------------
------------------------ Slicing Plan ------------------------
--------------------------------------------------------------

-- First: just cover our current model.

data SlicingPlan =
  SlicingPlan
    {
      _preObject :: ScadStep
    , _printObject :: ScadStep
    , _postObject :: ScadStep
    }

data ScadStep =
    PriorState MachineState
  | StateM [GCode]

data DivideStrategy =
    ZLayers
  | AllAtOnce

-- The new scad functions:

-- Builtins --
-- extrude     :: Contour -> UncookedGCode
-- inset       :: Contour -> Contour
-- speed       :: UncookedGCode -> GCode

-- Builtins, optional (user can supply implementations) --
-- infillHoriz :: Contour -> UncookedGCode

-- The default SCAD program.
defaultSlicer :: String
defaultSlicer =    "pathWidth = machine_nozzle_size"
                <> "layerHeight = layer_height"
                <> "maxHeight = maxHeightOf(inSTL)"
                <> "triangles = trianglesOf(inSTL)"
                <> "module sliceLayer(contour){ speed(50) inset (pathWidth/2) contour; infillHoriz inset (pathWidth) contour; }"
                <> "module sliceObject(triangles) {for (i=0;i*layerHeight<maxHeight;i++) { "

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------
-- FIXME: this and the next function should be replaced with functions that use a SlicingPlan to slice an object. In this way we could have different 'profiles' (vase mode, normal, etc)...
sliceObject :: Printer -> Print -> [([Contour], Fastℕ)] -> StateM [GCode]
sliceObject printer@(Printer _ _ extruder) print allLayers =
  cookExtrusions extruder (concat slicedLayers) threads
  where
    slicedLayers = [sliceLayer printer print (layer == last allLayers) layer | layer <- allLayers] `using` parListChunk (div (length allLayers) (fromFastℕ threads)) rdeepseq

sliceLayer :: Printer -> Print -> Bool -> ([Contour], Fastℕ) -> [GCode]
sliceLayer (Printer _ _ extruder) print@(Print _ infill lh _ _ ls outerWallBeforeInner infillSpeed) isLastLayer (layerContours, layerNumber) = do
  let
    -- FIXME: make travel gcode from the previous contour's last position?
    travelToContour :: Contour -> [GCode]
    travelToContour contour = [make3DTravelGCode (Point3 (0,0,0)) (raise $ firstPoint contour)]
    travelBetweenContours :: Contour -> Contour -> [GCode]
    travelBetweenContours source dest = [make2DTravelGCode (lastPoint source) $ firstPoint dest]
    travelFromContourToInfill :: Contour -> [[LineSeg]] -> [GCode]
    travelFromContourToInfill source lines = if firstPointOfInfill lines /= Nothing then [addFeedRate infillSpeed $ make2DTravelGCode (lastPoint source) $ fromMaybe (Point2 (0,0)) $ firstPointOfInfill lines] else []
    renderContourTree :: ContourTree -> [GCode]
    renderContourTree (ContourTree (thisContour, subContours)) = renderSurface thisContour (interiorContours subContours) <> concat (renderContourTree <$> insidePositiveSpaces subContours)
      where
        interiorContours :: [ContourTree] -> [Contour]
        interiorContours trees = (\(ContourTree (a,_)) -> a) <$> trees
        insidePositiveSpaces :: [ContourTree] -> [ContourTree]
        insidePositiveSpaces trees = concat $ (\(ContourTree (_,a)) -> a) <$> trees
    renderSurface :: Contour -> [Contour] -> [GCode]
    renderSurface outsideContourRaw insideContoursRaw
      | outerWallBeforeInner == True = concat [
            travelToContour outsideContour
          , drawOuterContour outsideContour
          , renderChildOuterContours outsideContour outsideContourInnerWall
          , drawInnerContour outsideContourInnerWall
          , renderChildInnerContours outsideContourInnerWall outsideContour
          , travelFromContourToInfill outsideContour infillLineSegs
          , drawInfill
          ]
      | otherwise = concat [
            travelToContour outsideContourInnerWall
          , drawInnerContour outsideContourInnerWall
          , renderChildInnerContours outsideContourInnerWall outsideContour
          , drawOuterContour outsideContour
          , renderChildOuterContours outsideContour outsideContourInnerWall
          , travelFromContourToInfill outsideContourInnerWall infillLineSegs
          , drawInfill
          ]
        where
          renderChildOuterContours src dest
            | null childContours = travelBetweenContours src dest
            | otherwise          = concat [
                travelBetweenContours src $ head childContours
                , drawOuterContour $ head childContours
                , drawChildContours
                , travelBetweenContours (last childContours) dest
                ]
          renderChildInnerContours src dest
            | null childContoursInnerWalls = travelBetweenContours src dest
            | otherwise          = concat [
                travelBetweenContours src $ head childContoursInnerWalls
                , drawInnerContour $ head childContoursInnerWalls
                , drawChildOuterContours
                , travelBetweenContours (last childContoursInnerWalls) dest
                ]
          outsideContour = fromMaybe (error "failed to clean outside contour") $ cleanContour $ fromMaybe (error "failed to shrink outside contour") $ shrinkContour (pathWidth*0.5) insideContoursRaw outsideContourRaw
          outsideContourInnerWall = fromMaybe (outsideContourInnerWallByShrink) outsideContourInnerWallBySkeleton
            where
              outsideContourInnerWallByShrink = fromMaybe (error "failed to clean outside contour") $ cleanContour $ fromMaybe (error "failed to shrink outside contour") $ shrinkContour (pathWidth*2) insideContoursRaw outsideContourRaw
              outsideContourInnerWallBySkeleton
                | isJust outsideContourSkeleton && not (null outsideContourNewSegs) = Just $ head $ getContours $ (\seg@(LineSeg p1 _) -> (p1, endpoint seg)) <$> outsideContourNewSegs
-- uncomment this line, and comment out tho following if you want to break when the skeleton code throws it's hands up.
--                | otherwise = error $ show outsideContourSkeleton <> "\n" <> show outsideContourNewSegs <> "\n" <> show outsideContourFaces <> "\n" <> show (firstLineSegOfContour outsideContourRaw) <> "\n"
                | otherwise = Nothing
                where
                  outsideContourSkeleton = findStraightSkeleton outsideContourRaw insideContoursRaw
                  outsideContourFaces    = facesFromStraightSkeleton (fromJust outsideContourSkeleton) (firstLineSegOfContour outsideContourRaw)
                  outsideContourNewSegs  = concat $ fst <$> addLineSegsToFace (pathWidth*2) (Just 1) <$> outsideContourFaces
                  firstLineSegOfContour :: Contour -> Maybe LineSeg
                  firstLineSegOfContour (PointSequence [])  = Nothing
                  firstLineSegOfContour (PointSequence [_]) = Nothing
                  firstLineSegOfContour (PointSequence (a:b:_)) = Just $ (\(Right v) -> v) $ lineSegFromEndpoints a b 
          childContours = mapMaybe cleanContour $ catMaybes $ res <$> insideContoursRaw
            where
              res c = expandContour (pathWidth*0.5) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          childContoursInnerWalls = mapMaybe cleanContour $ catMaybes $ res <$> insideContoursRaw
            where
              res c = expandContour (pathWidth*2) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          infillLineSegs = mapEveryOther (\l -> reverse $ flipLineSeg <$> l) $ makeInfill infillOutsideContour infillChildContours (ls * (1/infill)) $ getInfillType print layerNumber
            where
              infillOutsideContour = fromMaybe (error "failed to clean outside contour") $ cleanContour $ fromMaybe (error "failed to shrink outside contour") $ shrinkContour (pathWidth*2.5) insideContoursRaw outsideContourRaw
              infillChildContours = mapMaybe cleanContour $ catMaybes $ res <$> insideContoursRaw
                where
                  res c = expandContour (pathWidth*2.5) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          drawOuterContour c = GCMarkOuterWallStart : gcodeForContour lh pathWidth c
          drawInnerContour c = GCMarkInnerWallStart : gcodeForContour lh pathWidth c
          drawChildOuterContours = concat $ zipWith (\f l -> travelBetweenContours f l <> drawInnerContour l) (init childContoursInnerWalls) (tail childContoursInnerWalls)
          drawChildContours = concat $ zipWith (\f l -> travelBetweenContours f l <> drawOuterContour l) (init childContours) (tail childContours)
          drawInfill = GCMarkInfillStart : gcodeForInfill lh ls infillLineSegs
  -- extruding gcode generators should be handled here in the order they are printed, so that they are guaranteed to be called in the right order.
  layerStart <> concat (renderContourTree <$> allContours) <> support <> layerEnd 
    where
      allContours = makeContourTree layerContours
      firstOuterContour
        | null allContours = error $ "no contours on layer?\n" <> show layerContours <> "\n"
        | otherwise = (\(ContourTree (a,_)) -> a) $ head allContours
      layerEnd = if isLastLayer then [] else travelToLayerChange
      layerStart = [GCMarkLayerStart layerNumber]
      -- FIXME: make travel gcode from the previous contour's last position?
      travelToLayerChange :: [GCode]
      travelToLayerChange = [make2DTravelGCode (Point2 (0,0)) $ firstPoint firstOuterContour]
      -- FIXME: not all support is support. what about supportInterface?
      support :: [GCode]
      support = [] -- if null supportGCode then [] else GCMarkSupportStart : supportGCode
      -- FIXME: we need to totally reimplement support.
      {-
      supportContour :: Contour
      supportContour = foldMap (\l -> Contour [point l, endpoint l])
                        $ mapEveryOther flipLineSeg
                        $ makeSupport (head outerContours) outerContours layerThickness pathWidth zHeightOfLayer
      -}
      firstPoint (PointSequence contourPoints) = if not (null contourPoints) then head contourPoints else error "tried to get the first point of an empty contour.\n"
      -- since we always print contours as a big loop, the first point IS the last point.
      lastPoint (PointSequence contourPoints) = if not (null contourPoints) then head contourPoints else error "tried to get the last point of an empty contour.\n"
      firstPointOfInfill :: [[LineSeg]] -> Maybe Point2
      firstPointOfInfill [] = Nothing
      firstPointOfInfill (x:_) = Just $ startOfLineSeg $ head x
        where
          startOfLineSeg (LineSeg p _) = p
      -- FIXME: this is not necessarilly the case!
      pathWidth = nozzleDiameter extruder
      raise (Point2 (x,y)) = Point3 (x, y, zHeightOfLayer)
      zHeightOfLayer = lh * (1 + fromFastℕtoℝ layerNumber)

----------------------------------------------------------
------------------------ OPTIONS -------------------------
----------------------------------------------------------

-- Container for the global scope.
-- FIXME: extruders and STLs are supposed to be different scopes.
data ExtCuraEngineRootOpts =
  ExtCuraEngineRootOpts
    {
      _commandOpt             :: String
    , _targetOpt              :: Maybe String
    , _settingFileOpt         :: Maybe FilePath
    , _verboseOpt             :: Bool
    , _threadsOpt             :: Maybe Fastℕ
    , _progressOpt            :: Bool
    , outputFileOpt           :: Maybe String
    , inputFileOpt            :: Maybe String
    , settingOpts             :: [String]
    , _commandOpt2            :: Maybe String
    }

{-
-- Container for sub scopes.
data ExtCuraEngineSubOpts =
  ExtCuraEngineSubOpts
    {
      _
    }
-}

-- | A parser for curaengine style command line arguments.
extCuraEngineOpts :: Parser ExtCuraEngineRootOpts
extCuraEngineOpts = hsubparser $ 
  command "connect"
  (info connectParser (progDesc "Connect to target"))
  <>
  command "slice"
  (info sliceParser (progDesc "Slice input file"))
   

connectParser :: Parser ExtCuraEngineRootOpts
connectParser = pure (ExtCuraEngineRootOpts "connect")
  <*> optional (
  option auto
    (    long "connect"
      <> metavar "TARGET"
      <> help "Target to connect to (and optional port)"
    )
  )
  <*> optional (
  option auto
    (    short 'j'
      <> long "json"
      <> metavar "JSONFILE"
      <> help "A file defining your printer's parameters (in CURA json format 2.0)"
    )
  )
  <*> switch
    (    short 'v'
      <> long "verbose"
      <> help "Increase the verbosity"
    )
  <*> optional (
  option auto
    (    short 'm'
      <> long "threads"
      <> help "number of threads to use when slicing. Ignored."
    )
  )
  <*> pure False
  <*> pure Nothing
  <*> pure Nothing
  <*> pure []
  <*> optional (
  argument str
    (    metavar "TARGET"
      <> help "Target to connect to (and optional port)"
  )
  )

sliceParser :: Parser ExtCuraEngineRootOpts
sliceParser = pure (ExtCuraEngineRootOpts "slice")
  <*>
  pure Nothing
  <*> optional (
  option auto
    (    short 'j'
      <> long "json"
      <> metavar "JSONFILE"
      <> help "A file defining your printer's parameters (in CURA json format 2.0). Ignored"
    )
  )
  <*> switch
    (    short 'v'
      <> long "verbose"
      <> help "Increase the verbosity"
    )
  <*> optional (
  option auto
    (    short 'm'
      <> long "threads"
      <> help "number of threads to use when slicing. Ignored."
    )
  )
  <*> switch
    (    short 'p'
      <> long "progress"
      <> help "display progress information. Ignored."
    )
  <*> optional (
  strOption
    (    short 'o'
      <> long "output"
      <> metavar "OUTPUTFILE"
      <> help "Output file name"
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
  <*> many (
    strOption
      (  short 's'
         <> help "Set a setting to a value."
      )
    )
  <*> pure Nothing

-----------------------------------------------------------------------
--------------------------- Main --------------------------------------
-----------------------------------------------------------------------
-- Characteristics of the printer we are using.
data Printer = Printer
  {
    _printBed :: Bed
  , buildArea :: BuildArea
  , _extruder :: Extruder
  }

-- | The parameters of the print that is being requested.
data Print = Print
  {
    _perimeters              :: Fastℕ
  , _infillAmount            :: ℝ    -- ^ An amount of infill from 0 (none) to 1 (full density).
  , layerHeight              :: ℝ
  , topBottomThickness       :: ℝ    -- ^ The thickness of top and bottom surfaces.
  , _withSupport             :: Bool
  , _lineSpacing             :: ℝ    -- ^ In Millimeters.
  , _outer_wall_before_inner :: Bool -- ^ print outer wall before inside wall.
  , _infill_speed            :: ℝ    -- ^ In millimeters per second
  }

run :: ExtCuraEngineRootOpts -> IO ()
run rawArgs = do
    let
      args = rawArgs
      inFile = fromMaybe "in.stl" $ inputFileOpt args
    stl <- readFile inFile
    -- FIXME: do something with messages.
    (settings, messages) <- addConstants $ settingOpts args
    let
      printer   = printerFromSettings settings
      buildarea = buildArea printer
      print = printFromSettings settings
      triangles = centeredTrisFromSTL buildarea stl
      allLayers :: [[Contour]]
      allLayers = layers print triangles
      object = zip allLayers [(0::Fastℕ)..]
      (gcodes, _) = runState (sliceObject printer print object) (MachineState (EPos 0))
      gcodesAsText = [gcodeToText gcode | gcode <- gcodes] `using` parListChunk (div (length gcodes) (fromFastℕ threads)) rseq
      layerCount = length allLayers
      outFile = fromMaybe "out.gcode" $ outputFileOpt args
    --error $ show allLayers
    writeFile outFile $ startingGCode settings <> (";LAYER_COUNT:" <> fromString (show layerCount) <> "\n") <> unlines gcodesAsText <> endingGCode settings
      where
        -- The Printer.
        -- FIXME: pull defaults for these values from a curaengine json config.
        printerFromSettings :: VarLookup -> Printer
        printerFromSettings vars =
          Printer (getPrintBed vars) (defaultBuildArea vars) (defaultExtruder vars)
            where
              maybeX (lookupVarIn "machine_width" -> Just (ONum width)) = Just width
              maybeX _ = Nothing
              maybeY (lookupVarIn "machine_depth" -> Just (ONum depth)) = Just depth
              maybeY _ = Nothing
              maybeZ (lookupVarIn "machine_height" -> Just (ONum height)) = Just height
              maybeZ _ = Nothing
              maybeNozzleDiameter (lookupVarIn "machine_nozzle_size" -> Just (ONum diameter)) = Just diameter
              maybeNozzleDiameter _ = Nothing
              maybeFilamentDiameter (lookupVarIn "material_diameter" -> Just (ONum diameter)) = Just diameter
              maybeFilamentDiameter _ = Nothing

              -- FIXME: interpret 'machine_shape', and implement eliptic beds.
              -- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it.
              getPrintBed var = RectBed ( fromMaybe 150 $ maybeX var
                                        , fromMaybe 150 $ maybeY var )
              -- The area we can print inside of.
              defaultBuildArea var = RectArea ( fromMaybe 150 $ maybeX var
                                              , fromMaybe 150 $ maybeY var
                                              , fromMaybe  50 $ maybeZ var )
              -- The Extruder. note that this includes the diameter of the feed filament.
              defaultExtruder :: VarLookup -> Extruder
              defaultExtruder var = Extruder (fromMaybe 1.75 $ maybeFilamentDiameter var)
                                             (fromMaybe 0.4 $ maybeNozzleDiameter var)

        -- Print settings for the item currently being sliced.
        -- FIXME: pull all of these values from a curaengine json config or the command line.
        printFromSettings :: VarLookup -> Print
        printFromSettings vars = Print
                                 (fromMaybe 2 $ maybeWallLineCount vars)
                                 (fromMaybe 1 $ maybeInfillAmount vars)
                                 (fromMaybe 0.2 $ maybeLayerHeight vars)
                                 (fromMaybe 0.8 $ maybeTopBottomThickness vars)
                                 (fromMaybe False $ maybeSupport vars)
                                 (fromMaybe 0.6 $ maybeInfillLineWidth vars)
                                 (fromMaybe False $ maybeOuterWallBeforeInner vars)
                                 (fromMaybe 60 $ maybeInfillSpeed vars)
          where
            maybeLayerHeight (lookupVarIn "layer_height" -> Just (ONum thickness)) = Just thickness
            maybeLayerHeight _ = Nothing
            maybeInfillAmount (lookupVarIn "infill_sparse_density" -> Just (ONum amount)) = Just (amount / 100)
            maybeInfillAmount _ = Nothing
            maybeWallLineCount (lookupVarIn "wall_line_count" -> Just (ONum count)) = maybeToFastℕ count
              where
                maybeToFastℕ n = if fromInteger (floor n) == (n::ℝ) then Just . Fastℕ $ floor n else Nothing
            maybeWallLineCount _ = Nothing
            maybeSupport (lookupVarIn "support_enable" -> Just (OBool enable)) = Just enable
            maybeSupport _ = Nothing
            maybeTopBottomThickness (lookupVarIn "top_bottom_thickness" -> Just (ONum thickness)) = Just thickness
            maybeTopBottomThickness _ = Nothing
            maybeInfillLineWidth (lookupVarIn "infill_line_width" -> Just (ONum width)) = Just width
            maybeInfillLineWidth _ = Nothing
            maybeOuterWallBeforeInner (lookupVarIn "outer_inset_first" -> Just (OBool outerFirst)) = Just outerFirst
            maybeOuterWallBeforeInner  _ = Nothing
            maybeInfillSpeed (lookupVarIn "speed_infill" -> Just (ONum speed)) = Just speed
            maybeInfillSpeed _ = Nothing
        startingGCode, endingGCode :: VarLookup -> ByteString
        startingGCode (lookupVarIn "machine_start_gcode" -> Just (OString startGCode)) = fromString startGCode
        startingGCode _ = ";FLAVOR:Marlin\n"
                       <> "G21 ;metric values\n"
                       <> "G90 ;absolute positioning\n"
                       <> "M82 ;set extruder to absolute mode\n"
                       <> "M106 ;start with the fan on\n"
                       <> "G28 X0 Y0 ;move X/Y to min endstops\n"
                       <> "G28 Z0 ;move Z to min endstops\n"
                       <> "G29 ;Run the auto bed leveling\n"
                       <> "G1 Z15.0 F4200 ;move the platform down 15mm\n"
                       <> "G92 E0 ;zero the extruded length\n"
                       <> "G1 F200 E3 ;extrude 3mm of feed stock\n"
                       <> "G92 E0 ;zero the extruded length again\n"
                       <> "G1 F4200 ;default speed\n"
                       <> ";Put printing message on LCD screen\n"
                       <> "M117\n"
        endingGCode (lookupVarIn "machine_end_gcode" -> Just (OString endGCode)) = fromString endGCode
        endingGCode _ = ";End GCode\n"
                      <> "M104 S0 ;extruder heater off\n"
                      <> "M140 S0 ;heated bed heater off (if you have it)\n"
                      <> "G91 ;relative positioning\n"
                      <> "G1 E-1 F300 ;retract the filament a bit before lifting the nozzle, to release some of the pressure\n"
                      <> "G1 Z+0.5 E-5 X-20 Y-20 F300 ;move Z up a bit and retract filament even more\n"
                      <> "G28 X0 Y0 ;move X/Y to min endstops, so the head is out of the way\n"
                      <> "M107 ;fan off\n"
                      <> "M84 ;steppers off\n"
                      <> "G90 ;absolute positioning\n"

-- | The entry point. Use the option parser then run the slicer.
main :: IO ()
main = execParser opts >>= run
    where
      opts = info (helper <*> extCuraEngineOpts)
             ( fullDesc
               <> progDesc "HSlice: 3D FDM path planner (slicer)."
               <> header "extcuraengine - Extended CuraEngine"
             )
