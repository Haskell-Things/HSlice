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

-- FIXME: turn this warning back on at some point.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Prelude ((*), (/), (+), (-), odd, mod, round, floor, foldMap, (<>), FilePath, fromInteger, error, div, reverse, fst, filter, (<=), (>), ceiling)

import Control.Applicative (pure, (<*>), (<$>))

import Control.Monad ((>>=))

import Data.Bool(Bool(True, False), otherwise)

import Data.ByteString.UTF8 (fromString)

import Data.Eq ((==), (/=))

import Data.Function ((.), ($))

import Data.List (concatMap, head, length, zip, zipWith, maximum, minimum, concat, uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe, mapMaybe)

import Data.String (String)

import Slist.Type (Slist(Slist))

import System.IO (IO)

import Text.Show(show)

import Data.ByteString (readFile, writeFile, ByteString)

import Data.ByteString.Char8 (unlines)

import Control.Monad.State(runState)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch, hsubparser, command, many)

import Control.Parallel.Strategies (using, rdeepseq, rseq, parListChunk)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(ONum, OString, OBool), lookupVarIn)

import Graphics.Implicit.Definitions (ℝ, ℕ, Fastℕ(Fastℕ), fromFastℕ, fromFastℕtoℝ)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea), Contour, getContours, Extruder(Extruder), nozzleDiameter, EPos(EPos), FRate(FRate), StateM, MachineState(MachineState), ContourTree(ContourTree))

import Graphics.Slicer.Formats.STL.Definitions (trianglesFromSTL)

import Graphics.Slicer.Math.Contour (firstLineSegOfContour, firstPointOfContour, lastPointOfContour, ContourTreeSet(ContourTreeSet), makeContourTreeSet, firstContourOfContourTreeSet)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), LineSeg(LineSeg), xOf, yOf, zOf)

import Graphics.Slicer.Math.Tri (Tri, sidesOf, shiftTri, triIntersects)

import Graphics.Slicer.Math.Line (flipLineSeg)

import Graphics.Slicer.Math.Skeleton.Definitions (StraightSkeleton)

import Graphics.Slicer.Math.Skeleton.Face (orderedFacesOf)

import Graphics.Slicer.Math.Skeleton.Line (insetBy)

import Graphics.Slicer.Math.Skeleton.Skeleton (findStraightSkeleton)

import Graphics.Slicer.Machine.Infill (makeInfill, InfillFamily(Lines, Concentric), InfillType(ConcentricContours, Diag1, Diag2, Horiz, Vert))

import Graphics.Slicer.Machine.Contour (cleanContour, shrinkContour, expandContour)

import Graphics.Slicer.Machine.GCode (GCode(GCMarkOuterWallStart, GCMarkInnerWallStart, GCMarkInfillStart, GCMarkLayerStart), cookGCode, make3DTravelGCode, make2DTravelGCode, addFeedRate, gcodeForContour, gcodeForInfill, gcodeToText)

import Graphics.Slicer.Mechanics.Definitions(Printer(Printer), GCodeFlavor(GCFlavorMarlin), getExtruder, getBuildArea)

default (ℕ, Fastℕ, ℝ)

-------------------- TOTAL HACK -----------------------
threads :: Fastℕ
threads = 56

------------------------ STEAL ME ---------------------------
{-
https://crypto.stanford.edu/~blynn/haskell/
https://hackaday.com/2020/05/17/look-ma-no-support-for-my-floating-holes/
https://homes.cs.washington.edu/~ztatlock/pubs/reincarnate-nandi-icfp18.pdf
https://github.com/uwplse/reincarnate-aec
https://github.com/Zip-o-mat/Slic3r/tree/nonplanar
https://cadquery.readthedocs.io/en/latest/apireference.html#selectors
-}

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

centeredTrisFromSTLNonTotal :: BuildArea -> ByteString -> [Tri]
centeredTrisFromSTLNonTotal (RectArea (bedX,bedY,_)) stl = centeredTrisFromSTL bedX bedY stl
centeredTrisFromSTLNonTotal _ _ = error "centeredTrisFromSTLNonTotal: bad arguments."

-- Center triangles relative to the center of the build area.
-- FIXME: assumes the origin is at the front left corner.
centeredTrisFromSTL :: ℝ -> ℝ -> ByteString -> [Tri]
centeredTrisFromSTL bedX bedY stl = shiftedTris
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
    rawContours = [cleanContour <$> getContours (allIntersections $ zHeightOfMiddleOfLayer layerNo) | layerNo <- [0,1..lastLayer] ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    allIntersections :: ℝ -> [(Point2,Point2)]
    allIntersections zLayer = mapMaybe (triIntersects zLayer) fs
    zs = [zOf . fst <$> triPoints | triPoints <- sidesOf <$> fs ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    zmax = maximum $ concat zs
    lastLayer :: Fastℕ
    lastLayer
      | zmax > layer0Height print = ceiling ((zmax-layer0Height print) / layerHeight print) - 1
      | zmax > layer0Height print /2 = 1
      | otherwise = error "too short!"
    -- The height at the point we slice. in the middle of the layer being deposited.
    zHeightOfMiddleOfLayer layerNumber
      | layerNumber == 0 = layer0Height print / 2
      | otherwise = zHeightOfLayer print layerNumber - layerHeight print / 2

-- | The height of the top of the given layer. Where we are doing the depositing from.
zHeightOfLayer :: Print -> Fastℕ -> ℝ
zHeightOfLayer print layerNumber
      | layerNumber == 0 = layerHeight print
      | otherwise = layer0Height print + layerHeight print * fromFastℕtoℝ layerNumber

-- | Get the appropriate InfillType to use when generating infill for the given layer.
-- FIXME: handle the tops and bottoms of surfaces
-- FIXME: handle and the top N layers of the object.
getInfillType :: Print -> Fastℕ -> InfillType
getInfillType print layerNo
  | layerNo == 0 = topBottomPattern0 print
  | defaultPattern == Concentric = ConcentricContours
  | layerNo <= surfaceLayers = if layerNo `mod` 2 == 0 then Horiz else Vert
  | layerNo `mod` 2 == 0 = Diag1
  | otherwise            = Diag2
 where
   surfaceLayers :: Fastℕ
   surfaceLayers = round $ topBottomThickness print / layerHeight print
   defaultPattern = infillPattern print

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

-- The difference between a slicing plan, and a Print is that a Print should specify characteristics of the resulting object, where a Plan should specify what methods to attempt to use to accomplish that goal.
newtype Plan =
  Extrude (DivideStrategy, InsetStrategy)

-- the space that a plan is to be followed within.
-- FIXME: union, intersect, etc.. these?
-- FIXME: transitions between regions?
-- FIXME: the printer's working area is a Zone of type Box3.
newtype Zone =
  Everywhere Plan
--  | ZBetween !(ℝ,Maybe ℝ) !Plan
--  | BelowBottom !(Point2, Point2) !Plan
--  | Box3 !Point3 !Point3 !Plan

data DivideStrategy =
    ZLayers
--    | LongString
--    | PlaneLayers

-- the order of operations in a Plan.
--data LayerOrdering =

-- FIXME: what to do about conflicting orderings when a component crosses from one Zone to the other? need a Zone Transition solver.

data ScadStep =
    PriorState !MachineState
  | NoWork
  | StateM ![GCode]

data InsetStrategy =
    Skeleton
  | SkeletonFailThrough

--data Tool = Extruder
--   | Remover

-- The new scad functions:

-- new Plan Builtins --
-- for specifying how to perform actions within a region.
-- extrude        :: Contour -> UncookedGCode
-- inset          :: Contour -> Contour
-- speed          :: UncookedGCode -> GCode

-- new Geometric Builtins --
-- for specifying what regions of the print get which plans applied.
-- PlaneAndUp
-- RegionAndUp
-- zoneEverywhere :: Everything

-- Builtins, optional (user can supply implementations) --
-- infillHoriz :: Contour -> UncookedGCode

-- scad will get all of the variables (-s) from the command line.

-- The default SCAD program.
defaultPlan :: String
defaultPlan =    "pathWidth = machine_nozzle_size"
              <> "layerHeight = layer_height"
              <> "maxHeight = maxHeightOf(inSTL)"
              <> "triangles = trianglesOf(inSTL)"
              <> "module sliceLayer(contour){ speed(50) inSet 1 (pathWidth*1.5) contour; speed(30) inSet 1 (pathWidth*0.5) contour; infillHoriz inset (pathWidth) contour; }"
              <> "module sliceLayers(triangles) {for (i=0;i*layerHeight<maxHeight;i++) { sliceLayer (contoursAlongAxis i); }"
              <> "module sliceMain(triangles) = {zoneEverywhere sliceLayers(triangles);}"
              <> "sliceMain(triangles)"

-- An idea for a SCAD program:
-- multiply an object, printing it with variations in print settings.
multiplyPlan :: String
multiplyPlan =    "pathWidth = machine_nozzle_size"
               <> "layerHeight = layer_height"
               <> "maxHeight = maxHeightOf(inSTL)"
               <> "triangles = trianglesOf(inSTL)"
               <> "module sliceLayer(contour){ speed(50) inset (pathWidth/2) contour; infillHoriz inset (pathWidth) contour; }"
               <> "module sliceMain(triangles) {for (i=0;i*layerHeight<maxHeight;i++) { translate (x,y) sliceLayer (contoursAlongAxis i) }"
               <> "sliceMain(triangles)"

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------
-- FIXME: this and the next function should be replaced with functions that use a SlicingPlan to slice an object that fits in a Zone. In this way we could have different 'profiles' (vase mode, layers, etc)...
sliceObject :: Printer -> Print -> [([Contour], Fastℕ)] -> StateM [GCode]
sliceObject printer print allLayers =
  cookGCode printer (concat slicedLayers) threads
  where
    slicedLayers = [sliceLayer printer print slicingPlan (isLastLayer layer) (thicknessOfLayer layer) layer | layer <- allLayers] `using` parListChunk (div (length allLayers) (fromFastℕ threads)) rdeepseq
    isLastLayer layer = case unsnoc allLayers of
                          Nothing -> error "impossible!"
                          Just (_,l) -> l == layer
    thicknessOfLayer :: ([Contour], Fastℕ) -> ℝ
    thicknessOfLayer layer
      | isFirstLayer layer = layer0Height print
      | otherwise          = layerHeight print
    isFirstLayer :: ([Contour], Fastℕ) -> Bool
    isFirstLayer layer = case uncons allLayers of
                           Nothing -> error "not possible!"
                           Just (l,_) -> l == layer
    -- Hack: start to use types to explain how to slice.
    slicingPlan = Everywhere (Extrude (ZLayers,SkeletonFailThrough))

-- FIXME: remove layerheight and layerNumber from our calculations here?
-- MOREFIXME: break this into "place -> annotate -> trace". Place gets the geometry, annotate adds regions of rules, trace converts to (abstracted) GCode.
sliceLayer :: Printer -> Print -> Zone -> Bool -> ℝ -> ([Contour], Fastℕ) -> [GCode]
sliceLayer printer print@(Print _ infillRatio _ _ _ _ _ _ ls outerWallBeforeInner _ _ _ _ _) _plan isLastLayer lh (layerContours, layerNumber) = do
  let
    -- FIXME: make travel gcode from the previous contour's last position?
    travelToContour :: Contour -> [GCode]
    travelToContour contour = [addFeedRate travelFeedRate $ make3DTravelGCode (Point3 (0,0,0)) (raise $ firstPointOfContour contour)]
    travelBetweenContours :: Contour -> Contour -> [GCode]
    travelBetweenContours source dest = [addFeedRate travelFeedRate $ make2DTravelGCode (firstPointOfContour source) $ firstPointOfContour dest]
    travelFromContourToInfill :: Contour -> [[LineSeg]] -> [GCode]
    travelFromContourToInfill source lines
     | firstPointOfInfill lines /= Nothing = [addFeedRate travelFeedRate $ make2DTravelGCode (lastPointOfContour source) $ fromMaybe (Point2 (0,0)) $ firstPointOfInfill lines]
     | otherwise = []
    renderContourTreeSet :: ContourTreeSet -> [GCode]
    renderContourTreeSet (ContourTreeSet firstContourTree moreContourTrees) = renderContourTree firstContourTree <> concatMap renderContourTree  moreContourTrees
      where
        renderContourTree :: ContourTree -> [GCode]
        renderContourTree (ContourTree firstContour subContours) = renderSurface firstContour (interiorContours subContours) <> renderSubTrees subContours
        interiorContours :: Slist ContourTreeSet -> [Contour]
        interiorContours (Slist treeSets _) = firstContourOfContourTreeSet <$> treeSets
        renderSubTrees :: Slist ContourTreeSet -> [GCode]
        renderSubTrees (Slist subContours _) = concat $ concat [renderContourTreeSet <$> innerContourTreesOfContourTreeSet contourTree | contourTree <- subContours]
        innerContourTreesOfContourTreeSet :: ContourTreeSet -> [ContourTreeSet]
        innerContourTreesOfContourTreeSet (ContourTreeSet (ContourTree _ (Slist innerTrees _)) _) = innerTrees
    renderSurface :: Contour -> [Contour] -> [GCode]
    renderSurface outsideContourRaw insideContoursRaw
      | outerWallBeforeInner == True =
          travelToContour outsideContour
          <> drawOuterContour outsideContour
          <> renderChildOuterContours outsideContour outsideContourInnerWall
          <> drawInnerContour outsideContourInnerWall
          <> renderChildInnerContours outsideContourInnerWall outsideContour
          <> travelFromContourToInfill outsideContour infillLineSegs
          <> drawInfill
      | otherwise =
          travelToContour outsideContourInnerWall
          <> drawInnerContour outsideContourInnerWall
          <> renderChildInnerContours outsideContourInnerWall outsideContour
          <> drawOuterContour outsideContour
          <> renderChildOuterContours outsideContour outsideContourInnerWall
          <> travelFromContourToInfill outsideContourInnerWall infillLineSegs
          <> drawInfill
        where
          renderChildOuterContours src dest = case childContours of
            [] -> travelBetweenContours src dest
            [headContour] ->
              travelBetweenContours src headContour
              <> drawOuterContour headContour
              <> travelBetweenContours headContour dest
            (headContour:tailContours) ->
              travelBetweenContours src headContour
              <> drawOuterContour headContour
              <> concat (zipWith (\f l -> travelBetweenContours f l <> drawOuterContour l) childContours tailContours)
              <> travelBetweenContours (lastContourOf tailContours) dest
          renderChildInnerContours src dest = case childContoursInnerWalls of
            [] -> travelBetweenContours src dest
            [headContour] ->
              travelBetweenContours src headContour
              <> drawInnerContour headContour
              <> travelBetweenContours headContour dest
            (headContour:tailContours) ->
              travelBetweenContours src headContour
              <> drawInnerContour headContour
              <> concat (zipWith (\f l -> travelBetweenContours f l <> drawInnerContour l) childContoursInnerWalls tailContours)
              <> travelBetweenContours (lastContourOf tailContours) dest
          outsideContour = reduceContour outsideContourRaw insideContoursRaw outsideContourSkeleton (pathWidth*0.5)
          outsideContourInnerWall = reduceContour outsideContourRaw insideContoursRaw outsideContourSkeleton (pathWidth*1.5)
          outsideContourSkeleton = findStraightSkeleton outsideContourRaw insideContoursRaw
          childContours = mapMaybe cleanContour $ mapMaybe res insideContoursRaw
            where
              res c = expandContour (pathWidth*0.5) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          childContoursInnerWalls = mapMaybe cleanContour $ mapMaybe res insideContoursRaw
            where
              res c = expandContour (pathWidth*2) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          -- FIXME: handle multiple infillOutsideContours
          infillLineSegs
            | infillType == ConcentricContours = rawInfill
            | otherwise = mapEveryOther (\segSet -> reverse $ flipLineSeg <$> segSet) rawInfill
            where
              rawInfill = makeInfill infillOutsideContour infillChildContours (ls * (1/infillRatio)) infillType
              infillType = getInfillType print layerNumber
              infillOutsideContour
                | infillType == ConcentricContours = reduceContour outsideContourRaw insideContoursRaw outsideContourSkeleton (pathWidth*1.5)
                | otherwise = reduceContour outsideContourRaw insideContoursRaw outsideContourSkeleton (pathWidth*2)
              infillChildContours = mapMaybe cleanContour $ mapMaybe res insideContoursRaw
                where
                  res c = expandContour (pathWidth*2) (outsideContourRaw:filter (/= c) insideContoursRaw) c
          drawOuterContour c = GCMarkOuterWallStart : gcodeForContour lh pathWidth outerWallFeedRate c
          drawInnerContour c = GCMarkInnerWallStart : gcodeForContour lh pathWidth innerWallFeedRate c
          drawInfill = GCMarkInfillStart : gcodeForInfill lh ls infillFeedRate travelFeedRate infillLineSegs
    in
    -- extruding gcode generators should be handled here in the order they are printed, so that they are guaranteed to be called in the right order.
    layerStart <> renderContourTreeSet allContours <> support <> layerEnd
    where
      reduceContour :: Contour -> [Contour] -> Maybe StraightSkeleton -> ℝ -> Contour
      reduceContour targetContour insideContours targetSkeleton insetAmt = fromMaybe reduceByShrink reduceBySkeleton
        where
          -- Fail to the old contour shrink method when the skeleton based one knows it's failed.
          reduceByShrink = fromMaybe (error "failed to clean contour") $ cleanContour $ fromMaybe (error "failed to shrink contour") $ shrinkContour insetAmt insideContours targetContour
          reduceBySkeleton = case targetSkeleton of
                               Just skeleton -> if length foundContours == 1
                                                then Just $ head foundContours
                                                else error $ "could not reduce contour:\n" <> show targetContour <> "\n"
                                 where
                                   foundContours = fst $ insetBy insetAmt $ orderedFacesOf (firstLineSegOfContour targetContour) skeleton
-- uncomment this line, and comment out the following if you want to break when the skeleton code throws it's hands up.
--                             Nothing -> error $ show outsideContourSkeleton <> "\n" <> show outsideContourFaces <> "\n" <> show (firstLineSegOfContour outsideContourRaw) <> "\n"
                               Nothing -> Nothing
      allContours = makeContourTreeSet layerContours
      lastContourOf contours =
        case unsnoc contours of
        Nothing -> error "no last contour."
        Just (_,l) -> l
      layerEnd = if isLastLayer then [] else travelToLayerChange
      layerStart = [GCMarkLayerStart layerNumber]
      -- FIXME: make travel gcode from the previous contour's last position?
      travelToLayerChange :: [GCode]
      travelToLayerChange = [addFeedRate travelFeedRate $ make2DTravelGCode (Point2 (0,0)) $ firstPointOfContour $ firstContourOfContourTreeSet allContours]
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
      firstPointOfInfill :: [[LineSeg]] -> Maybe Point2
      firstPointOfInfill infillLineSets = case infillLineSets of
                                            [] -> Nothing
                                            ([]:_) -> error "starts with an empty set?"
                                            (((LineSeg p _):_):_) -> Just p
      -- FIXME: this is certainly not the case!
      pathWidth = nozzleDiameter extruder
      infillFeedRate
        | layerNumber == 0 = layer0Speed print
        | otherwise = infillSpeed print
      outerWallFeedRate
        | layerNumber == 0 = layer0Speed print
        | otherwise = wall0Speed print
      innerWallFeedRate
        | layerNumber == 0 = layer0Speed print
        | otherwise = wallXSpeed print
      travelFeedRate
        | layerNumber == 0 = layer0Speed print
        | otherwise = travelSpeed print
      -- convert a 2D point to a 3D location.
      raise (Point2 (x,y)) = Point3 (x, y, zHeightOfLayer print layerNumber)
      extruder = getExtruder printer

----------------------------------------------------------
------------------------ OPTIONS -------------------------
----------------------------------------------------------

-- Container for the global scope.
-- FIXME: extruders and STLs are supposed to be different scopes.
data ExtCuraEngineRootOpts =
  ExtCuraEngineRootOpts
    {
      _commandOpt             :: !String
    , _targetOpt              :: !(Maybe String)
    , _settingFileOpt         :: !(Maybe FilePath)
    , _verboseOpt             :: !Bool
    , _threadsOpt             :: !(Maybe Fastℕ)
    , _progressOpt            :: !Bool
    , outputFileOpt           :: !(Maybe String)
    , inputFileOpt            :: !(Maybe String)
    , settingOpts             :: ![String]
    , _commandOpt2            :: !(Maybe String)
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

-- | The parameters of the print that is being requested.
data Print = Print
  {
    _perimeters              :: !Fastℕ        -- ^ How many walls to place on the outside of an object.
  , _infillRatio             :: !ℝ            -- ^ An proportion of infill from 0 (none) to 1 (full density).
  , infillPattern            :: !InfillFamily -- ^ the type of infill to use by default.
  , layerHeight              :: !ℝ            -- ^ The default thickness for each layer
  , layer0Height             :: !ℝ            -- ^ The thickness of the first layer
  , topBottomThickness       :: !ℝ            -- ^ The thickness of top and bottom surfaces. only used for infill patterning of the bottom layers.
  , topBottomPattern0        :: !InfillType   -- ^ The type of pattern to use on the first layer
  , _withSupport             :: !Bool
  , _lineSpacing             :: !ℝ            -- ^ In Millimeters.
  , _outer_wall_before_inner :: !Bool         -- ^ print outer wall before inside wall.
  , infillSpeed              :: !ℝ            -- ^ In millimeters per second
  , layer0Speed              :: !ℝ            -- ^ In millimeters per second
  , travelSpeed              :: !ℝ            -- ^ In millimeters per second
  , wall0Speed               :: !ℝ            -- ^ In millimeters per second
  , wallXSpeed               :: !ℝ            -- ^ In millimeters per second
  }

run :: ExtCuraEngineRootOpts -> IO ()
run rawArgs = do
    let
      args = rawArgs
      inFile = fromMaybe "in.stl" $ inputFileOpt args
    stl <- readFile inFile
    -- FIXME: do something with messages.
    (settings, _messages) <- addConstants $ settingOpts args
    let
      printer   = printerFromSettings settings
      buildarea = getBuildArea printer
      print = printFromSettings settings
      triangles = centeredTrisFromSTLNonTotal buildarea stl
      allLayers :: [[Contour]]
      allLayers = layers print triangles
      object = zip allLayers [(0::Fastℕ)..]
      (gcodes, _) = runState (sliceObject printer print object) (MachineState (EPos 0) (FRate (travelSpeed print)))
      gcodesAsText = [gcodeToText gcode | gcode <- gcodes] `using` parListChunk (div (length gcodes) (fromFastℕ threads)) rseq
      layerCount = length allLayers
      outFile = fromMaybe "out.gcode" $ outputFileOpt args
    writeFile outFile $ startingGCode settings <> (";LAYER_COUNT:" <> fromString (show layerCount) <> "\n") <> unlines gcodesAsText <> endingGCode settings
      where
        -- The Printer.
        -- FIXME: pull defaults for these values from a curaengine json config.
        printerFromSettings :: VarLookup -> Printer
        printerFromSettings vars =
          Printer (getPrintBed vars) (defaultBuildArea vars) (defaultExtruder vars) (defaultFlavor vars)
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
              maybeGCodeFlavor (lookupVarIn "machine_gcode_flavor" -> Just (OString flavor))
                | flavor == "Marlin" = Just GCFlavorMarlin
                | otherwise = error "unsupported GCode flavor!"
              maybeGCodeFlavor _ = Nothing

              -- FIXME: interpret 'machine_shape', and implement eliptic beds.
              -- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it.
              getPrintBed var = RectBed ( fromMaybe 100 $ maybeX var
                                        , fromMaybe 100 $ maybeY var )
              -- The area we can print inside of.
              defaultBuildArea var = RectArea ( fromMaybe 100 $ maybeX var
                                              , fromMaybe 100 $ maybeY var
                                              , fromMaybe 100 $ maybeZ var )
              -- The Extruder. note that this includes the diameter of the feed filament.
              defaultExtruder :: VarLookup -> Extruder
              defaultExtruder var = Extruder (fromMaybe 2.85 $ maybeFilamentDiameter var)
                                             (fromMaybe 0.4 $ maybeNozzleDiameter var)

              -- The GCode Flavor. Marlin only, for now.
              defaultFlavor :: VarLookup -> GCodeFlavor
              defaultFlavor var = fromMaybe GCFlavorMarlin $ maybeGCodeFlavor var

        -- Print settings for the item currently being sliced.
        -- FIXME: pull all of these values from a curaengine json config or the command line.
        printFromSettings :: VarLookup -> Print
        printFromSettings vars = Print
                                 (fromMaybe 2 $ maybeWallLineCount vars)
                                 (fromMaybe 0.2 $ maybeInfillAmount vars)
                                 (fromMaybe Lines $ maybeInfillPattern vars)
                                 (fromMaybe 0.2 $ maybeLayerHeight vars)
                                 (fromMaybe 0.3 $ maybeLayer0Height vars)
                                 (fromMaybe 0.8 $ maybeTopBottomThickness vars)
                                 (fromMaybe ConcentricContours $ maybeTopBottomPattern0 vars)
                                 (fromMaybe False $ maybeSupport vars)
                                 (fromMaybe 0.4 $ maybeInfillLineWidth vars)
                                 (fromMaybe False $ maybeOuterWallBeforeInner vars)
                                 (fromMaybe 60 $ maybeInfillSpeed vars)
                                 (fromMaybe 30 $ maybeLayer0Speed vars)
                                 (fromMaybe 120 $ maybeTravelSpeed vars)
                                 (fromMaybe 30 $ maybeWall0Speed vars)
                                 (fromMaybe 60 $ maybeWallXSpeed vars)
          where
            maybeInfillAmount (lookupVarIn "infill_sparse_density" -> Just (ONum amount)) = Just (amount / 100)
            maybeInfillAmount _ = Nothing
            maybeInfillLineWidth (lookupVarIn "infill_line_width" -> Just (ONum width)) = Just width
            maybeInfillLineWidth _ = Nothing
            maybeInfillPattern (lookupVarIn "infill_pattern" -> Just (OString infillFamily)) = foundFamily
              where
                foundFamily = case infillFamily of
                                "lines" -> Just Lines
                                "concentric" -> Just Concentric
                                _ -> Nothing
            maybeInfillPattern _ = Nothing
            maybeInfillSpeed (lookupVarIn "speed_infill" -> Just (ONum speed)) = Just speed
            maybeInfillSpeed _ = Nothing
            maybeLayerHeight (lookupVarIn "layer_height" -> Just (ONum thickness)) = Just thickness
            maybeLayerHeight _ = Nothing
            maybeLayer0Height (lookupVarIn "layer_height_0" -> Just (ONum thickness)) = Just thickness
            maybeLayer0Height _ = Nothing
            maybeLayer0Speed (lookupVarIn "speed_layer_0" -> Just (ONum speed)) = Just speed
            maybeLayer0Speed _ = Nothing
            maybeOuterWallBeforeInner (lookupVarIn "outer_inset_first" -> Just (OBool outerFirst)) = Just outerFirst
            maybeOuterWallBeforeInner  _ = Nothing
            maybeTravelSpeed (lookupVarIn "speed_travel" -> Just (ONum speed)) = Just speed
            maybeTravelSpeed _ = Nothing
            maybeWall0Speed (lookupVarIn "speed_wall_0" -> Just (ONum speed)) = Just speed
            maybeWall0Speed _ = Nothing
            maybeWallLineCount (lookupVarIn "wall_line_count" -> Just (ONum count)) = maybeToFastℕ count
              where
                maybeToFastℕ n = if fromInteger (floor n) == (n::ℝ) then Just . Fastℕ $ floor n else Nothing
            maybeWallLineCount _ = Nothing
            maybeWallXSpeed (lookupVarIn "speed_wall_x" -> Just (ONum speed)) = Just speed
            maybeWallXSpeed _ = Nothing
            -- FIXME: implement this! no top and bottom layer support, or support support. :/
            maybeSupport (lookupVarIn "support_enable" -> Just (OBool enable)) = Just enable
            maybeSupport _ = Nothing
            maybeTopBottomThickness (lookupVarIn "top_bottom_thickness" -> Just (ONum thickness)) = Just thickness
            maybeTopBottomThickness _ = Nothing
            maybeTopBottomPattern0 (lookupVarIn "top_bottom_pattern_0" -> Just (OString infillType)) = foundType
              where
                foundType = case infillType of
                              "line" -> Just Horiz
                              "concentric" -> Just ConcentricContours
                              _ -> Nothing
            maybeTopBottomPattern0 _ = Nothing
--            maybeSupportInfillRate (lookupVarIn "support_enable" -> Just (OBool enable)) = Just enable
--            maybeSupportInfillRate _ = Nothing
--            maybeTopBottomSpeed (lookupVarIn "speed_topbottom" -> Just (ONum speed)) = Just speed
--            maybeTopBottomSpeed _ = Nothing
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
