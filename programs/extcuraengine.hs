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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)

import Prelude ((*), (/), (+), (-), odd, pi, mod, round, floor, foldMap, (<>), toRational, FilePath, Int, fromInteger, Eq, fromRational, init, error, seq, div, fromRational, reverse)

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($))

import Data.Ord ((>))

import Data.Tuple (fst)

import Data.ByteString.UTF8 (fromString)

import Data.String (String)

import Data.Bool(Bool(True, False), (&&), otherwise, not)

import Data.List (length, zip, tail, head, zipWith, maximum, minimum, last, (++), concat, null)

import Control.Monad ((>>=))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, fromMaybe)

import Text.Show(show)

import System.IO (IO)

import Data.ByteString (readFile, writeFile, ByteString)

import Data.ByteString.Char8 (unlines, spanEnd)

import Control.Monad.State(runState)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch, hsubparser, command, many)

import Data.Double.Conversion.ByteString (toFixed)

import Control.Parallel.Strategies (using, rdeepseq, rseq, parListChunk, parBuffer)

import Control.DeepSeq (NFData(rnf))

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(ONum, OString, OBool), lookupVarIn)

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, ℕ, Fastℕ(Fastℕ), fromFastℕ)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea, CylinderArea), Point(Point), Line(Line), point, distance, endpoint, flipLine, Facet, sides, Contour(Contour), LayerType(BaseOdd, BaseEven), shiftFacet, accumulateValues, facetIntersects, getContours, Extruder(Extruder), nozzleDiameter, filamentWidth, EPos(EPos), StateM, MachineState(MachineState), getEPos, setEPos, facetLinesFromSTL, makeContourTree, ContourTree(ContourTree))

import Graphics.Slicer.Machine.Infill (makeInfill, makeSupport)

import Graphics.Slicer.Machine.Contour (cleanContour, shrinkContour, expandContour)

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

---------------------------------------------------------
--------------------- GCode Generation ------------------
---------------------------------------------------------

data GCode =
    GCMove2 { _startPoint2 :: ℝ2, _stopPoint2 :: ℝ2 }
  | GCMove3 { _startPoint3 :: ℝ3, _stopPoint3 :: ℝ3 }
  | GCFeedRate { _rate :: ℝ, _code :: GCode }
  | GCExtrude2 { _startPoint2 :: ℝ2, _stopPoint2 :: ℝ2, _ePos :: ℝ }
  | GCExtrude3 { _startPoint3 :: ℝ3, _stopPoint3 :: ℝ3, _ePos :: ℝ }
  | GCRawExtrude2 { _startPoint2 :: ℝ2, _stopPoint2 :: ℝ2, _extrusion :: RawExtrude }
  | GCRawExtrude3 { _startPoint3 :: ℝ3, _stopPoint3 :: ℝ3, _extrusion :: RawExtrude }
  | GCMarkLayerStart { _layerNumber :: Fastℕ }
  | GCMarkInnerWallStart
  | GCMarkOuterWallStart
  | GCMarkSupportStart
  | GCMarkInfillStart
  deriving (Eq, Generic, NFData)

instance NFData Fastℕ where
  rnf a = seq a ()

data RawExtrude = RawExtrude { _pathLength :: ℝ, _pathWidth :: ℝ, _pathHeight :: ℝ }
  deriving (Eq, Generic, NFData)

-- Calculate the extrusion values for all of the GCodes that extrude.
cookExtrusions :: Extruder -> [GCode] -> StateM [GCode]
cookExtrusions extruder gcodes = do
  currentPos <- fromRational <$> getEPos
  let
    ePoses = [currentPos+amount | amount <- accumulateValues extrusionAmounts]
    extrusionAmounts = [calculateExtrusion gcode | gcode <- gcodes] `using` parListChunk (div (length gcodes) (fromFastℕ threads)) rseq
  setEPos . toRational $ last ePoses
  pure $ applyExtrusions gcodes ePoses
  where
    applyExtrusions :: [GCode] -> [ℝ] -> [GCode]
    applyExtrusions = zipWith applyExtrusion
    applyExtrusion :: GCode -> ℝ -> GCode
    applyExtrusion (GCRawExtrude2 startPoint stopPoint _) ePos = GCExtrude2 startPoint stopPoint ePos
    applyExtrusion (GCRawExtrude3 startPoint stopPoint _) ePos = GCExtrude3 startPoint stopPoint ePos
    -- FIXME: should these two generate warnings?
    applyExtrusion (GCExtrude2 startPoint stopPoint _) ePos = GCExtrude2 startPoint stopPoint ePos
    applyExtrusion (GCExtrude3 startPoint stopPoint _) ePos = GCExtrude3 startPoint stopPoint ePos
    applyExtrusion gcode _ = gcode
    calculateExtrusion :: GCode -> ℝ
    calculateExtrusion (GCRawExtrude2 _ _ (RawExtrude pathLength pathWidth pathHeight)) =
      pathWidth * pathHeight * (2 / filamentDia) * pathLength / pi
    calculateExtrusion (GCRawExtrude3 _ _ (RawExtrude pathLength pathWidth pathHeight)) =
      pathWidth * pathHeight * (2 / filamentDia) * pathLength / pi
    calculateExtrusion _ = 0
    filamentDia = filamentWidth extruder

-- travel to a point without extruding
make2DTravelGCode :: Point -> Point -> GCode
make2DTravelGCode (Point (x1,y1,_)) (Point (x2,y2,_)) = GCMove2 (x1,y1) (x2,y2)

make3DTravelGCode :: Point -> Point -> GCode
make3DTravelGCode (Point (x1,y1,z1)) (Point (x2,y2,z2)) = GCMove3 (x1,y1,z1) (x2,y2,z2)

-- GCode to travel to a point while extruding.
-- FIXME: assumes pathwidth == nozzle diameter, which is clearly wrong...
make2DExtrudeGCode :: ℝ -> ℝ -> Point -> Point -> GCode
make2DExtrudeGCode pathThickness pathWidth p1@(Point (x1,y1,_)) p2@(Point (x2,y2,_)) = GCRawExtrude2 (x1, y1) (x2, y2) (RawExtrude pathLength pathWidth pathThickness)
  where
   pathLength = distance p1 p2

-- Add a feedrate to a gcode.
addFeedRate :: ℝ -> GCode -> GCode
addFeedRate = GCFeedRate

-- render a value to ByteString, in the number of characters that are suitable to use in a gcode file. drops trailing zeroes, and the decimal, if there is no fractional component.
posIze :: ℝ -> ByteString
posIze pos
  | pos == 0 = "0"
  | otherwise = fst $ spanEnd (== '.') $ fst $ spanEnd (== '0') $ toFixed 5 pos

-- render a gcode into a piece of text.
gcodeToText :: GCode -> ByteString
gcodeToText (GCFeedRate f (GCMove2 (x1,y1) (x2,y2))) = "G0 F" <> posIze f <> " " <> (if x1 /= x2 then "X" <> posIze x2 <> " " else "") <> (if y1 /= y2 then "Y" <> posIze y2 <> " " else "")
gcodeToText (GCFeedRate f wtf) = error "applying feedrate " <> posIze f <> " to something other than a GCmove2: " <> gcodeToText wtf
gcodeToText (GCMove2 (x1,y1) (x2,y2)) = "G0 " <> (if x1 /= x2 then "X" <> posIze x2 <> " " else "") <> (if y1 /= y2 then "Y" <> posIze y2 <> " " else "")
gcodeToText (GCMove3 (x1,y1,z1) (x2,y2,z2)) = "G0 " <> (if x1 /= x2 then "X" <> posIze x2 <> " " else "") <> (if y1 /= y2 then "Y" <> posIze y2 <> " " else "") <> (if z1 /= z2 then "Z" <> posIze z2 else "")
gcodeToText (GCExtrude2 (x1,y1) (x2,y2) e) = "G1 " <> (if x1 /= x2 then "X" <> posIze x2 <> " " else "") <> (if y1 /= y2 then "Y" <> posIze y2 <> " " else "") <> "E" <> posIze e
gcodeToText (GCExtrude3 (x1,y1,z1) (x2,y2,z2) e) = "G1 " <> (if x1 /= x2 then "X" <> posIze x2 <> " " else "") <> (if y1 /= y2 then "Y" <> posIze y2 <> " " else "") <> (if z1 /= z2 then "Z" <> posIze z2 <> " " else "") <> "E" <> posIze e
gcodeToText GCRawExtrude2 {} = error "Attempting to generate gcode for a 2D extrude command that has not yet been rendered."
gcodeToText GCRawExtrude3 {} = error "Attempting to generate gcode for a 3D extrude command that has not yet been rendered."
-- The current layer count, where 1 == the bottom layer of the object being printed. rafts are represented as negative layers.
gcodeToText (GCMarkLayerStart layerNo) = ";LAYER:" <> fromString (show (fromFastℕ layerNo :: Int))
-- perimeters on the inside of the object. may contact the infill, or an outer paremeter, but will not be exposed on the outside of the object.
gcodeToText GCMarkInnerWallStart = ";TYPE:WALL-INNER"
-- a perimeter on the outside of the object. may contact the infill, or an inside paremeter.
gcodeToText GCMarkOuterWallStart = ";TYPE:WALL-OUTER"
-- Marker indicating the following gcode commands are part of the support, and do not touch the object or the build plate. think: the sparsely generated back-and-forth 
gcodeToText GCMarkSupportStart = ";TYPE:SUPPORT"
-- The interior of an object. should only contact inner parameters, skin, or outer paremeters.
gcodeToText GCMarkInfillStart = ";TYPE:FILL"

-- Generate G-Code for a given contour.
-- Assumes the printer is already at the first point.
-- Also assumes contours that have points.
gcodeFor2DContour :: ℝ -> ℝ -> Contour -> [GCode]
gcodeFor2DContour lh pathWidth (Contour contourPoints)
  | length contourPoints > 1  = zipWith (make2DExtrudeGCode lh pathWidth) (init contourPoints) (tail contourPoints)
  | length contourPoints == 1 = error $ "Given a contour with a single point in it:" <> show contourPoints <> "\n"
  | otherwise                 = []

-- for each group of lines, generate gcode for the segments, with move commands between them.
gcodeFor2DInfill :: ℝ -> ℝ -> [[Line]] -> [GCode]
gcodeFor2DInfill _ _ [] = []
gcodeFor2DInfill lh pathWidth lineGroups = concat $ renderLineGroup (head lineGroups) : (zipWith (\group1 group2 -> (moveBetweenLineGroups group1 group2) ++ (renderLineGroup group2)) (init lineGroups) (tail lineGroups))
  where
    -- FIXME: this should be a single gcode. why are we getting empty line groups given to us?
    moveBetweenLineGroups :: [Line] -> [Line] -> [GCode]
    moveBetweenLineGroups [] g2 = error $ "given empty line group?\n" <> show g2 <> "\n"
    moveBetweenLineGroups g1 [] = error $ "line group empty when finding line group following " <> show g1 <> "\n"
    moveBetweenLineGroups g1 g2 = [moveBetween (last g1) (head g2)]
    renderLineGroup :: [Line] -> [GCode]
    renderLineGroup [] = []
    renderLineGroup group = renderSegment (head group) : (concat $ zipWith (\ l1 l2 -> moveBetween l1 l2 : [renderSegment l2]) (init group) (tail group))
    moveBetween :: Line -> Line -> GCode
    moveBetween l1 (Line startPointl2 _) = make2DTravelGCode (endpoint l1) startPointl2
    renderSegment :: Line -> GCode
    renderSegment ln@(Line startPoint _) = make2DExtrudeGCode lh pathWidth startPoint $ endpoint ln

----------------------------------------------------
------------------ FIXED STRINGS -------------------
----------------------------------------------------
-- FIXME: put these in the right places.
{-
-- The beginning of a sequence of gcodes instructing the printer to place a skirt around the object.
skirtStartGCode :: [Text]
skirtStartGCode = [";TYPE:SKIRT"]
-- The time consumed by the gcode in the file being generated thus far. generated by cura after each layer transition.
timeMarkerGCode :: Text
timeMarkerGCode = ";TIME_ELAPSED:"
-- Part of the support, may touch the build plate, or be part of the last two layers before support contacts the object.
-- support-interface is generated with 100% infill.
supportInterfaceStartGCode :: [Text]
supportInterfaceStartGCode = [";TYPE:SUPPORT-INTERFACE"]
-- The top / bottom surfaces of an object.
skinStartGCode :: [Text]
skinStartGCode = [";TYPE:SKIN"]
-- A gcode identifying the source mesh that is being sliced.
meshStartGCode :: Text
meshStartGCode = ";MESH:"
-}

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- Center facets relative to the center of the build area.
-- FIXME: assumes the origin is at the front left corner.
centeredFacetsFromSTL :: BuildArea -> ByteString -> [Facet]
centeredFacetsFromSTL (RectArea (bedX,bedY,_)) stl = shiftedFacets
    where
      centerPoint = Point (dx,dy,dz)
      shiftedFacets = [shiftFacet centerPoint facet | facet <- facets] `using` parListChunk (div (length facets) (fromFastℕ threads)) rseq
      facets = facetLinesFromSTL threads stl
      (dx,dy,dz) = (bedX/2-x0, bedY/2-y0, -zMin)
      xMin = minimum $ xOf.point <$> foldMap sides facets
      yMin = minimum $ yOf.point <$> foldMap sides facets
      zMin = minimum $ zOf.point <$> foldMap sides facets
      xMax = maximum $ xOf.point <$> foldMap sides facets
      yMax = maximum $ yOf.point <$> foldMap sides facets
      (x0,y0) = ((xMax+xMin)/2-xMin, (yMax+yMin)/2-yMin)
      xOf, yOf, zOf :: Point -> ℝ
      xOf (Point (x,_,_)) = x
      yOf (Point (_,y,_)) = y
      zOf (Point (_,_,z)) = z

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: Print -> [Facet] -> [[Contour]]
layers print fs = catMaybes <$> rawContours
  where
    rawContours = [cleanContour <$> (getContours $ allIntersections currentLayer) | currentLayer <- [lh,lh*2..zmax] ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    allIntersections :: ℝ -> [[Point]]
    allIntersections zLayer = catMaybes $ facetIntersects zLayer <$> fs
    zs = [zOf $ point triPoints | triPoints <- foldMap sides fs ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    zmax :: ℝ
    zmax = maximum zs
    lh = layerHeight print
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- get the layer type for infill on this layer.
-- FIXME: handle top and bottom surfaces.
getLayerType :: Print -> Fastℕ -> LayerType
getLayerType print fromStart
--  | (fromStart <= topBottomLayers || (fromStart+1) <= topBottomLayers) &&
  | fromStart `mod` 2 == 0 = BaseEven
  | otherwise = BaseOdd
  where
    topBottomLayers :: Fastℕ
    topBottomLayers = round $ surfaceThickness print / layerHeight print

----------------------------------------------------------------------
---------------------------- MISC ------------------------------------
----------------------------------------------------------------------

-- Map a function to every other value in a list. This is used for infill generation.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f xs = zipWith (\x v -> if odd v then f x else x) xs [1::Fastℕ,2..]

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------
sliceObject :: Printer ->  Print ->  [([Contour], Fastℕ)] -> StateM [GCode]
sliceObject printer@(Printer _ _ extruder) print allLayers =
  cookExtrusions extruder (concat $ layers)
  where
    layers = [sliceLayer printer print (layer == last allLayers) layer | layer <- allLayers] `using` parListChunk (div (length allLayers) (fromFastℕ threads)) rdeepseq

sliceLayer ::  Printer ->  Print -> Bool -> ([Contour], Fastℕ) -> [GCode]
sliceLayer (Printer _ buildarea extruder) print@(Print perimeterCount _ lh _ hasSupport _ outerWallBeforeInner infillSpeed) isLastLayer (layerContours, layerNumber) = do
  let
    -- FIXME: make travel gcode from the previous contour's last position?
    travelToContour :: Contour -> [GCode]
    travelToContour contour = [make3DTravelGCode (Point (0,0,0)) $ firstPoint contour]
    travelBetweenContours :: Contour -> Contour -> [GCode]
    travelBetweenContours sourceContour destContour = [make2DTravelGCode (lastPoint sourceContour) $ firstPoint destContour]
    travelFromContourToInfill :: Contour -> [[Line]] -> [GCode]
    travelFromContourToInfill source dest = if firstPointOfInfill dest /= Nothing then [addFeedRate infillSpeed $ make2DTravelGCode (lastPoint source) $ fromMaybe (Point (0,0,0)) $ firstPointOfInfill dest] else []
    renderContourTree :: ContourTree -> [GCode]
    renderContourTree (ContourTree (thisContour, subContours)) = (renderSurface thisContour (interiorContours subContours)) <> (concat $ renderContourTree <$> (insidePositiveSpaces subContours))
      where
        interiorContours :: [ContourTree] -> [Contour]
        interiorContours trees = (\(ContourTree (a,_)) -> a) <$> trees
        insidePositiveSpaces :: [ContourTree] -> [ContourTree]
        insidePositiveSpaces trees = concat $ (\(ContourTree (_,a)) -> a) <$> trees
    renderSurface :: Contour -> [Contour] -> [GCode]
    renderSurface outsideContour insideContours
      | outerWallBeforeInner == True =
        (travelToContour contour) <> (drawOuterContour contour)
        <> renderChildOuterContours contour (innerContourOf contour)
        <> (drawInnerContour $ innerContourOf contour)
        <> renderChildInnerContours (innerContourOf contour) contour
        <> remainder contour childContours
      | otherwise =
        (travelToContour $ innerContourOf contour) <> (drawInnerContour $ innerContourOf contour)
        <> renderChildInnerContours (innerContourOf contour) contour
        <> drawOuterContour contour
        <> renderChildOuterContours contour (innerContourOf contour)
        <> remainder contour childContours
        where
          renderChildOuterContours src dest = if null childContours
                                              then travelBetweenContours src dest
                                              else (travelBetweenContours src $ head childContours) <> (drawOuterContour $ head childContours) <> drawChildContours <> (travelBetweenContours (last childContours) $ dest)
          renderChildInnerContours src dest = if null childContours
                                              then travelBetweenContours src dest
                                              else (travelBetweenContours src $ outerContourOf $ head childContours)
                                                   <> (drawInnerContour $ outerContourOf $ head childContours) <> drawChildOuterContours <> (travelBetweenContours (outerContourOf $ last childContours) $ dest)
          contour = fromMaybe (error "failed to shrink contour") $ cleanContour $ shrinkContour buildarea (pathWidth/2) (insideContours ++ [outsideContour]) outsideContour
          childContours = (fromMaybe (error "failed to expand contour") . cleanContour . expandContour buildarea (pathWidth/2) (insideContours ++ [outsideContour])) <$> insideContours
          drawChildContours = (concat $ zipWith (\f l -> travelBetweenContours f l <> drawOuterContour l) (init childContours) (tail childContours))
          drawChildOuterContours = (concat $ zipWith (\f l -> travelBetweenContours f l <> drawInnerContour l) (init $ outerContourOf <$> childContours) (tail $ outerContourOf <$> childContours))
          drawOuterContour c = GCMarkOuterWallStart : gcodeFor2DContour lh pathWidth c
          drawInnerContour c = GCMarkInnerWallStart : gcodeFor2DContour lh pathWidth c
          drawInfill :: Contour -> [Contour] -> [GCode]
          drawInfill c cs = GCMarkInfillStart : (gcodeFor2DInfill lh pathWidth $ infillLines c cs)
          remainder :: Contour -> [Contour] -> [GCode]
          remainder c cs
            | outerWallBeforeInner == True = (travelFromContourToInfill (innerContourOf c) (infillLines c cs)) <> (drawInfill c cs)
            | otherwise = (travelFromContourToInfill c (infillLines (innerContourOf c) (outerContourOf <$> cs))) <> (drawInfill (innerContourOf c) (outerContourOf <$> cs))
          -- FIXME: move this to a maybe.
          innerContourOf c = fromMaybe (Contour []) $ cleanContour $ shrinkContour buildarea pathWidth [c] c
          outerContourOf c = fromMaybe (Contour []) $ cleanContour $ expandContour buildarea pathWidth [c] c
          infillLines c cs = mapEveryOther (\l -> reverse $ flipLine <$> l) $ makeInfill (innerContourOf c) (outerContourOf <$> cs) ls zHeightOfLayer $ getLayerType print layerNumber
    supportGCode, layerEnd :: [GCode]
    supportGCode = if hasSupport then gcodeFor2DContour lh pathWidth supportContour else []
    layerEnd = if isLastLayer then [] else travelToLayerChange -- ++ gcodeFor2DContourNoExtrude (firstOuterContour)
    layerStart = [GCMarkLayerStart layerNumber]
    -- FIXME: not all support is support. what about supportInterface?
    support = if null supportGCode then [] else GCMarkSupportStart : supportGCode
    -- FIXME: make travel gcode from the previous contour's last position?
    travelToLayerChange = [make2DTravelGCode (Point (0,0,0)) $ firstPoint $ firstOuterContour]
  -- extruding gcode generators should be handled here in the order they are printed, so that they are guaranteed to be called in the right order.
--  error $ show $ makeContourTree layerContours
  layerStart <> (concat $ renderContourTree <$> makeContourTree layerContours) <> support <> layerEnd 
    where
      outerContours = catMaybes ( [cleanContour $ shrinkContour buildarea (pathWidth/2) layerContours contour | contour <- layerContours] `using` parBuffer (fromFastℕ threads) rdeepseq)
      firstOuterContour = if not (null outerContours) && not (null $ pointsOfContour $ head outerContours) then head outerContours else error $ "no outer contour?\n"
      supportContour :: Contour
      supportContour = foldMap (\l -> Contour [point l, endpoint l])
                        $ mapEveryOther flipLine
                        $ makeSupport (head outerContours) outerContours layerThickness pathWidth zHeightOfLayer
      firstPoint (Contour contourPoints) = if not (null contourPoints) then head contourPoints else error "tried to get the first point of an empty contour.\n"
      -- since we always print contours as a big loop, the first point IS the last point.
      lastPoint (Contour contourPoints) = head contourPoints
      firstPointOfInfill :: [[Line]] -> Maybe Point
      firstPointOfInfill [] = Nothing
      firstPointOfInfill (x:_) = Just $ startOfLine $ head x
        where
          startOfLine (Line p _) = p
      pointsOfContour (Contour points) = points
      pathWidth = nozzleDiameter extruder
      layerThickness = layerHeight print
      ls = lineSpacing print
      zHeightOfLayer = zOfContour $ head layerContours
      zOfContour (Contour contourPoints) = zOf $ head contourPoints
      zOf :: Point -> ℝ
      zOf (Point (_,_,z)) = z

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
connectParser = ExtCuraEngineRootOpts <$>
  pure "connect"
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
sliceParser = ExtCuraEngineRootOpts <$>
  pure "slice"
  <*>
  pure Nothing
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
  <*> switch
    (    short 'p'
      <> long "progress"
      <> help "display progress information. Ignored."
    )
  <*> optional (
  strOption
    (    short 'o'
      <> long "output"
      <> metavar "OUTPUT"
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

-- The parameters of the print that is being requested.
data Print = Print
  {
    _perimeters              :: Fastℕ
  , infillAmount             :: ℝ -- as an amount from 0 (none) to 1 (full density).
  , layerHeight              :: ℝ
  , surfaceThickness         :: ℝ
  , _withSupport             :: Bool
  , lineSpacing              :: ℝ -- In Millimeters.
  , _outer_wall_before_inner :: Bool -- print outer wall before inside wall.
  , _infill_speed            :: ℝ -- In millimeters per second
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
      facets = centeredFacetsFromSTL buildarea stl
      allLayers :: [[Contour]]
      allLayers = layers print facets
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
      opts= info (helper <*> extCuraEngineOpts)
            ( fullDesc
              <> progDesc "HSlice: STL to GCode slicer."
              <> header "extcuraengine - Extended CuraEngine"
            )

