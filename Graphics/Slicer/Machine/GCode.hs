{- ORMOLU_DISABLE -}
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

-- To treat literal strings as Text
{-# LANGUAGE OverloadedStrings #-}

-- for NFData.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- | code for generating GCode.
module Graphics.Slicer.Machine.GCode (GCode(GCMarkOuterWallStart, GCMarkInnerWallStart, GCMarkInfillStart, GCMarkLayerStart, GCMarkSupportStart), cookGCode, make3DTravelGCode, make2DTravelGCode, addFeedRate, gcodeForContour, gcodeForInfill, gcodeToText) where

import GHC.Generics (Generic)

import Prelude (Eq, Int, Rational, Show, ($), zipWith, concat, (<>), show, error, otherwise, (==), length, fst, pi, (/), (*), pure, toRational, fromRational, (+), div, Bool, head, snd, tail)

import Data.ByteString (ByteString)

import Data.ByteString.Char8 (spanEnd)

import Control.Parallel.Strategies (using, rseq, parListChunk)

import Data.ByteString.UTF8 (fromString)

import Data.Double.Conversion.ByteString (toFixed)

import Data.List (foldl', uncons)

import Data.List.Extra (unsnoc)

import Data.Maybe ( Maybe(Just, Nothing), fromMaybe )

import Control.DeepSeq (NFData)

import Graphics.Slicer.Definitions(ℝ, ℝ2, ℝ3, ℕ, Fastℕ, fromFastℕ)

import Graphics.Slicer.Math.Contour (pointsOfContour, lastPointOfContour)

import Graphics.Slicer.Math.Definitions (Point3(Point3), Point2(Point2), Contour, LineSeg(LineSeg), distance, endPoint, roundToFifth)

import Graphics.Slicer.Math.Slicer (accumulateValues)

import Graphics.Slicer.Machine.StateM (StateM, MachineState(MachineState), EPos(EPos), FRate(FRate), setMachineState, getMachineState)

import Graphics.Slicer.Mechanics.Definitions (Printer, GCodeFlavor(GCFlavorMarlin), filamentWidth, getExtruder, gCodeFlavor)

default (ℕ, Fastℕ, ℝ)

---------------------------------------------------------
--------------------- GCode Generation ------------------
---------------------------------------------------------

-- | A single gcode statement.
data GCode =
    GCMove2 { _startPoint2 :: !ℝ2, _stopPoint2 :: !ℝ2 }
  | GCMove3 { _startPoint3 :: !ℝ3, _stopPoint3 :: !ℝ3 }
  | GCFeedRate { _rate :: !ℝ, _code :: !GCode }
  | GCRawFeedRate { _rawRate :: !ℝ, _code :: !GCode }
  | GCExtrude2 { _startPoint2 :: !ℝ2, _stopPoint2 :: !ℝ2, _ePos :: !ℝ }
  | GCExtrude3 { _startPoint3 :: !ℝ3, _stopPoint3 :: !ℝ3, _ePos :: !ℝ }
  | GCRawExtrude2 { _startPoint2 :: !ℝ2, _stopPoint2 :: !ℝ2, _extrusion :: !RawExtrude }
  | GCRawExtrude3 { _startPoint3 :: !ℝ3, _stopPoint3 :: !ℝ3, _extrusion :: !RawExtrude }
  | GCMarkLayerStart { _layerNumber :: !Fastℕ }
  | GCMarkInnerWallStart
  | GCMarkOuterWallStart
  | GCMarkSupportStart
  | GCMarkInfillStart
  deriving (Eq, Generic, NFData, Show)

-- | The dimensions of a section of material to be extruded.
data RawExtrude = RawExtrude { _pathLength :: !ℝ, _pathWidth :: !ℝ, _pathHeight :: !ℝ }
  deriving (Eq, Generic, NFData, Show)

-- | Calculate the extrusion values for all of the GCodes that extrude.
cookGCode :: Printer -> [GCode] -> Fastℕ -> StateM [GCode]
cookGCode printer gcodes threads = do
  currentMachine <- getMachineState
  let
    (MachineState (EPos currentPos) (FRate currentFeedRate)) = currentMachine
    ePoses = [currentPos+amount | amount <- accumulateValues extrusionAmounts]
      where
        extrusionAmounts = [calculateExtrusion gcode | gcode <- gcodes] `using` parListChunk (div (length gcodes) (fromFastℕ threads)) rseq
    (ratedGCodes, finalFRate) = applySpeeds gcodes currentFeedRate
    finalEPos = snd $ fromMaybe ([],currentPos) $ unsnoc ePoses
    finalState = MachineState (EPos finalEPos) (FRate finalFRate)
    cookedResults = applyExtrusions ratedGCodes ePoses
  setMachineState finalState
  pure cookedResults
  where
    applyExtrusions :: [GCode] -> [Rational] -> [GCode]
    applyExtrusions = zipWith applyExtrusion
    applyExtrusion :: GCode -> Rational -> GCode
    applyExtrusion (GCFeedRate r myGCode@(GCRawExtrude2 {})) ePos = GCFeedRate r (applyExtrusion myGCode ePos)
    applyExtrusion (GCFeedRate r myGCode@(GCRawExtrude3 {})) ePos = GCFeedRate r (applyExtrusion myGCode ePos)
    applyExtrusion (GCRawExtrude2 startPoint stopPoint _) ePos = GCExtrude2 startPoint stopPoint (fromRational ePos)
    applyExtrusion (GCRawExtrude3 startPoint stopPoint _) ePos = GCExtrude3 startPoint stopPoint (fromRational ePos)
    -- FIXME: should these two generate warnings?
    applyExtrusion (GCExtrude2 {}) _ = error "cooking a previously cooked extrusion?"
    applyExtrusion (GCExtrude3 {}) _ = error "cooking a previously cooked extrusion?"
    applyExtrusion gcode _ = gcode
    applySpeeds :: [GCode] -> ℝ -> ([GCode], ℝ)
    applySpeeds myGCodes firstSpeed = foldl' applySpeed ([],firstSpeed) myGCodes
    applySpeed :: ([GCode], ℝ) -> GCode -> ([GCode], ℝ)
    applySpeed (myGCodes, lastSpeed) (GCRawFeedRate newSpeed myGCode)
      | lastSpeed == newSpeed = (myGCodes <> [myGCode], lastSpeed)
      | otherwise = (myGCodes <> [GCFeedRate flavoredNewSpeed myGCode], newSpeed)
      where
        -- Note: for marlin firmwares, we multiply feedrates by 60, because it wants feed rates in mm/min instead of mm/sec.
        flavoredNewSpeed
          | gCodeFlavor printer == GCFlavorMarlin = newSpeed * 60
          | otherwise = newSpeed
    applySpeed (myGCodes, lastSpeed) gcode = (myGCodes <> [gcode], lastSpeed)
    calculateExtrusion :: GCode -> Rational
    calculateExtrusion (GCRawExtrude2 _ _ (RawExtrude pathLength pathWidth pathHeight)) =
      toRational $ pathWidth * pathHeight * (2 / filamentDia) * pathLength / pi
    calculateExtrusion (GCRawExtrude3 _ _ (RawExtrude pathLength pathWidth pathHeight)) =
      toRational $ pathWidth * pathHeight * (2 / filamentDia) * pathLength / pi
    calculateExtrusion (GCRawFeedRate _ myGCode) = calculateExtrusion myGCode
    calculateExtrusion _ = 0
    extruder = getExtruder printer
    filamentDia = filamentWidth extruder

-- | Construct a GCode to travel to a point without extruding (2D)
make2DTravelGCode :: Point2 -> Point2 -> GCode
make2DTravelGCode (Point2 (x1,y1)) (Point2 (x2,y2)) = GCMove2 (x1,y1) (x2,y2)

-- | Construct a GCode to travel to a point without extruding (3D)
make3DTravelGCode :: Point3 -> Point3 -> GCode
make3DTravelGCode (Point3 p1) (Point3 p2) = GCMove3 p1 p2

-- | Construct a GCode to travel to a point while extruding.
make2DExtrudeGCode :: ℝ -> ℝ -> Point2 -> Point2 -> GCode
make2DExtrudeGCode pathThickness pathWidth p1@(Point2 (x1,y1)) p2@(Point2 (x2,y2)) = GCRawExtrude2 (x1, y1) (x2, y2) (RawExtrude pathLength pathWidth pathThickness)
  where
    pathLength = distance p1 p2

-- | Add a feedrate to a piece of gcode.
addFeedRate :: ℝ -> GCode -> GCode
addFeedRate = GCRawFeedRate

-- | Render a value to ByteString, in the precision that is suitable to use in a gcode file. drops trailing zeroes, and the decimal, if there is no fractional component.
posIze :: ℝ -> ByteString
posIze pos
  | pos == 0 = "0"
  | otherwise = fst $ spanEnd (== '.') $ fst $ spanEnd (== '0') $ toFixed 5 pos

-- | Operator for determining whether two values are equal, after being converted to gcode.
(~==) :: ℝ -> ℝ -> Bool
infixl 9 ~==
(~==) a b = roundToFifth a == roundToFifth b

-- | Render a GCode into a piece of text, ready to print. Only handles 'cooked' gcode, that has had extrusion values calculated.
gcodeToText :: GCode -> ByteString
gcodeToText (GCFeedRate f (GCMove2 (x1,y1) (x2,y2))) = "G0 F" <> posIze f <> " " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ")

gcodeToText (GCFeedRate f (GCMove3 (x1,y1,z1) (x2,y2,z2))) = "G0 F" <> posIze f <> " " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> (if z1 ~== z2 then "" else "Z" <> posIze z2)
gcodeToText (GCFeedRate f (GCExtrude2 (x1,y1) (x2,y2) e)) = "G1 F" <> posIze f <> " " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> "E" <> posIze e

gcodeToText (GCFeedRate f (GCExtrude3 (x1,y1,z1) (x2,y2,z2) e)) = "G1 F" <> posIze f <> " " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> (if z1 ~== z2 then "" else "Z" <> posIze z2) <> "E" <> posIze e
gcodeToText (GCMove2 (x1,y1) (x2,y2)) = "G0 " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ")
gcodeToText (GCMove3 (x1,y1,z1) (x2,y2,z2)) = "G0 " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> (if z1 ~== z2 then "" else "Z" <> posIze z2)
gcodeToText (GCExtrude2 (x1,y1) (x2,y2) e) = "G1 " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> "E" <> posIze e
gcodeToText (GCExtrude3 (x1,y1,z1) (x2,y2,z2) e) = "G1 " <> (if x1 ~== x2 then "" else "X" <> posIze x2 <> " ") <> (if y1 ~== y2 then "" else "Y" <> posIze y2 <> " ") <> (if z1 ~== z2 then "" else "Z" <> posIze z2 <> " ") <> "E" <> posIze e
gcodeToText (GCFeedRate f wtf) = error $ "applying feedrate " <> show (posIze f) <> " to something other than a GCmove(2,3) or a GCExtrude(2,3): " <> show wtf
gcodeToText (GCRawFeedRate a b) = error $ "Attempting to generate gcode for a feedrate change command that has not yet been cooked:\nRate: " <> show a <> "\nGCode: " <> show b <> "\n"
gcodeToText (GCRawExtrude2 a b c) = error $ "Attempting to generate gcode for a 2D extrude command that has not yet been cooked:\nStart: " <> show a <> "\nStop: " <> show b <> "\nExtrude: " <> show c <> "\n"
gcodeToText (GCRawExtrude3 a b c) = error $ "Attempting to generate gcode for a 3D extrude command that has not yet been cooked:\nStart: " <> show a <> "\nStop: " <> show b <> "\nExtrude: " <> show c <> "\n"
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

-- | Generate GCode for a given contour.
-- Assumes the printer is already at the first point of the contour.
gcodeForContour :: ℝ -> ℝ -> ℝ -> Contour -> [GCode]
gcodeForContour lh pathWidth feedRate contour = addFeedRate feedRate headRes : tailRes
  where
    (headRes, tailRes) = fromMaybe (error "no results?") $ uncons res
    (headPoint,tailPoints) = fromMaybe (error "no contour points?") $ uncons contourPoints
    res = zipWith (make2DExtrudeGCode lh pathWidth) contourPoints tailPoints <> [make2DExtrudeGCode lh pathWidth (lastPointOfContour contour) headPoint]
    contourPoints = pointsOfContour contour

-- | For each group of lines, generate gcode for the segments, with move commands between them.
gcodeForInfill :: ℝ -> ℝ -> ℝ -> ℝ -> [[LineSeg]] -> [GCode]
gcodeForInfill _ _ _ _ [] = []
gcodeForInfill lh pathWidth infillFeedRate travelFeedRate lineGroups
  | lineGroups == [] = []
  | lineGroups == [[]] = []
  | otherwise = concat $ renderLineSegGroup headGroup : zipWith (\group1 group2 -> moveBetweenLineSegGroups group1 group2 <> renderLineSegGroup group2) lineGroups tailGroups
  where
    (headGroup, tailGroups) = (head lineGroups, tail lineGroups)
    -- FIXME: this should be a single gcode. why are we getting empty line groups given to us?
    moveBetweenLineSegGroups :: [LineSeg] -> [LineSeg] -> [GCode]
    moveBetweenLineSegGroups g1 g2 = case unsnoc g1 of
                                       Nothing -> error $ "given empty line group?\n" <> show g2 <> "\n"
                                       (Just (_,lastg1)) -> case g2 of
                                                              [] -> error $ "line group empty when finding line group following " <> show g1 <> "\n"
                                                              (firstg2:_) -> [moveBetween lastg1 firstg2]
    renderLineSegGroup :: [LineSeg] -> [GCode]
    renderLineSegGroup lineSegSet = case lineSegSet of
                                      [] -> []
                                      (myHeadGroup:myTailGroups) -> renderSegment myHeadGroup : concat (zipWith (\ l1 l2 -> moveBetween l1 l2 : [renderSegment l2]) lineSegSet myTailGroups)
    moveBetween :: LineSeg -> LineSeg -> GCode
    moveBetween l1 (LineSeg startPointl2 _) = addFeedRate travelFeedRate $ make2DTravelGCode (endPoint l1) startPointl2
    renderSegment :: LineSeg -> GCode
    renderSegment ln@(LineSeg startPoint _) = addFeedRate infillFeedRate $ make2DExtrudeGCode lh pathWidth startPoint $ endPoint ln

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

