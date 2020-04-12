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

-- FIXME: Force compilation.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types #-}

-- To treat literal strings as Text
{-# LANGUAGE OverloadedStrings #-}

-- For matching our OpenScad variable types.
{-# LANGUAGE ViewPatterns #-}

import Prelude ((*), (/), (+), (-), fromIntegral, odd, pi, sqrt, mod, round, floor, foldMap, fmap, (<>), toRational, FilePath, (**), Int, fromInteger, Eq, fromRational, init)

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord ((<=), (<), (>))

import Data.Tuple (fst, snd)

import Data.Text.Lazy (Text, pack, unpack, unlines)

import Data.String (String)

import Data.Bool(Bool(False), (||), (&&), otherwise)

import Data.List (nub, sortBy, lines, length, zip3, filter, tail, head, zipWith, maximum, (!!), minimum, splitAt, elem, last)

import Control.Monad ((>>=))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, mapMaybe, fromMaybe)

import Text.Show(show)

import System.IO (IO, writeFile, readFile)

import Control.Monad.State(runState)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch, hsubparser, command, many)

import Formatting(format, fixed)

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(ONum, OString, OBool), lookupVarIn, Message(Message), MessageType(TextOut), ScadOpts(ScadOpts))

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, ℕ, Fastℕ(Fastℕ), fromFastℕ, toFastℕ)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea, CylinderArea), Point(Point), Line(Line), point, lineIntersection, scalePoint, addPoints, distance, lineFromEndpoints, endpoint, midpoint, flipLine, Facet, sides, Contour(Contour), LayerType(BaseOdd, BaseEven, Middle), pointSlopeLength, perpendicularBisector, shiftFacet, orderPoints, roundToFifth, roundPoint, shortenLineBy, accumulateValues, makeLines, facetIntersects, getContours, simplifyContour, Extruder(Extruder), nozzleDiameter, filamentWidth, EPos(EPos), StateM, MachineState(MachineState), getEPos, setEPos, facetLinesFromSTL)

default (ℕ, Fastℕ, ℝ)

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- | Given a point and slope (on an xy plane), make "infinite" line
-- (i.e. a line that hits two edges of the bed)
-- FIXME: assumes the origin is at the corner.
lineToEdges :: BuildArea -> Point -> ℝ -> Line
lineToEdges (RectArea (bedX,bedY,_)) p@(Point (_,_,c)) m = head . makeLines $ nub points
    where edges = lineFromEndpoints <$> [Point (0,0,c), Point (bedX,bedY,c)]
                                    <*> [Point (0,bedY,c), Point (bedX,0,c)]
          longestLength = sqrt $ bedX*bedX + bedY*bedY
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = mapMaybe (lineIntersection line) edges

-- Center facets relative to the center of the build area.
-- FIXME: assumes the origin is at the corner.
centerFacets :: BuildArea -> [Facet] -> ([Facet], Point)
centerFacets (RectArea (bedX,bedY,_)) fs = (shiftFacet (Point (dx,dy,dz)) <$> fs, Point (dx,dy,dz))
    where (dx,dy,dz) = (bedX/2-x0, bedY/2-y0, -zMin)
          xMin = minimum $ xOf.point <$> foldMap sides fs
          yMin = minimum $ yOf.point <$> foldMap sides fs
          zMin = minimum $ zOf.point <$> foldMap sides fs
          xMax = maximum $ xOf.point <$> foldMap sides fs
          yMax = maximum $ yOf.point <$> foldMap sides fs
          (x0,y0) = ((xMax+xMin)/2-xMin, (yMax+yMin)/2-yMin)
          xOf, yOf, zOf :: Point -> ℝ
          xOf (Point (x,_,_)) = x
          yOf (Point (_,y,_)) = y
          zOf (Point (_,_,z)) = z

-----------------------------------------------------------------------
---------------------- Contour filling --------------------------------
-----------------------------------------------------------------------

-- The amount to extrude when making a line between two points.
extrusionAmount :: Extruder -> ℝ -> Point -> Point -> ℝ
extrusionAmount extruder lh p1 p2 = nozzleDia * lh * (2 / filamentDia) * pathLength / pi
    where pathLength = distance p1 p2
          nozzleDia = nozzleDiameter extruder
          filamentDia = filamentWidth extruder

-- Calculate the amount of material to extrude for each line in a contour.
extrusions :: Extruder -> ℝ -> Point -> Contour -> [ℝ]
extrusions _ _ _ (Contour []) = []
extrusions extruder lh startPoint (Contour contourPoints) = zipWith (extrusionAmount extruder lh) (startPoint:contourPoints) contourPoints

-- Make infill
makeInfill :: BuildArea -> Print -> [Contour] -> ℝ -> LayerType -> [Line]
makeInfill buildarea print contours zHeight layerType = foldMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill buildarea print zHeight
          infillCover BaseEven = coveringLinesUp buildarea ls zHeight
          infillCover BaseOdd = coveringLinesDown buildarea ls zHeight
          ls = lineSpacing print

-- Get the segments of an infill line that are inside the contour
infillLineInside :: [Contour] -> Line -> [Line]
infillLineInside contours line = (allLines !!) <$> [0,2..length allLines - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: [Contour] -> Line -> [Point]
getInfillLineIntersections contours line = nub $ mapMaybe (lineIntersection line) contourLines
    where
      contourLines = foldMap makeLines $ contourPoints <$> contours
      contourPoints (Contour points) = points

-- Generate covering lines as infill.
-- FIXME: Very bad algorithm. Prunes down lines instead of adjusting their spacing.
coveringInfill :: BuildArea -> Print -> ℝ -> [Line]
coveringInfill buildarea print zHeight
    | infill == 0 = []
    | otherwise = pruneInfill (coveringLinesUp buildarea ls zHeight) <> pruneInfill (coveringLinesDown buildarea ls zHeight)
    where
      pruneInfill :: [Line] -> [Line]
      pruneInfill l = (l !!) <$> [0, (floor $ 1/infill) ..length l-1]
      ls = lineSpacing print
      infill = infillAmount print

-- Generate lines over entire print area
coveringLinesUp :: BuildArea -> ℝ -> ℝ -> [Line]
coveringLinesUp (RectArea (bedX,bedY,_)) lt zHeight = flip Line s . f <$> [-bedX,-bedX + lt..bedY]
    where s = Point (bedX + bedY,bedX + bedY,0)
          f v = Point (0,v,zHeight)

coveringLinesDown :: BuildArea -> ℝ -> ℝ -> [Line]
coveringLinesDown (RectArea (bedX,bedY,_)) lt zHeight = flip Line s . f <$> [0,lt..bedY + bedX]
    where s =  Point (bedX + bedY,- bedX - bedY,0)
          f v = Point (0,v,zHeight)

-- FIXME: better way to handle infinity.
lineSlope :: Point -> ℝ
lineSlope m = case xOf m of 0 -> if yOf m > 0 then 10**101 else -(10**101)
                            _ -> yOf m / xOf m
  where
    yOf, xOf :: Point -> ℝ
    xOf (Point (x,_,_)) = x
    yOf (Point (_,y,_)) = y

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: Extruder -> Fastℕ -> Line -> [Point]
pointsForPerimeters extruder perimeterCount l = endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia) <$> filter (/= 0) [-n..n]
  where
    n :: ℝ
    n = fromIntegral $ perimeterCount - 1
    Line _ m = perpendicularBisector l
    nozzleDia :: ℝ
    nozzleDia = nozzleDiameter extruder

-- Lines to count intersections to determine if we're on the inside or outside
perimeterLinesToCheck :: Extruder -> Line -> [Line]
perimeterLinesToCheck extruder l@(Line p _) = (`lineFromEndpoints` Point (0,0,zOf p)) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia) <$> [-1,1]
  where
    Line _ m = perpendicularBisector l
    nozzleDia :: ℝ
    nozzleDia = nozzleDiameter extruder
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- Find the point corresponding to the inner perimeter of a given line, given all of the
-- contours in the object
innerPerimeterPoint :: Extruder -> Line -> [Contour] -> Point
innerPerimeterPoint extruder l contours
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | otherwise = snd $ head intersections
    where linesToCheck = perimeterLinesToCheck extruder l
          contourLines = foldMap makeLines $ contourPoints <$> contours
          contourPoints (Contour points) = points
          simplifiedContour = simplifyContour contourLines
          numIntersections l' = length $ mapMaybe (lineIntersection l') simplifiedContour
          intersections = (\a -> (numIntersections a, point a)) <$> linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct lines on the interior for a given line
interiorLines :: Printer -> Fastℕ -> Line -> [Contour] -> [Line]
interiorLines (Printer _ buildarea extruder) perimeterCount l@(Line _ m) contours
    | innerPoint `elem` firstHalf = flip (lineToEdges buildarea) (lineSlope m) <$> firstHalf
    | otherwise = flip (lineToEdges buildarea) (lineSlope m) <$> secondHalf
    where innerPoint = innerPerimeterPoint extruder l contours
          (firstHalf, secondHalf) = splitAt (fromFastℕ $ perimeterCount - 1) $ pointsForPerimeters extruder perimeterCount l

-- List of interior lines for each line in a contour
allInteriors :: Printer -> Fastℕ -> Contour -> [Contour] -> [[Line]]
allInteriors printer perimeterCount (Contour contourPoints) contours = flip (interiorLines printer perimeterCount) contours <$> targetLines
    where targetLines = makeLines contourPoints

-- Make inner contours from a list of (outer) contours.
-- note that we do not retain the outermost contour.
innerContours :: Printer -> Fastℕ -> [Contour] -> [[Contour]]
innerContours printer perimeterCount contours = foldMap (constructInnerContours .(\i -> last i : i)) interiors
    where interiors = flip (allInteriors printer perimeterCount) contours <$> contours

-- Construct inner contours for a list of Contours.
-- Essentially a helper function for innerContours.
constructInnerContours :: [[Line]] -> [[Contour]]
constructInnerContours interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours $ tail interiors
    | otherwise = [Contour intersections] : constructInnerContours (tail <$> interiors)
    where intersections = catMaybes $ consecutiveIntersections $ head <$> interiors

consecutiveIntersections :: [Line] -> [Maybe Point]
consecutiveIntersections [] = [Nothing]
consecutiveIntersections [_] = [Nothing]
consecutiveIntersections points = zipWith lineIntersection points (tail points)

-- Generate G-code for a given contour.
gcodeForContour :: Extruder
                -> ℝ
                -> Contour
                -> StateM [Text]
gcodeForContour extruder lh (Contour contourPoints) = do
  currentPos <- fromRational <$> getEPos
  let
    ePoses = (currentPos+) <$> accumulateValues extrusionAmounts
    extrusionAmounts = extrusions extruder lh (head contourPoints) (Contour $ tail contourPoints)
  setEPos . toRational $ last ePoses
  pure $ makeTravelGCode (head contourPoints) : zipWith makeExtrudeGCode (tail contourPoints) ePoses

-- GCode to travel to a point while extruding.
makeExtrudeGCode :: Point -> ℝ -> Text
makeExtrudeGCode p e = "G1 " <> showPoint p <> " E" <> posIze e
  where
    showPoint (Point (x1,y1,z1)) = "X" <> posIze x1 <> " Y" <> posIze y1 <> " Z" <> posIze z1
    posIze :: ℝ -> Text
    posIze pos
      | pos == 0 = "0"
      | pos < 0.1 && pos > -0.1 = format (fixed 5) pos
      | otherwise = pack . show $ roundToFifth pos

-- GCode to travel to a point without extruding
makeTravelGCode :: Point -> Text
makeTravelGCode p = "G0 " <> showPoint p
  where
    showPoint (Point (x1,y1,z1)) = "X" <> posIze x1 <> " Y" <> posIze y1 <> " Z" <> posIze z1
    posIze :: ℝ -> Text
    posIze pos
      | pos == 0 = "0"
      | pos < 0.1 && pos > -0.1 = format (fixed 5) pos
      | otherwise = pack . show $ roundToFifth pos

-- FIXME: generate ';LAYER_COUNT:75'
-- FIXME: generate ';TYPE:WALL-INNER'
-- FIXME: generate ';TYPE:WALL-OUTER'
-- FIXME: generate ';TYPE:SKIN'
gcodeForNestedContours :: Extruder
                       -> ℝ
                       -> [[Contour]]
                       -> StateM [Text]
gcodeForNestedContours _ _ [] = pure []
gcodeForNestedContours extruder lh [c] = gcodeForContours extruder lh c
gcodeForNestedContours extruder lh (c:cs) = do
  oneContour <- firstContourGCode
  remainingContours <- gcodeForNestedContours extruder lh cs
  pure $ oneContour <> remainingContours
    where firstContourGCode = gcodeForContours extruder lh c

-- FIXME: generate ';LAYER:0'
gcodeForContours :: Extruder
                 -> ℝ
                 -> [Contour]
                 -> StateM [Text]
gcodeForContours _ _ [] = pure []
gcodeForContours extruder lh [c] = gcodeForContour extruder lh c
gcodeForContours extruder lh (c:cs) = do
  oneContour <- firstContourGCode
  remainingContours <- gcodeForContours extruder lh cs
  pure $ oneContour <> remainingContours
    where firstContourGCode = gcodeForContour extruder lh c

-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

-- A bounding box. a box around a contour.
data BBox = BBox ℝ2 ℝ2

-- Check if a bounding box is empty.
isEmpty :: BBox -> Bool
isEmpty (BBox (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

-- Get a bounding box of all contours.
boundingBoxAll :: [Contour] -> Maybe BBox
boundingBoxAll contours = if isEmpty box then Nothing else Just box
    where
      box  = BBox (minX, minY) (maxX, maxY)
      minX = minimum $ (\(BBox (x1,_) _) -> x1) <$> bBoxes
      minY = minimum $ (\(BBox (_,y1) _) -> y1) <$> bBoxes
      maxX = maximum $ (\(BBox _ (x2,_)) -> x2) <$> bBoxes
      maxY = maximum $ (\(BBox _ (_,y2)) -> y2) <$> bBoxes
      bBoxes = mapMaybe boundingBox contours


-- Get a bounding box of a contour.
boundingBox :: Contour -> Maybe BBox
boundingBox (Contour []) = Nothing
boundingBox (Contour contourPoints) = if isEmpty box then Nothing else Just box
  where
    box  = BBox (minX, minY) (maxX, maxY)
    minX = minimum $ xOf <$> contourPoints
    minY = minimum $ yOf <$> contourPoints
    maxX = maximum $ xOf <$> contourPoints
    maxY = maximum $ yOf <$> contourPoints
    xOf,yOf :: Point -> ℝ
    xOf (Point (x,_,_)) = x
    yOf (Point (_,y,_)) = y

-- Put a fixed amount around the bounding box.
incBBox :: BBox -> ℝ -> BBox
incBBox (BBox (x1,y1) (x2,y2)) amount = BBox (x1+amount, y1+amount) (x2-amount, y2-amount)

-- add a 2D bounding box to a list of contours, as the first contour in the list.
-- FIXME: assumes 2D contour.
addBBox :: [Contour] -> ℝ -> [Contour]
addBBox contours z0 = Contour [Point (x1,y1,z0), Point (x2,y1,z0), Point (x2,y2,z0), Point (x1,y2,z0), Point (x1,y1,z0)] : contours
    where
      bbox = fromMaybe (BBox (1,1) (-1,-1)) $ boundingBoxAll contours
      (BBox (x1, y1) (x2, y2)) = incBBox bbox 1

-- Generate support
-- FIXME: hard coded infill amount.
makeSupport :: BuildArea
            -> Print
            -> [Contour]
            -> ℝ
            -> [Line]
makeSupport buildarea print contours zHeight = fmap (shortenLineBy $ 2 * lh)
                                             $ foldMap (infillLineInside (addBBox contours zHeight))
                                             $ coveringInfill buildarea print zHeight
    where
      lh = layerHeight print

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: Print -> [Facet] -> [[[Point]]]
layers print fs = allIntersections <$> [lh,lh*2..maxheight] <*> pure fs
    where zmax = maximum $ zOf . point <$> foldMap sides fs
          maxheight = lh * fromIntegral (floor (zmax / lh)::Fastℕ)
          lh = layerHeight print
          zOf :: Point -> ℝ
          zOf (Point (_,_,z)) = z

getLayerType :: Print -> (Fastℕ, Fastℕ) -> LayerType
getLayerType print (fromStart, toEnd)
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
  | otherwise = Middle
  where
    topBottomLayers :: Fastℕ
    topBottomLayers = round $ surfaceThickness print / layerHeight print

----------------------------------------------------------------------
---------------------------- MISC ------------------------------------
----------------------------------------------------------------------

fixContour :: Contour -> Contour
fixContour (Contour c) = Contour (head c : tail c <> [head c])

-- Find all the points in the mesh at a given z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: ℝ -> [Facet] -> [[Point]]
allIntersections v fs = fmap (fmap roundPoint) $ catMaybes $ facetIntersects v <$> fs

-- Map a function to every other value in a list. This is useful for fixing non-extruding lines.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f xs = zipWith (\x v -> if odd v then f x else x) xs [1::Fastℕ,2..]

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------

-- Input should be top to bottom, output should be bottom to top
-- FIXME: construct ';TYPE:FILL'
sliceObject ::  Printer ->  Print
                  -> [([Contour], Fastℕ, Fastℕ)] -> StateM [Text]
sliceObject _ _ [] = pure []
sliceObject printer@(Printer _ buildarea extruder) print@(Print perimeterCount _ lh _ hasSupport _) ((a, fromStart, toEnd):as) = do
  outerContourGCode <- gcodeForContours extruder lh contours
  innerContourGCode <- gcodeForNestedContours extruder lh interior
  supportGCode <- if hasSupport then gcodeForContour extruder lh supportContours else pure []
  infillGCode <- gcodeForContour extruder lh infillContours
  theRest <- sliceObject printer print as
  let
    travelGCode = if theRest == [] then [] else makeTravelGCode <$> pointsOfContour (head contours)
  pure $ outerContourGCode <> innerContourGCode <> travelGCode <> supportGCode <> infillGCode <> theRest
    where
      contours = getContours (pointsOfContour <$> a)
      pointsOfContour (Contour points) = points
      interior = fmap fixContour <$> innerContours printer perimeterCount contours
      supportContours = foldMap (\l -> Contour [point l, endpoint l])
                        $ mapEveryOther flipLine
                        $ makeSupport buildarea print contours (zHeightOfLayer contours)
      infillContours = foldMap (\l -> Contour [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeInfill buildarea print innermostContours (zHeightOfLayer innermostContours)
                       $ getLayerType print (fromStart, toEnd)
      allContours = zipWith (:) contours interior
      innermostContours = if interior == [] then contours else last <$> allContours
      zHeightOfLayer targetContours = zOfContour $ head targetContours
      zOfContour (Contour contourPoints) = zOf $ head contourPoints
      zOf :: Point -> ℝ
      zOf (Point (_,_,z)) = z

----------------------------------------------------------
------------------------ OPTIONS -------------------------
----------------------------------------------------------

{-
-- Optional settings, that can happen at the global scope, or in the individual scope.
data CuraSettings =
  CuraSettings
    {
      perimeterCountCOpt       :: Maybe Fastℕ
    , infillPercentCOpt        :: Maybe ℝ
    , layerHeightMMCOpt        :: Maybe ℝ
    , topBottomThicknessMMCOpt :: Maybe ℝ
    , generateSupportCOpt      :: Bool
    , lineSpacingMMCOpt        :: Maybe ℝ
    }

-- Container for the individual scope.
data ExtCuraObjectOpts =
  ExtCuraObjectOpts
    {
    }
-}

-- Container for the global scope.
-- FIXME: extruders and STLs are supposed to be different scopes.
data ExtCuraEngineOpts =
  ExtCuraEngineOpts
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

-- | A parser for curaengine style command line arguments.
extCuraEngineOpts :: Parser ExtCuraEngineOpts
extCuraEngineOpts = hsubparser
  (( command "connect"
    (info connectParser (progDesc "Connect to target"))
  ) <>
  ( command "slice"
    (info sliceParser (progDesc "Slice input file"))
  )) 

--connectParser :: Parser ExtCuraEngineOpts
connectParser = ExtCuraEngineOpts <$>
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

--sliceParser :: Parser ExtCuraEngineOpts
sliceParser = ExtCuraEngineOpts <$>
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
    _perimeters      :: Fastℕ
  , infillAmount     :: ℝ -- as an amount from 0 (none) to 1 (full density).
  , layerHeight      :: ℝ
  , surfaceThickness :: ℝ
  , _withSupport     :: Bool
  , lineSpacing      :: ℝ -- In Millimeters.
  }

run :: ExtCuraEngineOpts -> IO ()
run rawArgs = do
    let
      args = rawArgs
      inFile = fromMaybe "in.stl" $ inputFileOpt args
    stl <- readFile inFile
    -- FIXME: do something with messages.
    (settings, messages) <- addConstants $ settingOpts args
    let
      stlLines  = lines stl
      printer   = printerFromSettings settings
      buildarea = buildArea printer
      (facets, _) = centerFacets buildarea $ facetLinesFromSTL stlLines
      print = printFromSettings settings
      allLayers :: [[Contour]]
      allLayers = filter (\(Contour l) -> head l /= head (tail l)) . filter (/=Contour []) <$> (fmap Contour <$> layers print facets)
      object = zip3 allLayers [1..(toFastℕ $ length allLayers)] [2..(toFastℕ $ length allLayers + 1)]
      (gcode, _) = runState (sliceObject printer print object) (MachineState (EPos 0))
      outFile = fromMaybe "out.gcode" $ outputFileOpt args
    writeFile outFile $ unpack ((startingGCode settings) <> unlines gcode <> (endingGCode settings))
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
              maybeFillamentDiameter (lookupVarIn "material_diameter" -> Just (ONum diameter)) = Just diameter
              maybeFillamentDiameter _ = Nothing

              -- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it.
              getPrintBed var = RectBed ((fromMaybe 150 $ maybeX var),
                                         (fromMaybe 150 $ maybeY var))
              -- The area we can print inside of.
              defaultBuildArea var = RectArea ((fromMaybe 150 $ maybeX var),
                                           (fromMaybe 150 $ maybeY var),
                                           (fromMaybe 50  $ maybeZ var))
              -- The Extruder. note that this includes the diameter of the feed filament.
              defaultExtruder :: VarLookup -> Extruder
              defaultExtruder var = Extruder (fromMaybe 1.75 $ maybeFillamentDiameter var)
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
          where
            maybeLayerHeight (lookupVarIn "layer_height" -> Just (ONum thickness)) = Just thickness
            maybeLayerHeight _ = Nothing
            maybeInfillAmount (lookupVarIn "infill_sparse_density" -> Just (ONum amount)) = Just (amount / 100)
            maybeInfillAmount _ = Nothing
            maybeWallLineCount (lookupVarIn "wall_line_count" -> Just (ONum count)) = maybeToFastℕ count
              where
                maybeToFastℕ n = if (fromInteger $ floor n) == (n::ℝ) then Just . Fastℕ $ floor n else Nothing
            maybeWallLineCount _ = Nothing
            maybeSupport (lookupVarIn "support_enable" -> Just (OBool enable)) = Just enable
            maybeSupport _ = Nothing
            maybeTopBottomThickness (lookupVarIn "top_bottom_thickness" -> Just (ONum thickness)) = Just thickness
            maybeTopBottomThickness _ = Nothing
            maybeInfillLineWidth (lookupVarIn "infill_line_width" -> Just (ONum width)) = Just width
            maybeInfillLineWidth _ = Nothing
        startingGCode, endingGCode :: VarLookup -> Text
        startingGCode (lookupVarIn "machine_start_gcode" -> Just (OString startGCode)) = pack startGCode
        startingGCode _ = "G21 ;metric values\n"
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
        endingGCode (lookupVarIn "machine_end_gcode" -> Just (OString endGCode)) = pack endGCode
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

