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

import Prelude ((*), (/), (+), (-), fromIntegral, odd, pi, error, sqrt, mod, round, floor, foldMap, fmap, (<>), toRational, error, FilePath, (**))

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord ((<=), (<), (>), max)

import Data.Tuple (fst, snd)

import Text.Read(read)

import Data.Text (Text, pack, unpack, unlines, unwords)

import Data.Text as DT (words)

import Data.String (String)

import Data.Bool(Bool, (||), (&&), otherwise)

import Data.List (nub, sortBy, lines, length, reverse, zip3, filter, tail, head, zipWith, maximum, (!!), minimum, init, splitAt, elem, take, last)

import Data.List as DL (words)

import Control.Monad ((>>=))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, mapMaybe, fromMaybe)

import Text.Show(show)

import System.IO (IO, writeFile, readFile)

import Control.Monad.State(runState)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea, CylinderArea), ℝ, ℝ2, toℝ, ℕ, Fastℕ, fromFastℕ, toFastℕ, Point(Point), Line(Line), point, lineIntersection, scalePoint, addPoints, distance, lineFromEndpoints, endpoint, midpoint, flipLine, Facet(Facet), sides, Contour, LayerType(BaseOdd, BaseEven, Middle), pointSlopeLength, perpendicularBisector, shiftFacet, orderPoints, roundToFifth, roundPoint, shortenLineBy, accumulateValues, facetsFromSTL, cleanupFacet, makeLines, facetIntersects, getContours, simplifyContour, Extruder(Extruder), nozzleDiameter, filamentWidth, EPos(EPos), StateM, MachineState(MachineState), getEPos, setEPos)

default (ℕ, Fastℕ, ℝ)

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- | Given a point and slope (on an xy plane), make "infinite" line
-- FIXME: assumes the origin is at the corner.
-- (i.e. a line that hits two edges of the bed)
lineToEdges :: BuildArea -> Point -> ℝ -> Line
lineToEdges (RectArea (bedX,bedY,_)) p@(Point (_,_,c)) m = head . makeLines $ nub points
    where edges = lineFromEndpoints <$> [Point (0,0,c), Point (bedX,bedY,c)]
                                    <*> [Point (0,bedY,c), Point (bedX,0,c)]
          longestLength = sqrt $ bedX*bedX + bedY*bedY
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = mapMaybe (lineIntersection line) edges

-- Center facets relative to the center of the build area.
centerFacets :: BuildArea -> [Facet] -> ([Facet], Point)
centerFacets (RectArea (bedX,bedY,_)) fs = (shiftFacet (Point (dx,dy,dz)) <$> fs, Point (dx,dy,dz))
    where (dx,dy,dz) = ((bedX/2-x0), (bedY/2-y0), (-zMin))
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

----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- Read a point when it's given a string of the form "x y z"
readPoint :: String -> Point
readPoint s = do
  let
    xval, yval, zval :: ℝ
    (xval, yval, zval) = readThree $ take 3 $ DL.words s
  Point ((xval),(yval),(zval))
    where
      readThree :: [String] -> (ℝ,ℝ,ℝ)
      readThree [xv,yv,zv] = (read xv,read yv,read zv)
      readThree _ = error "unexpected value when reading point."

-- Read a list of three coordinates (as strings separated by spaces) and generate a facet.
readFacet :: [String] -> Facet
readFacet f
    | length f < 3 = error "Invalid facet"
    | otherwise = Facet . makeLines $ readPoint <$> f'
    where f' = last f : f -- So that we're cyclic

-- From STL file (as a list of Strings, each String corresponding to one line),
-- produce a list of lists of Lines, where each list of Lines corresponds to a
-- facet in the original STL
facetLinesFromSTL :: [String] -> [Facet]
facetLinesFromSTL = fmap (readFacet . cleanupFacet) . facetsFromSTL

-----------------------------------------------------------------------
---------------------- Contour filling --------------------------------
-----------------------------------------------------------------------

-- The amount to extrude when making a line between two points.
extrusionAmount :: Extruder -> ℝ -> Point -> Point -> ℝ
extrusionAmount extruder t p1 p2 = nozzleDia * t * (2 / filamentDia) * l / pi
    where l = distance p1 p2
          nozzleDia = nozzleDiameter extruder
          filamentDia = filamentWidth extruder

-- Given a contour and the point to start from, calculate the amount of material to extrude for each line.
extrusions :: Extruder -> ℝ -> Point -> [Point] -> [ℝ]
extrusions _ _ _ [] = []
extrusions extruder lh p c = extrusionAmount extruder lh p (head c) : extrusions extruder lh (head c) (tail c)

-- Make infill
makeInfill :: BuildArea -> Print -> [[Point]] -> LayerType -> [Line]
makeInfill buildarea print contours layerType = foldMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill buildarea print zHeight
          infillCover BaseEven = coveringLinesUp buildarea ls zHeight
          infillCover BaseOdd = coveringLinesDown buildarea ls zHeight
          zHeight = zOf . head $ head contours
          ls = lineSpacing print
          zOf :: Point -> ℝ
          zOf (Point (_,_,z)) = z

-- Get the segments of an infill line that are inside the contour
infillLineInside :: [[Point]] -> Line -> [Line]
infillLineInside contours line = (allLines !!) <$> [0,2..length allLines - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: [[Point]] -> Line -> [Point]
getInfillLineIntersections contours line = nub $ mapMaybe (lineIntersection line) contourLines
    where contourLines = foldMap makeLines contours

-- Generate covering lines for a given percent infill
coveringInfill :: BuildArea -> Print -> ℝ -> [Line]
coveringInfill buildarea print zHeight
    | infill == 0 = []
    | otherwise = pruneInfill (coveringLinesUp buildarea ls zHeight) <> pruneInfill (coveringLinesDown buildarea ls zHeight)
    where
      n :: ℝ
      n = max 1 (infill/100)
      pruneInfill :: [Line] -> [Line]
      pruneInfill l = (l !!) <$> [0, (floor n)..length l-1]
      ls = lineSpacing print
      infill = infillAmmount print

-- Generate lines over entire print area
coveringLinesUp :: BuildArea -> ℝ -> ℝ -> [Line]
coveringLinesUp (RectArea (bedX,bedY,_)) lt zHeight = flip Line s . f <$> [-bedX,-bedX + lt..bedY]
    where s = Point ((bedX + bedY),(bedX + bedY),0)
          f v = Point (0,v,zHeight)

coveringLinesDown :: BuildArea -> ℝ -> ℝ -> [Line]
coveringLinesDown (RectArea (bedX,bedY,_)) lt zHeight = flip Line s . f <$> [0,lt..bedY + bedX]
    where s =  Point ((bedX + bedY),(- bedX - bedY),0)
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
perimeterLinesToCheck extruder l@(Line p _) = ((`lineFromEndpoints` Point (0,0,(zOf p))) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia)) <$> [-1,1]
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
          contourLines = foldMap makeLines contours
          simplifiedContour = simplifyContour contourLines
          numIntersections l' = length $ mapMaybe (lineIntersection l') simplifiedContour
          intersections = (\a -> (numIntersections a, point a)) <$> linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct lines on the interior for a given line
interiorLines :: Printer -> Fastℕ -> Line -> [[Point]] -> [Line]
interiorLines (Printer _ buildarea extruder) perimeterCount l@(Line _ m) contours
    | innerPoint `elem` firstHalf = flip (lineToEdges buildarea) (lineSlope m) <$> firstHalf
    | otherwise = flip (lineToEdges buildarea) (lineSlope m) <$> secondHalf
    where innerPoint = innerPerimeterPoint extruder l contours
          (firstHalf, secondHalf) = splitAt (fromFastℕ $ perimeterCount - 1) $ pointsForPerimeters extruder perimeterCount l

-- List of lists of interior lines for each line in a contour
allInteriors :: Printer -> Fastℕ -> [Point] -> [[Point]] -> [[Line]]
allInteriors printer perimeterCount c contours = flip (interiorLines printer perimeterCount) contours <$> targetLines
    where targetLines = makeLines c

-- Make inner contours from a list of (outer) contours.
-- note that we do not retain the outermost contour.
innerContours :: Printer -> Fastℕ -> [Contour] -> [[Contour]]
innerContours printer perimeterCount contours = foldMap (constructInnerContours .(\i -> last i : i)) interiors
    where interiors = flip (allInteriors printer perimeterCount) contours <$> contours

-- Construct inner contours, given a list of lines constituting the interior
-- lines. Essentially a helper function for innerContours.
constructInnerContours :: [[Line]] -> [[Contour]]
constructInnerContours interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours $ tail interiors
    | otherwise = [intersections] : constructInnerContours (tail <$> interiors)
    where intersections = catMaybes $ consecutiveIntersections $ head <$> interiors

consecutiveIntersections :: [Line] -> [Maybe Point]
consecutiveIntersections [] = [Nothing]
consecutiveIntersections [_] = [Nothing]
consecutiveIntersections (a:b:cs) = lineIntersection a b : consecutiveIntersections (b : cs)

-- Generate G-code for a given contour c.
gcodeForContour :: Extruder
                -> ℝ
                -> [Point]
                -> StateM [Text]
gcodeForContour extruder lh c = do
  currentPos <- toℝ <$> getEPos
  let
    extrusionAmounts = extrusions extruder lh (head c) (tail c)
    ePoses = accumulateValues extrusionAmounts
    newPoses = (currentPos+) <$> ePoses
    es = (" E" <>) . pack . show <$> newPoses
  setEPos . toRational $ last newPoses
  pure $ ("G1 " <>) <$> zipWith (<>) (pack . show <$> c) ("":es)

gcodeForNestedContours :: Extruder
                       -> ℝ
                       -> [[Contour]]
                       -> StateM [Text]
gcodeForNestedContours _ _ [] = pure []
gcodeForNestedContours extruder lh [c] = gcodeForContours extruder lh c
gcodeForNestedContours extruder lh (c:cs) = do
  oneContour <- firstContoursGCode
  remainingContours <- gcodeForNestedContours extruder lh cs
  pure $ oneContour <> remainingContours
    where firstContoursGCode = gcodeForContours extruder lh c

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

-- G-code to travel to a point without extruding
makeTravelGCode :: Point -> Text
makeTravelGCode p = ("G1 " <>) $ pack $ show p

-- I'm not super happy about this, but it makes extrusion values correct
fixGCode :: [Text] -> [Text]
fixGCode [] = []
fixGCode [a] = [a]
fixGCode (a:b:cs) = unwords (init $ DT.words a) : b : fixGCode cs

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
boundingBoxAll contours = if (isEmpty box) then Nothing else Just box
    where
      box  = BBox (minX, minY) (maxX, maxY)
      minX = minimum $ (\(BBox (x1,_) _) -> x1) <$> bBoxes
      minY = minimum $ (\(BBox (_,y1) _) -> y1) <$> bBoxes
      maxX = maximum $ (\(BBox _ (x2,_)) -> x2) <$> bBoxes
      maxY = maximum $ (\(BBox _ (_,y2)) -> y2) <$> bBoxes
      bBoxes = mapMaybe boundingBox contours


-- Get a bounding box of a contour.
boundingBox :: Contour -> Maybe BBox
boundingBox [] = Nothing
boundingBox contour = if (isEmpty box) then Nothing else Just box
  where
    box  = BBox (minX, minY) (maxX, maxY)
    minX = minimum $ xOf <$> contour
    minY = minimum $ yOf <$> contour
    maxX = maximum $ xOf <$> contour
    maxY = maximum $ yOf <$> contour
    xOf,yOf :: Point -> ℝ
    xOf (Point (x,_,_)) = x
    yOf (Point (_,y,_)) = y

-- Put a fixed amount around the bounding box.
incBBox :: BBox -> ℝ -> BBox
incBBox (BBox (x1,y1) (x2,y2)) ammount = BBox (x1+ammount, y1+ammount) (x2-ammount, y2-ammount)

-- add the bounding box to a list of contours, as the first layer.
-- FIXME: magic number.
addBBox :: [Contour] -> [Contour]
addBBox contours = [Point (x1,y1,z0), Point (x2,y1,z0), Point (x2,y2,z0), Point (x1,y2,z0), Point (x1,y1,z0)] : contours
    where
      bbox = fromMaybe (BBox (1,1) (-1,-1)) $ boundingBoxAll contours
      (BBox (x1, y1) (x2, y2)) = incBBox bbox 1
      z0 = zOf . head $ head contours
      zOf :: Point -> ℝ
      zOf (Point (_,_,z)) = z

-- Generate support
-- FIXME: hard coded infill amount.
makeSupport :: BuildArea
            -> Print
            -> [[Point]]
            -> [Line]
makeSupport buildarea print contours = fmap (shortenLineBy $ 2 * lh)
                                       $ foldMap (infillLineInside (addBBox contours))
                                       $ coveringInfill buildarea print zHeight
    where
      lh = layerHeight print
      zHeight = zOf . head $ head contours
      zOf :: Point -> ℝ
      zOf (Point (_,_,z)) = z

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: Print -> [Facet] -> [[[Point]]]
layers print fs = fmap (allIntersections.roundToFifth) [maxheight,maxheight-lh..0] <*> pure fs
    where zmax = maximum $ (zOf.point) <$> (foldMap sides fs)
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

fixContour :: [Point] -> [Point]
fixContour c = head c : tail c <> [head c]

-- Find all the points in the mesh at a given z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: ℝ -> [Facet] -> [[Point]]
allIntersections v fs = fmap (fmap roundPoint) $ filter (/= []) $ (facetIntersects v) <$> fs

-- Map a function to every other value in a list. This is useful for fixing non-extruding
-- lines.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f (a:b:cs) = f a : b : mapEveryOther f cs

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------

-- Input should be top to bottom, output should be bottom to top
sliceObject ::  Printer ->  Print
                  -> [([Contour], Fastℕ, Fastℕ)] -> StateM [Text]
sliceObject _ _ [] = pure []
sliceObject printer@(Printer _ buildarea extruder) print@(Print perimeterCount _ lh _ hasSupport _) ((a, fromStart, toEnd):as) = do
  theRest <- sliceObject printer print as
  outerContourGCode <- gcodeForContours extruder lh contours
  innerContourGCode <- gcodeForNestedContours extruder lh interior
  let
    travelGCode = if theRest == [] then [] else makeTravelGCode <$> head contours
  supportGCode <- if hasSupport then fixGCode <$> gcodeForContour extruder lh supportContours else pure []
  infillGCode <- fixGCode <$> gcodeForContour extruder lh infillContours
  pure $ theRest <> outerContourGCode <> innerContourGCode <> travelGCode <> supportGCode <> infillGCode
    where
      contours = getContours a
      interior = fmap fixContour <$> innerContours printer perimeterCount contours
      supportContours = foldMap (\l -> [point l, endpoint l])
                        $ mapEveryOther flipLine
                        $ makeSupport buildarea print contours
      infillContours = foldMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeInfill buildarea print innermostContours
                       $ getLayerType print (fromStart, toEnd)
      allContours = zipWith (:) contours interior
      innermostContours = if interior == [] then contours else last <$> allContours

----------------------------------------------------------
------------------------ OPTIONS -------------------------
----------------------------------------------------------

-- Note: we're modeling the current engine options, and will switch to curaEngine style after the json parser is integrated.
data ExtCuraEngineOpts = ExtCuraEngineOpts
    { perimeterCountOpt       :: Maybe Fastℕ
    , infillPercentOpt        :: Maybe ℝ
    , layerHeightMMOpt        :: Maybe ℝ
    , topBottomThicknessMMOpt :: Maybe ℝ
    , generateSupportOpt      :: Bool
    , outputFile              :: Maybe FilePath
--    , center           :: Maybe Point
    , lineSpacingMMOpt        :: Maybe ℝ
    , inputFile               :: String
    }

-- | The parser for our command line arguments.
extCuraEngineOpts :: Parser ExtCuraEngineOpts
extCuraEngineOpts = ExtCuraEngineOpts
  <$> optional (
  option auto
    (    short 'p'
      <> long "perimeters"
      <> help "How many layers go around each contour"
    )
  )
-- FIXME: constrain this to be 0 or greater.
  <*> optional (
  option auto
    (    short 'i'
      <> long "infill"
      <> metavar "INFILL"
      <> help "Infill amount (ranging from 0 to 1)"
    )
  )
  <*> optional (
  option auto
    (    short 't'
      <> long "thickness"
      <> metavar "THICKNESS"
      <> help "The layer height (in millimeters)"
    )
  )
  <*> optional (
  option auto
    (    short 's'
      <> long "surfacethickness"
      <> metavar "SURFACE"
      <> help "The thickness of the top and bottom layers (in millimeters)"
    )
  )
  <*> switch
  (     long "support"
     <> help "Whether to generate support structures"
  )
  <*> optional (
  strOption
    (    short 'o'
      <> long "output"
      <> metavar "OUTPUT"
      <> help "Output file name"
      )
    )
{-  <*> optional (
  option auto
    (    short 'c'
      <> long "center"
      <> metavar "CENTER"
      <> help "The position on the print bed to center the object."
    )
  )
-}
  <*> optional (
  option auto
    (    short 'l'
      <> long "linespacing"
      <> metavar "LINESPACING"
      <> help "The distance between lines of the infill (in millimeters)"
    )
  )
  <*> argument str
  (  metavar "FILE"
     <> help "Input ASCII formatted STL file"
  )

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
  , infillAmmount    :: ℝ
  , layerHeight      :: ℝ
  , surfaceThickness :: ℝ
  , _withSupport     :: Bool
  , lineSpacing      :: ℝ -- In Millimeters.
  }

run :: ExtCuraEngineOpts -> IO ()
run rawArgs = do
    let
      args = rawArgs
    stl <- readFile (inputFile args)
    let
      stlLines  = lines stl
      printer   = printerFromArgs args
      buildarea = buildArea printer
      (facets, _) = centerFacets buildarea $ facetLinesFromSTL stlLines
      print = printFromArgs args
      allLayers = (filter (\l -> head l /= head (tail l)) . filter (/=[])) <$> layers print facets
      object = zip3 allLayers [1..(toFastℕ $ length allLayers)] $ reverse [1..(toFastℕ $ length allLayers)]
      (gcode, _) = runState (sliceObject printer print object) (MachineState (EPos 0))
      outFile = fromMaybe "out.gcode" $ outputFile args
    writeFile outFile $ unpack (startingGCode <> unlines gcode <> endingGCode)
      where
        -- The Printer.
        -- FIXME: pull all of these values from a curaengine json config.
        printerFromArgs :: ExtCuraEngineOpts -> Printer
        printerFromArgs _ = Printer printbed buildarea extruder1
          where
            -- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it.
            printbed = RectBed (150,150)
            -- The area we can print inside of.
            buildarea = RectArea (150,150,50)
            -- The Extruder. note that this includes the diameter of the feed filament.
            extruder1 = Extruder 1.75 0.4

        -- The Print
        -- FIXME: pull all of these values from a curaengine json config.
        printFromArgs :: ExtCuraEngineOpts -> Print
        printFromArgs args = Print
                             (fromMaybe defaultPerimeterCount $ perimeterCountOpt args)
                             (fromMaybe defaultInfillPercent $ infillPercentOpt args)
                             (fromMaybe defaultThickness $ layerHeightMMOpt args)
                             (fromMaybe defaultBottomTopThickness $ topBottomThicknessMMOpt args)
                             (generateSupportOpt args)
                             (fromMaybe defaultLineThickness $ lineSpacingMMOpt args)
          where
            defaultInfillPercent,defaultThickness,defaultBottomTopThickness,defaultLineThickness :: ℝ
            defaultInfillPercent = 20
            defaultThickness = 0.2
            defaultBottomTopThickness = 0.8
            defaultLineThickness = 0.6
            defaultPerimeterCount :: Fastℕ
            defaultPerimeterCount = 2
        startingGCode, endingGCode :: Text
        startingGCode = "G21 ;metric values\n"
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
        endingGCode = ";End GCode\n"
                      <> "M104 S0 ;extruder heater off\n"
                      <> "M140 S0 ;heated bed heater off (if you have it)\n"
                      <> "G91 ;relative positioning\n"
                      <> "G1 E-1 F300 ;retract the filament a bit before lifting the nozzle, to release some of the pressure\n"
                      <> "G1 Z+0.5 E-5 X-20 Y-20 F{travel_speed} ;move Z up a bit and retract filament even more\n"
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

