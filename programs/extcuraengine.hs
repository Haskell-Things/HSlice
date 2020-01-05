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

import Prelude ((*), (/), (+), (-), (^), fromIntegral, odd, pi, error, sqrt, mod, round, floor, foldMap, fmap, (<>), toRational)

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord ((<=), (<), (>), (>=), max)

import Data.Tuple (fst, snd)

import Text.Read(read)

import Data.String (String)

import Data.Bool(Bool(True, False), (||), (&&), not, otherwise)

import Data.List (nub, sortBy, lines, unlines, length, reverse, zip3, filter, tail, head, zipWith, maximum, (!!), minimum, words, init, unwords, splitAt, elem, take, foldl, last)

import Control.Monad ((>>=))

import Data.Maybe (fromJust, Maybe(Nothing))

import Text.Show(show)

import System.Console.GetOpt (OptDescr(Option), ArgOrder(Permute), getOpt, ArgDescr(NoArg,ReqArg))

import System.Environment (getArgs)

import System.IO (IO, writeFile, readFile, putStrLn)

import Control.Monad.State(liftIO, runStateT)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea), ℝ, toℝ, ℕ, Fastℕ, fromFastℕ, toFastℕ, Point(Point), x,y,z, Line(Line), point, lineIntersection, scalePoint, addPoints, distance, lineFromEndpoints, endpoint, midpoint, flipLine, Facet(Facet), sides, Contour, LayerType(BaseOdd, BaseEven, Middle), pointSlopeLength, perpendicularBisector, shiftFacet, orderPoints, roundToFifth, roundPoint, shortenLineBy, accumulateValues, facetsFromSTL, cleanupFacet, makeLines, facetIntersects, getContours, simplifyContour, Extruder(Extruder), nozzleDiameter, filamentWidth, EPos(EPos), StateM, MachineState(MachineState), getEPos, setEPos)

default (ℕ, Fastℕ, ℝ)

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- Given a point and slope (in xy plane), make "infinite" line (i.e. a line that
-- hits two edges of the bed
infiniteLine :: Bed -> Point -> ℝ -> Line
infiniteLine (RectBed (bedX,bedY)) p@(Point _ _ c) m = head $ makeLines $ nub points
    where edges = fmap lineFromEndpoints [Point 0 0 c, Point bedX bedY c]
                  <*> [Point 0 bedY c, Point bedX 0 c]
          longestLength = sqrt $ bedX*bedX + bedY*bedY
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = fmap fromJust $ filter (/= Nothing) $ lineIntersection line <$> edges

----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- Given the printer bed and a list of facets, center them on the print bed
centerFacets :: Bed -> [Facet] -> ([Facet], Point)
centerFacets (RectBed (bedX,bedY)) fs = (shiftFacet (Point dx dy dz) <$> fs, Point dx dy dz)
    where [dx,dy,dz] = zipWith (-) (fmap (/2) [bedX,bedY,0]) [x0,y0,zmin]
          [xmin,ymin,zmin] = fmap minimum $
                             foldl (zipWith (flip (:))) [[],[],[]] $
                             (f.point) <$> foldMap sides fs
          [xmax,ymax] = fmap maximum $
                        foldl (zipWith (flip (:))) [[],[]] $
                        (take 2 . f . point) <$> foldMap sides fs
          [x0,y0] = zipWith (\a b -> (a + b) / 2 - b) [xmax,ymax] [xmin,ymin]
          f p = [x,y,z] <*> pure p

-- Read a point when it's given a string of the form "x y z"
readPoint :: String -> Point
readPoint s = Point a b c
    where [a,b,c] = fmap read $ take 3 $ words s

-- Read a list of three coordinates (as strings separated by spaces) and generate a facet.
readFacet :: [String] -> Facet
readFacet f
    | length f < 3 = error "Invalid facet"
    | otherwise = Facet $ makeLines $ readPoint <$> f'
    where f' = last f : f -- So that we're cyclic

-- From STL file (as a list of Strings, each String corresponding to one line),
-- produce a list of lists of Lines, where each list of Lines corresponds to a
-- facet in the original STL
facetLinesFromSTL :: [String] -> [Facet]
facetLinesFromSTL = fmap (readFacet . cleanupFacet) . facetsFromSTL

-- Amount to extrude when making a line between two points.
extrusionAmount :: Extruder -> Options -> Point -> Point -> ℝ
extrusionAmount extruder opts p1 p2 = nozzleDia * t * (2 / filamentDia) * l / pi
    where l = distance p1 p2
          t = thickness opts
          nozzleDia = nozzleDiameter extruder
          filamentDia = filamentWidth extruder

-- Given a contour and the point to start from, calculate the amount of material to extrude for each line.
extrusions :: Extruder -> Options -> Point -> [Point] -> [ℝ]
extrusions _ _ _ [] = []
extrusions extruder opts p c = extrusionAmount extruder opts p (head c) : extrusions extruder opts (head c) (tail c)

-----------------------------------------------------------------------
---------------------- Contour filling --------------------------------
-----------------------------------------------------------------------

-- Make infill
makeInfill :: Bed -> Options -> [[Point]] -> LayerType -> [Line]
makeInfill bed opts contours layerType = foldMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill bed fillAmount zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = z $ head $ head contours
          fillAmount = infill opts

-- Get the segments of an infill line that are inside the contour
infillLineInside :: [[Point]] -> Line -> [Line]
infillLineInside contours line = (allLines !!) <$> [0,2..length allLines - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: [[Point]] -> Line -> [Point]
getInfillLineIntersections contours line = nub $ fmap fromJust $ filter (/= Nothing)
                                         $ lineIntersection line <$> contourLines
    where contourLines = foldMap makeLines contours

-- Generate covering lines for a given percent infill
coveringInfill :: Bed -> ℝ -> ℝ -> [Line]
coveringInfill bed infillAmount zHeight
    | infillAmount == 0 = []
    | otherwise = pruneInfill (coveringLinesUp bed zHeight) <> pruneInfill (coveringLinesDown bed zHeight)
    where
      n :: ℝ
      n = max 1 (infillAmount/100)
      pruneInfill :: [Line] -> [Line]
      pruneInfill l = (l !!) <$> [0, (floor n)..length l-1]

-- Generate lines over entire print area
coveringLinesUp :: Bed -> ℝ -> [Line]
coveringLinesUp (RectBed (bedX,bedY)) zHeight = flip Line s . f <$> [-bedX,-bedX + lineThickness..bedY]
    where s = Point (bedX + bedY) (bedX + bedY) 0
          f v = Point 0 v zHeight

coveringLinesDown :: Bed -> ℝ -> [Line]
coveringLinesDown (RectBed (bedX,bedY)) zHeight = flip Line s . f <$> [0,lineThickness..bedY + bedX]
    where s =  Point (bedX + bedY) (- bedX - bedY) 0
          f v = Point 0 v zHeight

lineSlope :: Point -> ℝ
lineSlope m = case x m of 0 -> if y m > 0 then 10^101 else -(10^101)
                          _ -> y m / x m

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: Extruder -> Options -> Line -> [Point]
pointsForPerimeters extruder opts l = (endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia)) <$> filter (/= 0) [-n..n]
  where
    n :: ℝ
    n = fromIntegral $ perimeterLayers opts - 1
    Line _ m = perpendicularBisector l
    nozzleDia :: ℝ
    nozzleDia = nozzleDiameter extruder

-- Lines to count intersections to determine if we're on the inside or outside
perimeterLinesToCheck :: Extruder -> Line -> [Line]
perimeterLinesToCheck extruder l@(Line p _) = fmap ((`lineFromEndpoints` Point 0 0 (z p)) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia)) [-1,1]
  where Line _ m = perpendicularBisector l
        nozzleDia :: ℝ
        nozzleDia = nozzleDiameter extruder

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
          numIntersections l' = length $ filter (/= Nothing) $ lineIntersection l' <$> simplifiedContour
          intersections = (\a -> (numIntersections a, point a)) <$> linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct infinite lines on the interior for a given line
infiniteInteriorLines :: Bed -> Extruder -> Options -> Line -> [[Point]] -> [Line]
infiniteInteriorLines bed extruder opts l@(Line _ m) contours
    | innerPoint `elem` firstHalf = flip (infiniteLine bed) (lineSlope m) <$> firstHalf
    | otherwise = flip (infiniteLine bed) (lineSlope m) <$> secondHalf
    where innerPoint = innerPerimeterPoint extruder l contours
          (firstHalf, secondHalf) = splitAt (fromFastℕ $ perimeterLayers opts - 1) $ pointsForPerimeters extruder opts l

-- List of lists of interior lines for each line in a contour
allInteriors :: Bed -> Extruder -> Options -> [Point] -> [[Point]] -> [[Line]]
allInteriors bed extruder opts c contours = flip (infiniteInteriorLines bed extruder opts) contours <$> targetLines
    where targetLines = makeLines c

-- Make inner contours from a list of (outer) contours---note that we do not
-- retain the outermost contour.
innerContours :: Bed -> Extruder -> Options -> [Contour] -> [[Contour]]
innerContours bed extruder opts contours = foldMap (constructInnerContours opts .(\i -> last i : i)) interiors
    where interiors = flip (allInteriors bed extruder opts) contours <$> contours

-- Construct inner contours, given a list of lines constituting the infinite interior
-- lines. Essentially a helper function for innerContours
constructInnerContours :: Options -> [[Line]] -> [[Contour]]
constructInnerContours opts interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours opts $ tail interiors
    | otherwise = [intersections] : constructInnerContours opts (tail <$> interiors)
    where intersections = fmap fromJust $ filter (/= Nothing) $ consecutiveIntersections $ head <$> interiors

consecutiveIntersections :: [Line] -> [Maybe Point]
consecutiveIntersections [] = [Nothing]
consecutiveIntersections [_] = [Nothing]
consecutiveIntersections (a:b:cs) = lineIntersection a b : consecutiveIntersections (b : cs)

-- Generate G-code for a given contour c.
gcodeForContour :: Extruder
                -> Options
                -> [Point]
                -> StateM [String]
gcodeForContour extruder opts c = do
  currentPos <- toℝ <$> getEPos
  let
    extrusionAmounts = extrusions extruder opts (head c) (tail c)
    ePoses = accumulateValues $ extrusionAmounts
    newPoses = (currentPos+) <$> ePoses
    es = fmap (" E" <>) $ show <$> newPoses
  setEPos $ toRational $ last newPoses
  pure $ ("G1 " <>) <$> zipWith (<>) (show <$> c) ("":es)

gcodeForNestedContours :: Extruder
                       -> Options
                       -> [[Contour]]
                       -> StateM [String]
gcodeForNestedContours _ _ [] = pure []
gcodeForNestedContours extruder opts [c] = gcodeForContours extruder opts c
gcodeForNestedContours extruder opts (c:cs) = do
  oneContour <- firstContoursGCode
  remainingContours <- gcodeForNestedContours extruder opts cs
  pure $ oneContour <> remainingContours
    where firstContoursGCode = gcodeForContours extruder opts c

gcodeForContours :: Extruder
                 -> Options
                 -> [Contour]
                 -> StateM [String]
gcodeForContours _ _ [] = pure []
gcodeForContours extruder opts [c] = gcodeForContour extruder opts c
gcodeForContours extruder opts (c:cs) = do
  oneContour <- firstContourGCode
  remainingContours <- gcodeForContours extruder opts cs
  pure $ oneContour <> remainingContours
    where firstContourGCode = gcodeForContour extruder opts c

-- G-code to travel to a point without extruding
makeTravelGCode :: Point -> String
makeTravelGCode p = "G1 " <> show p

-- I'm not super happy about this, but it makes extrusion values correct
fixGCode :: [String] -> [String]
fixGCode [] = []
fixGCode [a] = [a]
fixGCode (a:b:cs) = unwords (init $ words a) : b : fixGCode cs

-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

-- Get a bounding box of all contours 
boundingBoxAll :: [Contour] -> [ℝ]
boundingBoxAll contours =
  fmap (minimum.(\n -> fmap (!!n) bBoxes)) [0, 1] <>
  fmap (maximum.(\n -> fmap (!!n) bBoxes)) [2, 3]
    where bBoxes = filter (/= []) $ boundingBox <$> filter (/= []) contours


-- Get a bounding box of a contour
boundingBox :: Contour -> [ℝ]
boundingBox contour = [minX, minY, maxX, maxY]
    where maxX = maximum $ fmap x contour
          maxY = maximum $ fmap y contour
          minX = minimum $ fmap x contour
          minY = minimum $ fmap y contour 

-- Bounding box contour
addBBox :: [Contour] -> [Contour]
addBBox contours = [Point x1  y1 z0, Point x2 y1 z0, Point x2 y2 z0, Point x1 y2 z0, Point x1 y1 z0] : contours
    where bBox = boundingBoxAll contours
          x1 = 1 + head bBox
          y1 = 1 + (bBox !! 1)
          x2 = (-1) + (bBox !! 2)
          y2 = (-1) + (bBox !! 3)
          z0 = z $ head $ head contours

-- Generate support
-- FIXME: hard coded infill amount.
makeSupport :: Bed
            -> Options
            -> [[Point]]
            -> LayerType
            -> [Line]
makeSupport bed opts contours _ = fmap (shortenLineBy $ 2 * thickness opts)
                                  $ foldMap (infillLineInside (addBBox contours))
                                  $ infillCover Middle
    where infillCover Middle = coveringInfill bed 20 zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = z $ head $ head contours

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: Options -> [Facet] -> [[[Point]]]
layers opts fs = fmap (allIntersections.roundToFifth) [maxheight,maxheight-t..0] <*> pure fs
    where zmax = maximum $ fmap (z.point) (foldMap sides fs)
          maxheight = t * fromIntegral (floor (zmax / t)::Fastℕ)
          t = thickness opts

getLayerType :: Options -> (Fastℕ, Fastℕ) -> LayerType
getLayerType opts (fromStart, toEnd)
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
  | otherwise = Middle
  where
    topBottomLayers :: Fastℕ
    topBottomLayers = round $ defaultBottomTopThickness / t
    defaultBottomTopThickness :: ℝ
    defaultBottomTopThickness = 0.8
    t = thickness opts

----------------------------------------------------------------------
---------------------------- MISC ------------------------------------
----------------------------------------------------------------------

fixContour :: [Point] -> [Point]
fixContour c = head c : tail c <> [head c]

-- Find all the points in the mesh at a given z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: ℝ -> [Facet] -> [[Point]]
allIntersections v fs = fmap (fmap roundPoint) $ filter (/= []) $ fmap (facetIntersects v) fs

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
sliceObject ::  Bed -> Extruder -> Options
                  -> [([Contour], Fastℕ, Fastℕ)] -> StateM [String]
sliceObject _ _ _ [] = pure []
sliceObject bed extruder opts ((a, fromStart, toEnd):as) = do
  theRest <- sliceObject bed extruder opts as
  outerContourGCode <- gcodeForContours extruder opts contours
  innerContourGCode <- gcodeForNestedContours extruder opts interior
  travelGCode <- pure $ if theRest /= [] then makeTravelGCode <$> (head contours) else []
  supportGCode <- if not $ support opts then pure [] else fixGCode <$> gcodeForContour extruder opts supportContours
  infillGCode <- fixGCode <$> gcodeForContour extruder opts infillContours
  pure $ theRest <> outerContourGCode <> innerContourGCode <> travelGCode <> supportGCode <> infillGCode 
    where
      contours = getContours a
      interior = fmap fixContour <$> innerContours bed extruder opts contours
      supportContours = foldMap (\l -> [point l, endpoint l])
                        $ mapEveryOther flipLine
                        $ makeSupport bed opts contours
                        $ getLayerType opts (fromStart, toEnd)
      infillContours = foldMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeInfill bed opts innermostContours
                       $ getLayerType opts (fromStart, toEnd)
      allContours = zipWith (:) contours interior
      innermostContours = if interior == [] then contours else fmap last allContours

----------------------------------------------------------
----------------------- Constants ------------------------
----------------------------------------------------------

-- FIXME: pull these values from a curaengine config.

-- in mm
lineThickness :: ℝ
lineThickness = 0.6

----------------------------------------------------------
----------------------- Options --------------------------
----------------------------------------------------------

-- Options adapted from https://wiki.haskell.org/High-level_option_handling_with_GetOpt

data Options = Options { perimeterLayers :: Fastℕ -- how many parameters go around each contour
                       , infill :: ℝ              -- the amouth of infill ranging from 0 to 1.
                       , thickness :: ℝ           -- the layer height, in millimeters.
                       , support :: Bool          -- whether to print support.
                       , help :: Bool             -- output help
                       , output :: String         -- file to store results in
                       , center :: Point          -- where to place the object being printed on the bed.
                       }

defaultOptions :: Options
defaultOptions = Options defaultPerimeterLayers defaultFill defaultThickness False False "out.gcode" (Point 0 0 0)
  where
    defaultPerimeterLayers :: Fastℕ
    defaultPerimeterLayers = 2
    defaultFill, defaultThickness :: ℝ
    defaultFill = 20
    defaultThickness = 0.2

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "h" ["help"]
        (NoArg
            (\opt -> pure opt { help = True }))
        "Get help"
    , Option "i" ["infill"]
        (ReqArg
            (\arg opt -> if (read arg :: ℝ) >= 0 then pure opt { infill = read arg }
                         else pure opt)
            "INFILL")
        "Infill percentage"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> pure opt { output = arg })
            "OUTPUT")
        "Output file name"
    , Option "p" ["perimeter"]
        (ReqArg
            (\arg opt -> if (read arg :: Fastℕ) > 0 then pure opt { perimeterLayers = read arg }
                         else pure opt)
            "PERIMETER")
        "Perimeter layers"
    , Option "s" ["support"]
        (NoArg
            (\opt -> pure opt { support = True }))
        "Enable support"
    , Option "t" ["thickness"]
        (ReqArg tParser "THICKNESS")
        "Layer thickness (mm)"
    ]

tParser :: String -> Options -> IO Options
tParser arg opt
    | argVal > 0 = pure opt { thickness = read $ show argVal }
    | otherwise = pure opt
    where argVal = read arg :: ℝ

-----------------------------------------------------------------------
--------------------------- Main --------------------------------------
-----------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOpt Permute options args
    initialOpts <- foldl (>>=) (pure defaultOptions) actions
    let Options { help = help
                , output = output
                } = initialOpts
    let helpString = "Usage: slicer filename [-i infill] [-p perimeter] [-s support] [-t thickness] [-o outfile]"
    if help then
      putStrLn helpString
      else
        if length nonOptions == 0 then
          putStrLn "Error: Enter a file name"
          else do
            let fname = head nonOptions
            stl <- readFile fname
            let stlLines = lines stl
                (facets, c) = centerFacets printerBed $ facetLinesFromSTL stlLines
                opts = initialOpts { center = c }
                allLayers = fmap (filter (\l -> head l /= head (tail l))) $ filter (/=[]) $ layers opts facets
                object = zip3 allLayers [1..(toFastℕ $ length allLayers)] $ reverse [1..(toFastℕ $ length allLayers)]
              in do
              (gcode, _) <- liftIO $ runStateT (sliceObject printerBed extruder1 opts object) (MachineState (EPos 0))
              writeFile output (startingGCode <> unlines gcode <> endingGCode)
              where
                -- FIXME: pull all of these values from a curaengine json config.
                -- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it.
                printerBed :: Bed
                printerBed = RectBed (150,150)
                -- The Extruder. note that this includes the diameter of the feed filament.
                extruder1 = Extruder 1.75 0.4
                startingGCode =    "G21 ;metric values\n"
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
                endingGCode =    ";End GCode\n"
                              <> "M104 S0 ;extruder heater off\n"
                              <> "M140 S0 ;heated bed heater off (if you have it)\n"
                              <> "G91 ;relative positioning\n"
                              <> "G1 E-1 F300 ;retract the filament a bit before lifting the nozzle, to release some of the pressure\n"
                              <> "G1 Z+0.5 E-5 X-20 Y-20 F{travel_speed} ;move Z up a bit and retract filament even more\n"
                              <> "G28 X0 Y0 ;move X/Y to min endstops, so the head is out of the way\n"
                              <> "M107 ;fan off\n"
                              <> "M84 ;steppers off\n"
                              <> "G90 ;absolute positioning\n"

