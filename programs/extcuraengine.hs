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

import Prelude (RealFrac, Floating, Num, Double, Enum, Fractional, fromRational, (*), (/), (+), (-), (^), fromIntegral, odd, pi, error, sqrt, mod, round)

import Control.Applicative (pure, (<*>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord (Ord, (<=), (<), (>), (>=), max)

import Data.Ratio ((%), Rational, numerator, denominator)

import Data.Tuple (fst, snd)

import Text.Read(Read, read)

import Data.Int (Int)

import Data.String (String)

import Data.Bool(Bool(True, False), (||), (&&), not, otherwise)

import Data.List (nub, sortBy, (++), lines, unlines, length, reverse, zip3, filter, tail, head, zipWith, maximum, (!!), minimum, words, init, unwords, concat, splitAt, elem, take, map, foldl, concatMap, last)

import Control.Monad (return, (>>=))

import Data.Maybe (fromJust, Maybe(Just, Nothing))

import Text.Show(Show, show)

import System.Console.GetOpt (OptDescr(Option), ArgOrder(Permute), getOpt, ArgDescr(NoArg,ReqArg))

import System.Environment (getArgs)

import System.IO (IO, writeFile, readFile, putStrLn)

import Graphics.Slicer (Point(Point), x,y,z, Line(Line), point, lineIntersection, scalePoint, addPoints, distance, lineFromEndpoints, endpoint, midpoint, flipLine, Facet(Facet), sides, Contour, LayerType(BaseOdd, BaseEven, Middle), pointSlopeLength, perpendicularBisector, shiftFacet, orderPoints, roundToFifth, roundPoint, shortenLineBy, accumulateValues, facetsFromSTL, cleanupFacet, makeLines, facetIntersects, getContours, simplifyContour, Bed(Rect3), bedWidth, bedDepth, bedHeight, Extruder(Extruder), nozzleDiameter, filamentWidth)

-- Map a function to every other value in a list. This is useful for fixing non-extruding
-- lines.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f (a:b:cs) = (f a) : b : mapEveryOther f cs

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- FIXME: i think this should go away.

-- Given a point and slope (in xy plane), make "infinite" line (i.e. a line that
-- hits two edges of the bed
infiniteLine :: (Fractional a, RealFrac a, Floating a) =>  Bed -> Point a -> a -> Line a
infiniteLine bed p@(Point _ _ c) m = head $ makeLines $ nub points
    where edges = (map lineFromEndpoints [Point 0 0 c, Point bedSizeX bedSizeY c])
                <*> [Point 0 bedSizeY c, Point bedSizeX 0 c]
          longestLength :: (RealFrac a, Floating a) => a
          longestLength = sqrt . fromRational $ bedX2 + bedY2
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = map fromJust $ filter (/= Nothing) $ map (lineIntersection line) edges
          bedY2 = (bedDepth bed) * (bedDepth bed)
          bedX2 = (bedWidth bed) * (bedWidth bed)
          bedSizeY :: RealFrac a => a
          bedSizeY = fromRational $ bedDepth bed
          bedSizeX :: RealFrac a => a
          bedSizeX = fromRational $ bedWidth bed

----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- Given the printer bed and a list of facets, center them on the print bed
centerFacets :: (Fractional a, RealFrac a) => Bed -> [Facet a] -> ([Facet a], Point a)
centerFacets bed fs = (map (shiftFacet (Point dx dy dz)) fs, Point dx dy dz)
    where [dx,dy,dz] = zipWith (-) (map (/2) [fromRational $ bedWidth bed, fromRational $ bedDepth bed, 0]) [x0,y0,zmin]
          [xmin,ymin,zmin] = map minimum $
                             foldl (zipWith (flip (:))) [[],[],[]] $
                             map f $
                             map point (concatMap sides fs)
          [xmax,ymax] = map maximum $
                        foldl (zipWith (flip (:))) [[],[]] $
                        map (take 2 . f) $
                        map point (concatMap sides fs)
          [x0,y0] = zipWith (\a b -> (a + b) / 2 - b) [xmax,ymax] [xmin,ymin]
          f p = [x,y,z] <*> pure p

-- Read a point when it's given a string of the form "x y z"
readPoint :: Read a => String -> Point a
readPoint s = Point a b c
    where [a,b,c] = map read $ take 3 $ words s 

-- Read a list of three coordinates (as strings separated by spaces) into the correct
-- Lines
readFacet :: (Num a, Read a) => [String] -> Facet a
readFacet f
    | length f < 3 = error "Invalid facet"
    | otherwise = Facet $ makeLines $ map readPoint f'
    where f' = last f : f -- So that we're cyclic

-- From STL file (as a list of Strings, each String corresponding to one line),
-- produce a list of lists of Lines, where each list of Lines corresponds to a
-- facet in the original STL
facetLinesFromSTL :: (Num a, Read a) => [String] -> [Facet a]
facetLinesFromSTL = map readFacet . map cleanupFacet . facetsFromSTL

-- Find all the points in a mesh at a given Z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: (RealFrac a) => a -> [Facet a] -> [[Point a]]
allIntersections v fs = map (map roundPoint) $ filter (/= []) $ map (facetIntersects v) fs

-- Witchcraft
fixContour :: [Point a] -> [Point a]
fixContour c = (head c) : (tail c ++ [head c])

-- Amount to extrude when making a line between two points
extrusionAmount :: (Floating a) => Extruder -> Options -> Point a -> Point a -> a
extrusionAmount extruder opts p1 p2 = (fromRational $ nozzleDia * t * 2 * (recip filamentDia)) * l / pi
    where l = distance p1 p2
          t = thickness opts
          nozzleDia = nozzleDiameter extruder
          filamentDia = filamentWidth extruder
          recip :: Rational -> Rational
          recip a = (denominator a) % (numerator a)

-- Given a contour and the point to start from, evaluate to the amount to extrude between
-- each move
extrusions :: (Read a, Floating a, RealFrac a) => Extruder -> Options -> Point a -> [Point a] -> [a]
extrusions _ _ _ [] = []
extrusions extruder opts p c = extrusionAmount extruder opts p (head c) : extrusions extruder opts (head c) (tail c)

-- Given a list of G-code lines, find the last amount extruded
lastExtrusionAmount :: Read a => [String] -> Maybe a
lastExtrusionAmount gcode
    | extrusionValues == [] = Nothing
    | otherwise = Just $ read $ tail $ last extrusionValues
    where extrusionValues = filter (\s -> (head s == 'E')) $ map last $ map words gcode

-----------------------------------------------------------------------
---------------------- Contour filling --------------------------------
-----------------------------------------------------------------------

-- Make infill
makeInfill :: (Enum a, RealFrac a) => Bed -> Options -> [[Point a]] -> LayerType -> [Line a]
makeInfill bed opts contours layerType = concatMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill bed fillAmount zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = (z (head (head contours)))
          fillAmount = infill opts

-- Get the segments of an infill line that are inside the contour
infillLineInside :: (RealFrac a) => [[Point a]] -> Line a -> [Line a]
infillLineInside contours line = map ((!!) allLines) [0,2..(length allLines) - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: (RealFrac a) => [[Point a]] -> Line a -> [Point a]
getInfillLineIntersections contours line = nub $ map fromJust $ filter (/= Nothing)
                                         $ map (lineIntersection line) contourLines
    where contourLines = concatMap makeLines contours

-- Generate covering lines for a given ratio of infill on a given layer.
coveringInfill :: (Enum a, RealFrac a) => Bed -> Rational -> a -> [Line a]
coveringInfill bed fillAmount zHeight
  | fillAmount == 0 = []
  | otherwise = pruneInfill (coveringLinesUp bed zHeight) ++ pruneInfill (coveringLinesDown bed zHeight)
  where
    n :: Int
    n = max 1 (round $ 1 / fillAmount)
    pruneInfill :: [a] -> [a]
    pruneInfill l = map ((!!) l) [0, n..(length l)-1]

-- Generate lines over entire print area
coveringLinesUp :: (RealFrac a, Enum a) => Bed -> a -> [Line a]
coveringLinesUp bed zHeight = map (flip Line s) (map f [- mybedSizeX, -mybedSizeX + lineThickness..mybedSizeY])
  where
    s :: (RealFrac a) => Point a
    s = Point (bedSum) (bedSum) 0
    f v = Point 0 v zHeight
    bedSum :: (RealFrac a) => a
    bedSum = fromRational $ (bedWidth bed) + (bedHeight bed)
    mybedSizeX :: (RealFrac a) => a 
    mybedSizeX = fromRational $ bedWidth bed
    mybedSizeY :: (RealFrac a) => a 
    mybedSizeY = fromRational $ bedHeight bed

coveringLinesDown :: (Enum a, RealFrac a) => Bed -> a -> [Line a]
coveringLinesDown bed zHeight = map (flip Line s) (map f [0,lineThickness..mybedSizeY + mybedSizeX])
  where
    s :: (RealFrac a) => Point a
    s =  Point (bedSum) (- mybedSizeX - mybedSizeY) 0
    f v = Point 0 v zHeight
    bedSum :: (RealFrac a) => a
    bedSum = fromRational $ (bedWidth bed) + (bedHeight bed)
    mybedSizeX :: (RealFrac a) => a 
    mybedSizeX = fromRational $ bedWidth bed
    mybedSizeY :: (RealFrac a) => a 
    mybedSizeY = fromRational $ bedHeight bed

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: (Enum a, Fractional a, Floating a, RealFrac a)
                    => Extruder
                    -> Options
                    -> Line a
                    -> [Point a]
pointsForPerimeters extruder opts l = map endpoint
                                      $ map (pointSlopeLength (midpoint l) slope)
                                      $ map (* nozzleDia) $ filter (/= 0) [-n..n]
  where
    n :: (RealFrac a) => a
    n = fromIntegral $ perimeterLayers opts - 1
    Line _ m = perpendicularBisector l
    slope = (y m) / (x m)
    nozzleDia :: (RealFrac a) => a
    nozzleDia = fromRational $ nozzleDiameter extruder

-- Lines to count intersections to determine if we're on the inside or outside
perimeterLinesToCheck :: (Fractional a, Floating a, RealFrac a) => Extruder -> Line a -> [Line a]
perimeterLinesToCheck extruder l@(Line p _) = map (flip lineFromEndpoints (Point 0 0 (z p)))
                                              $ map endpoint
                                              $ map (pointSlopeLength (midpoint l) slope)
                                              $ map (*nozzleDia) [-1,1]
  where
    Line _ m = perpendicularBisector l
    slope = case (x m) of 0 -> if (y m) > 0 then 10^101 else -(10^101)
                          _ -> (y m) / (x m)
    nozzleDia :: (RealFrac a) => a
    nozzleDia = fromRational $ nozzleDiameter extruder

-- Find the point corresponding to the inner perimeter of a given line, given all of the
-- contours in the object
innerPerimeterPoint :: (Fractional a, Floating a, RealFrac a)
                    => Extruder
                    -> Line a
                    -> [Contour a]
                    -> Point a
innerPerimeterPoint extruder l contours
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | otherwise = snd $ head intersections
    where linesToCheck = perimeterLinesToCheck  extruder l
          contourLines = concatMap makeLines contours
          simplifiedContour = simplifyContour contourLines
          numIntersections l' = length $ filter (/= Nothing) $ map (lineIntersection l') simplifiedContour
          intersections = map (\a -> (numIntersections a, point a)) linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct infinite lines on the interior for a given line
infiniteInteriorLines :: (Enum a, Floating a, RealFrac a)
                      => Bed
                      -> Extruder
                      -> Options
                      -> Line a
                      -> [[Point a]]
                      -> [Line a]
infiniteInteriorLines bed extruder opts l@(Line _ m) contours
    | innerPoint `elem` firstHalf = map (flip (infiniteLine bed) slope) firstHalf
    | otherwise = map (flip (infiniteLine bed) slope) secondHalf
    where innerPoint = innerPerimeterPoint extruder l contours
          (firstHalf, secondHalf) = splitAt (perimeterLayers opts - 1) $ pointsForPerimeters extruder opts l
          slope = (y m) / (x m)

-- List of lists of interior lines for each line in a contour
allInteriors :: (Enum a, Fractional a, Floating a, RealFrac a) => Bed -> Extruder -> Options -> [Point a] -> [[Point a]] -> [[Line a]]
allInteriors bed extruder opts c contours = map (flip (infiniteInteriorLines bed extruder opts) contours) targetLines
    where targetLines = makeLines c

-- Make inner contours from a list of (outer) contours---note that we do not
-- retain the outermost contour.
innerContours :: (Enum a, Floating a, RealFrac a) => Bed -> Extruder -> Options -> [Contour a] -> [[Contour a]]
innerContours bed extruder opts contours = map concat $ map (constructInnerContours opts) (map (\i -> (last i : i)) interiors)
    where interiors = map (flip (allInteriors bed extruder opts) contours) contours

-- Construct inner contours, given a list of lines constituting the infinite interior
-- lines. Essentially a helper function for innerContours
constructInnerContours :: (Enum a, Floating a, RealFrac a)
                       => Options
                       -> [[Line a]]
                       -> [[Contour a]]
constructInnerContours opts interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours opts $ tail interiors
    | otherwise = [intersections] : constructInnerContours opts (map tail interiors)
    where intersections = map fromJust $ filter (/= Nothing) $ consecutiveIntersections $ map head interiors

consecutiveIntersections :: (Enum a,Floating a, RealFrac a) => [Line a] -> [Maybe (Point a)]
consecutiveIntersections [] = []
consecutiveIntersections [_] = []
consecutiveIntersections (a:b:cs) = (lineIntersection a b) : consecutiveIntersections (b : cs)

-- Generate G-code for a given contour c, where g is the most recent G-code produced
gcodeForContour :: (Read a, Show a, Floating a, RealFrac a)
                => Extruder
                -> Options
                -> [String]
                -> [Point a]
                -> [String]
gcodeForContour extruder opts g c = map ((++) "G1 ") $ zipWith (++) (map show c) ("":es)
    where es = map ((++) " E") $ map show exVals
          exVals = map (+e) $ accumulateValues $ extrusions extruder opts (head c) (tail c)
          lastE :: (Read a) => Maybe a
          lastE = lastExtrusionAmount g
          e :: (Read a, Num a) => a
          e = case lastE of Nothing -> 0
                            Just something -> something

gcodeForNestedContours :: (Read a, Show a, Floating a, RealFrac a)
                       => Extruder
                       -> Options
                       -> [String]
                       -> [[Contour a]]
                       -> [String]
gcodeForNestedContours _ _ _ [] = []
gcodeForNestedContours extruder opts g [cs] = gcodeForContours extruder opts g cs
gcodeForNestedContours extruder opts g cs = firstContoursGcode
                                            ++ gcodeForNestedContours extruder opts firstContoursGcode (tail cs)
  where firstContoursGcode = gcodeForContours extruder opts g (head cs)

gcodeForContours :: (Read a, Show a, Floating a, RealFrac a)
                 => Extruder
                 -> Options
                 -> [String]
                 -> [Contour a]
                 -> [String]
gcodeForContours _ _ _ [] = []
gcodeForContours extruder opts g [c] = gcodeForContour extruder opts g c
gcodeForContours extruder opts g (c:cs) = firstContourGcode
                               ++ gcodeForContours extruder opts firstContourGcode cs
    where firstContourGcode = gcodeForContour extruder opts g c

-- G-code to travel to a point without extruding
travelGcode :: (Show a) => Point a -> String
travelGcode p = "G1 " ++ (show p)

-- I'm not super happy about this, but it makes extrusion values correct
fixGcode :: [String] -> [String]
fixGcode [] = []
fixGcode [a] = [a]
fixGcode (a:b:cs) = (unwords $ init $ words a) : b : (fixGcode cs)

-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

-- Get a bounding box of all contours 
boundingBoxAll :: (Ord a) => [Contour a] -> [a]
boundingBoxAll contours = (map minimum $ map (\n -> map (!!n) bBoxes) [0, 1])
                        ++ (map maximum $ map (\n -> map (!!n) bBoxes) [2, 3])
    where bBoxes = filter (/= []) $ map boundingBox $ filter (/= []) contours


-- Get a bounding box of a contour
boundingBox :: (Ord a) => Contour a -> [a]
boundingBox contour = [minX, minY, maxX, maxY]
    where maxX = maximum $ map x contour
          maxY = maximum $ map y contour
          minX = minimum $ map x contour
          minY = minimum $ map y contour 

-- Bounding box contour
addBBox :: (RealFrac a) => [Contour a] -> [Contour a]
addBBox contours = [Point x1  y1 z0, Point x2 y1 z0, Point x2 y2 z0, Point x1 y2 z0, Point x1 y1 z0] : contours
    where bBox = boundingBoxAll contours
          x1 = (1) + (bBox !! 0)
          y1 = (1) + (bBox !! 1)
          x2 = (-1) + (bBox !! 2)
          y2 = (-1) + (bBox !! 3)
          z0 = z $ head $ head contours

-- Generate support
-- FIXME: hard coded infill amount.
makeSupport :: (Enum a, RealFrac a, Floating a)
            => Bed
            -> Options
            -> [[Point a]]
            -> LayerType
            -> [Line a]
makeSupport bed opts contours _ = map (shortenLineBy $ (fromRational $ 2 * defaultThickness))
                                  $ concatMap (infillLineInside (addBBox contours))
                                  $ infillCover Middle
    where infillCover Middle = coveringInfill bed (20%100) zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = (z (head (head contours)))
          defaultThickness = thickness opts

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: (Floating a, RealFrac a, Enum a) => Options -> [Facet a] -> [[[Point a]]]
layers opts fs = map allIntersections (map (fromRational . roundToFifth) [maxheight, maxheight-t..0]) <*> pure fs
  where 
    maxheight = zmax
    zmax = maximum $ map z $ map point (concatMap sides fs)
    t :: (Floating a) => a
    t = fromRational $ thickness opts

getLayerType :: Options -> (Int, Int) -> LayerType
getLayerType opts (fromStart, toEnd)
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
  | otherwise = Middle
  where
    topBottomLayers :: Int
    topBottomLayers = round $ defaultBottomTopThickness / t
    t = thickness opts

-- Input should be top to bottom, output should be bottom to top
theWholeDamnThing :: (Floating a, RealFrac a, Enum a, Read a, Show a)
                  => Bed
                  -> Extruder
                  -> Options
                  -> [([Contour a], Int, Int)] -> [String]
theWholeDamnThing _ _ _ [] = []
theWholeDamnThing bed extruder opts [(a, fromStart, toEnd)] = contourGcode ++ supportGcode ++ infillGcode 
    where contours = getContours a
          interior = map (map fixContour) $ innerContours bed extruder opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours extruder opts [] $ contours
          innerContourGcode = gcodeForNestedContours extruder opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour extruder opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport bed opts contours
                       $ getLayerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour extruder opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill bed opts innermostContours
                      $ getLayerType opts (fromStart, toEnd)
theWholeDamnThing bed extruder opts ((a, fromStart, toEnd):as) = theRest
                                                  ++ contourGcode
                                                  ++ (map travelGcode $ head contours)
                                                  ++ supportGcode
                                                  ++ infillGcode 
    where theRest = theWholeDamnThing bed extruder opts as
          contours = getContours a
          interior = map (map fixContour) $ innerContours bed extruder opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours extruder opts theRest $ contours
          innerContourGcode = gcodeForNestedContours extruder opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour extruder opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport bed opts contours
                       $ getLayerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour extruder opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill bed opts innermostContours
                      $ getLayerType opts (fromStart, toEnd)

----------------------------------------------------------
----------------------- Constants ------------------------
----------------------------------------------------------
-- in mm
defaultBottomTopThickness, 
    lineThickness :: (RealFrac a) => a

-- FIXME: pull these values from a cura config.

-- The Extruder. note that this includes the diameter of the feed filament.
extruder1 = Extruder 2.85 0.35

-- The bed of the printer. assumed to be some form of rectangle, with the build area coresponding to all of the space above it to a layer.
printerBed = Rect3 230 230 150

defaultBottomTopThickness = 0.8
lineThickness = 0.6

helpString :: String
helpString = "Usage: slicer filename [-i infill] [-p perimeter] [-s support] [-t thickness] [-o outfile]"

startingGcode, endingGcode :: [String]
startingGcode = ["G21 ;metric values"
                ,"G90 ;absolute positioning"
                ,"M82 ;set extruder to absolute mode"
                ,"M106 ;start with the fan on"
                ,"G28 X0 Y0 ;move X/Y to min endstops"
                ,"G28 Z0 ;move Z to min endstops"
                ,"G29 ;Run the auto bed leveling"
                ,"G1 Z15.0 F4200 ;move the platform down 15mm"
                ,"G92 E0 ;zero the extruded length"
                ,"G1 F200 E3 ;extrude 3mm of feed stock"
                ,"G92 E0 ;zero the extruded length again"
                ,"G1 F4200" -- default speed
                ,";Put printing message on LCD screen"
                ,"M117"
                ]
endingGcode = [";End GCode"
              ,"M104 S0 ;extruder heater off"
              ,"M140 S0 ;heated bed heater off (if you have it)"
              ,"G91 ;relative positioning"
              ,"G1 E-1 F300 ;retract the filament a bit before lifting the nozzle, to release some of the pressure"
              ,"G1 Z+0.5 E-5 X-20 Y-20 F{travel_speed} ;move Z up a bit and retract filament even more"
              ,"G28 X0 Y0 ;move X/Y to min endstops, so the head is out of the way"
              ,"M107 ;fan off"
              ,"M84 ;steppers off"
              ,"G90 ;absolute positioning"
              ]

----------------------------------------------------------
------------ Overhead (data structures, etc.) ------------
----------------------------------------------------------

-- Flags and options adapted from https://wiki.haskell.org/High-level_option_handling_with_GetOpt

data Options = Options { perimeterLayers :: Int -- how many parameters for each contour
                       , infill :: Rational     -- the amouth of infill ranging from 0 to 1.
                       , thickness :: Rational  -- the layer height, in millimeters.
                       , support :: Bool        -- whether to print support.
                       , help :: Bool           -- output help
                       , output :: String       -- file to store results in
                       , center :: Point Double -- where to place the object being printed on the bed.
                       }

defaultOptions :: Options
defaultOptions = Options defaultPerimeterLayers defaultFill defaultThickness False False "out.gcode" (Point 0 0 0)
  where
    defaultPerimeterLayers :: Int
    defaultPerimeterLayers = 2
    defaultFill, defaultThickness :: Rational
    defaultFill = 1 % 5
    defaultThickness = 1 % 5

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "h" ["help"]
        (NoArg
            (\opt -> return opt { help = True }))
        "Get help"
    , Option "i" ["infill"]
        (ReqArg
            (\arg opt -> if (read arg) >= 0 then return opt { infill = (read arg) % 100 }
                         else return opt)
            "INFILL")
        "Infill percentage"
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { output = arg })
            "OUTPUT")
        "Output file name"
    , Option "p" ["perimeter"]
        (ReqArg
            (\arg opt -> if (read arg) > 0 then return opt { perimeterLayers = read arg }
                         else return opt)
            "PERIMETER")
        "Perimeter layers"
    , Option "s" ["support"]
        (NoArg
            (\opt -> return opt { support = True }))
        "Enable support"
    , Option "t" ["thickness"]
        (ReqArg tParser "THICKNESS")
        "Layer thickness (mm)"
    ]

tParser :: String -> Options -> IO Options
tParser arg opt
    | argVal > 0 = return opt { thickness = read $ show argVal }
    | otherwise = return opt
    where argVal = read arg :: Double

----------------------------------------------------------------------- 
--------------------------- Main --------------------------------------
----------------------------------------------------------------------- 
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _ ) = getOpt Permute options args
    initialOpts <- foldl (>>=) (return defaultOptions) actions
    let Options { perimeterLayers = perimeter
                , infill = infill
                , thickness = thickness
                , support = support
                , help = help
                , output = output
                } = initialOpts
    if help then (putStrLn helpString) else do
        if length nonOptions == 0 then (putStrLn "Error: Enter a file name") else do
            let fname = head nonOptions
            stl <- readFile fname
            let stlLines = lines stl
            let (facets, c) = centerFacets printerBed $ facetLinesFromSTL stlLines
            let opts = initialOpts { center = c }
            let allLayers = map (filter (\l -> (head l) /= (head $ tail l))) $ filter (/=[]) $ layers opts facets
            let gcode = theWholeDamnThing printerBed extruder1 opts $ zip3 allLayers [1..length allLayers] $ reverse [1..length allLayers]
            writeFile output (unlines $ startingGcode ++ gcode ++ endingGcode)
