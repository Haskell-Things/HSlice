{-# LANGUAGE Rank2Types #-}
 
module Main where

import Control.Monad (guard)
import Data.Char (toUpper, toLower, isSpace)
import Data.List (nub, sortBy, find, delete)
import Data.Maybe (fromJust)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

----------------------------------------------------------
----------------------- Constants ------------------------
----------------------------------------------------------
-- in mm
nozzleDiameter, 
    filamentDiameter, 
    defaultThickness, 
    bedSizeX, 
    bedSizeY, 
    defaultBottomTopThickness, 
    lineThickness :: (Num a, RealFrac a) => a

nozzleDiameter = 0.4
filamentDiameter = 1.75
defaultThickness = 0.2
bedSizeX = 150.0
bedSizeY = 150.0
defaultBottomTopThickness = 0.8
lineThickness = 0.6

defaultPerimeterLayers, defaultFill :: Int 
defaultPerimeterLayers = 2
defaultFill = 20

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
              ,"M104 S0 ;extruder heater off"
              ,"M140 S0 ;heated bed heater off (if you have it)"
              ,"G91 ;relative positioning"
              ,"G1 E-1 F300 ;retract the filament a bit before lifting the nozzle, to release some of the pressure"
              ,"G1 Z+0.5 E-5 X-20 Y-20 F{travel_speed} ;move Z up a bit and retract filament even more"
              ,"G28 X0 Y0 ;move X/Y to min endstops, so the head is out of the way"
              ,"M107 ;fan off"
              ,"M84 ;steppers off"
              ,"G90 ;absolute positioning"
              ]

----------------------------------------------------------
------------ Overhead (data structures, etc.) ------------
----------------------------------------------------------

-- Flags and options adapted from https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- Parts of main also adapted from there.
data Flag = PerimeterLayers Int
          | Infill Int
          | Thickness Double

data Options = Options { perimeterLayers :: Int
                       , infill :: Int
                       , thickness :: forall a. (Read a, Floating a, RealFrac a) => a
                       , support :: Bool
                       , help :: Bool
                       , output :: String
                       , center :: Point Double
                       }

defaultOptions :: Options
defaultOptions = Options defaultPerimeterLayers defaultFill defaultThickness False False "out.gcode" (Point 0 0 0)

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "h" ["help"]
        (NoArg
            (\opt -> return opt { help = True }))
        "Get help"
    , Option "i" ["infill"]
        (ReqArg
            (\arg opt -> if (read arg) >= 0 then return opt { infill = read arg }
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

-- A Point data structure
data Point a = Point { x :: a, y :: a, z :: a } deriving Eq

instance Functor Point where
    fmap f (Point x y z) = Point (f x) (f y) (f z)

-- Display a Point in the format expected by G-code
instance (Show a) => Show (Point a) where
    show p = unwords $ zipWith (++) ["X","Y","Z"] (map show [x p, y p, z p])

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
data Line a = Line { point :: Point a, slope :: Point a } deriving Show

instance (Ord a, Eq a, Floating a, RealFrac a, Num a) => Eq (Line a) where
    (==) (Line p1 m1) (Line p2 m2) = distance p1 p2 < 0.0001 && distance m1 m2 < 0.0001

data Facet a = Facet { sides :: [Line a] } deriving Eq

data LayerType = BaseOdd | BaseEven | Middle

-- This should correspond to one line of G-code
type Command = [String]

type Contour a = [Point a]


-- Given a command, write it as one line of G-code
showCommand :: Command -> String
showCommand = map toUpper . unwords

-- Map a function to every other value in a list. This is useful for fixing non-extruding
-- lines.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f (a:b:cs) = (f a) : b : mapEveryOther f cs

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------


-- Add the coordinates of two points
addPoints :: Num a => Point a -> Point a -> Point a
addPoints (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- Scale the coordinates of a point by s
scalePoint :: Num a => a -> Point a -> Point a
scalePoint = fmap . (*)

magnitude :: (Floating a, Num a) => Point a -> a
magnitude (Point x y z) = sqrt $ x^2 + y^2 + z^2

-- Distance between two points
distance :: (Floating a, RealFrac a, Num a) => Point a -> Point a -> a
distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)

-- Create a line given its endpoints
lineFromEndpoints :: Num a => Point a -> Point a -> Line a
lineFromEndpoints p1 p2 = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the other endpoint
endpoint :: Num a => Line a -> Point a
endpoint l = addPoints (point l) (slope l)

-- Midpoint of a line
midpoint :: (Fractional a, Num a) => Line a -> Point a
midpoint (Line p s) = addPoints p (scalePoint 0.5 s)

-- Given a point and slope, make a line with that slope from that point of a specified
-- distance, in the same z plane
pointSlopeLength :: (Ord a, Fractional a, Floating a, Num a) => Point a -> a -> a -> Line a
pointSlopeLength p m d
    | m > 10^100 = Line p (Point 0 d 0)
    | m < -(10^100) = Line p (Point 0 (-d) 0)
    | otherwise = Line p s
    where s = scalePoint scale $ Point 1 yVal 0
          yVal = m
          scale = d / sqrt (1 + yVal^2)

-- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
-- if p2 == p3 and the lines are parallel (see canCombineLines)
combineLines :: Num a => Line a -> Line a -> Line a
combineLines (Line p _) l2 = lineFromEndpoints p (endpoint l2)

-- Determine if lines can be combined
canCombineLines :: (Eq a, Fractional a, RealFrac a, Floating a, Num a) => Line a -> Line a -> Bool
canCombineLines l1 l2@(Line p _) = (extendToInfiniteLine l1) == (extendToInfiniteLine l2)
                                 && (endpoint l1 == p)

-- Given a point and slope (in xy plane), make "infinite" line (i.e. a line that
-- hits two edges of the bed
infiniteLine :: (Fractional a, RealFrac a, Floating a, Num a) => Point a -> a -> Line a
infiniteLine p@(Point _ _ c) m = head $ makeLines $ nub points
    where edges = (map lineFromEndpoints [Point 0 0 c, Point bedSizeX bedSizeY c])
                <*> [Point 0 bedSizeY c, Point bedSizeX 0 c]
          longestLength = sqrt $ bedSizeX^2 + bedSizeY^2
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = map fromJust $ filter (/= Nothing) $ map (lineIntersection line) edges

extendToInfiniteLine :: (Fractional a, RealFrac a, Floating a, Num a) => Line a -> Line a
extendToInfiniteLine l@(Line p m) = infiniteLine p slope
    where slope = case (x m) of 0 -> 10^100
                                _ -> (y m) / (x m)

-- Construct a perpendicular bisector of a line (with the same length, assuming
-- a constant z value)
perpendicularBisector :: (RealFrac a, Fractional a, Floating a, Num a) => Line a -> Line a
perpendicularBisector l@(Line p s)
    | y s == 0 = Line (midpoint l) (Point 0 (magnitude s) 0)
    | otherwise = pointSlopeLength (midpoint l) m (distance p (endpoint l))
    where m = -(x s) / (y s)

-- Express a line in terms of the other endpoint
flipLine :: Num a => Line a -> Line a
flipLine l@(Line _ s) = Line (endpoint l) (scalePoint (-1) s)

-- Shift a facet by the vector p
shiftFacet :: Num a => Point a -> Facet a -> Facet a
shiftFacet p = Facet . map (\l -> l { point = addPoints p (point l) }) . sides

-- Find the point on a line for a given Z value. Note that this evaluates to Nothing
-- in the case that there is no point with that Z value, or if that is the only
-- Z value present in that line. The latter should be okay because the properties
-- of our meshes mean that the two endpoints of our line should be captured by
-- the other two segments of a triangle.
pointAtXValue :: (Num a, RealFrac a) => Line a -> a -> Maybe (Point a)
pointAtXValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - x p) / x m

pointAtYValue :: (Num a, RealFrac a) => Line a -> a -> Maybe (Point a)
pointAtYValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - y p) / y m

pointAtZValue :: (Num a, RealFrac a) => Line a -> a -> Maybe (Point a)
pointAtZValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - z p) / z m

-- Line intersection algorithm from http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
-- (WOW!)
lineIntersection :: (Num a, RealFrac a, Eq a, Floating a) => Line a -> Line a -> Maybe (Point a)
lineIntersection l1@(Line p r) l2@(Line q s) 
    | twoDCrossProduct r s == 0 = Nothing
    | 0 <= t && t <= 1 && 0 <= u && u <= 1 = Just (addPoints p (scalePoint t r))
    | otherwise = Nothing
    where t = (twoDCrossProduct (addPoints q (scalePoint (-1) p)) s) / (twoDCrossProduct r s)
          u = (twoDCrossProduct (addPoints q (scalePoint (-1) p)) r) / (twoDCrossProduct r s)

crossProduct :: (Num a, RealFrac a) => Point a -> Point a -> Point a
crossProduct (Point x y z) (Point a b c) = Point (y * c - z * b) (z * a - x * c) (x * b - y * a)

twoDCrossProduct :: (Num a, RealFrac a, Floating a) => Point a -> Point a -> a
twoDCrossProduct p1 p2 = z $ (crossProduct p1 {z = 0} p2 {z = 0})

-- Orders points by x and y (x first, then sorted by y for the same x-values)
orderPoints:: (Ord a) => Point a -> Point a -> Ordering
orderPoints (Point x1 y1 z1) (Point x2 y2 z2) 
    | x1 == x2 = compare y1 y2
    | otherwise = compare x1 x2

orderAlongLine :: (Ord a, Floating a) => Line a -> Point a -> Point a -> Ordering
orderAlongLine line p1@(Point x1 y1 z1) p2@(Point x2 y2 z2)
    | x1 == x2 && y1 == y2 && z1 == z2 = compare z1 z2
    | otherwise = compare (magnitude $ addPoints (point line) (scalePoint (-1) p1))
                $ (magnitude $ addPoints (point line) (scalePoint (-1) p2))


-- round
roundToFifth :: (Num a, RealFrac a, Fractional a) => a -> a
roundToFifth a = (fromIntegral $ round (100000 * a)) / 100000

-- round point
roundPoint :: (Num a, RealFrac a, Fractional a) => Point a -> Point a 
roundPoint (Point x y z) = Point (roundToFifth x) (roundToFifth y) (roundToFifth z)

-- shorten line by a millimeter amount on each end 
shortenLineBy :: (Num a, RealFrac a, Fractional a, Floating a) => a -> Line a -> Line a
shortenLineBy amt line = Line newStart newSlope
    where pct = (amt / (magnitude (slope line)))
          newStart = addPoints (point line) $ scalePoint pct (slope line)
          newSlope = scalePoint (1 - 2 * pct) (slope line)


----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- Separate lines of STL file into facets
facetsFromSTL :: [String] -> [[String]]
facetsFromSTL [] = []
facetsFromSTL [a] = []
facetsFromSTL l = map (map (dropWhile isSpace)) $ f : facetsFromSTL (tail r)
    where (f, r) = break (\s -> filter (not . isSpace) (map toLower s) == "endfacet") l

-- Given a list of facets, center them on the print bed
centerFacets :: (Num a, Fractional a, RealFrac a) => [Facet a] -> ([Facet a], Point a)
centerFacets fs = (map (shiftFacet (Point dx dy dz)) fs, Point dx dy dz)
    where [dx,dy,dz] = zipWith (-) (map (/2) [bedSizeX,bedSizeY,0]) [x0,y0,zmin]
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

-- Clean up a list of strings from STL file (corresponding to a facet) into just
-- the vertices
cleanupFacet :: [String] -> [String]
cleanupFacet = map unwords . map tail . filter ((== "vertex") . head) . map words

-- Read a point when it's given a string of the form "x y z"
readPoint :: Read a => String -> Point a
readPoint s = Point a b c
    where [a,b,c] = map read $ take 3 $ words s 

-- Given a list of points (in order), construct lines that go between them. Note
-- that this is NOT cyclic, which is why we make sure we have cyclicity in readFacet
makeLines :: Num a => [Point a] -> [Line a]
makeLines l
    | length l < 2 = []
    | otherwise = lineFromEndpoints (head l) (head l') : makeLines l'
    where l' = tail l

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

-- Determine if a triangle intersects a plane at a given z value
triangleIntersects :: (Eq a, RealFrac a) => a -> Facet a -> [Point a]
triangleIntersects v f = trimIntersections $ map fromJust $ filter (/= Nothing) intersections
    where intersections = map (flip pointAtZValue v) (sides f)

-- Get rid of the case where a triangle intersects the plane at one point
trimIntersections :: Eq a => [Point a] -> [Point a]
trimIntersections l
    | length l' <= 1 = []
    | otherwise = l'
    where l' = nub l

-- Find all the points in the mesh at a given z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: (Eq a, RealFrac a) => a -> [Facet a] -> [[Point a]]
allIntersections v fs = map (map roundPoint) $ filter (/= []) $ map (triangleIntersects v) fs

-- Turn pairs of points into lists of connected points
getContours :: (Eq a) => [[Point a]] -> [[Point a]]
getContours = makeContours . (,) []

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: (Eq a) => ([[Point a]], [[Point a]]) -> [[Point a]]
makeContours (contours, pairs)
    | pairs == [] = contours
    | otherwise = makeContours (contours ++ [next], ps)
    where (next, ps) = findContour (head pairs, tail pairs)

-- Extract a single contour from a list of points
findContour :: (Eq a) => ([Point a], [[Point a]]) -> ([Point a], [[Point a]])
findContour (contour, pairs)
    | p == Nothing = (contour, pairs)
    | otherwise = findContour (contour ++ (delete (last contour) p'), delete p' pairs)
    where match p0 = head p0 == last contour || last p0 == last contour
          p = find match pairs 
          p' = fromJust p 

-- Combine lines when possible. Note that we're working with lists of Lines, not with
-- Contours.
simplifyContour :: (Eq a, Fractional a, RealFrac a, Floating a, Num a) => [Line a] -> [Line a]
simplifyContour [] = []
simplifyContour [a] = [a]
simplifyContour (a:b:cs)
    | canCombineLines a b = simplifyContour $ (combineLines a b) : cs
    | otherwise = a : simplifyContour (b : cs)

-- Witchcraft
fixContour :: (Ord a, Eq a) => [Point a] -> [Point a]
fixContour c = (head c) : (tail c ++ [head c])

-- Sort lists of point pairs by x-value of first point in the pair
sortSegments :: (Ord a) => [[Point a]] -> [[Point a]]
sortSegments = sortBy orderSegments 

orderSegments :: (Ord a) => [Point a] -> [Point a] -> Ordering
orderSegments (p1:_) (p2:_) 
    | x p1 == x p2 = compare (y p1) (y p2)
    | otherwise = compare (x p1) (x p2)

-- Amount to extrude when making a line between two points
extrusionAmount :: (Read a, Floating a, Num a, RealFrac a) => Options -> Point a -> Point a -> a
extrusionAmount opts p1 p2 = nozzleDiameter * t * (2 / filamentDiameter) * l / pi
    where l = distance p1 p2
          t = thickness opts

-- Given a contour and the point to start from, evaluate to the amount to extrude between
-- each move
extrusions :: (Read a, Floating a, Num a, RealFrac a) => Options -> Point a -> [Point a] -> [a]
extrusions _ _ [] = []
extrusions opts p c = extrusionAmount opts p (head c) : extrusions opts (head c) (tail c)

-- Take absolute values, turn into accumulated values
accumulateValues :: Num a => [a] -> [a]
accumulateValues [] = []
accumulateValues [a] = [a]
accumulateValues (a:b:cs) = a : accumulateValues (a + b : cs)

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
makeInfill :: (Enum a, Num a, RealFrac a, Floating a) => Options -> [[Point a]] -> LayerType -> [Line a]
makeInfill opts contours layerType = concatMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill fillAmount zHeight
          infillCover BaseEven = coveringLinesUp zHeight
          infillCover BaseOdd = coveringLinesDown zHeight
          zHeight = (z (head (head contours)))
          fillAmount = infill opts

-- Get the segments of an infill line that are inside the contour
infillLineInside :: (Num a, RealFrac a, Floating a) => [[Point a]] -> Line a -> [Line a]
infillLineInside contours line = map ((!!) allLines) [0,2..(length allLines) - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: (Num a, RealFrac a, Floating a) => [[Point a]] -> Line a -> [Point a]
getInfillLineIntersections contours line = nub $ map fromJust $ filter (/= Nothing)
                                         $ map (lineIntersection line) contourLines
    where contourLines = concatMap makeLines contours

-- Generate covering lines for a given percent infill
coveringInfill :: (Enum a, Num a, RealFrac a) => Int -> a -> [Line a]
coveringInfill infill z
    | infill == 0 = []
    | otherwise = pruneInfill (coveringLinesUp z) ++ pruneInfill (coveringLinesDown z)
    where n = max 1 (div 100 infill)
          pruneInfill l = map ((!!) l)[0, n..(length l)-1]

-- Generate lines over entire print area
coveringLinesUp :: (Enum a, Num a, RealFrac a) => a -> [Line a]
coveringLinesUp z = map (flip Line s) (map f [-bedSizeX,-bedSizeX + lineThickness..bedSizeY])
    where s = Point (bedSizeX + bedSizeY) (bedSizeX + bedSizeY) 0
          f v = Point 0 v z

coveringLinesDown :: (Enum a, Num a, RealFrac a) => a -> [Line a]
coveringLinesDown z = map (flip Line s) (map f [0,lineThickness..bedSizeY + bedSizeX])
    where s =  Point (bedSizeX + bedSizeY) (- bedSizeX - bedSizeY) 0
          f v = Point 0 v z

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: (Enum a, Fractional a, Floating a, Num a, RealFrac a)
                    => Options
                    -> Line a
                    -> [Point a]
pointsForPerimeters opts l@(Line p _) = map endpoint
                                      $ map (pointSlopeLength (midpoint l) slope)
                                      $ map (*nozzleDiameter) $ filter (/= 0) [-n..n]
    where n = fromIntegral $ perimeterLayers opts - 1
          Line _ m = perpendicularBisector l
          slope = (y m) / (x m)

-- Lines to count intersections to determine if we're on the inside or outside
perimeterLinesToCheck :: (Enum a, Fractional a, Floating a, Num a, RealFrac a) => Line a -> [Line a]
perimeterLinesToCheck l@(Line p _) = map (flip lineFromEndpoints (Point 0 0 (z p)))
                                   $ map endpoint
                                   $ map (pointSlopeLength (midpoint l) slope)
                                   $ map (*nozzleDiameter) [-1,1]
    where Line _ m = perpendicularBisector l
          slope = case (x m) of 0 -> if (y m) > 0 then 10^101 else -(10^101)
                                _ -> (y m) / (x m)

-- Find the point corresponding to the inner perimeter of a given line, given all of the
-- contours in the object
innerPerimeterPoint :: (Enum a, Fractional a, Floating a, Num a, RealFrac a)
                    => Line a
                    -> [Contour a]
                    -> Point a
innerPerimeterPoint l contours
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | otherwise = snd $ head intersections
    where linesToCheck = perimeterLinesToCheck l
          contourLines = concatMap makeLines contours
          simplifiedContour = simplifyContour contourLines
          numIntersections l' = length $ filter (/= Nothing) $ map (lineIntersection l') simplifiedContour
          intersections = map (\a -> (numIntersections a, point a)) linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct infinite lines on the interior for a given line
infiniteInteriorLines :: (Enum a, Fractional a, Floating a, Num a, RealFrac a)
                      => Options
                      -> Line a
                      -> [[Point a]]
                      -> [Line a]
infiniteInteriorLines opts l@(Line _ m) contours
    | innerPoint `elem` firstHalf = map (flip infiniteLine slope) firstHalf
    | otherwise = map (flip infiniteLine slope) secondHalf
    where innerPoint = innerPerimeterPoint l contours
          (firstHalf, secondHalf) = splitAt (perimeterLayers opts - 1) $ pointsForPerimeters opts l
          slope = (y m) / (x m)

-- List of lists of interior lines for each line in a contour
allInteriors :: (Enum a, Fractional a, Floating a, Num a, RealFrac a) => Options -> [Point a] -> [[Point a]] -> [[Line a]]
allInteriors opts c contours = map (flip (infiniteInteriorLines opts) contours) lines
    where lines = makeLines c

-- Make inner contours from a list of (outer) contours---note that we do not
-- retain the outermost contour.
innerContours :: (Enum a, Fractional a, Floating a, Num a, RealFrac a) => Options -> [Contour a] -> [[Contour a]]
innerContours opts contours = map concat $ map (constructInnerContours opts) (map (\i -> (last i : i)) interiors)
    where interiors = map (flip (allInteriors opts) contours) contours
          cyclic l = last l : l

-- Construct inner contours, given a list of lines constituting the infinite interior
-- lines. Essentially a helper function for innerContours
constructInnerContours :: (Enum a, Fractional a, Floating a, Num a, RealFrac a)
                       => Options
                       -> [[Line a]]
                       -> [[Contour a]]
constructInnerContours opts interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours opts $ tail interiors
--    | length interiors == 1 = [[intersections]]
    | otherwise = [intersections] : constructInnerContours opts (map tail interiors)
    where intersections = map fromJust $ filter (/= Nothing) $ consecutiveIntersections $ map head interiors

changeShape :: Eq a => [[a]] -> [[a]]
changeShape ls
    | ls == [] = []
    | head ls == [] = []
    | otherwise = (map head ls) : changeShape (map tail ls)

consecutiveIntersections :: (Enum a, Fractional a, Floating a, Num a, RealFrac a) => [Line a] -> [Maybe (Point a)]
consecutiveIntersections [] = []
consecutiveIntersections [a] = []
consecutiveIntersections (a:b:cs) = (lineIntersection a b) : consecutiveIntersections (b : cs)

-- Generate G-code for a given contour c, where g is the most recent G-code produced
gcodeForContour :: (Eq a, Read a, Show a, Floating a, Num a, RealFrac a, Fractional a)
                => Options
                -> [String]
                -> [Point a]
                -> [String]
gcodeForContour opts g c = map ((++) "G1 ") $ zipWith (++) (map show c) ("":es)
    where es = map ((++) " E") $ map show exVals
          exVals = map (+e) $ accumulateValues $ extrusions opts (head c) (tail c)
          lastE = lastExtrusionAmount g
          e = case lastE of Nothing -> 0
                            Just x -> x

gcodeForNestedContours :: (Eq a, Read a, Show a, Floating a, Num a, RealFrac a, Fractional a)
                       => Options
                       -> [String]
                       -> [[Contour a]]
                       -> [String]
gcodeForNestedContours _ _ [] = []
gcodeForNestedContours opts g [cs] = gcodeForContours opts g cs
gcodeForNestedContours opts g cs = firstContoursGcode
                                 ++ gcodeForNestedContours opts firstContoursGcode (tail cs)
    where firstContoursGcode = gcodeForContours opts g (head cs)

gcodeForContours :: (Eq a, Read a, Show a, Floating a, Num a, RealFrac a, Fractional a)
                 => Options
                 -> [String]
                 -> [Contour a]
                 -> [String]
gcodeForContours _ _ [] = []
gcodeForContours opts g [c] = gcodeForContour opts g c
gcodeForContours opts g (c:cs) = firstContourGcode
                               ++ gcodeForContours opts firstContourGcode cs
    where firstContourGcode = gcodeForContour opts g c

gcodeForLine :: (Eq a, Read a, Enum a, Num a, RealFrac a, Floating a, Show a)
             => Options
             -> [String]
             -> Line a
             -> [String]
gcodeForLine opts g l@(Line p s) = gcodeForContour opts g [p, endpoint l]

gcodeForLines :: (Eq a, Read a, Enum a, Num a, RealFrac a, Floating a, Show a)
              => Options
              -> [String]
              -> [Line a]
              -> [String]
gcodeForLines opts g ls = interleave (gcodeForContour opts g $ (point $ head ls) : (map point ls)) travels
    where travels = map travelGcode $ map point ls

-- G-code to travel to a point without extruding
travelGcode :: (Num a, Fractional a, RealFrac a, Show a) => Point a -> String
travelGcode p = "G1 " ++ (show p)

-- Interleave two lists
interleave :: [a] -> [a] -> [a]
interleave [] l2 = l2
interleave l1 [] = l1
interleave (a:as) (b:bs) = a:b:(interleave as bs)

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


-- Get a bounding box of the contour
boundingBox :: (Ord a) => Contour a -> [a]
boundingBox contour = [minX, minY, maxX, maxY]
    where maxX = maximum $ map x contour
          maxY = maximum $ map y contour
          minX = minimum $ map x contour
          minY = minimum $ map y contour 

-- Bounding box contour
addBBox :: (Enum a, Num a, RealFrac a, Floating a) => [Contour a] -> [Contour a]
addBBox contours = [Point x1  y1 z0, Point x2 y1 z0, Point x2 y2 z0, Point x1 y2 z0, Point x1 y1 z0] : contours
    where bBox = boundingBoxAll contours
          x1 = (1) + (bBox !! 0)
          y1 = (1) + (bBox !! 1)
          x2 = (-1) + (bBox !! 2)
          y2 = (-1) + (bBox !! 3)
          z0 = z $ head $ head contours

-- Make support
makeSupport :: (Enum a, Num a, RealFrac a, Floating a)
            => Options
            -> [[Point a]]
            -> LayerType
            -> [Line a]
makeSupport opts contours layerType = map (shortenLineBy $ 2 * defaultThickness)
                                    $ concatMap (infillLineInside (addBBox contours))
                                    $ infillCover Middle
    where infillCover Middle = coveringInfill 20 zHeight
          infillCover BaseEven = coveringLinesUp zHeight
          infillCover BaseOdd = coveringLinesDown zHeight
          zHeight = (z (head (head contours)))
          fillAmount = infill opts

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: (Floating a, Read a, RealFrac a, Ord a, Enum a) => Options -> [Facet a] -> [[[Point a]]]
layers opts fs = map allIntersections (map roundToFifth [maxheight,maxheight-t..0]) <*> pure fs
    where zmax = maximum $ map z $ map point (concatMap sides fs)
          maxheight = t * (fromIntegral $ floor $ zmax / t)
          t = thickness opts

-- Input should be top to bottom, output should be bottom to top
theWholeDamnThing :: (Eq a, Floating a, RealFrac a, Ord a, Enum a, Read a, Show a)
                  => Options
                  -> [([Contour a], Int, Int)] -> [String]
theWholeDamnThing _ [] = []
theWholeDamnThing opts [(a, fromStart, toEnd)] = contourGcode ++ supportGcode ++ infillGcode 
    where contours = getContours a
          interior = map (map fixContour) $ innerContours opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours opts [] $ contours
          innerContourGcode = gcodeForNestedContours opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport opts contours
                       $ layerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill opts innermostContours
                      $ layerType opts (fromStart, toEnd)
theWholeDamnThing opts ((a, fromStart, toEnd):as) = theRest
                                                  ++ contourGcode
                                                  ++ (map travelGcode $ head contours)
                                                  ++ supportGcode
                                                  ++ infillGcode 
    where theRest = theWholeDamnThing opts as
          contours = getContours a
          interior = map (map fixContour) $ innerContours opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours opts theRest $ contours
          innerContourGcode = gcodeForNestedContours opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport opts contours
                       $ layerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill opts innermostContours
                      $ layerType opts (fromStart, toEnd)

layerType :: (Floating a, RealFrac a, Ord a, Enum a, Read a, Show a)
          => Options
          -> (Int, Int)
          -> LayerType
layerType opts (fromStart, toEnd)
    | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
    | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
    | otherwise = Middle
    where topBottomLayers = round $ defaultBottomTopThickness / t
          t = thickness opts


-----------------------------------------------------------------------
--------------------- CONTOUR ACCUMULATION ----------------------------
-----------------------------------------------------------------------

-- First argument is a pair of points that define a line segment, 
-- second is a list of contours that you want to find intersections with
splitAtIntersections :: (Num a, Floating a, RealFrac a ) => [Contour a] -> [Point a] ->[Point a]
splitAtIntersections contours linePts@[p1, p2] = [head linePts]
                                               ++ (sortBy (orderAlongLine line) intersections)
                                               ++ [last linePts] 
    where intersections = nub $ map fromJust $ filter (/= Nothing) $ map (lineIntersection line) allLines
          allLines = concatMap (makeLines) contours
          line = lineFromEndpoints p1 p2


-----------------------------------------------------------------------
--------------------------- Main --------------------------------------
----------------------------------------------------------------------- 
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
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
            let (facets, c) = centerFacets $ facetLinesFromSTL stlLines
            let opts = initialOpts { center = c }
            let allLayers = map (filter (\l -> (head l) /= (head $ tail l))) $ filter (/=[]) $ layers opts facets
            let gcode = theWholeDamnThing opts $ zip3 allLayers [1..length allLayers] $ reverse [1..length allLayers]
            writeFile output (unlines $ startingGcode ++ gcode ++ endingGcode)
