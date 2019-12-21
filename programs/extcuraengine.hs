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

import Prelude (RealFrac, Floating, Num, Double, Enum, Fractional, (*), (/), (+), (-), (^), floor, fromIntegral, odd, div, pi, error, sqrt, mod, round)

import Control.Applicative (pure, (<*>))

import Data.Eq (Eq, (==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord (Ord, Ordering, (<=), (<), (>), (>=), compare, max)

import Data.Tuple (fst, snd)

import Text.Read(Read, read)

import Data.Int (Int)

import Data.String (String)

import Data.Bool(Bool(True, False), (||), (&&), not, otherwise)

import Data.List (nub, sortBy, find, delete, (++), lines, unlines, length, reverse, zip3, filter, tail, head, zipWith, maximum, (!!), minimum, words, init, unwords, concat, splitAt, elem, take, break, dropWhile, map, foldl, concatMap, last)

import Control.Monad (Functor, fmap, return, (>>=))

import Data.Char (toLower, isSpace)

import Data.Maybe (fromJust, Maybe(Just, Nothing))

import Text.Show(Show, show)

import System.Console.GetOpt (OptDescr(Option), ArgOrder(Permute), getOpt, ArgDescr(NoArg,ReqArg))

import System.Environment (getArgs)

import System.IO (IO, writeFile, readFile, putStrLn)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea), ℝ, ℕ, Fastℕ, fromFastℕ, toFastℕ)

default (ℕ, Fastℕ, ℝ)

----------------------------------------------------------
------------ Overhead (data structures, etc.) ------------
----------------------------------------------------------

-- A Point data structure
data Point = Point { x :: ℝ, y :: ℝ, z :: ℝ } deriving Eq

-- Display a Point in the format expected by G-code
instance Show Point where
    show p = unwords $ zipWith (++) ["X","Y","Z"] (map show [x p, y p, z p])

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
data Line = Line { point :: Point, slope :: Point} deriving Show

instance Eq Line where
    (==) (Line p1 m1) (Line p2 m2) = distance p1 p2 < 0.0001 && distance m1 m2 < 0.0001

data Facet = Facet { sides :: [Line] } deriving Eq

data LayerType = BaseOdd | BaseEven | Middle

type Contour = [Point]


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
addPoints :: Point -> Point -> Point
addPoints (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- Scale the coordinates of a point by s
scalePoint :: ℝ -> Point -> Point
scalePoint val (Point a b c) = Point (val*a) (val*b) (val*c)

magnitude :: Point -> ℝ
magnitude (Point x y z) = sqrt $ x^2 + y^2 + z^2

-- Distance between two points
distance :: Point -> Point -> ℝ
distance p1 p2 = magnitude $ addPoints p1 (scalePoint (-1) p2)

-- Create a line given its endpoints
lineFromEndpoints :: Point -> Point -> Line
lineFromEndpoints p1 p2 = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the other endpoint
endpoint :: Line -> Point
endpoint l = addPoints (point l) (slope l)

-- Midpoint of a line
midpoint :: Line -> Point
midpoint (Line p s) = addPoints p (scalePoint 0.5 s)

-- Given a point and slope, make a line with that slope from that point of a specified
-- distance, in the same z plane
pointSlopeLength :: Point -> ℝ -> ℝ -> Line
pointSlopeLength p m d
    | m > 10^100 = Line p (Point 0 d 0)
    | m < -(10^100) = Line p (Point 0 (-d) 0)
    | otherwise = Line p s
    where s = scalePoint scale $ Point 1 yVal 0
          yVal = m
          scale = d / sqrt (1 + yVal^2)

-- Combine lines (p1 -- p2) (p3 -- p4) to (p1 -- p4). We really only want to call this
-- if p2 == p3 and the lines are parallel (see canCombineLines)
combineLines :: Line -> Line -> Line
combineLines (Line p _) l2 = lineFromEndpoints p (endpoint l2)

-- Determine if lines can be combined
canCombineLines :: Bed -> Line -> Line -> Bool
canCombineLines bed l1 l2@(Line p _) = (extendToInfiniteLine bed l1) == (extendToInfiniteLine bed l2)
                                 && (endpoint l1 == p)

-- Given a point and slope (in xy plane), make "infinite" line (i.e. a line that
-- hits two edges of the bed
infiniteLine :: Bed -> Point -> ℝ -> Line
infiniteLine (RectBed (bedX,bedY)) p@(Point _ _ c) m = head $ makeLines $ nub points
    where edges = (map lineFromEndpoints [Point 0 0 c, Point bedX bedY c])
                <*> [Point 0 bedY c, Point bedX 0 c]
          longestLength = sqrt $ bedX*bedX + bedY*bedY
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = map fromJust $ filter (/= Nothing) $ map (lineIntersection line) edges

extendToInfiniteLine :: Bed -> Line -> Line
extendToInfiniteLine bed l@(Line p m) = infiniteLine bed p slope
    where slope = case (x m) of 0 -> 10^100
                                _ -> (y m) / (x m)

-- Construct a perpendicular bisector of a line (with the same length, assuming
-- a constant z value)
perpendicularBisector :: Line -> Line
perpendicularBisector l@(Line p s)
    | y s == 0 = Line (midpoint l) (Point 0 (magnitude s) 0)
    | otherwise = pointSlopeLength (midpoint l) m (distance p (endpoint l))
    where m = -(x s) / (y s)

-- Express a line in terms of the other endpoint
flipLine :: Line -> Line
flipLine l@(Line _ s) = Line (endpoint l) (scalePoint (-1) s)

-- Shift a facet by the vector p
shiftFacet :: Point -> Facet -> Facet
shiftFacet p = Facet . map (\l -> l { point = addPoints p (point l) }) . sides

-- Find the point on a line for a given Z value. Note that this evaluates to Nothing
-- in the case that there is no point with that Z value, or if that is the only
-- Z value present in that line. The latter should be okay because the properties
-- of our meshes mean that the two endpoints of our line should be captured by
-- the other two segments of a triangle.
pointAtXValue :: Line -> ℝ -> Maybe (Point)
pointAtXValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - x p) / x m

pointAtYValue :: Line -> ℝ -> Maybe (Point)
pointAtYValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - y p) / y m

pointAtZValue :: Line -> ℝ -> Maybe (Point)
pointAtZValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - z p) / z m

-- Line intersection algorithm from http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
-- (WOW!)
lineIntersection :: Line -> Line -> Maybe (Point)
lineIntersection l1@(Line p r) l2@(Line q s) 
    | twoDCrossProduct r s == 0 = Nothing
    | 0 <= t && t <= 1 && 0 <= u && u <= 1 = Just (addPoints p (scalePoint t r))
    | otherwise = Nothing
    where t = (twoDCrossProduct (addPoints q (scalePoint (-1) p)) s) / (twoDCrossProduct r s)
          u = (twoDCrossProduct (addPoints q (scalePoint (-1) p)) r) / (twoDCrossProduct r s)

crossProduct :: Point -> Point -> Point
crossProduct (Point x y z) (Point a b c) = Point (y * c - z * b) (z * a - x * c) (x * b - y * a)

twoDCrossProduct :: Point -> Point -> ℝ
twoDCrossProduct p1 p2 = z $ (crossProduct p1 {z = 0} p2 {z = 0})

-- Orders points by x and y (x first, then sorted by y for the same x-values)
orderPoints :: Point -> Point -> Ordering
orderPoints (Point x1 y1 z1) (Point x2 y2 z2) 
    | x1 == x2 = compare y1 y2
    | otherwise = compare x1 x2

orderAlongLine :: Line -> Point -> Point -> Ordering
orderAlongLine line p1@(Point x1 y1 z1) p2@(Point x2 y2 z2)
    | x1 == x2 && y1 == y2 && z1 == z2 = compare z1 z2
    | otherwise = compare (magnitude $ addPoints (point line) (scalePoint (-1) p1))
                $ (magnitude $ addPoints (point line) (scalePoint (-1) p2))


-- round
roundToFifth :: ℝ -> ℝ
roundToFifth a = (fromIntegral $ round (100000 * a)) / 100000

-- round point
roundPoint :: Point -> Point 
roundPoint (Point x y z) = Point (roundToFifth x) (roundToFifth y) (roundToFifth z)

-- shorten line by a millimeter amount on each end 
shortenLineBy :: ℝ -> Line -> Line
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

-- Given a list of facets, center them on the print area
centerFacets :: Bed -> [Facet] -> ([Facet], Point)
centerFacets (RectBed (bedX,bedY)) fs = (map (shiftFacet (Point dx dy dz)) fs, Point dx dy dz)
    where [dx,dy,dz] = zipWith (-) (map (/2) [bedX,bedY,0]) [x0,y0,zmin]
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
readPoint :: String -> Point
readPoint s = Point a b c
    where [a,b,c] = map read $ take 3 $ words s 

-- Given a list of points (in order), construct lines that go between them. Note
-- that this is NOT cyclic, which is why we make sure we have cyclicity in readFacet
makeLines :: [Point] -> [Line]
makeLines l
    | length l < 2 = []
    | otherwise = lineFromEndpoints (head l) (head l') : makeLines l'
    where l' = tail l

-- Read a list of three coordinates (as strings separated by spaces) into the correct
-- Lines
readFacet :: [String] -> Facet
readFacet f
    | length f < 3 = error "Invalid facet"
    | otherwise = Facet $ makeLines $ map readPoint f'
    where f' = last f : f -- So that we're cyclic

-- From STL file (as a list of Strings, each String corresponding to one line),
-- produce a list of lists of Lines, where each list of Lines corresponds to a
-- facet in the original STL
facetLinesFromSTL :: [String] -> [Facet]
facetLinesFromSTL = map readFacet . map cleanupFacet . facetsFromSTL

-- Determine if a triangle intersects a plane at a given z value
triangleIntersects :: ℝ -> Facet -> [Point]
triangleIntersects v f = trimIntersections $ map fromJust $ filter (/= Nothing) intersections
    where intersections = map (flip pointAtZValue v) (sides f)

-- Get rid of the case where a triangle intersects the plane at one point
trimIntersections :: [Point] -> [Point]
trimIntersections l
    | length l' <= 1 = []
    | otherwise = l'
    where l' = nub l

-- Find all the points in the mesh at a given z value
-- Each list in the output should have length 2, corresponding to a line segment
allIntersections :: ℝ -> [Facet] -> [[Point]]
allIntersections v fs = map (map roundPoint) $ filter (/= []) $ map (triangleIntersects v) fs

-- Turn pairs of points into lists of connected points
getContours :: [[Point]] -> [[Point]]
getContours = makeContours . (,) []

-- From a list of contours we have already found and a list of pairs of points
-- (each corresponding to a segment), get all contours described by the points
makeContours :: ([[Point]], [[Point]]) -> [[Point]]
makeContours (contours, pairs)
    | pairs == [] = contours
    | otherwise = makeContours (contours ++ [next], ps)
    where (next, ps) = findContour (head pairs, tail pairs)

-- Extract a single contour from a list of points
findContour :: ([Point], [[Point]]) -> ([Point], [[Point]])
findContour (contour, pairs)
    | p == Nothing = (contour, pairs)
    | otherwise = findContour (contour ++ (delete (last contour) p'), delete p' pairs)
    where match p0 = head p0 == last contour || last p0 == last contour
          p = find match pairs 
          p' = fromJust p 

-- Combine lines when possible. Note that we're working with lists of Lines, not with
-- Contours.
simplifyContour :: Bed -> [Line] -> [Line]
simplifyContour _ [] = []
simplifyContour _ [a] = [a]
simplifyContour bed (a:b:cs)
    | canCombineLines bed a b = simplifyContour bed $ (combineLines a b) : cs
    | otherwise = a : simplifyContour bed (b : cs)

-- Witchcraft
fixContour :: [Point] -> [Point]
fixContour c = (head c) : (tail c ++ [head c])

-- Sort lists of point pairs by x-value of first point in the pair
sortSegments :: [[Point]] -> [[Point]]
sortSegments = sortBy orderSegments 

orderSegments :: [Point] -> [Point] -> Ordering
orderSegments (p1:_) (p2:_) 
    | x p1 == x p2 = compare (y p1) (y p2)
    | otherwise = compare (x p1) (x p2)

-- Amount to extrude when making a line between two points
extrusionAmount :: Options -> Point -> Point -> ℝ
extrusionAmount opts p1 p2 = nozzleDiameter * t * (2 / filamentDiameter) * l / pi
    where l = distance p1 p2
          t = thickness opts

-- Given a contour and the point to start from, evaluate to the amount to extrude between
-- each move
extrusions :: Options -> Point -> [Point] -> [ℝ]
extrusions _ _ [] = []
extrusions opts p c = extrusionAmount opts p (head c) : extrusions opts (head c) (tail c)

-- Take absolute values, turn into accumulated values
accumulateValues :: [ℝ] -> [ℝ]
accumulateValues [] = []
accumulateValues [a] = [a]
accumulateValues (a:b:cs) = a : accumulateValues (a + b : cs)

-- Given a list of G-code lines, find the last amount extruded
lastExtrusionAmount :: [String] -> Maybe ℝ
lastExtrusionAmount gcode
    | extrusionValues == [] = Nothing
    | otherwise = Just $ read $ tail $ last extrusionValues
    where extrusionValues = filter (\s -> (head s == 'E')) $ map last $ map words gcode



-----------------------------------------------------------------------
---------------------- Contour filling --------------------------------
-----------------------------------------------------------------------

-- Make infill
makeInfill :: Bed -> Options -> [[Point]] -> LayerType -> [Line]
makeInfill bed opts contours layerType = concatMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill bed fillAmount zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = (z (head (head contours)))
          fillAmount = infill opts

-- Get the segments of an infill line that are inside the contour
infillLineInside :: [[Point]] -> Line -> [Line]
infillLineInside contours line = map ((!!) allLines) [0,2..(length allLines) - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour line 
getInfillLineIntersections :: [[Point]] -> Line -> [Point]
getInfillLineIntersections contours line = nub $ map fromJust $ filter (/= Nothing)
                                         $ map (lineIntersection line) contourLines
    where contourLines = concatMap makeLines contours

-- Generate covering lines for a given percent infill
coveringInfill :: Bed -> ℝ -> ℝ -> [Line]
coveringInfill bed infill z
    | infill == 0 = []
    | otherwise = pruneInfill (coveringLinesUp bed z) ++ pruneInfill (coveringLinesDown bed z)
    where
      n :: ℝ
      n = max 1 (infill/100)
      pruneInfill :: [Line] -> [Line]
      pruneInfill l = map ((!!) l)[0, (floor n)..length l-1]

-- Generate lines over entire print area
coveringLinesUp :: Bed -> ℝ -> [Line]
coveringLinesUp (RectBed (bedX,bedY)) z = map (flip Line s) (map f [-bedX,-bedX + lineThickness..bedY])
    where s = Point (bedX + bedY) (bedX + bedY) 0
          f v = Point 0 v z

coveringLinesDown :: Bed -> ℝ -> [Line]
coveringLinesDown (RectBed (bedX,bedY)) z = map (flip Line s) (map f [0,lineThickness..bedY + bedX])
    where s =  Point (bedX + bedY) (- bedX - bedY) 0
          f v = Point 0 v z

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: Options -> Line -> [Point]
pointsForPerimeters opts l@(Line p _) = map endpoint
                                      $ map (pointSlopeLength (midpoint l) slope)
                                      $ map (*nozzleDiameter) $ filter (/= 0) [-n..n]
    where n = fromIntegral $ perimeterLayers opts - 1
          Line _ m = perpendicularBisector l
          slope = (y m) / (x m)

-- Lines to count intersections to determine if we're on the inside or outside
perimeterLinesToCheck :: Line -> [Line]
perimeterLinesToCheck l@(Line p _) = map (flip lineFromEndpoints (Point 0 0 (z p)))
                                   $ map endpoint
                                   $ map (pointSlopeLength (midpoint l) slope)
                                   $ map (*nozzleDiameter) [-1,1]
    where Line _ m = perpendicularBisector l
          slope = case (x m) of 0 -> if (y m) > 0 then 10^101 else -(10^101)
                                _ -> (y m) / (x m)

-- Find the point corresponding to the inner perimeter of a given line, given all of the
-- contours in the object
innerPerimeterPoint :: Bed -> Line -> [Contour] -> Point
innerPerimeterPoint bed l contours
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | otherwise = snd $ head intersections
    where linesToCheck = perimeterLinesToCheck l
          contourLines = concatMap makeLines contours
          simplifiedContour = simplifyContour bed contourLines
          numIntersections l' = length $ filter (/= Nothing) $ map (lineIntersection l') simplifiedContour
          intersections = map (\a -> (numIntersections a, point a)) linesToCheck
          oddIntersections = filter (odd . fst) intersections
          nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Construct infinite lines on the interior for a given line
infiniteInteriorLines :: Bed -> Options -> Line -> [[Point]] -> [Line]
infiniteInteriorLines bed opts l@(Line _ m) contours
    | innerPoint `elem` firstHalf = map (flip (infiniteLine bed) slope) firstHalf
    | otherwise = map (flip (infiniteLine bed) slope) secondHalf
    where innerPoint = innerPerimeterPoint bed l contours
          (firstHalf, secondHalf) = splitAt (fromFastℕ $ perimeterLayers opts - 1) $ pointsForPerimeters opts l
          slope = (y m) / (x m)

-- List of lists of interior lines for each line in a contour
allInteriors :: Bed -> Options -> [Point] -> [[Point]] -> [[Line]]
allInteriors bed opts c contours = map (flip (infiniteInteriorLines bed opts) contours) lines
    where lines = makeLines c

-- Make inner contours from a list of (outer) contours---note that we do not
-- retain the outermost contour.
innerContours :: Bed -> Options -> [Contour] -> [[Contour]]
innerContours bed opts contours = map concat $ map (constructInnerContours opts) (map (\i -> (last i : i)) interiors)
    where interiors = map (flip (allInteriors bed opts) contours) contours
          cyclic l = last l : l

-- Construct inner contours, given a list of lines constituting the infinite interior
-- lines. Essentially a helper function for innerContours
constructInnerContours :: Options -> [[Line]] -> [[Contour]]
constructInnerContours opts interiors
    | length interiors == 0 = []
    | length (head interiors) == 0 && (length interiors == 1) = []
    | length (head interiors) == 0 = constructInnerContours opts $ tail interiors
--    | length interiors == 1 = [[intersections]]
    | otherwise = [intersections] : constructInnerContours opts (map tail interiors)
    where intersections = map fromJust $ filter (/= Nothing) $ consecutiveIntersections $ map head interiors

consecutiveIntersections :: [Line] -> [Maybe (Point)]
consecutiveIntersections [] = []
consecutiveIntersections [_] = []
consecutiveIntersections (a:b:cs) = (lineIntersection a b) : consecutiveIntersections (b : cs)

-- Generate G-code for a given contour c, where g is the most recent G-code produced
gcodeForContour :: Options
                -> [String]
                -> [Point]
                -> [String]
gcodeForContour opts g c = map ((++) "G1 ") $ zipWith (++) (map show c) ("":es)
    where es = map ((++) " E") $ map show exVals
          exVals = map (+e) $ accumulateValues $ extrusions opts (head c) (tail c)
          lastE :: Maybe ℝ
          lastE = lastExtrusionAmount g
          e :: ℝ
          e = case lastE of Nothing -> 0
                            Just something -> something

gcodeForNestedContours :: Options
                       -> [String]
                       -> [[Contour]]
                       -> [String]
gcodeForNestedContours _ _ [] = []
gcodeForNestedContours opts g [cs] = gcodeForContours opts g cs
gcodeForNestedContours opts g cs = firstContoursGcode
                                 ++ gcodeForNestedContours opts firstContoursGcode (tail cs)
    where firstContoursGcode = gcodeForContours opts g (head cs)

gcodeForContours :: Options
                 -> [String]
                 -> [Contour]
                 -> [String]
gcodeForContours _ _ [] = []
gcodeForContours opts g [c] = gcodeForContour opts g c
gcodeForContours opts g (c:cs) = firstContourGcode
                               ++ gcodeForContours opts firstContourGcode cs
    where firstContourGcode = gcodeForContour opts g c

-- G-code to travel to a point without extruding
travelGcode :: Point -> String
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
boundingBoxAll :: [Contour] -> [ℝ]
boundingBoxAll contours = (map minimum $ map (\n -> map (!!n) bBoxes) [0, 1])
                        ++ (map maximum $ map (\n -> map (!!n) bBoxes) [2, 3])
    where bBoxes = filter (/= []) $ map boundingBox $ filter (/= []) contours


-- Get a bounding box of a contour
boundingBox :: Contour -> [ℝ]
boundingBox contour = [minX, minY, maxX, maxY]
    where maxX = maximum $ map x contour
          maxY = maximum $ map y contour
          minX = minimum $ map x contour
          minY = minimum $ map y contour 

-- Bounding box contour
addBBox :: [Contour] -> [Contour]
addBBox contours = [Point x1  y1 z0, Point x2 y1 z0, Point x2 y2 z0, Point x1 y2 z0, Point x1 y1 z0] : contours
    where bBox = boundingBoxAll contours
          x1 = (1) + (bBox !! 0)
          y1 = (1) + (bBox !! 1)
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
makeSupport bed opts contours _ = map (shortenLineBy $ 2 * (thickness opts))
                                  $ concatMap (infillLineInside (addBBox contours))
                                  $ infillCover Middle
    where infillCover Middle = coveringInfill bed 20 zHeight
          infillCover BaseEven = coveringLinesUp bed zHeight
          infillCover BaseOdd = coveringLinesDown bed zHeight
          zHeight = (z (head (head contours)))

-----------------------------------------------------------------------
--------------------------- LAYERS ------------------------------------
-----------------------------------------------------------------------

-- Create contours from a list of facets
layers :: Options -> [Facet] -> [[[Point]]]
layers opts fs = map allIntersections (map roundToFifth [maxheight,maxheight-t..0]) <*> pure fs
    where zmax = maximum $ map z $ map point (concatMap sides fs)
          maxheight = t * (fromIntegral $ floor $ zmax / t)
          t = thickness opts

getLayerType :: Options -> (Fastℕ, Fastℕ) -> LayerType
getLayerType opts (fromStart, toEnd)
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
  | (fromStart <= topBottomLayers || toEnd <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
  | otherwise = Middle
  where
    topBottomLayers :: Fastℕ
    topBottomLayers = round $ defaultBottomTopThickness / t
    t = thickness opts

-- Input should be top to bottom, output should be bottom to top
sliceObject ::  Bed -> Options
                  -> [([Contour], Fastℕ, Fastℕ)] -> [String]
sliceObject _ _ [] = []
sliceObject bed opts [(a, fromStart, toEnd)] = contourGcode ++ supportGcode ++ infillGcode 
    where contours = getContours a
          interior = map (map fixContour) $ innerContours bed opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours opts [] $ contours
          innerContourGcode = gcodeForNestedContours opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport bed opts contours
                       $ getLayerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill bed opts innermostContours
                      $ getLayerType opts (fromStart, toEnd)
sliceObject bed opts ((a, fromStart, toEnd):as) = theRest
                                                  ++ contourGcode
                                                  ++ (map travelGcode $ head contours)
                                                  ++ supportGcode
                                                  ++ infillGcode 
    where theRest = sliceObject bed opts as
          contours = getContours a
          interior = map (map fixContour) $ innerContours bed opts contours
          allContours = zipWith (:) contours interior
          innermostContours = if interior == [] then contours else map last allContours
          outerContourGcode = gcodeForContours opts theRest $ contours
          innerContourGcode = gcodeForNestedContours opts outerContourGcode interior
          contourGcode = outerContourGcode ++ innerContourGcode
          supportGcode = if (not $ support opts) then [] else fixGcode
                       $ gcodeForContour opts contourGcode
                       $ concatMap (\l -> [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeSupport bed opts contours
                       $ getLayerType opts (fromStart, toEnd)
          infillGcode = fixGcode
                      $ gcodeForContour opts (contourGcode ++ supportGcode)
                      $ concatMap (\l -> [point l, endpoint l])
                      $ mapEveryOther flipLine
                      $ makeInfill bed opts innermostContours
                      $ getLayerType opts (fromStart, toEnd)

----------------------------------------------------------
----------------------- Constants ------------------------
----------------------------------------------------------
-- in mm
nozzleDiameter,
    filamentDiameter,
    defaultBottomTopThickness,
    lineThickness :: ℝ

nozzleDiameter = 0.4
filamentDiameter = 1.75
defaultBottomTopThickness = 0.8
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
            (\opt -> return opt { help = True }))
        "Get help"
    , Option "i" ["infill"]
        (ReqArg
            (\arg opt -> if (read arg :: ℝ) >= 0 then return opt { infill = read arg }
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
            (\arg opt -> if (read arg :: Fastℕ) > 0 then return opt { perimeterLayers = read arg }
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
    where argVal = read arg :: ℝ

-----------------------------------------------------------------------
--------------------------- Main --------------------------------------
----------------------------------------------------------------------- 
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOpt Permute options args
    initialOpts <- foldl (>>=) (return defaultOptions) actions
    let Options { help = help
                , output = output
                } = initialOpts
    let helpString = "Usage: slicer filename [-i infill] [-p perimeter] [-s support] [-t thickness] [-o outfile]"
    if help then (putStrLn helpString) else do
        if length nonOptions == 0 then (putStrLn "Error: Enter a file name") else do
            let fname = head nonOptions
            stl <- readFile fname
            let stlLines = lines stl
                (facets, c) = centerFacets printerBed $ facetLinesFromSTL stlLines
                opts = initialOpts { center = c }
                allLayers = map (filter (\l -> (head l) /= (head $ tail l))) $ filter (/=[]) $ layers opts facets
                gcode = sliceObject printerBed opts $ zip3 allLayers [1..(toFastℕ $ length allLayers)] $ reverse [1..(toFastℕ $ length allLayers)]
              in writeFile output (unlines $ startingGcode ++ gcode ++ endingGcode)
              where
                -- FIXME: pull these values from a cura config.
                printerBed :: Bed
                printerBed = RectBed (150,150)
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


