module Main where

import Data.Char (toUpper, toLower, isSpace)
import Data.List (nub)
import Data.Maybe (fromJust)

----------------------------------------------------------
------------ Overhead (data structures, etc.) ------------
----------------------------------------------------------

-- A Point data structure
data Point a = Point { x :: a, y :: a, z :: a } deriving Eq

instance Functor Point where
    fmap f (Point x y z) = Point (f x) (f y) (f z)

-- Display a Point in the format expected by G-code
-- TODO: Will we have too many decimal places when trying to do this with Doubles?
-- Data.Scientific might be useful, but trying to deal with that case will probably
-- break this---and redefining Point to essentially be a Point Double will break the
-- Functor instance.
instance (Show a) => Show (Point a) where
    show p = unwords $ zipWith (++) ["X","Y","Z"] (map show [x p, y p, z p])

-- Data structure for a line segment in the form (x,y,z) = (x0,y0,z0) + t(mx,my,mz)
-- t should run from 0 to 1, so the endpoints are (x0,y0,z0) and (x0 + mx, y0 + my, z0 + mz)
-- TODO: Is this the best representation, or does it make sense to just have a line
-- defined by its endpoints? It feels like doing that may make some computations more
-- complex than they need to be.
data Line a = Line { point :: Point a, slope :: Point a }

data Facet a = Facet { sides :: [Line a] }

-- This should correspond to one line of G-code
type Command = [String]

-- Given a command, write it as one line of G-code
showCommand :: Command -> String
showCommand = map toUpper . unwords

-- Add the coordinates of two points
addPoints :: Num a => Point a -> Point a -> Point a
addPoints (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- Scale the coordinates of a point by s
scalePoint :: Num a => a -> Point a -> Point a
scalePoint = fmap . (*)

-- Create a line given its endpoints
lineFromEndpoints :: Num a => Point a -> Point a -> Line a
lineFromEndpoints p1 p2 = Line p1 (addPoints (scalePoint (-1) p1) p2)

-- Get the other endpoint
endpoint :: Num a => Line a -> Point a
endpoint l = addPoints (point l) (slope l)

-- Find the point on a line for a given Z value. Note that this evaluates to Nothing
-- in the case that there is no point with that Z value, or if that is the only
-- Z value present in that line. The latter should be okay because the properties
-- of our meshes mean that the two endpoints of our line should be captured by
-- the other two segments of a triangle.
pointAtZValue :: (Num a, RealFrac a) => Line a -> a -> Maybe (Point a)
pointAtZValue (Line p m) v
    | 0 <= t && t <= 1 = Just $ addPoints p (scalePoint t m)
    | otherwise = Nothing
    where t = (v - z p) / z m

----------------------------------------------------------
----------- Functions to deal with STL parsing -----------
----------------------------------------------------------

-- Separate lines of STL file into facets
facetsFromSTL :: [String] -> [[String]]
facetsFromSTL [] = []
facetsFromSTL [a] = []
facetsFromSTL l = map (map (dropWhile isSpace)) $ f : facetsFromSTL (tail r)
    where (f, r) = break (\s -> filter (not . isSpace) (map toLower s) == "endfacet") l

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
allIntersections = map . triangleIntersects
