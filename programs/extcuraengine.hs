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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)

import Prelude ((*), (/), (+), (-), fromIntegral, odd, pi, sqrt, mod, round, floor, concatMap, foldMap, fmap, (<>), toRational, FilePath, Int, fromInteger, Eq, fromRational, init, error, seq, div, fromRational, putStrLn)

import Control.Applicative (pure, (<*>), (<$>))

import Data.Eq ((==), (/=))

import Data.Function ((.), ($), flip)

import Data.Ord ((<=), (>), (<))

import Data.Tuple (fst, snd)

import Data.ByteString.UTF8 (fromString)

import Data.String (String)

import Data.Bool(Bool(True, False), (||), (&&), otherwise, not)

import Data.List (nub, sortBy, length, zip, filter, tail, head, zipWith, maximum, (!!), minimum, splitAt, elem, last, null, (++), concat, foldl')

import Control.Monad ((>>=))

import Data.Maybe (Maybe(Just, Nothing), catMaybes, mapMaybe, fromMaybe)

import Text.Show(show)

import System.IO (IO)

import Data.ByteString (readFile, writeFile, ByteString)

import Data.ByteString.Char8 (unlines, spanEnd)

import Control.Monad.State(runState, liftIO)

import Options.Applicative (fullDesc, progDesc, header, auto, info, helper, help, str, argument, long, short, option, metavar, execParser, Parser, optional, strOption, switch, hsubparser, command, many)

import Data.Double.Conversion.ByteString (toFixed)

import Control.Parallel.Strategies (using, rdeepseq, rseq, parListChunk, parBuffer)

import Control.DeepSeq (NFData(rnf))

import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)

-- The definition of the symbol type, so we can access variables, and see settings.
import Graphics.Implicit.ExtOpenScad.Definitions (VarLookup, OVal(ONum, OString, OBool), lookupVarIn, Message(Message), MessageType(TextOut), ScadOpts(ScadOpts))

import Graphics.Implicit.Definitions (ℝ, ℝ2, ℝ3, ℕ, Fastℕ(Fastℕ), fromFastℕ)

import Graphics.Slicer (Bed(RectBed), BuildArea(RectArea, CylinderArea), Point(Point), Line(Line), point, lineIntersection, scalePoint, addPoints, distance, lineFromEndpoints, endpoint, midpoint, flipLine, Facet, sides, Contour(Contour), LayerType(BaseOdd, BaseEven, Middle), Direction(Positive, Negative), lineSlope, Slope(IsOrigin, OnXAxis, OnYAxis, HasSlope), pointSlopeLength, perpendicularBisector, shiftFacet, orderPoints, roundPoint, shortenLineBy, accumulateValues, makeLines, facetIntersects, getContours, Extruder(Extruder), nozzleDiameter, filamentWidth, EPos(EPos), StateM, MachineState(MachineState), getEPos, setEPos, facetLinesFromSTL, canCombineLines, combineLines, combineConsecutiveLines)

default (ℕ, Fastℕ, ℝ)

-------------------- TOTAL HACK -----------------------
threads :: Fastℕ
threads = 32

---------------------------------------------------------------------------
-------------------- Point and Line Arithmetic ----------------------------
---------------------------------------------------------------------------

-- | Given a point and slope (on an xy plane), make a line segment, where the far end is at the edge of the print bed.
-- FIXME: assumes the origin is at the corner.
-- FIXME: round beds?
-- FIXMEMORE: concave beds?
lineToEdge :: BuildArea -> Slope -> Point -> Line
lineToEdge (RectArea (bedX,bedY,_)) m p@(Point (_,_,c)) = head . makeLines $ nub points
    where edges = lineFromEndpoints <$> [Point (0,0,c), Point (bedX,bedY,c)]
                                    <*> [Point (0,bedY,c), Point (bedX,0,c)]
          longestLength = sqrt $ bedX*bedX + bedY*bedY
          halfLine@(Line p' s) = pointSlopeLength p m longestLength -- should have p' == p
          line = lineFromEndpoints (endpoint halfLine) (addPoints p' (scalePoint (-1) s))
          points = mapMaybe (lineIntersection line) edges

-- Center facets relative to the center of the build area.
-- FIXME: assumes the origin is at the corner.
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
---------------------- Infill Generation ------------------------------
-----------------------------------------------------------------------

-- Generate infill for a layer.
-- Basically, cover the build plane in lines, then remove the portions of those lines that are not inside of one of the target contours.
-- The target contours should be the innermost parameters.
-- FIXME: reduce size of build area to box around contours, to reduce the area searched?
makeInfill :: BuildArea -> Print -> [Contour] -> ℝ -> LayerType -> [Line]
makeInfill buildarea print contours zHeight layerType = foldMap (infillLineInside contours) $ infillCover layerType
    where infillCover Middle = coveringInfill buildarea print zHeight
          infillCover BaseEven = coveringLinesUp buildarea ls zHeight
          infillCover BaseOdd = coveringLinesDown buildarea ls zHeight
          ls = lineSpacing print

-- Get the segments of an infill line that are inside of any of the contours.
-- May return multiple lines, or empty set.
infillLineInside :: [Contour] -> Line -> [Line]
infillLineInside contours line = (allLines !!) <$> [0,2..length allLines - 1]
    where allLines = makeLines $ sortBy orderPoints $ getInfillLineIntersections contours line

-- Find all places where an infill line intersects any contour in our set of contours.
getInfillLineIntersections :: [Contour] -> Line -> [Point]
getInfillLineIntersections contours line = nub $ mapMaybe (lineIntersection line) contourLines
    where
      contourLines = foldMap makeLines $ (\(Contour points) -> points) <$> contours

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

-- Generate lines over entire print area
coveringLinesDown :: BuildArea -> ℝ -> ℝ -> [Line]
coveringLinesDown (RectArea (bedX,bedY,_)) lt zHeight = flip Line s . f <$> [0,lt..bedY + bedX]
    where s =  Point (bedX + bedY,- bedX - bedY,0)
          f v = Point (0,v,zHeight)

-- Helper function to generate the points we'll need to make the inner perimeters
pointsForPerimeters :: Extruder -> Fastℕ -> Line -> [Point]
pointsForPerimeters extruder perimeterCount l = endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*nozzleDia) <$> filter (/= 0) [-n..n]
  where
    n :: ℝ
    n = fromIntegral $ perimeterCount - 1
    Line _ m = perpendicularBisector l
    nozzleDia :: ℝ
    nozzleDia = nozzleDiameter extruder

-- Given a line, generate a pair of perpendicular lines from the given line's midpoint, one path width long, on the same z plane as the given line.
perimeterLinesToCheck :: Line -> ℝ -> (Line, Line)
perimeterLinesToCheck l@(Line p _) pathWidth = (head linePair, last linePair)
  where
    linePair = (`lineFromEndpoints` Point (0,0,zOf p)) . endpoint . pointSlopeLength (midpoint l) (lineSlope m) . (*pathWidth) <$> [-1,1]
    Line _ m = perpendicularBisector l
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- Find a point on the inside side of a contour line.
-- the found point should be on the perpendicular bisector of the given line, pathWidth from the line.
innerPerimeterPoint :: ℝ -> Line -> [Contour] -> Point
innerPerimeterPoint pathWidth l contours
    | length oddIntersections > 0 = snd $ head oddIntersections
    | length nonzeroIntersections > 0 = snd $ head nonzeroIntersections
    | otherwise = snd $ head intersections
    where
      linesToCheck = perimeterLinesToCheck l pathWidth
      bothLinesToCheck = fst linesToCheck : [snd linesToCheck] 
      contourLines = foldMap makeLines $ (\(Contour points) -> points) <$> contours
      intersections = (\a -> (numIntersections a, point a)) <$> bothLinesToCheck
      numIntersections l' = length $ mapMaybe (lineIntersection l') contourLines
      oddIntersections = filter (odd . fst) intersections
      nonzeroIntersections = filter (\(v,_) -> v /= 0) intersections

-- Contour optimizer. Merges small line fragments into larger ones.
cleanContour :: Contour -> Contour
cleanContour (Contour points) = Contour $ cleanPoints points
  where
    cleanPoints :: [Point] -> [Point]
    cleanPoints pts
      | pts == [] = []
      -- FIXME: nub here is a hack.
      | otherwise = makePointsFromLines $ combineConsecutiveLines $ nub $ makeLines $ pts
    makePointsFromLines :: [Line] -> [Point]
    makePointsFromLines lines = (fst $ pointsFromLine $ head lines) : (snd . pointsFromLine <$> lines)
    pointsFromLine :: Line -> (Point, Point)
    pointsFromLine ln@(Line p _) = (p,(endpoint ln))

-- Get all of the contours inside of the given contours. may be recursed into by the helper when contours close off a region of the interior of the object.
-- FIXME: return result should be a tree.
getInnerContours :: Printer -> Fastℕ -> [Contour] -> [[Contour]]
getInnerContours (Printer _ _ extruder) innerPerimeterCount contours
  | innerPerimeterCount == 0 = []
  | otherwise  = [ addInsideContour pathWidth contours thisContour | thisContour <- contours ]
  where
    pathWidth = nozzleDiameter extruder
    fixContour :: Contour -> Contour
    fixContour (Contour c)
      | length c > 1 = Contour (c <> [head c])
      -- FIXME: we should not get one point, or 0 point contours.
      | otherwise = error $ "fixContour: contour has too few points: " <> (show $ length c) <> "\n" <> show c

newtype ContourZipper = ContourZipper ([Line],[Line])

-- Add one contour inside of the given contour. accepts contours for appending to the output.
-- FIXME: traverse?
-- FIXME: there are four types of contour: raw, inside, middle, and outside. this code needs to be aware of the differences. fix the type system.
addInsideContour :: ℝ -> [Contour] -> Contour -> [Contour]
addInsideContour pathWidth allContours contour@(Contour contourPoints) = do
  --error $ (show contourLines) <> "\n" <> show foundContour <> "\n" <> show (perpendicularBisector <$> contourLines) 
  [Contour (makePointsFromLines $ catMaybes foundContour)] ++ (foundNewContours)
  where
    (_, foundContour, foundNewContours) = foldl' (addInteriorLines pathWidth (isInsideContour allContours contour) allContours) (makeContourZipper contour, [], []) contourLines
    contourLines = makeLines $ contourPoints
    -- FIXME: implement me.
    isInsideContour :: [Contour] -> Contour -> Bool 
    isInsideContour contourSet thisContour = False
    makePointsFromLines :: [Line] -> [Point]
    makePointsFromLines lines
      | lines == [] = error $ "found no inner points for contour."   
      | otherwise = (fst $ pointsFromLine $ head lines) : (snd . pointsFromLine <$> lines)
    pointsFromLine :: Line -> (Point, Point)
    pointsFromLine ln@(Line p _) = (p,(endpoint ln))
    makeContourZipper :: Contour -> ContourZipper
    makeContourZipper contour@(Contour contourPoints) = ContourZipper (makeLines $ contourPoints, [])
    -- when you have a line, and an interior center point, draw an interior line of equal length plus 2*pathLength, and check it for intersections against this contour, and the previous lines in this contour.
    -- if it hits the contour on line other than the line before or after the parent, you have a pinch. draw a line before, and after the intersection. the line after is the first line of the new contour, the line before is still this contour.
    -- a line is from the point that is path_width from both the current and the next/last line. straight line = lengthen 0, obtuse right angle = lengthen path_width, acute right angle = shorten path width. 
    -- Optimization: only check non-neighbor lines when the angles add up to a certain amount? looking for curling back. anything over ~180 degrees, relative to the slope of this line.
    addInteriorLines :: ℝ -> Bool -> [Contour] -> (ContourZipper, [Maybe Line], [Contour]) -> Line -> (ContourZipper, [Maybe Line], [Contour])
    addInteriorLines pathWidth isIsideContour allContours (contourZipper@(ContourZipper (remainingLines, handledLines)), contourLines, extraContours) line
      -- skip if this is not a 2d space.
      | (length remainingLines + length handledLines) < 3 = (nextLine contourZipper, contourLines, extraContours)
      | otherwise = (nextLine contourZipper, contourLines ++ [findLine allContours (head remainingLines)], extraContours ++ (findExtraContours (head remainingLines)))
      where
        -- push our zipper one step forward.
        nextLine :: ContourZipper -> ContourZipper
        nextLine (ContourZipper (left,right)) = ContourZipper (tail left, head left:right)
        findLine contours ln = Just $ idealLine contours ln
        idealLine contours ln@(Line p m) =
          (flipLine midToStart) `combineLines` midToEnd 
          where
            midToEnd, midToStart :: Line
            midToEnd =   pointSlopeLength (innerPoint contours ln) (lineSlope m) (lineLength ln/2 - (shortenEnd ln $ lineNext contourZipper))
            midToStart = pointSlopeLength (innerPoint contours ln) (lineSlope m) (0-lineLength ln/2 - (0-(shortenStart ln $ linePrevious contourZipper)))
        innerPoint contours ln = innerPerimeterPoint pathWidth ln contours
        -- FIXME: implement this.
        findExtraContours ln = []
        -- find the line after the one we are evaluating.
        lineNext :: ContourZipper -> Line
        lineNext (ContourZipper (followingLines, handledLines))
          | length followingLines > 1 = head $ tail followingLines
          | otherwise = last handledLines
        -- find the line before the one we are evaluating.
        linePrevious :: ContourZipper -> Line
        linePrevious (ContourZipper (followingLines,handledLines)) 
          | handledLines == [] = last followingLines
          | otherwise = head handledLines
        -- optimization: sqrt (x*x+y*y)
        lineLength :: Line -> ℝ
        lineLength ln@ (Line p _) = distance p $ endpoint ln
        -- line segments for a hypothetical line, without being shortened yet.
        rawMidtoEnd contours ln@(Line p m) = pointSlopeLength (innerPoint contours ln) (lineSlope m) (lineLength ln / 2)
        rawMidtoStart contours ln@(Line p m) = pointSlopeLength (innerPoint contours ln) (lineSlope m) (0-(lineLength ln / 2))
        shortenStart :: Line -> Line -> ℝ
        shortenStart l1 l2 =
          distance l1Point $ fromMaybe (error $ "no intersection on line " <> show (length handledLines) <> "\n" <> show l1 <> " -> " <> show newL1 <> "\n" <> show l2 <> " -> " <> show newL2 <> "\n") $ intersection
          where
            intersection = lineIntersection newL1 newL2
            l1Point = (\(Line p m) -> p) $ flipLine newL1
            newL1 = (rawMidtoStart allContours l1)
            newL2 = ((flipLine $ rawMidtoEnd allContours l2))
        shortenEnd :: Line -> Line -> ℝ 
        shortenEnd l1 l2 =
          distance l1Point $ fromMaybe (error $ "no intersection on line " <> show (length handledLines) <> "\n" <> show l1 <> " -> " <> show newL1 <> "\n" <> show l2 <> " -> " <> show newL2 <> "\n") $ intersection
          where
            intersection = lineIntersection newL1 newL2
            l1Point = (\(Line p m) -> p) $ newL2
            newL1 = (rawMidtoEnd allContours l1)
            newL2 = ((flipLine $ rawMidtoStart allContours l2))
        
        -- then, we are looking for the position on either line, where a line drawn at a right angle from that position to the new line is d.

-- Make inner contours from a list of (outer) contours.
{-
getInnerContoursOld :: Printer -> Fastℕ -> [Contour] -> [[Contour]]
getInnerContoursOld printer perimeterCount contours = foldMap ( constructInnerContours .(\i -> last i : i)) interiors
    where
      interiors = allInteriors <$> contours
      -- List of interior lines for each line in a contour
      allInteriors :: Contour -> [[Line]]
      allInteriors (Contour contourPoints) = interiorLines printer perimeterCount contours <$> targetLines
        where targetLines = makeLines contourPoints
      -- Construct lines on the interior for a given line
      interiorLines :: Printer -> Fastℕ -> [Contour] -> Line -> [Line]
      interiorLines (Printer _ buildarea extruder) perimeterCount contours l@(Line _ m)
        | innerPoint `elem` firstHalf = lineToEdge buildarea (lineSlope m) <$> firstHalf
        | otherwise = lineToEdge buildarea (lineSlope m) <$> secondHalf
        where
          innerPoint = innerPerimeterPoint filamentDia l contours
          (firstHalf, secondHalf) = splitAt (fromFastℕ $ perimeterCount - 1) $ pointsForPerimeters extruder perimeterCount l
          filamentDia = filamentWidth extruder

-- Construct inner contours for a list of Contours.
-- Essentially a helper function for getInnerContours.
constructInnerContours :: [[Line]] -> [[Contour]]
constructInnerContours lineset@(lines:morelines)
    | length lines == 0 = []
    | length lines == 0 && morelines == [] = []
    | length lines == 0 = constructInnerContours morelines
    | otherwise = if intersections == [] then constructInnerContours (tail <$> lineset) else contours : constructInnerContours (tail <$> lineset)
    where
      contours = [ fixContour $ Contour $ intersections ]
      intersections = catMaybes $ consecutiveIntersections $ head <$> lineset
      -- 'fix' a contour by making sure the first and last point are the same.
      fixContour :: Contour -> Contour
      fixContour (Contour c)
        | length c > 1 = Contour (c <> [head c])
        -- FIXME: we should not get one point, or 0 point contours.
        | otherwise = error $ "fixContour: contour has too few points: " <> (show $ length c) <> "\n" <> show c
      consecutiveIntersections :: [Line] -> [Maybe Point]
      consecutiveIntersections [] = [Nothing]
      consecutiveIntersections [_] = [Nothing]
      consecutiveIntersections lines = zipWith lineIntersection (init lines) (tail lines)

-}

data GCode =
    GCMove2 { _startPoint2 :: ℝ2, _stopPoint2 :: ℝ2 }
  | GCMove3 { _startPoint3 :: ℝ3, _stopPoint3 :: ℝ3 }
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

-- Generate G-Code for a given contour.
-- Assumes the printer is already at the first point.
-- Also assumes contours that have points.
gcodeFor2DContour :: ℝ -> ℝ -> Contour -> [GCode]
gcodeFor2DContour lh pathWidth (Contour contourPoints)
  | length contourPoints > 1 = zipWith (make2DExtrudeGCode lh pathWidth) (init contourPoints) (tail contourPoints)
-- FIXME: what do we do with a 'contour' that is just a point?
--  | length contourPoints     = make2DExtrudeGCode lh pathWidth
  | otherwise                = []
  
-- GCode to travel to a point while extruding.
-- FIXME: assumes pathwidth == nozzle diameter, which is clearly wrong...
make2DExtrudeGCode :: ℝ -> ℝ -> Point -> Point -> GCode
make2DExtrudeGCode pathThickness pathWidth p1@(Point (x1,y1,_)) p2@(Point (x2,y2,_)) = GCRawExtrude2 (x1, y1) (x2, y2) (RawExtrude pathLength pathWidth pathThickness)
  where
   pathLength = distance p1 p2

-- render a value to ByteString, in the number of characters that are suitable to use in a gcode file. drops trailing zeroes, and the decimal, if there is no fractional component.
posIze :: ℝ -> ByteString
posIze pos
  | pos == 0 = "0"
  | otherwise = fst $ spanEnd (== '.') $ fst $ spanEnd (== '0') $ toFixed 5 pos

-- render a gcode into a piece of text.
gcodeToText :: GCode -> ByteString
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

-- travel to a point without extruding
make2DTravelGCode :: Point -> Point -> GCode
make2DTravelGCode (Point (x1,y1,_)) (Point (x2,y2,_)) = GCMove2 (x1,y1) (x2,y2)

make3DTravelGCode :: Point -> Point -> GCode
make3DTravelGCode (Point (x1,y1,z1)) (Point (x2,y2,z2)) = GCMove3 (x1,y1,z1) (x2,y2,z2)

gcodeForNested2DContours :: ℝ -> ℝ -> [[Contour]] -> [GCode]
gcodeForNested2DContours lh pathWidth = concatMap (gcodeFor2DContours lh pathWidth)

gcodeFor2DContours :: ℝ -> ℝ -> [Contour] -> [GCode]
gcodeFor2DContours lh pathWidth = concatMap (gcodeFor2DContour lh pathWidth)

-----------------------------------------------------------------------
----------------------------- SUPPORT ---------------------------------
-----------------------------------------------------------------------

-- A bounding box. a box around a contour.
data BBox = BBox ℝ2 ℝ2

-- Check if a bounding box is empty.
isEmptyBBox :: BBox -> Bool
isEmptyBBox (BBox (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

-- Get a bounding box of all contours.
boundingBoxAll :: [Contour] -> Maybe BBox
boundingBoxAll contours = if isEmptyBBox box then Nothing else Just box
    where
      box  = BBox (minX, minY) (maxX, maxY)
      minX = minimum $ (\(BBox (x1,_) _) -> x1) <$> bBoxes
      minY = minimum $ (\(BBox (_,y1) _) -> y1) <$> bBoxes
      maxX = maximum $ (\(BBox _ (x2,_)) -> x2) <$> bBoxes
      maxY = maximum $ (\(BBox _ (_,y2)) -> y2) <$> bBoxes
      bBoxes = mapMaybe boundingBox contours


-- Get a 2D bounding box of a 2D contour.
boundingBox :: Contour -> Maybe BBox
boundingBox (Contour []) = Nothing
boundingBox (Contour contourPoints) = if isEmptyBBox box then Nothing else Just box
  where
    box  = BBox (minX, minY) (maxX, maxY)
    minX = minimum $ xOf <$> contourPoints
    minY = minimum $ yOf <$> contourPoints
    maxX = maximum $ xOf <$> contourPoints
    maxY = maximum $ yOf <$> contourPoints
    xOf,yOf :: Point -> ℝ
    xOf (Point (x,_,_)) = x
    yOf (Point (_,y,_)) = y


-- add a 2D bounding box to a list of contours, as the first contour in the list.
-- FIXME: assumes 2D contour.
addBBox :: [Contour] -> ℝ -> [Contour]
addBBox contours z0 = Contour [Point (x1,y1,z0), Point (x2,y1,z0), Point (x2,y2,z0), Point (x1,y2,z0), Point (x1,y1,z0)] : contours
    where
      bbox = fromMaybe (BBox (1,1) (-1,-1)) $ boundingBoxAll contours
      (BBox (x1, y1) (x2, y2)) = incBBox bbox 1
      -- Put a fixed amount around the 2d bounding box.
      incBBox (BBox (x1,y1) (x2,y2)) amount = BBox (x1+amount, y1+amount) (x2-amount, y2-amount)

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
layers :: Print -> [Facet] -> [[Contour]]
layers print fs = [ allIntersections currentLayer | currentLayer <- [lh,lh*2..maxheight] ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
  where
    allIntersections :: ℝ -> [Contour]
    allIntersections zLayer = fmap Contour <$> catMaybes $ facetIntersects zLayer <$> fs
    zs = [zOf $ point triPoints | triPoints <- foldMap sides fs ] `using` parListChunk (div (length fs) (fromFastℕ threads)) rseq
    zmax :: ℝ
    zmax = maximum zs
    -- the highest point on the object.
    maxheight = lh * fromIntegral (floor (zmax / lh)::Fastℕ)
    lh = layerHeight print
    zOf :: Point -> ℝ
    zOf (Point (_,_,z)) = z

-- FIXME: detect top and bottoms seperately.
getLayerType :: Print -> Fastℕ -> LayerType
getLayerType print fromStart
  | (fromStart <= topBottomLayers || (fromStart+1) <= topBottomLayers) && fromStart `mod` 2 == 0 = BaseEven
  | (fromStart <= topBottomLayers || (fromStart+1) <= topBottomLayers) && fromStart `mod` 2 == 1 = BaseOdd
  | otherwise = Middle
  where
    topBottomLayers :: Fastℕ
    topBottomLayers = round $ surfaceThickness print / layerHeight print

----------------------------------------------------------------------
---------------------------- MISC ------------------------------------
----------------------------------------------------------------------

-- Map a function to every other value in a list. This is useful for fixing non-extruding lines.
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [a] = [f a]
mapEveryOther f xs = zipWith (\x v -> if odd v then f x else x) xs [1::Fastℕ,2..]

-------------------------------------------------------------
----------------------- ENTRY POINT -------------------------
-------------------------------------------------------------
sliceObject :: Printer ->  Print ->  [([Contour], Fastℕ)] -> StateM [GCode]
sliceObject printer@(Printer _ _ extruder) print allLayers = do
  let
    layersWithFollowers = [sliceLayer printer print False layer | layer <- init allLayers] `using` parListChunk (div (length allLayers) (fromFastℕ threads)) rdeepseq
    lastLayer = sliceLayer printer print True (last allLayers)
  cookExtrusions extruder (concat $ layersWithFollowers ++ [lastLayer])

sliceLayer ::  Printer ->  Print -> Bool -> ([Contour], Fastℕ) -> [GCode]
sliceLayer printer@(Printer _ buildarea extruder) print@(Print perimeterCount _ lh _ hasSupport _) isLastLayer (layerContours, layerNumber) = do
  let
    outerContourGCode = gcodeFor2DContours lh pathWidth outerContours
    innerContourGCode = gcodeForNested2DContours lh pathWidth innerContours
    infillGCode = gcodeFor2DContour lh pathWidth infillContours
    supportGCode = if hasSupport then gcodeFor2DContour lh pathWidth supportContours else []
    layerStart = [GCMarkLayerStart layerNumber]
    -- FIXME: make travel gcode from the previous contour's last position?
    travelToOuterContour = [make3DTravelGCode (Point (0,0,0)) $ firstPoint $ head outerContours]
    outerContour = GCMarkOuterWallStart : outerContourGCode
    innerContour = GCMarkInnerWallStart : innerContourGCode
    infill = if infillGCode == [] then [] else GCMarkInfillStart : infillGCode
    -- FIXME: not all support is support. what about supportInterface?
    support = if supportGCode == [] then [] else GCMarkSupportStart : supportGCode
    travelGCode = if isLastLayer then [] else layerChange
    layerChange = (make2DTravelGCode $ Point (0,0,0)) (head $ pointsOfContour $ head outerContours) : zipWith make2DTravelGCode (init $ pointsOfContour $ head outerContours) (tail $ pointsOfContour $ head outerContours)
  -- extruding gcode generators should be handled here in the order they are printed, so that they are guaranteed to be called in the right order.
  layerStart <> travelToOuterContour <> outerContour <> innerContour <> infill <> support <> travelGCode 
    where
      firstPoint (Contour contourPoints) = head contourPoints
      rawOuterContours = getContours [pointsOfContour contour | contour <- layerContours] `using` parBuffer (fromFastℕ threads) rdeepseq
      outerContours = [cleanContour contour | contour <- rawOuterContours] `using` parBuffer (fromFastℕ threads) rdeepseq
      rawInnerContours = getInnerContours printer perimeterCount outerContours
      innerContours = [cleanContour <$> contour | contour <- rawInnerContours ] `using` parBuffer (fromFastℕ threads) rdeepseq
      -- FIXME: infill and support should remain lines.
      infillContours = foldMap (\l -> Contour [point l, endpoint l])
                       $ mapEveryOther flipLine
                       $ makeInfill buildarea print innermostContours zHeightOfLayer
                       $ getLayerType print layerNumber
      supportContours = foldMap (\l -> Contour [point l, endpoint l])
                        $ mapEveryOther flipLine
                        $ makeSupport buildarea print outerContours zHeightOfLayer
      allContours = zipWith (:) outerContours innerContours
      innermostContours = if innerContours == [] then outerContours else last <$> allContours
      pointsOfContour (Contour points) = points
      pathWidth = nozzleDiameter extruder
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
    _perimeters      :: Fastℕ
  , infillAmount     :: ℝ -- as an amount from 0 (none) to 1 (full density).
  , layerHeight      :: ℝ
  , surfaceThickness :: ℝ
  , _withSupport     :: Bool
  , lineSpacing      :: ℝ -- In Millimeters.
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
      facets = centeredFacetsFromSTL buildarea  stl
--      rawLayers = (filter(\l -> head l /= head (tail l)).filter (not . null) $ layers print facets)
      allLayers :: [[Contour]]
--      allLayers = fmap Contour <$> rawLayers
      allLayers = layers print facets
      object = zip allLayers [(0::Fastℕ)..]
      (gcodes, _) = runState (sliceObject printer print object) (MachineState (EPos 0))
      gcodesAsText = [gcodeToText gcode | gcode <- gcodes] `using` parListChunk (div (length gcodes) (fromFastℕ threads)) rseq
      layerCount = length allLayers
      outFile = fromMaybe "out.gcode" $ outputFileOpt args
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

