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

{- | No reason to get excited. just some dumping facilities.

 to use:

 @
 juri@ni:/disk4/faikvm.com/HSlice/HSlice-current$ cabal repl
 Ok, 30 modules loaded.
 *Graphics.Slicer> import Prelude (($),putStrLn)
 *Graphics.Slicer Prelude> import Graphics.Slicer.Math.Ganja(dumpGanja)
 *Graphics.Slicer Prelude Graphics.Slicer.Math.Ganja> putStrLn $ dumpGanja (Point2 (1,1))
 Algebra(2,0,1,()=>{
   var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
   var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
   var pa = point(1.0,1.0);
   document.body.appendChild(this.graph([
     pa, "pa",
   ],{
     grid: true,
     labels: true,
     lineWidth: 3,
     pointRadius: 1,
     fontSize: 1,
     scale: 1,
   }));
 });
 @

 or:

 @
 juri@ni:/disk4/faikvm.com/HSlice/HSlice-current$ cabal repl
 Ok, 30 modules loaded.
 *Graphics.Slicer> import Prelude (($),putStrLn)
 *Graphics.Slicer Prelude> import Graphics.Slicer.Math.Ganja(dumpGanja)
 *Graphics.Slicer Prelude Graphics.Slicer.Math.Ganja> putStrLn $ dumpGanjas [toGanja (Point2 (1,1)), toGanja (Point2 (2,2))]
 Algebra(2,0,1,()=>{
   var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
   var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
   var a = point(1.0,1.0);
   var b = point(2.0,2.0);
   document.body.appendChild(this.graph([
     a, "a",
     b, "b",
   ],{
     grid: true,
     labels: true,
     lineWidth: 3,
     pointRadius: 1,
     fontSize: 1,
     scale: 1,
 }));
 });
 @

 cut, and paste the output into https://enkimute.github.io/ganja.js/examples/coffeeshop.html, click 'Run'
 -}

-- loosen some instance restrictions, so we can work with String and others.
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Slicer.Math.Ganja (
  GanjaAble,
  dumpGanja,
  dumpGanjas,
  toGanja
  ) where

import Prelude (String, (<>), (<>), (<$>), ($), (>=), (==), (.), (/=), concat, error, fst, otherwise, show, snd, zip)

import Data.List (concatMap)

import Data.Maybe (Maybe(Nothing), catMaybes)

import Numeric (showFFloat)

import Slist.Type (Slist(Slist))

import Slist (last, len)

import Graphics.Slicer.Math.Definitions (Contour, Point2(Point2), LineSeg, endPoint, mapWithFollower, pointsOfContour, startPoint)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEPlus, GEZero), GVec(GVec), getVal, valOf)

import Graphics.Slicer.Math.PGA (PLine2Err, PPoint2Err, ProjectivePoint, ProjectiveLine, hasArc, normalizeL, outAndErrOf, outOf, vecOfL, vecOfP)

import Graphics.Slicer.Math.Skeleton.Definitions(Cell(Cell), ENode, ENodeSet(ENodeSet), INode(INode), INodeSet(INodeSet), Motorcycle(Motorcycle), NodeTree(NodeTree), StraightSkeleton(StraightSkeleton), RemainingContour(RemainingContour), CellDivide(CellDivide), DividingMotorcycles(DividingMotorcycles), getFirstLineSeg, getLastLineSeg)

import Graphics.Slicer.Math.Skeleton.Face(Face(Face))

class GanjaAble a where
  toGanja :: a -> String -> (String, String)

instance GanjaAble String where
  toGanja string varname = (" // " <> varname <> " -- " <> string <> "\n","")

instance GanjaAble Point2 where
  toGanja (Point2 (x,y)) varname = (
    "  var " <> varname <> " = point(" <> showFullPrecision x <> "," <> showFullPrecision y <> ");\n",
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""

instance GanjaAble LineSeg where
  toGanja lineSeg varname = (
       p1var
    <> p2var,
       "    0x882288,\n"
    <> "    [" <> varname <> "a," <> varname <> "b],\n"
    <> "    0x00AA88,\n"
    <> p1ref
    <> p2ref)
    where
      (p1var, p1ref) = toGanja (startPoint lineSeg) (varname <> "a")
      (p2var, p2ref) = toGanja (endPoint lineSeg) (varname <> "b")

instance GanjaAble ProjectivePoint where
  toGanja point varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVal [GEPlus 1, GEPlus 2] vals)) <> "e12"
      <> (if e02 >= 0 then "+" <> showFullPrecision e02 else showFullPrecision e02)
      <> "e02"
      <> (if e01 >= 0 then "+" <> showFullPrecision e01 else showFullPrecision e01)
      <> "e01;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e02 = valOf 0 (getVal [GEZero 1, GEPlus 2] vals)
      e01 = valOf 0 (getVal [GEZero 1, GEPlus 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""
      (GVec vals) = vecOfP point

instance GanjaAble (ProjectivePoint, PPoint2Err) where
  toGanja (point, _) varname = toGanja point varname

instance GanjaAble ProjectiveLine where
  toGanja line varname = (
    "  var " <> varname <> " = "
      <> showFullPrecision (valOf 0 (getVal [GEPlus 1] vals)) <> "e1"
      <> (if e2 >= 0 then "+" <> showFullPrecision e2 else showFullPrecision e2)
      <> "e2"
      <> (if e0 >= 0 then "+" <> showFullPrecision e0 else showFullPrecision e0)
      <> "e0;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e2 = valOf 0 (getVal [GEPlus 2] vals)
      e0 = valOf 0 (getVal [GEZero 1] vals)
      -- because ganja's website does not handle scientific notation.
      showFullPrecision v = showFFloat Nothing v ""
      (GVec vals) = vecOfL $ fst $ normalizeL line

instance GanjaAble (ProjectiveLine, PLine2Err) where
  toGanja (line, _) varname = toGanja line varname

instance GanjaAble Contour where
  toGanja contour varname = (invars, inrefs)
    where
      contourPoints = pointsOfContour contour
      (invars, inrefs) = (concatMap fst res, concatMap snd res <> "    0x882288,\n" <> linePairs)
        where
          linePairs    = concat $ mapWithFollower (\(_,a) (_,b) -> "    [" <> varname <> a <> "," <> varname <> b <> "],\n") pairs
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> pairs
          pairs        = zip contourPoints allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]

instance GanjaAble ENode where
  toGanja eNode varname = (
    l1var
    <> l2var
    <> plvar
    ,
    l1ref
    <> l2ref
    <> plref
    )
    where
      (l1var, l1ref) = toGanja (getFirstLineSeg eNode) (varname <> "a")
      (l2var, l2ref) = toGanja (getLastLineSeg eNode) (varname <> "b")
      (plvar, plref) = toGanja (outOf eNode) (varname <> "c")

instance GanjaAble Motorcycle where
  toGanja (Motorcycle (l1, l2) outPLine _) varname = (
    l1var
    <> l2var
    <> plvar
    ,
    l1ref
    <> l2ref
    <> plref
    )
    where
      (l1var, l1ref) = toGanja l1 (varname <> "a")
      (l2var, l2ref) = toGanja l2 (varname <> "b")
      (plvar, plref) = toGanja outPLine (varname <> "c")

instance GanjaAble Cell where
  toGanja (Cell segsDivides) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concatMap (listFromSlist . fst) segsDivides
          allDivides   = catMaybes $ listFromSlist $ snd <$> segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble CellDivide where
  toGanja (CellDivide (DividingMotorcycles firstMotorcycle (Slist moreMotorcycles _)) _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res            = (\(a,b) -> toGanja a (varname <> b)) <$> zip allMotorcycles allStrings
          allStrings     = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allMotorcycles = firstMotorcycle:moreMotorcycles

instance GanjaAble RemainingContour where
  toGanja (RemainingContour (Slist segsDivides _)) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip allSides allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allSegs      = concatMap (listFromSlist . fst) segsDivides
          allDivides   = concatMap snd segsDivides
          allSides     = (toGanja <$> allSegs) <> (toGanja <$> allDivides)
          listFromSlist (Slist a _) = a

instance GanjaAble INode where
  toGanja iNode@(INode firstPLine secondPLine (Slist rawMorePLines _) _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> zip allPLines allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = firstPLine:secondPLine:rawMorePLines <> (if hasArc iNode then [outAndErrOf iNode] else [])

instance GanjaAble StraightSkeleton where
  toGanja (StraightSkeleton (Slist [[nodetree]] _) _) = toGanja nodetree
  toGanja a = error $ "no a, only b: " <> show a <> "\n"

instance GanjaAble NodeTree where
  toGanja (NodeTree (ENodeSet eNodeSides) iNodeSet) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (allEdges <> allINodes) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allEdges     = toGanja <$> (firstLine <> remainingLines)
          allINodes    = toGanja <$> iNodesOf iNodeSet
          firstLine    = case eNodeSides of
                           (Slist [] _) -> []
                           (Slist [(firstNode,Slist [] _)] _) -> [getFirstLineSeg firstNode]
                           (Slist [(firstNode,otherNodes)] _) -> if getFirstLineSeg firstNode /= getLastLineSeg (last otherNodes)
                                                                 then [getFirstLineSeg firstNode]
                                                                 else []
                           (Slist _ _) -> error "too many sides."
          remainingLines
            | len eNodeSides == 0 = []
            | otherwise = getLastLineSeg <$> eNodesOf eNodeSides
            where
              eNodesOf (Slist [] _) = error "no enodes?"
              eNodesOf (Slist [(first,Slist more _)] _) = first : more
              eNodesOf (Slist _ _) = error "too many sides?"
          iNodesOf :: INodeSet -> [INode]
          iNodesOf (INodeSet (Slist inodes _)) = concat inodes

instance GanjaAble Face where
  toGanja (Face edge firstArc (Slist arcs _) lastArc) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (toGanja edge : allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = toGanja <$> ([firstArc] <> arcs <> [lastArc])

instance GanjaAble (Slist Face) where
  toGanja (Slist faces _) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concatMap fst res, concatMap snd res)
        where
          allArcs      = concatMap (\(Face _ firstArc (Slist arcs _) lastArc) -> [firstArc] <> arcs <> [lastArc]) faces
          allEdges     = (\(Face edge _ _ _) -> toGanja edge) <$> faces
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (allEdges <> allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]
          allPLines    = toGanja <$> allArcs

-- | Create a single program, covering a series of objects.
dumpGanjas :: [String -> (String, String)] -> String
dumpGanjas [] = error "no items to dump."
dumpGanjas [f] = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = f "a"
dumpGanjas xs = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) =  (concatMap fst res, concatMap snd res)
      where
        res          = (\(a,b) -> a b) <$> zip xs allStrings
        allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] <> ['0'..'9'] ]

-- | create a single program for a single object.
dumpGanja :: (GanjaAble a) => a -> String
dumpGanja target = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = toGanja target "a"

-- | the headaer at the beginning of all programs.
ganjaHeader :: String
ganjaHeader = "Algebra(2,0,1,()=>{\n  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;\n  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);\n"

-- | the beginning of the footer section.
ganjaFooterStart :: String
ganjaFooterStart = "  document.body.appendChild(this.graph([\n"

-- | the end of the footer.
ganjaFooterEnd :: String
ganjaFooterEnd = "  ],{\n"
                 <> "    grid: true,\n"
                 <> "    labels: true,\n"
                 <> "    lineWidth: 3,\n"
                 <> "    pointRadius: 1,\n"
                 <> "    fontSize: 1,\n"
                 <> "    scale: 1,\n"
                 <> "}));\n"
                 <> "});\n"

