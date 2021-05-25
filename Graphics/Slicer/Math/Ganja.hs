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

{- No reason to get excited. just some dumping facilities.
 - to use:
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

 - cut and paste, drop into https://enkimute.github.io/ganja.js/examples/coffeeshop.html, click 'Run'
 -}


module Graphics.Slicer.Math.Ganja (toGanja, dumpGanja, dumpGanjas) where

import Prelude (String, (<>), (++), (<$>), ($), (>=), concat, error, fst, show, snd, zip)

import Data.Maybe(isJust, fromJust)

import Graphics.Slicer.Math.Definitions (Point2(Point2), Contour(PointSequence), mapWithFollower)

import Graphics.Slicer.Math.GeometricAlgebra (GNum(GEPlus, GEZero), GVec(GVec), getVals, valOf)

import Graphics.Slicer.Math.Line (LineSeg(LineSeg), endpoint)

import Graphics.Slicer.Math.PGA (PPoint2(PPoint2), PLine2(PLine2))

import Graphics.Slicer.Math.Skeleton.Definitions(ENode(ENode), INode(INode), Motorcycle(Motorcycle))

import Graphics.Slicer.Math.Skeleton.Face(Face(Face))

class GanjaAble a where
  toGanja :: a -> String -> (String, String)

instance GanjaAble Point2 where
  toGanja (Point2 (x,y)) varname = (
    "  var " <> varname <> " = point(" <> show x <> "," <> show y <> ");\n",
    "    " <> varname <> ", " <> show varname <> ",\n")

instance GanjaAble LineSeg where
  toGanja l1@(LineSeg p1 _) varname = (
       p1var
    <> p2var,
       "    0x882288,\n"
    <> "    [" <> varname <> "a," <> varname <> "b],\n"
    <> "    0x00AA88,\n"
    <> p1ref
    <> p2ref)
    where
      (p1var, p1ref) = toGanja p1 (varname <> "a")
      (p2var, p2ref) = toGanja (endpoint l1) (varname <> "b")

instance GanjaAble PPoint2 where
  toGanja (PPoint2 (GVec vals)) varname = (
    "  var " <> varname <> " = "
      <> show (valOf 0 (getVals [GEPlus 1, GEPlus 2] vals)) <> "e12"
      <> (if e02 >= 0 then "+" <> show e02 else show e02)
      <> "e02"
      <> (if e01 >= 0 then "+" <> show e01 else show e01)
      <> "e01;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e02 = valOf 0 (getVals [GEZero 1, GEPlus 2] vals)
      e01 = valOf 0 (getVals [GEZero 1, GEPlus 1] vals)

instance GanjaAble PLine2 where
  toGanja (PLine2 (GVec vals)) varname = (
    "  var " <> varname <> " = "
      <> show (valOf 0 (getVals [GEPlus 1] vals)) <> "e1"
      <> (if e2 >= 0 then "+" <> show e2 else show e2)
      <> "e2"
      <> (if e0 >= 0 then "+" <> show e0 else show e0)
      <> "e0;\n"
    ,
    "    " <> varname <> ", " <> show varname <> ",\n")
    where
      e2 = valOf 0 (getVals [GEPlus 2] vals)
      e0 = valOf 0 (getVals [GEZero 1] vals)

instance GanjaAble ENode where
  toGanja (ENode (l1, l2) outPLine) varname = (
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

instance GanjaAble Motorcycle where
  toGanja (Motorcycle (l1, l2) outPLine) varname = (
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

instance GanjaAble INode where
  toGanja (INode inPLines outPLine) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> zip allPLines allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]
          allPLines    = inPLines ++ (if isJust outPLine then [fromJust outPLine] else [])

instance GanjaAble Contour where
  toGanja (PointSequence contourPoints) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat (snd <$> res) <> "    0x882288,\n" <> linePairs)
        where
          linePairs    = concat $ mapWithFollower (\(_,a) (_,b) -> "    [" <> varname <> a <> "," <> varname <> b <> "],\n") pairs
          res          = (\(a,b) -> toGanja a (varname <> b)) <$> pairs
          pairs        = zip contourPoints allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

instance GanjaAble Face where
  toGanja (Face edge firstArc arcs lastArc) varname = (invars, inrefs)
    where
      (invars, inrefs) = (concat $ fst <$> res, concat $ snd <$> res)
        where
          res          = (\(a,b) -> a (varname <> b)) <$> pairs
          pairs        = zip (toGanja edge : allPLines) allStrings
          allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]
          allPLines    = toGanja <$> ([firstArc] ++ arcs ++ [lastArc])

dumpGanjas :: [String -> (String, String)] -> String
dumpGanjas [] = error "no items to dump."
dumpGanjas [f] = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = f "a"
dumpGanjas xs = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) =  (concat $ fst <$> res, concat $ snd <$> res)
      where
        res          = (\(a,b) -> a b) <$> zip xs allStrings
        allStrings   = [ c : s | s <- "": allStrings, c <- ['a'..'z'] ++ ['0'..'9'] ]

dumpGanja :: (GanjaAble a) => a -> String
dumpGanja target = ganjaHeader <> vars <> ganjaFooterStart <> refs <> ganjaFooterEnd
  where
    (vars, refs) = toGanja target "a"

ganjaHeader :: String
ganjaHeader = "Algebra(2,0,1,()=>{\n  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;\n  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);\n"
ganjaFooterStart :: String
ganjaFooterStart = "  document.body.appendChild(this.graph([\n"
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
  
