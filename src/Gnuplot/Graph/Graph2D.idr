module Gnuplot.Graph.Graph2D

import Gnuplot.Schema.Expr
import Gnuplot.File
import Gnuplot.Graph.Interface
import Gnuplot.Options
import Gnuplot.Schema
import Gnuplot.Util

%default total

public export
data Mod : Type where
  TicLabels : Axis -> Mod

public export
0 Mods : Type
Mods = List Mod

public export
AddCol : (x,y : Universe) -> Mod -> List Universe
AddCol x y (TicLabels _) = [GString]

public export
AddCols : (x,y : Universe) -> Mods -> List Universe
AddCols x y []        = []
AddCols x y (m :: ms) = AddCol x y m ++ AddCols x y ms

public export
data Axes2D = X | Y | XY

export
Interpolation Axes2D where
  interpolate X  = "x"
  interpolate Y  = "y"
  interpolate XY = "xy"

public export
data ErrorType = Relative | Absolute

public export
ErrorBarColumns : (x,y : Universe)
                -> Axes2D
                -> ErrorType
                -> List Universe
ErrorBarColumns x y X  Relative = [x,y,x]
ErrorBarColumns x y X  Absolute = [x,y,x,x]
ErrorBarColumns x y Y  Relative = [x,y,y]
ErrorBarColumns x y Y  Absolute = [x,y,y,y]
ErrorBarColumns x y XY Relative = [x,y,x,y]
ErrorBarColumns x y XY Absolute = [x,y,x,x,y,y]

public export
data GraphType : (x,y : Universe) -> List Universe -> Type where
  ||| A line connecting the points in a 2D graph
  Lines             : GraphType x y [x,y]

  ||| Each x,y data point is displayed using a small icon
  Points            : GraphType x y [x,y]

  ||| Combination of `Lines` and `Points`
  LinesPoints       : GraphType x y [x,y]

  ||| Vertical line fron [x,0] to [x,y]
  Impulses          : GraphType x y [x,y]

  ||| Prints a tiny dot at each point. Useful for scatterplots with
  ||| many data points.
  Dots              : GraphType x y [x,y]

  ||| Prints a vertical or horizontal error bar (or both).
  ||| If the `ErrorType` is `Relative`, only one additional
  ||| column per axis with the error delta is required.
  ||| If the `ErrorType` is `Absolute`, two additional columns
  ||| are required (as returned by `ErrorBarColumns`)
  ErrorBars         :  (xs : Axes2D)
                    -> (bs : ErrorType)
                    -> GraphType x y (ErrorBarColumns x y xs bs)

  ||| Combines `ErrorBars` and `Lines`
  ErrorLines        :  (xs : Axes2D)
                    -> (bs : ErrorType)
                    -> GraphType x y (ErrorBarColumns x y xs bs)

export
Interpolation (GraphType x y ts) where
  interpolate Lines              = "lines"
  interpolate Points             = "points"
  interpolate LinesPoints        = "linespoints"
  interpolate Impulses           = "impulses"
  interpolate Dots               = "dots"
  interpolate (ErrorBars xs _)   = "\{xs}errorbars"
  interpolate (ErrorLines xs _)  = "\{xs}errorlines"

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
record Graph (x,y : Universe) (s : Schema) where
  constructor G
  {types : List Universe}
  type : GraphType x y types
  mods : Mods
  cols : NP (Expr s) (types ++ AddCols x y mods) 
  line : LineSettings

splitNP : (ts : List t) -> NP f (ts ++ ss) -> (NP f ts, NP f ss)
splitNP []        vs        = ([],vs)
splitNP (t :: ts) (v :: vs) =
  let (xs,ys) = splitNP ts vs in (v :: xs, ys)

modCols : (mods : Mods) -> NP (Expr s) (AddCols x y mods) -> List String
modCols []                 []        = []
modCols (TicLabels x :: xs) (e :: es) = case e of
  SCol n    => "\{x}ticlabels(\{n})" :: modCols xs es
  e         => "\{x}ticlabels(\{e})" :: modCols xs es

tpeCols : NP (Expr s) us -> List String
tpeCols []             = []
tpeCols (ColNr  :: vs) = "0"    :: tpeCols vs
tpeCols (NCol x :: vs) = "\{x}" :: tpeCols vs
tpeCols (e      :: vs) = "\{e}" :: tpeCols vs

export
Interpolation (Graph x y s) where
  interpolate (G {types} tpe mods cols line) =
    let (cs,mcs) := splitNP types cols
        strs     := tpeCols cs ++ modCols mods mcs
     in ?foo


-- export
-- Atom x => Atom y => Interpolation (Graph x y s) where
--   interpolate (G t cols ls) = 
--     let as = gtAtoms t
--      in "\{cols} with \{t} \{ls}"
-- 
-- export
-- Atom x => Atom y => IsGraph (Graph x y) where
--   command_      = "plot"
--   toString (Just fp) g@(G t [x] ls) = case hasVar x of
--     True  => "\"\{fp}\" using \{g}"
--     False => "\{g}"
--   toString (Just fp) g              = "\"\{fp}\" using \{g}"
--   toString Nothing g                = "\{g}"
-- 
-- export
-- deflt : GraphType x y ts -> Selection s ts -> Graph x y s
-- deflt t c = G t c []
