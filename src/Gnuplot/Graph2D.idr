module Gnuplot.Graph2D

import Gnuplot.Schema.Expr
import Gnuplot.File
import Gnuplot.Options
import Gnuplot.Schema
import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Interface
--------------------------------------------------------------------------------

public export
interface IsGraph (0 g : FunctionType -> Schema -> Type) where
  command_ : String
  plot     : Maybe FilePath -> g t s -> String

export %inline
command : (0 g : FunctionType -> Schema -> Type) -> IsGraph g => String
command g = command_ {g}

--------------------------------------------------------------------------------
--          Graph Modifiers
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
--          Graph Types
--------------------------------------------------------------------------------

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

  ||| Draws a rectangle centered about the given x coordinate
  ||| that extends from the x axis to the given y coordinate.
  Boxes             : GraphType x y [x,y]

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

  ||| Places a string label at the given position
  Labels            : GraphType x y [x,y,GString]

  ||| Connects consecutive points in a plot with line
  ||| segments.
  Steps             : GraphType x y [x,y]

export
Interpolation (GraphType x y ts) where
  interpolate Lines              = "lines"
  interpolate Points             = "points"
  interpolate LinesPoints        = "linespoints"
  interpolate Impulses           = "impulses"
  interpolate Dots               = "dots"
  interpolate Boxes              = "boxes"
  interpolate (ErrorBars xs _)   = "\{xs}errorbars"
  interpolate (ErrorLines xs _)  = "\{xs}errorlines"
  interpolate Labels             = "labels"
  interpolate Steps              = "steps"

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
data Graph :  (x,y : Universe)
           -> (t   : FunctionType)
           -> (s   : Schema)
           -> Type where
  G   :  {types : List Universe}
      -> (type  : GraphType x y types)
      -> (mods  : Mods)
      -> (cols  : NP (Expr t s) (types ++ AddCols x y mods))
      -> (0 prf : NonEmpty s)
      => (line  : LineSettings)
      -> Graph x y t s

  Fun :  (type : GraphType x y [x,y])
      -> (fun  : Expr Regular [] y)
      -> (line : LineSettings)
      -> Graph x y Regular s

  Param :  (type : GraphType x y [x,y])
        -> (funX : Expr Parametric [] x)
        -> (funY : Expr Parametric [] y)
        -> (line : LineSettings)
        -> Graph x y Parametric s

splitNP : (ts : List t) -> NP f (ts ++ ss) -> (NP f ts, NP f ss)
splitNP []        vs        = ([],vs)
splitNP (t :: ts) (v :: vs) =
  let (xs,ys) = splitNP ts vs in (v :: xs, ys)

modCols : (mods : Mods) -> NP (Expr t s) (AddCols x y mods) -> List String
modCols []                 []        = []
modCols (TicLabels x :: xs) (e :: es) = case e of
  SCol n    => "\{x}ticlabels(\{n})" :: modCols xs es
  e         => "\{x}ticlabels(\{e})" :: modCols xs es

tpeCols : NP (Expr t s) us -> List String
tpeCols []             = []
tpeCols (ColNr  :: vs) = "0"      :: tpeCols vs
tpeCols (X      :: vs) = "x"      :: tpeCols vs
tpeCols (NCol x :: vs) = "\{x}"   :: tpeCols vs
tpeCols (e      :: vs) = "(\{e})" :: tpeCols vs

export
Interpolation (Graph x y t s) where
  interpolate (G {types} tpe mods cols ls) =
    let (cs,mcs) := splitNP types cols
        strs     := tpeCols cs ++ modCols mods mcs
        columns  := fastConcat $ intersperse ":" strs
     in "\{columns} with \{tpe} \{ls}"

  interpolate (Fun   tpe fun ls) = "\{fun} with \{tpe} \{ls}"
  interpolate (Param tpe fx fy ls) = "\{fx},\{fy} with \{tpe} \{ls}"

export
IsGraph (Graph x y) where
  command_  = "plot"

  plot Nothing  g = "\{g}"
  plot (Just z) g = let fp = quote "\{z}" in "\{fp} using \{g}"

--------------------------------------------------------------------------------
--          Titled Graphs
--------------------------------------------------------------------------------

public export
getX : GraphType x y us -> NP (Expr t s) (us ++ r) -> Expr t s x
getX Lines                    (h :: _) = h
getX Points                   (h :: _) = h
getX LinesPoints              (h :: _) = h
getX Impulses                 (h :: _) = h
getX Dots                     (h :: _) = h
getX Boxes                    (h :: _) = h
getX (ErrorBars X Relative)   (h :: _) = h
getX (ErrorBars X Absolute)   (h :: _) = h
getX (ErrorBars Y Relative)   (h :: _) = h
getX (ErrorBars Y Absolute)   (h :: _) = h
getX (ErrorBars XY Relative)  (h :: _) = h
getX (ErrorBars XY Absolute)  (h :: _) = h
getX (ErrorLines X Relative)  (h :: _) = h
getX (ErrorLines X Absolute)  (h :: _) = h
getX (ErrorLines Y Relative)  (h :: _) = h
getX (ErrorLines Y Absolute)  (h :: _) = h
getX (ErrorLines XY Relative) (h :: _) = h
getX (ErrorLines XY Absolute) (h :: _) = h
getX Labels                   (h :: _) = h
getX Steps                    (h :: _) = h

public export
getY : GraphType x y us -> NP (Expr t s) (us ++ r) -> Expr t s y
getY Lines                    (_ :: h :: _) = h
getY Points                   (_ :: h :: _) = h
getY LinesPoints              (_ :: h :: _) = h
getY Impulses                 (_ :: h :: _) = h
getY Dots                     (_ :: h :: _) = h
getY Boxes                    (_ :: h :: _) = h
getY (ErrorBars X Relative)   (_ :: h :: _) = h
getY (ErrorBars X Absolute)   (_ :: h :: _) = h
getY (ErrorBars Y Relative)   (_ :: h :: _) = h
getY (ErrorBars Y Absolute)   (_ :: h :: _) = h
getY (ErrorBars XY Relative)  (_ :: h :: _) = h
getY (ErrorBars XY Absolute)  (_ :: h :: _) = h
getY (ErrorLines X Relative)  (_ :: h :: _) = h
getY (ErrorLines X Absolute)  (_ :: h :: _) = h
getY (ErrorLines Y Relative)  (_ :: h :: _) = h
getY (ErrorLines Y Absolute)  (_ :: h :: _) = h
getY (ErrorLines XY Relative) (_ :: h :: _) = h
getY (ErrorLines XY Absolute) (_ :: h :: _) = h
getY Labels                   (_ :: h :: _) = h
getY Steps                    (_ :: h :: _) = h

public export
getTitle :  {s : _}
         -> GraphType x y us
         -> NP (Expr t s) (us ++ r)
         -> Maybe String
getTitle g cols = case getY g cols of
  NCol z => Just $ columnName z.prf
  _      => Nothing

public export
titled :  {s  : _}
       -> {us : _}
       -> (gt : GraphType x y us)
       -> (cols : NP (Expr t s) (us ++ []))
       -> (0 prf : IsJust (getTitle {r = []} gt cols))
       => (0 p2  : NonEmpty s)
       => Graph x y t s
titled gt cols =
  let ttl = fromJust $ getTitle {r = []} gt cols
   in G gt [] cols [ title $ fromString ttl ]
