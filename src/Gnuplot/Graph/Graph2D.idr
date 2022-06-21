module Gnuplot.Graph.Graph2D

import Gnuplot.Options
import Gnuplot.File
import Gnuplot.Graph.Interface
import Gnuplot.LineSpec
import Gnuplot.Schema
import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Graph Type
--------------------------------------------------------------------------------

public export
data GraphType : (x,y : Type) -> List Type -> Type where
--  ListLines       : GraphType Nat y [y]
--  ListPoints      : GraphType Nat y [y]
--  ListLinesPoints : GraphType Nat y [y]
--  ListImpulses    : GraphType Nat y [y]
--  ListDots        : GraphType Nat y [y]
  Histograms      : GraphType Nat y [y]
  
--  XErrorBarsRelative   : GraphType x y [x,y,x]
--  YErrorBarsRelative   : GraphType x y [x,y,y]
--  XyErrorBarsRelative  : GraphType x y [x,y,x,y]
--  XErrorBarsAbsolute   : GraphType x y [x,y,x,x]
--  YErrorBarsAbsolute   : GraphType x y [x,y,y,y]
--  XyErrorBarsAbsolute  : GraphType x y [x,y,x,x,y,y]
 
--  XErrorLinesRelative  : GraphType x y [x,y,x]
--  YErrorLinesRelative  : GraphType x y [x,y,y]
--  XyErrorLinesRelative : GraphType x y [x,y,x,y]
--  XErrorLinesAbsolute  : GraphType x y [x,y,x,x]
--  YErrorLinesAbsolute  : GraphType x y [x,y,y,y]
--  XyErrorLinesAbsolute : GraphType x y [x,y,x,x,y,y]
  
  Lines          : GraphType x y [x,y]
  Points         : GraphType x y [x,y]
  LinesPoints    : GraphType x y [x,y]
  Impulses       : GraphType x y [x,y]
  Dots           : GraphType x y [x,y]
  Steps          : GraphType x y [x,y]
  FSteps         : GraphType x y [x,y]
  HiSteps        : GraphType x y [x,y]
  ErrorBars      : GraphType x y [x,y]
  ErrorLines     : GraphType x y [x,y]
  Boxes          : GraphType x y [x,y]
  FilledCurves   : GraphType x y [x,y]

--  FinanceBars    : GraphType x y [x,y,y,y,y]
--  CandleSticks   : GraphType x y [x,y,y,y,y]
--  Vectors        : GraphType x y [x,y,x,y]
--  Image          : (0 z : Type) -> GraphType x y [x,y,z]
  
--  FilledStripe      : GraphType x y [x,y,y]
--  FilledStripeAbove : GraphType x y [x,y,y]
--  FilledStripeBelow : GraphType x y [x,y,y]

export
Interpolation (GraphType x y ts) where
--  interpolate ListLines            = "lines"
--  interpolate ListPoints           = "points"
--  interpolate ListLinesPoints      = "linespoints"
--  interpolate ListImpulses         = "impulses"
--  interpolate ListDots             = "dots"
  interpolate Histograms           = "histograms"

--  interpolate XErrorBarsRelative   = "xerrorbars"
--  interpolate YErrorBarsRelative   = "yerrorbars"
--  interpolate XyErrorBarsRelative  = "xyerrorbars"
--  interpolate XErrorBarsAbsolute   = "xerrorbars"
--  interpolate YErrorBarsAbsolute   = "yerrorbars"
--  interpolate XyErrorBarsAbsolute  = "xyerrorbars"
--
--  interpolate XErrorLinesRelative  = "xerrorlines"
--  interpolate YErrorLinesRelative  = "yerrorlines"
--  interpolate XyErrorLinesRelative = "xyerrorlines"
--  interpolate XErrorLinesAbsolute  = "xerrorlines"
--  interpolate YErrorLinesAbsolute  = "yerrorlines"
--  interpolate XyErrorLinesAbsolute = "xyerrorlines"

  interpolate Lines                = "lines"
  interpolate Points               = "points"
  interpolate LinesPoints          = "linespoints"
  interpolate Impulses             = "impulses"
  interpolate Dots                 = "dots"
  interpolate Steps                = "steps"
  interpolate FSteps               = "fsteps"
  interpolate HiSteps              = "histeps"
  interpolate ErrorBars            = "errorbars"
  interpolate ErrorLines           = "errorlines"
  interpolate Boxes                = "boxes"
  interpolate FilledCurves         = "filledcurves"
--  interpolate FinanceBars          = "financebars"
--  interpolate CandleSticks         = "candlesticks"
--  interpolate Vectors              = "vectors"
--  interpolate (Image z)            = "image"
--  interpolate FilledStripe         = "filledcurves"
--  interpolate FilledStripeAbove    = "filledcurves above"
--  interpolate FilledStripeBelow    = "filledcurves below"

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
record Graph (x,y : Type) (s : Schema) where
  constructor G
  {0 types : List Type}
  type : GraphType x y types
  cols : Selection s types
  line : List (Line -> Line)

export
Interpolation (Graph x y s) where
  interpolate (G t cols ls) = "using \{cols} with \{t} \{setAll ls}"

export
Atom x => Atom y => IsGraph (Graph x y) where
  command_      = "plot"
  toString      = interpolate

export
deflt : GraphType x y ts -> Selection s ts -> Graph x y s
deflt t c = G t c []

title : {s : _} -> GraphType x y ts -> Selection s ts -> String
title Histograms [x]     = colName x
title Lines [_,y]        = colName y
title Points [_,y]       = colName y
title LinesPoints [_,y]  = colName y
title Impulses [_,y]     = colName y
title Dots [_,y]         = colName y
title Steps [_,y]        = colName y
title FSteps [_,y]       = colName y
title HiSteps [_,y]      = colName y
title ErrorBars [_,y]    = colName y
title ErrorLines [_,y]   = colName y
title Boxes [_,y]        = colName y
title FilledCurves [_,y] = colName y

||| A titled curve, where the title is taken from
||| the corresponding column of the schema.
export
titled : {s : _} -> GraphType x y ts -> Selection s ts -> Graph x y s
titled t c = G t c [Title .= title t c]
