module Gnuplot.Graph.Graph2D

import Gnuplot.Frame.Option
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
  ListLines       : GraphType Nat y [y]
  ListPoints      : GraphType Nat y [y]
  ListLinesPoints : GraphType Nat y [y]
  ListImpulses    : GraphType Nat y [y]
  ListDots        : GraphType Nat y [y]
  Histograms      : GraphType Nat y [y]
  
  XErrorBarsRelative   : GraphType x y [x,y,x]
  YErrorBarsRelative   : GraphType x y [x,y,y]
  XyErrorBarsRelative  : GraphType x y [x,y,x,y]
  XErrorBarsAbsolute   : GraphType x y [x,y,x,x]
  YErrorBarsAbsolute   : GraphType x y [x,y,y,y]
  XyErrorBarsAbsolute  : GraphType x y [x,y,x,x,y,y]
 
  XErrorLinesRelative  : GraphType x y [x,y,x]
  YErrorLinesRelative  : GraphType x y [x,y,y]
  XyErrorLinesRelative : GraphType x y [x,y,x,y]
  XErrorLinesAbsolute  : GraphType x y [x,y,x,x]
  YErrorLinesAbsolute  : GraphType x y [x,y,y,y]
  XyErrorLinesAbsolute : GraphType x y [x,y,x,x,y,y]
  
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

  FinanceBars    : GraphType x y [x,y,y,y,y]
  CandleSticks   : GraphType x y [x,y,y,y,y]
  Vectors        : GraphType x y [x,y,x,y]
  Image          : (0 z : Type) -> GraphType x y [x,y,z]
  
  FilledStripe      : GraphType x y [x,y,y]
  FilledStripeAbove : GraphType x y [x,y,y]
  FilledStripeBelow : GraphType x y [x,y,y]

export
Interpolation (GraphType x y ts) where
  interpolate ListLines            = "lines"
  interpolate ListPoints           = "points"
  interpolate ListLinesPoints      = "linespoints"
  interpolate ListImpulses         = "impulses"
  interpolate ListDots             = "dots"
  interpolate Histograms           = "histograms"

  interpolate XErrorBarsRelative   = "xerrorbars"
  interpolate YErrorBarsRelative   = "yerrorbars"
  interpolate XyErrorBarsRelative  = "xyerrorbars"
  interpolate XErrorBarsAbsolute   = "xerrorbars"
  interpolate YErrorBarsAbsolute   = "yerrorbars"
  interpolate XyErrorBarsAbsolute  = "xyerrorbars"

  interpolate XErrorLinesRelative  = "xerrorlines"
  interpolate YErrorLinesRelative  = "yerrorlines"
  interpolate XyErrorLinesRelative = "xyerrorlines"
  interpolate XErrorLinesAbsolute  = "xerrorlines"
  interpolate YErrorLinesAbsolute  = "yerrorlines"
  interpolate XyErrorLinesAbsolute = "xyerrorlines"

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
  interpolate FinanceBars          = "financebars"
  interpolate CandleSticks         = "candlesticks"
  interpolate Vectors              = "vectors"
  interpolate (Image z)            = "image"
  interpolate FilledStripe         = "filledcurves"
  interpolate FilledStripeAbove    = "filledcurves above"
  interpolate FilledStripeBelow    = "filledcurves below"

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
record Graph (x,y : Type) (s : Schema) where
  constructor MkGraph
  {0 types : List Type}
  type : GraphType x y types
  cols : Selection s types
  line : Line

export
defltOptions : (0 x,y : Type) -> Atom x => Atom y => Opts
defltOptions x y =
  let mk : Option -> Option -> Options a -> List (Option, List String)
      mk odata oformat os =
       (odata, optData os) :: (oformat, format os) :: others os

      result : Options x -> Options y -> Opts
      result optX optY =
         flip merge deflt $
         fromList $
           mk xData xFormat optX ++
           mk yData yFormat optY ++
           [(zData, []), (zFormat, [])]
  in result (options x) (options y)

export
Interpolation (Graph x y s) where
  interpolate (MkGraph t cols l) = "using \{cols} with \{t} \{l}"

export
Atom x => Atom y => IsGraph (Graph x y) where
  command_      = "plot"
  defltOptions_ = Graph2D.defltOptions x y
  toString      = interpolate

export
deflt : GraphType x y ts -> Selection s ts -> Graph x y s
deflt t c = MkGraph t c deflt

export
line : Line -> Graph x y s -> Graph x y s
line l = {line := l}
