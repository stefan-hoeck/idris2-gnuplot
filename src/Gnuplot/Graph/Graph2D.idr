module Gnuplot.Graph.Graph2D

import Gnuplot.File
import Gnuplot.Value

%default total

--------------------------------------------------------------------------------
--          Graph Type
--------------------------------------------------------------------------------

public export
data GraphType : (x,y : GTpe) -> List GTpe -> Type where
  ListLines       : GraphType GNat y [y]
  ListPoints      : GraphType GNat y [y]
  ListLinesPoints : GraphType GNat y [y]
  ListImpulses    : GraphType GNat y [y]
  ListDots        : GraphType GNat y [y]
  Histograms      : GraphType GNat y [y]
  
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
  Image          : (0 z : GTpe) -> GraphType x y [x,y,z]
  
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
--          Graph Type
--------------------------------------------------------------------------------

public export
data LineSpec : Type where

export
Interpolation LineSpec where
  interpolate x impossible

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
record Graph (x,y : GTpe) where
  constructor MkGraph
  cols : (Nat,Nat)
  type : String
  line : LineSpec

export
toString : Graph x y -> String
toString (MkGraph (c1,c2) t l) =
  "using \{show c1}:\{show c2} with \{t} \{l}"

-- type AxisOption x y a =
--    OptionSet.T (T x y) -> Atom.OptionSet a
-- 
-- defltOptions :: (Atom.C x, Atom.C y) => OptionSet.T (T x y)
-- defltOptions =
--    let mk ::
--           Option.T -> Option.T ->
--           Atom.OptionSet a -> [(Option.T, [String])]
--        mk optData optFormat opts =
--           (optData, Atom.optData opts) :
--           (optFormat, Atom.optFormat opts) :
--           Atom.optOthers opts
--        result ::
--           Atom.OptionSet x ->
--           Atom.OptionSet y ->
--           OptionSet.T (T x y)
--        result optX optY =
--           OptionSet.Cons $
--           flip Map.union OptionSet.deflt $
--           Map.fromList $
--           mk Option.xData Option.xFormat optX ++
--           mk Option.yData Option.yFormat optY ++
--           (Option.zData, []) :
--           (Option.zFormat, []) :
--           []
--    in  result Atom.options Atom.options
-- 
-- 
-- instance (Atom.C x, Atom.C y) => Graph.C (T x y) where
--    command = Graph.Command "plot"
--    toString = toString
--    defltOptions = defltOptions
-- 
-- 
-- deflt :: GraphType.T x y a -> Columns -> T x y
-- deflt t c = Cons c (GraphType.toString t) LineSpec.deflt
-- 
-- typ :: Type -> T x y -> T x y
-- typ t gr = gr{type_ = t}
-- 
-- {- |
-- You can alter the line specification of graphs in a plot using 'fmap'.
-- -}
-- lineSpec :: LineSpec.T -> T x y -> T x y
-- lineSpec ls gr = gr{lineSpec_ = ls}
