module Gnuplot.Graph.Graph2D

import Gnuplot.Schema.Expr
import Gnuplot.File
import Gnuplot.Graph.Interface
import Gnuplot.Options
import Gnuplot.Schema
import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Graph Type
--------------------------------------------------------------------------------

public export
data GraphType : (x,y : Type) -> List Type -> Type where
  FunLines       : GraphType x y [y]
  FunPoints      : GraphType x y [y]
  FunLinesPoints : GraphType x y [y]
  FunImpulses    : GraphType x y [y]
  FunDots        : GraphType x y [y]
  Histograms     : GraphType x y [y]
  
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
  interpolate FunLines            = "lines"
  interpolate FunPoints           = "points"
  interpolate FunLinesPoints      = "linespoints"
  interpolate FunImpulses         = "impulses"
  interpolate FunDots             = "dots"
  interpolate Histograms          = "histograms"

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

gtAtoms : Atom x => Atom y => GraphType x y ts -> NP Atom ts
gtAtoms Histograms     = %search
gtAtoms Lines          = %search
gtAtoms Points         = %search
gtAtoms LinesPoints    = %search
gtAtoms Impulses       = %search
gtAtoms Dots           = %search
gtAtoms Steps          = %search
gtAtoms FSteps         = %search
gtAtoms HiSteps        = %search
gtAtoms ErrorBars      = %search
gtAtoms ErrorLines     = %search
gtAtoms Boxes          = %search
gtAtoms FilledCurves   = %search
gtAtoms FunLines       = %search
gtAtoms FunPoints      = %search
gtAtoms FunLinesPoints = %search
gtAtoms FunImpulses    = %search
gtAtoms FunDots        = %search

--------------------------------------------------------------------------------
--          Graph
--------------------------------------------------------------------------------

public export
record Graph (x,y : Type) (s : Schema) where
  constructor G
  {0 types : List Type}
  type : GraphType x y types
  cols : Selection s types
  line : LineSettings

export
Atom x => Atom y => Interpolation (Graph x y s) where
  interpolate (G t cols ls) = 
    let as = gtAtoms t
     in "\{cols} with \{t} \{ls}"

export
Atom x => Atom y => IsGraph (Graph x y) where
  command_      = "plot"
  toString (Just fp) g@(G t [x] ls) = case hasVar x of
    True  => "\"\{fp}\" using \{g}"
    False => "\{g}"
  toString (Just fp) g              = "\"\{fp}\" using \{g}"
  toString Nothing g                = "\{g}"

export
deflt : GraphType x y ts -> Selection s ts -> Graph x y s
deflt t c = G t c []
