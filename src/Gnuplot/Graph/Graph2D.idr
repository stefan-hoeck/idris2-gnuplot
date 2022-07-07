module Gnuplot.Graph.Graph2D

import Gnuplot.Schema.Expr
import Gnuplot.File
import Gnuplot.Graph.Interface
import Gnuplot.LineSpec
import Gnuplot.Options
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

gtAtoms : Atom x => Atom y => GraphType x y ts -> NP Atom ts
gtAtoms Histograms   = %search
gtAtoms Lines        = %search
gtAtoms Points       = %search
gtAtoms LinesPoints  = %search
gtAtoms Impulses     = %search
gtAtoms Dots         = %search
gtAtoms Steps        = %search
gtAtoms FSteps       = %search
gtAtoms HiSteps      = %search
gtAtoms ErrorBars    = %search
gtAtoms ErrorLines   = %search
gtAtoms Boxes        = %search
gtAtoms FilledCurves = %search

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
Atom x => Atom y => Interpolation (Graph x y s) where
  interpolate (G t cols ls) = 
    let as = gtAtoms t
     in "using \{cols} with \{t} \{setAll ls}"

export
Atom x => Atom y => IsGraph (Graph x y) where
  command_      = "plot"
  toString      = interpolate

export
deflt : GraphType x y ts -> Selection s ts -> Graph x y s
deflt t c = G t c []

public export
data BareCol : (gt : GraphType x y ts) -> Selection s ts -> Type where
  [search gt]
  Hi : BareCol Histograms [Var sel]
  Ls : BareCol Lines [x, Var sel]
  Ps : BareCol Points [_,Var sel]
  LP : BareCol LinesPoints [_,Var sel]
  Im : BareCol Impulses [_,Var sel]
  Do : BareCol Dots [_,Var sel]
  St : BareCol Steps [_,Var sel]
  FS : BareCol FSteps [_,Var sel]
  HS : BareCol HiSteps [_,Var sel]
  EB : BareCol ErrorBars [_,Var sel]
  EL : BareCol ErrorLines [_,Var sel]
  Bo : BareCol Boxes [_,Var sel]
  FC : BareCol FilledCurves [_,Var sel]

title :  {s : _}
      -> (gt   : GraphType x y ts)
      -> (sel  : Selection s ts)
      -> (0 bc : BareCol gt sel)
      -> String
title Histograms [Var x] Hi     = colName x
title Lines [_,Var y] Ls        = colName y
title Points [_,Var y] Ps       = colName y
title LinesPoints [_,Var y] LP  = colName y
title Impulses [_,Var y] Im     = colName y
title Dots [_,Var y] Do         = colName y
title Steps [_,Var y] St        = colName y
title FSteps [_,Var y] FS       = colName y
title HiSteps [_,Var y] HS      = colName y
title ErrorBars [_,Var y] EB    = colName y
title ErrorLines [_,Var y] EL   = colName y
title Boxes [_,Var y] Bo        = colName y
title FilledCurves [_,Var y] FC = colName y

||| A titled curve, where the title is taken from
||| the corresponding column of the schema.
export
titled :  {s : _}
       -> (gt   : GraphType x y ts)
       -> (sel  : Selection s ts)
       -> (0 bc : BareCol gt sel)
       => Graph x y s
titled gt sel = G gt sel [Title .= title gt sel bc]
