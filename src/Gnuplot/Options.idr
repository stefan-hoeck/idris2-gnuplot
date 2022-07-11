module Gnuplot.Options

import public Gnuplot.Options.Axis
import public Gnuplot.Options.Color
import public Gnuplot.Options.Label
import public Gnuplot.Options.LineStyle
import public Gnuplot.Options.Range
import public Gnuplot.Options.Tics
import public Gnuplot.Options.Title

import Gnuplot.Util

%default total

public export
data Style : Type -> Type where
  Line : (n : Nat) -> Style LineStyles

export
Interpolation (Style t) where
  interpolate (Line n) = "line \{show n}"

public export
data Option : (unset : Bool) -> (opt : Type) -> Type where
  Label    : (axis : Axis) -> Option True LabelSettings
  Range    : (axis : Axis) -> Option False RangeSettings
  LogScale : Option False Axis.LogScale
  Stle     : Style t -> Option False t
  Tics     : (axis : Axis) -> Option True TicSettings

export
Interpolation (Option u o) where
  interpolate (Label x) = "\{x}label"
  interpolate (Range x) = "\{x}range"
  interpolate (Tics x)  = "\{x}tics"
  interpolate LogScale  = "logscale"
  interpolate (Stle s)  = "style \{s}"

inter : Option b o -> Interpolation o
inter (Label _)       = %search
inter (Range _)       = %search
inter (Tics _)        = %search
inter LogScale        = %search
inter (Stle $ Line _) = %search

--------------------------------------------------------------------------------
--          Setting
--------------------------------------------------------------------------------

public export
data Setting : Type where
  Set   : (opt : Option unset optType) -> (value : optType) -> Setting
  Unset : Option True optType -> Setting

export
Interpolation Setting where
  interpolate (Set o v) = let i = inter o in "set \{o} \{v}"
  interpolate (Unset o) = "unset \{o}"

public export
0 Settings : Type
Settings = List Setting

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export
xrange : RangeSettings -> Setting
xrange = Set (Range X)

export
yrange : RangeSettings -> Setting
yrange = Set (Range Y)

export
zrange : RangeSettings -> Setting
zrange = Set (Range Z)

export
cbrange : RangeSettings -> Setting
cbrange = Set (Range CB)

export
rrange : RangeSettings -> Setting
rrange = Set (Range R)

export
x2range : RangeSettings -> Setting
x2range = Set (Range X2)

export
y2range : RangeSettings -> Setting
y2range = Set (Range Y2)

export
xtics : TicSettings -> Setting
xtics = Set (Tics X)

export
ytics : TicSettings -> Setting
ytics = Set (Tics Y)

export
ztics : TicSettings -> Setting
ztics = Set (Tics Z)

export
cbtics : TicSettings -> Setting
cbtics = Set (Tics CB)

export
x2tics : TicSettings -> Setting
x2tics = Set (Tics X2)

export
y2tics : TicSettings -> Setting
y2tics = Set (Tics Y2)

export %inline
xlabel : LabelSettings -> Setting
xlabel = Set (Label X)

export %inline
x2label : LabelSettings -> Setting
x2label = Set (Label X2)

export %inline
ylabel : LabelSettings -> Setting
ylabel = Set (Label Y)

export %inline
y2label : LabelSettings -> Setting
y2label = Set (Label Y2)

export %inline
zlabel : LabelSettings -> Setting
zlabel = Set (Label Z)

export %inline
cblabel : LabelSettings -> Setting
cblabel = Set (Label CB)

export %inline
linestyle : Nat -> LineStyles -> Setting
linestyle n = Set (Stle $ Line n)

export %inline
logscale : Axis.LogScale -> Setting
logscale = Set LogScale

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

namespace Settings
  export %inline
  xrange : Range -> Range -> Setting
  xrange r1 r2 = xrange [range r1 r2]

  export %inline
  yrange : Range -> Range -> Setting
  yrange r1 r2 = yrange [range r1 r2]

  export %inline
  zrange : Range -> Range -> Setting
  zrange r1 r2 = zrange [range r1 r2]

  export %inline
  x2range : Range -> Range -> Setting
  x2range r1 r2 = x2range [range r1 r2]

  export %inline
  y2range : Range -> Range -> Setting
  y2range r1 r2 = y2range [range r1 r2]

  export %inline
  cbrange : Range -> Range -> Setting
  cbrange r1 r2 = cbrange [range r1 r2]

  export %inline
  rrange : Range -> Range -> Setting
  rrange r1 r2 = rrange [range r1 r2]

  export %inline
  logscale : Setting
  logscale = Set LogScale (LS "" Nothing)
