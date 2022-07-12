module Gnuplot.Options

import public Gnuplot.Options.Axis
import public Gnuplot.Options.Border
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
  LineType : (n : Nat) -> Option False LineStyles
  LogScale : Option False Axis.LogScale
  Stle     : Style t -> Option False t
  Tics     : (axis : Axis) -> Option True TicSettings
  Border   : (pos  : Maybe Nat) -> Option (isNothing pos) BorderSettings

export
Interpolation (Option u o) where
  interpolate (Label x)    = "\{x}label"
  interpolate (Range x)    = "\{x}range"
  interpolate (Tics x)     = "\{x}tics"
  interpolate (LineType n) = "linetype \{show n}"
  interpolate LogScale     = "logscale"
  interpolate (Stle s)     = "style \{s}"
  interpolate (Border s)   = "border \{maybe neutral show s}"

inter : Option b o -> Interpolation o
inter (Label _)       = %search
inter (Range _)       = %search
inter (Tics _)        = %search
inter (LineType _)    = %search
inter LogScale        = %search
inter (Stle $ Line _) = %search
inter (Border _)      = %search

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

export %inline
xrange : RangeSettings -> Setting
xrange = Set (Range X)

export %inline
yrange : RangeSettings -> Setting
yrange = Set (Range Y)

export %inline
zrange : RangeSettings -> Setting
zrange = Set (Range Z)

export %inline
cbrange : RangeSettings -> Setting
cbrange = Set (Range CB)

export %inline
rrange : RangeSettings -> Setting
rrange = Set (Range R)

export %inline
x2range : RangeSettings -> Setting
x2range = Set (Range X2)

export %inline
y2range : RangeSettings -> Setting
y2range = Set (Range Y2)

export %inline
xtics : TicSettings -> Setting
xtics = Set (Tics X)

export %inline
ytics : TicSettings -> Setting
ytics = Set (Tics Y)

export %inline
ztics : TicSettings -> Setting
ztics = Set (Tics Z)

export %inline
cbtics : TicSettings -> Setting
cbtics = Set (Tics CB)

export %inline
x2tics : TicSettings -> Setting
x2tics = Set (Tics X2)

export %inline
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
linetype : Nat -> LineStyles -> Setting
linetype n = Set (LineType n)

export %inline
logscale : Axis.LogScale -> Setting
logscale = Set LogScale

export %inline
border : BorderSettings -> Setting
border = Set (Border Nothing)

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

  export %inline
  border : Option True BorderSettings
  border = Border Nothing
