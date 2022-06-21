module Gnuplot.Options

import public Gnuplot.Options.Label
import public Gnuplot.Options.Range

import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Option
--------------------------------------------------------------------------------

public export
record Option (a : Type) where
  constructor MkOption
  name : String

export
Interpolation (Option t) where
  interpolate = name

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

infix 3 .=

public export
record Setting where
  constructor (.=)
  opt : Option type
  val : type
  {auto ip : Interpolation type}

public export
0 Settings : Type
Settings = List Setting

export
Interpolation Setting where
  interpolate (o .= v) = "set \{o} \{v}"

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

export
xrange : Option (Range,Range)
xrange = MkOption "xrange"

export
xlabel : Option Label
xlabel = MkOption "xlabel"

export
yrange : Option (Range,Range)
yrange = MkOption "yrange"

export
ylabel : Option Label
ylabel = MkOption "ylabel"
