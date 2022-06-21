module Gnuplot.Options.Label

import Gnuplot.Util

%default total

public export
record Label where
  constructor MkLabel
  label : String

public export
FromString Label where fromString = MkLabel

public export
Interpolation Label where
  interpolate (MkLabel l) = quote l
