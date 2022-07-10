module Gnuplot.Options.Title

import Gnuplot.Util

%default total

public export
data Title : Type where
  NoTitle : Title
  T : String -> Title

export
Interpolation Title where
  interpolate NoTitle = "notitle"
  interpolate (T s)   = "title \{quote s}"

export %inline
FromString Title where fromString = T
