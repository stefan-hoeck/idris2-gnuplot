module Gnuplot.Frame

import Gnuplot.Display
import Gnuplot.Frame.Option
import Gnuplot.Graph.Interface
import Gnuplot.Plot.Types
import Gnuplot.Schema
import Gnuplot.Util

%default total

public export
record Frame (g : Schema -> Type) where
  constructor MkFrame
  options : Opts
  plot    : Plot g

export
IsGraph g => ToScript (Frame g) where
  toScript (MkFrame os p) = optionsToScript os <+> script p
