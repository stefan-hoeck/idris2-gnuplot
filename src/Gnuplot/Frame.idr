module Gnuplot.Frame

import Gnuplot.Display
import Gnuplot.Graph2D
import Gnuplot.Options
import Gnuplot.Plot2D
import Gnuplot.Schema
import Gnuplot.Util

%default total

public export
record Frame (g : Schema -> Type) where
  constructor MkFrame
  settings : Settings
  plot     : Plot g

export
IsGraph g => ToScript (Frame g) where
  toScript (MkFrame ss p) = set ss <+> script p

public export
0 Frame2D : (x,y : Universe) -> Type
Frame2D x y = Frame (Graph x y)

export
frame : Settings -> Plot2D x y -> Frame2D x y
frame = MkFrame
