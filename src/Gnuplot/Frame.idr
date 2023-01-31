module Gnuplot.Frame

import Gnuplot.Display
import Gnuplot.File
import Gnuplot.Graph2D
import Gnuplot.Options
import Gnuplot.Plot2D
import Gnuplot.Schema
import Gnuplot.Schema.Expr
import Gnuplot.Util

%default total

public export
record Frame (g : FunctionType -> Schema -> Type) where
  constructor MkFrame
  settings : Settings
  type     : FunctionType
  plot     : Plot type g

param : FunctionType -> Script
param Regular    = pure $ MkContent [<] [< "unset parametric"]
param Parametric = pure $ MkContent [<] [< "set parametric"]

export
IsGraph g => ToScript (Frame g) where
  toScript (MkFrame ss tpe p) = set ss <+> script p

public export
0 Frame2D : (x,y : Universe) -> Type
Frame2D x y = Frame (Graph x y)

export
frame : Settings -> Plot2D Regular x y -> Frame2D x y
frame ss = MkFrame ss Regular
