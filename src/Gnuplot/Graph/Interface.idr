module Gnuplot.Graph.Interface

import Gnuplot.Options
import Gnuplot.Schema
import Gnuplot.Util

%default total

public export
record Command (g : Schema -> Type) where
  constructor MkCommand
  string : String

export %inline
FromString (Command g) where fromString = MkCommand

export
Interpolation (Command g) where
  interpolate = string

||| Originally Private.Graph.C
public export
interface IsGraph (0 g : Schema -> Type) where
  command_      : Command g
  toString      : g s -> String

export
command : (0 g : Schema -> Type) -> IsGraph g => Command g
command _ = command_
