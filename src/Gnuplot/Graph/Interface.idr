module Gnuplot.Graph.Interface

import Gnuplot.Frame.Option

%default total

public export
record Command (graph : Type) where
  constructor MkCommand
  string : String

export %inline
FromString (Command g) where fromString = MkCommand

public export
interface Graph graph where
  command      : Command graph
  toString     : graph -> String
  defltOptions : OptionSet graph
