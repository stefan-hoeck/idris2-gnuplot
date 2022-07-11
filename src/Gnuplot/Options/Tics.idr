module Gnuplot.Options.Tics

%default total

public export
data Position = Axis | Border

export
Interpolation Position where
  interpolate Axis   = "axis"
  interpolate Border = "border"


public export
data Dir = In | Out

export
Interpolation Dir where
  interpolate In  = "in"
  interpolate Out = "out"

public export
data Mirror = DoMirror | NoMirror

export
Interpolation Mirror where
  interpolate DoMirror = "mirror"
  interpolate NoMirror = "nomirror"

public export
record Tics where
  constructor MkTics
  pos    : Position
  dir    : Dir
  mirror : Mirror

export
Interpolation Tics where
  interpolate ts =
    "\{ts.pos} \{ts.dir} \{ts.mirror}"
