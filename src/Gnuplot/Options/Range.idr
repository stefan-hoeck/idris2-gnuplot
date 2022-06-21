module Gnuplot.Options.Range

%default total

public export
data Range : Type where
  Auto : Range
  Val  : Double -> Range

public export
fromInteger : Integer -> Range
fromInteger = Val . fromInteger

public export
FromDouble Range where fromDouble = Val

export
Interpolation Range where
  interpolate Auto    = "*"
  interpolate (Val v) = show v

export
Interpolation (Range,Range) where
  interpolate (x,y) = "[\{x}:\{y}]"
