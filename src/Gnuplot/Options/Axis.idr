module Gnuplot.Options.Axis

import Gnuplot.Util

%default total

public export
data Axis = X | X2 | Y | Y2 | Z | CB | R

export
Interpolation Axis where
  interpolate X  = "x"
  interpolate X2 = "x2"
  interpolate Y  = "y"
  interpolate Y2 = "y2"
  interpolate Z  = "z"
  interpolate CB = "cb"
  interpolate R  = "r"

public export
0 Axes : Type
Axes = List Axis

export
Interpolation Axes where interpolate = foldMap interpolate

public export
readAxes : List Char -> Maybe Axes
readAxes []                = Just []
readAxes ('x' :: '2' :: t) = (X2 ::) <$> readAxes t
readAxes ('y' :: '2' :: t) = (Y2 ::) <$> readAxes t
readAxes ('c' :: 'b' :: t) = (CB ::) <$> readAxes t
readAxes ('x' :: t)        = (X ::)  <$> readAxes t
readAxes ('y' :: t)        = (Y ::)  <$> readAxes t
readAxes ('z' :: t)        = (Z ::)  <$> readAxes t
readAxes ('r' :: t)        = (R ::)  <$> readAxes t
readAxes _                 = Nothing

export
fromString :  (s : String)
           -> (0 _ : IsJust (readAxes $ unpack s))
           => Axes
fromString s = fromJust (readAxes $ unpack s)

public export
record LogScale where
  constructor LS
  axes : Axes
  base : Maybe Double

export
Interpolation LogScale where
  interpolate (LS as b) = "\{as} \{maybe neutral show b}"

namespace LogScale
  export
  fromString :  (s : String)
             -> (0 _ : IsJust (readAxes $ unpack s))
             => LogScale
  fromString s = LS (fromString s) Nothing
