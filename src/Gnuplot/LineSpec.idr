module Gnuplot.LineSpec

import Gnuplot.ColorSpec
import Gnuplot.Util

%default total

||| TODO: Replace the Nats with something more
|||       expressive. We have dependent types after all
|||
||| Originally this was Private.LineSpecification.T
public export
record Line where
  constructor MkLine
  style     : Maybe Nat
  type      : Maybe Nat
  width     : Maybe Double
  color     : Maybe Color
  pointType : Maybe Nat
  pointSize : Maybe Double
  title     : Maybe String

export
deflt : Line
deflt = MkLine Nothing Nothing Nothing Nothing Nothing Nothing Nothing

export
Interpolation Line where
  interpolate (MkLine s t w c pt ps ttl) = unwords $ concatList [
      asList (\n => "linestyle \{show n}") s
    , asList (\n => "linetype \{show n}") t
    , asList (\n => "linewidth \{show n}") w
    , asList (\n => "linecolor \{n}") c
    , asList (\n => "pointtype \{show n}") pt
    , asList (\n => "pointsize \{show n}") ps
    , asList (\n => "title \{quote n}") ttl
    ]
