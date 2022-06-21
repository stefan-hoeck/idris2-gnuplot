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

public export
data Prop : Type -> Type where
  Style     : Prop Nat
  Tpe       : Prop Nat
  Width     : Prop Double
  Color     : Prop Color
  PointType : Prop Nat
  PointSize : Prop Double
  Title     : Prop String

infix 3 .=

public export
(.=) : Prop t -> t -> Line -> Line
(.=) Style v     = {style     := Just v}
(.=) Tpe v       = {type      := Just v}
(.=) Width v     = {width     := Just v}
(.=) Color v     = {color     := Just v}
(.=) PointType v = {pointType := Just v}
(.=) PointSize v = {pointSize := Just v}
(.=) Title v     = {title     := Just v}

public export
unset : Prop t -> Line -> Line
unset Style     = {style     := Nothing}
unset Tpe       = {type      := Nothing}
unset Width     = {width     := Nothing}
unset Color     = {color     := Nothing}
unset PointType = {pointType := Nothing}
unset PointSize = {pointSize := Nothing}
unset Title     = {title     := Nothing}

export
setAll : List (Line -> Line) -> Line
setAll = foldl (\l,f => f l) deflt
