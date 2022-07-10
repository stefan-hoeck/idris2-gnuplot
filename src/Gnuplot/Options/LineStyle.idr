module Gnuplot.Options.LineStyle

import Gnuplot.Options.Color
import Gnuplot.Options.Title
import Gnuplot.Util

%default total

infix 3 .=

namespace LineStyle
  ||| TODO: Replace the Nats with something more
  |||       expressive. We have dependent types after all
  |||
  ||| Originally this was Private.LineSpecification.T
  public export
  record LineStyle where
    constructor LS
    type      : Maybe Nat
    width     : Maybe Double
    color     : Maybe Color
    pointType : Maybe Nat
    pointSize : Maybe Double
  
  export
  deflt : LineStyle
  deflt = LS Nothing Nothing Nothing Nothing Nothing
  
  export
  Interpolation LineStyle where
    interpolate (LS t w c pt ps) = unwords $ concatList [
        asList (\n => "linetype \{show n}") t
      , asList (\n => "linewidth \{show n}") w
      , asList (\n => "linecolor \{n}") c
      , asList (\n => "pointtype \{show n}") pt
      , asList (\n => "pointsize \{show n}") ps
      ]

  export
  Interpolation (List (LineStyle -> LineStyle)) where
    interpolate = interpolate . foldl (flip apply) deflt
  
  public export
  data LSProp : Type -> Type where
    Tpe       : LSProp Nat
    Width     : LSProp Double
    Color     : LSProp Color
    PointType : LSProp Nat
    PointSize : LSProp Double
  
  public export
  (.=) : LSProp t -> t -> LineStyle -> LineStyle
  (.=) Tpe v       = {type      := Just v}
  (.=) Width v     = {width     := Just v}
  (.=) Color v     = {color     := Just v}
  (.=) PointType v = {pointType := Just v}
  (.=) PointSize v = {pointSize := Just v}
  
  public export
  unset : LSProp t -> LineStyle -> LineStyle
  unset Tpe       = {type      := Nothing}
  unset Width     = {width     := Nothing}
  unset Color     = {color     := Nothing}
  unset PointType = {pointType := Nothing}
  unset PointSize = {pointSize := Nothing}
  
  export
  setAll : List (LineStyle -> LineStyle) -> LineStyle
  setAll = foldl (\l,f => f l) deflt

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
  title     : Maybe Title

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
    , asList (\n => "\{n}") ttl
    ]

public export
data LineProp : Type -> Type where
  Style     : LineProp Nat
  Tpe       : LineProp Nat
  Width     : LineProp Double
  Color     : LineProp Color
  PointType : LineProp Nat
  PointSize : LineProp Double
  Title     : LineProp Title

public export
(.=) : LineProp t -> t -> Line -> Line
(.=) Style v     = {style     := Just v}
(.=) Tpe v       = {type      := Just v}
(.=) Width v     = {width     := Just v}
(.=) Color v     = {color     := Just v}
(.=) PointType v = {pointType := Just v}
(.=) PointSize v = {pointSize := Just v}
(.=) Title v     = {title     := Just v}

public export
unset : LineProp t -> Line -> Line
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
