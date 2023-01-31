module Gnuplot.Options.LineStyle

import Gnuplot.Options.Color
import Gnuplot.Options.Title
import Gnuplot.Util

%default total

public export
data LSOption : Type -> Type where
  Tpe       : LSOption Nat
  Width     : LSOption Double
  Color     : LSOption Color
  PointType : LSOption Nat
  PointSize : LSOption Double

||| set style line <index> default
||| set style line <index> {{linetype  | lt} <line_type> | <colorspec>}
|||                        {{linecolor | lc} <colorspec>}
|||                        {{linewidth | lw} <line_width>}
|||                        {{pointtype | pt} <point_type>}
|||                        {{pointsize | ps} <point_size>}
|||                        {{pointinterval | pi} <interval>}
|||                        {{pointnumber | pn} <max_symbols>}
|||                        {{dashtype | dt} <dashtype>}
|||                        {palette}
public export
record LineStyle where
  constructor LS
  option : LSOption lsOptType
  value  : lsOptType

export
Interpolation LineStyle where
  interpolate (LS Tpe v)       = "linetype \{show v}"
  interpolate (LS Width v)     = "linewidth \{show v}"
  interpolate (LS Color v)     = "linecolor \{v}"
  interpolate (LS PointType v) = "pointtype \{show v}"
  interpolate (LS PointSize v) = "pointsize \{show v}"

public export
0 LineStyles : Type
LineStyles = List LineStyle

export
Interpolation LineStyles where
  interpolate = unwords . map interpolate

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export %inline
type : Nat -> LineStyle
type = LS Tpe

export %inline
width : Double -> LineStyle
width = LS Width

export %inline
color : Color -> LineStyle
color = LS Color

export %inline
pointtype : Nat -> LineStyle
pointtype = LS PointType

export %inline
pointsize : Double -> LineStyle
pointsize = LS PointSize

--------------------------------------------------------------------------------
--          Line
--------------------------------------------------------------------------------

public export
data LineOption : Type -> Type where
  LSO   : LSOption t -> LineOption t
  Style : LineOption Nat
  Title : LineOption Title

public export
record LineSetting where
  constructor MkLS
  option : LineOption lineOptType
  value  : lineOptType

export
Interpolation LineSetting where
  interpolate (MkLS (LSO o) v) = "\{LS o v}"
  interpolate (MkLS Style v)   = "linestyle \{show v}"
  interpolate (MkLS Title v)   = "\{v}"

public export
0 LineSettings : Type
LineSettings = List LineSetting

export
Interpolation LineSettings where
  interpolate = unwords . map interpolate

namespace Line
  export %inline
  type : Nat -> LineSetting
  type = MkLS (LSO Tpe)

  export %inline
  width : Double -> LineSetting
  width = MkLS (LSO Width)

  export %inline
  color : Color -> LineSetting
  color = MkLS (LSO Color)

  export %inline
  pointtype : Nat -> LineSetting
  pointtype = MkLS (LSO PointType)

  export %inline
  pointsize : Double -> LineSetting
  pointsize = MkLS (LSO PointSize)

  export %inline
  notitle : LineSetting
  notitle = MkLS Title NoTitle

  export %inline
  title : Title -> LineSetting
  title = MkLS Title
