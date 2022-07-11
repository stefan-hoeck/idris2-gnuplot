module Gnuplot.Options.Border

import Gnuplot.Options.Color
import Gnuplot.Util

%default total

public export
data Position = Front | Back | Behind

export
Interpolation Position where
  interpolate Front  = "front"
  interpolate Back   = "back"
  interpolate Behind = "behind"

||| set border {<integer>}
|||            {front | back | behind}
|||            {linestyle | ls <line_style>}
|||            {linetype | lt <line_type>} {linewidth | lw <line_width>}
|||            {linecolor | lc <colorspec>} {dashtype | dt <dashtype>}
|||            {polar}
public export
data BorderOption : Type -> Type where
  Pos : BorderOption Position
  LS  : BorderOption Nat
  LT  : BorderOption Nat
  LW  : BorderOption Double
  LC  : BorderOption Color

public export
record BorderSetting where
  constructor MkBS
  option : BorderOption borderOptionType
  value  : borderOptionType

export
Interpolation BorderSetting where
  interpolate (MkBS Pos v)         = "\{v}"
  interpolate (MkBS LT v)          = "linetype \{show v}"
  interpolate (MkBS LS v)          = "linestyle \{show v}"
  interpolate (MkBS LW v)          = "linewidth \{show v}"
  interpolate (MkBS LC v)          = "linecolor \{v}"

public export
0 BorderSettings : Type
BorderSettings = List BorderSetting

export
Interpolation BorderSettings where
  interpolate = unwords . map interpolate

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export %inline
front : BorderSetting
front = MkBS Pos Front

export %inline
back : BorderSetting
back = MkBS Pos Back

export %inline
behind : BorderSetting
behind = MkBS Pos Behind

export %inline
linetype : Nat -> BorderSetting
linetype = MkBS LT

export %inline
linecolor : Color -> BorderSetting
linecolor = MkBS LC

export %inline
linewidth : Double -> BorderSetting
linewidth = MkBS LW

export %inline
linestyle : Nat -> BorderSetting
linestyle = MkBS LS
