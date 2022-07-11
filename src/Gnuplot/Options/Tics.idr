module Gnuplot.Options.Tics

import Gnuplot.Options.Color
import Gnuplot.Options.Font
import Gnuplot.Options.Label
import Gnuplot.Util

%default total

public export
data Position = Axis | Border

export
Interpolation Position where
  interpolate Axis   = "axis"
  interpolate Border = "border"


public export
data Direction = In | Out

export
Interpolation Direction where
  interpolate In  = "in"
  interpolate Out = "out"

public export
data Mirror = DoMirror | NoMirror

export
Interpolation Mirror where
  interpolate DoMirror = "mirror"
  interpolate NoMirror = "nomirror"

public export
data TicRotation : Type where
  Angle    : Double -> TicRotation
  Rotate   : TicRotation
  NoRotate : TicRotation

export
Interpolation TicRotation where
  interpolate (Angle dbl) = "rotate by \{show dbl}"
  interpolate Rotate      = "rotate"
  interpolate NoRotate    = "norotate"

public export
data Justify = Left | Right | Center | Auto

export
Interpolation Justify where
  interpolate Left   = "left"
  interpolate Right  = "right"
  interpolate Center = "center"
  interpolate Auto   = "autojustify"

public export
data TicType = Numeric | Timedate | Geographic

export
Interpolation TicType where
  interpolate Numeric    = "numeric"
  interpolate Timedate   = "timedate"
  interpolate Geographic = "geographic"

public export
data LogScale = Logscale | NoLogscale

export
Interpolation LogScale where
  interpolate Logscale   = "logscale"
  interpolate NoLogscale = "nologscale"

||| set xtics {axis | border} {{no}mirror}
|||           {in | out} {scale {default | <major> {,<minor>}}}
|||           {{no}rotate {by <ang>}} {offset <offset> | nooffset}
|||           {left | right | center | autojustify}
|||           {add}
|||           {  autofreq
|||            | <incr>
|||            | <start>, <incr> {,<end>}
|||            | ({"<label>"} <pos> {<level>} {,{"<label>"}...) }
|||           {format "formatstring"} {font "name{,<size>}"} {{no}enhanced}
|||           { numeric | timedate | geographic }
|||           {{no}logscale}
|||           { rangelimited }
public export
data TicOption : Type -> Type where
  Pos          : TicOption Position
  Dir          : TicOption Direction
  Mir          : TicOption Mirror
  Rot          : TicOption TicRotation
  Just         : TicOption Justify
  TT           : TicOption TicType
  LS           : TicOption LogScale
  Font         : TicOption Font
  Enh          : TicOption Enhancement
  Add          : TicOption ()
  RangeLimited : TicOption ()
  Color        : TicOption Color

public export
record TicSetting where
  constructor MkTS
  option : TicOption ticOptionType
  value  : ticOptionType

export
Interpolation TicSetting where
  interpolate (MkTS Pos v)          = "\{v}"
  interpolate (MkTS Dir v)          = "\{v}"
  interpolate (MkTS Mir v)          = "\{v}"
  interpolate (MkTS Rot v)          = "\{v}"
  interpolate (MkTS Just v)         = "\{v}"
  interpolate (MkTS TT v)           = "\{v}"
  interpolate (MkTS LS v)           = "\{v}"
  interpolate (MkTS Font v)         = "font \{v}"
  interpolate (MkTS Enh v)          = "\{v}"
  interpolate (MkTS Add _)          = "add"
  interpolate (MkTS RangeLimited _) = "rangelimited"
  interpolate (MkTS Color v)        = "textcolor \{v}"

public export
0 TicSettings : Type
TicSettings = List TicSetting

export
Interpolation TicSettings where
  interpolate = unwords . map interpolate

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export %inline
axis : TicSetting
axis = MkTS Pos Axis

export %inline
border : TicSetting
border = MkTS Pos Border

export %inline
in_ : TicSetting
in_ = MkTS Dir In

export %inline
out : TicSetting
out = MkTS Dir Out

export %inline
rotate : TicSetting
rotate = MkTS Rot Rotate

export %inline
rotateBy : Double -> TicSetting
rotateBy = MkTS Rot . Angle

export %inline
norotate : TicSetting
norotate = MkTS Rot NoRotate

export %inline
left : TicSetting
left = MkTS Just Left

export %inline
right : TicSetting
right = MkTS Just Right

export %inline
center : TicSetting
center = MkTS Just Center

export %inline
autojustify : TicSetting
autojustify = MkTS Just Auto

export %inline
add : TicSetting
add = MkTS Add ()

export %inline
font : Font -> TicSetting
font = MkTS Font

export %inline
enhanced : TicSetting
enhanced = MkTS Enh Enhanced

export %inline
noenhanced : TicSetting
noenhanced = MkTS Enh NoEnhanced

export %inline
numeric : TicSetting
numeric = MkTS TT Numeric

export %inline
timedate : TicSetting
timedate = MkTS TT Timedate

export %inline
geographic : TicSetting
geographic = MkTS TT Geographic

export %inline
rangelimited : TicSetting
rangelimited = MkTS RangeLimited ()

export %inline
logscale : TicSetting
logscale = MkTS LS Logscale

export %inline
nologscale : TicSetting
nologscale = MkTS LS NoLogscale

export %inline
textcolor : Color -> TicSetting
textcolor = MkTS Color
