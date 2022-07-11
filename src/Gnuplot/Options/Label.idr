module Gnuplot.Options.Label

import Gnuplot.Options.Color
import Gnuplot.Options.Font
import Gnuplot.Options.Offset
import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
record LabelText where
  constructor MkLabelText
  label : String

export
FromString LabelText where fromString = MkLabelText

export
Interpolation LabelText where
  interpolate (MkLabelText l) = quote l

public export
data Rotation : Type where
  Angle    : Double -> Rotation
  Parallel : Rotation
  None     : Rotation

export
Interpolation Rotation where
  interpolate (Angle dbl) = "rotate by \{show dbl}"
  interpolate Parallel    = "rotate parallel"
  interpolate None        = "norotate"

public export
data Enhancement = Enhanced | NoEnhanced

export
Interpolation Enhancement where
  interpolate Enhanced   = "enhanced"
  interpolate NoEnhanced = "noenhanced"

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

||| According to the Gnuplot help, the following settings
||| for labels are available:
|||
||| set xlabel {"<label>"} {offset <offset>} {font "<font>{,<size>}"}
|||            {textcolor <colorspec>} {{no}enhanced}
|||            {rotate by <degrees> | rotate parallel | norotate}
public export
data LabelOption : Type -> Type where
  Text   : LabelOption LabelText
  Font   : LabelOption Font
  Color  : LabelOption Color
  Offset : LabelOption Offset
  Enh    : LabelOption Enhancement
  Rot    : LabelOption Rotation

||| A dependent pair, pairing a label option with a value
||| of the corresponding type.
public export
record LabelSetting where
  constructor MkLS
  option : LabelOption lblOptType
  value  : lblOptType

export
FromString LabelSetting where
  fromString v = MkLS Text $ fromString v

export
Interpolation LabelSetting where
  interpolate (MkLS Text v)   = "\{v}"
  interpolate (MkLS Font v)   = "font \{v}"
  interpolate (MkLS Color v)  = "textcolor \{v}"
  interpolate (MkLS Offset v) = "offset \{v}"
  interpolate (MkLS Enh v)    = "\{v}"
  interpolate (MkLS Rot v)    = "\{v}"

public export
0 LabelSettings : Type
LabelSettings = List LabelSetting

||| This allow us to write `xlabel "foo"`.
export %inline
FromString LabelSettings where
  fromString v = [fromString v]

export
Interpolation LabelSettings where
  interpolate = unwords . map interpolate

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export %inline
font : String -> LabelSetting
font = MkLS Font . fromString

export %inline
textcolor : Color -> LabelSetting
textcolor = MkLS Color

export %inline
enhanced : LabelSetting
enhanced = MkLS Enh Enhanced

export %inline
noenhanced : LabelSetting
noenhanced = MkLS Enh NoEnhanced

export %inline
rotateBy : Double -> LabelSetting
rotateBy = MkLS Rot . Angle

export %inline
parallel : LabelSetting
parallel = MkLS Rot Parallel

export %inline
norotate : LabelSetting
norotate = MkLS Rot None

namespace Alt
  export %inline
  font : String -> Nat -> LabelSetting
  font s n = MkLS Font $ MkFont s (Just n)
