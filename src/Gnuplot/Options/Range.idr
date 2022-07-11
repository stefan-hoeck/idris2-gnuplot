module Gnuplot.Options.Range

import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

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

public export
data Rev = Reverse | NoReverse

export
Interpolation Rev where
  interpolate Reverse   = "reverse"
  interpolate NoReverse = "noreverse"

public export
data WBack = Writeback | NoWriteback

export
Interpolation WBack where
  interpolate Writeback   = "writeback"
  interpolate NoWriteback = "nowriteback"

public export
data Ext = Extend | NoExtend

export
Interpolation Ext where
  interpolate Extend   = "extend"
  interpolate NoExtend = "noextend"

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

||| According to the Gnuplot help, the following settings
||| for ranges are available:
|||
||| set xrange [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} {{no}extend}
|||            | restore
public export
data RangeOption : Type -> Type where
  Rng : RangeOption (Range,Range)
  RV  : RangeOption Rev
  WB  : RangeOption WBack
  EX  : RangeOption Ext

||| A dependent pair, pairing a range option with a value
||| of the corresponding type.
public export
record RangeSetting where
  constructor MkRS
  option : RangeOption rangeOptType
  value  : rangeOptType

export
Interpolation RangeSetting where
  interpolate (MkRS Rng v) = "\{v}"
  interpolate (MkRS RV v)  = "\{v}"
  interpolate (MkRS WB v)  = "\{v}"
  interpolate (MkRS EX v)  = "\{v}"

public export
0 RangeSettings : Type
RangeSettings = List RangeSetting

export
Interpolation RangeSettings where
  interpolate = unwords . map interpolate

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

export %inline
range : Range -> Range -> RangeSetting
range mi ma = MkRS Rng (mi,ma)

export %inline
reverse : RangeSetting
reverse = MkRS RV Reverse

export %inline
noreverse : RangeSetting
noreverse = MkRS RV NoReverse

export %inline
writeback : RangeSetting
writeback = MkRS WB Writeback

export %inline
nowriteback : RangeSetting
nowriteback = MkRS WB NoWriteback

export %inline
extend : RangeSetting
extend = MkRS EX Extend

export %inline
noextend : RangeSetting
noextend = MkRS EX NoExtend
