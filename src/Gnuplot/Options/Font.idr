module Gnuplot.Options.Font

import Gnuplot.Util

%default total

public export
record Font where
  constructor MkFont
  name : String
  size : Maybe Nat

export
Interpolation Font where
  interpolate (MkFont n s) = 
    let str = maybe n (\v => "\{n},\{show v}") s
     in "\{quote str}"

export %inline
FromString Font where
  fromString s = MkFont s Nothing

export %inline
font : String -> Nat -> Font
font s = MkFont s . Just
