module Gnuplot.Display

import Gnuplot.Frame.Option
import Gnuplot.File
import Gnuplot.Util

||| Body of a gnuplot script, consisting of a
||| list of files, which need to be written, as well
||| as a list of commands representing the script
public export
record Body where
  constructor MkBody
  files    : List GPFile
  commands : List String

export
Semigroup Body where
  MkBody fs1 cs1 <+> MkBody fs2 cs2 = MkBody (fs1 ++ fs2) (cs1 ++ cs2)

export
Monoid Body where
  neutral = MkBody [] []

||| Originally Private.Display.C
public export
record Script where
  constructor MkScript
  run : Nat -> Opts -> FilePath -> (Nat,Opts,Body)

export
pure : Body -> Script
pure b = MkScript $ \n,o,_ => (n,o,b)

export
Semigroup Script where
  MkScript f <+> MkScript g = MkScript $ \n,o,fp =>
    let (n1,o1,b1) = f n o fp
        (n2,o2,b2) = g n1 o1 fp
     in (n2,o2,b1 <+> b2)

export
Monoid Script where
  neutral = pure neutral

||| Interface describing how to convert a value to
||| a gnuplot script.
public export
interface ToScript a where
  toScript : a -> Script
