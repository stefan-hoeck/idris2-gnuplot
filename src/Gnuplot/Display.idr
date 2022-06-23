module Gnuplot.Display

import Gnuplot.Options
import Gnuplot.File
import Gnuplot.Util

||| Content of a gnuplot script, consisting of a
||| list of files, which need to be written, as well
||| as a list of commands representing the script
public export
record Content where
  constructor MkContent
  files    : SnocList GPFile
  commands : SnocList String

export
Semigroup Content where
  MkContent fs1 cs1 <+> MkContent fs2 cs2 =
    MkContent (fs1 ++ fs2) (cs1 ++ cs2)

export
Monoid Content where
  neutral = MkContent [<] [<]

||| Originally Private.Display.C
public export
record Script where
  constructor MkScript
  run : Nat -> Settings -> Path Abs -> (Nat,Settings,Content)

export
pure : Content -> Script
pure b = MkScript $ \n,s,_ => (n,s,b)

export
set : Settings -> Script
set ss = MkScript $ \n,_,_ => (n,ss,neutral)

export
reset : Settings -> Script
reset ss = MkScript $ \n,_,_ => (n,[],MkContent [<] [<"reset"])

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
