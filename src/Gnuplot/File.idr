module Gnuplot.File

import Control.Monad.Either

%default total

infixl 5 />
infixr 7 <.>

public export
record FilePath where
  constructor MkFilePath
  path : String

export
(/>) : FilePath -> String -> FilePath
MkFilePath p /> s = MkFilePath $ "\{p}/\{s}"

export
(<.>) : String -> String -> String
(<.>) p ext = "\{p}.\{ext}"

export
Interpolation FilePath where
  interpolate p = show p.path

public export
record GPFile where
  constructor MkFile
  path    : FilePath
  content : String
