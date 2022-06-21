module Gnuplot.File

import public Data.FilePath

%default total

public export
record GPFile where
  constructor MkFile
  path    : FilePath
  content : String
