module Gnuplot.File

import public Data.FilePath.File

%default total

public export
record GPFile where
  constructor MkFile
  path    : File Abs
  content : String
