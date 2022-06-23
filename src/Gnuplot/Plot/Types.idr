module Gnuplot.Plot.Types

import Data.FilePath
import Data.FilePath.Body

import Data.SortedMap
import Gnuplot.Display
import Gnuplot.Options
import Gnuplot.Graph.Interface
import Gnuplot.File
import Gnuplot.Schema
import Gnuplot.Util

%default total

||| Data plotted in a graph.
public export
data GraphData : (g : Schema -> Type) -> Type where
  ||| Data stored in a file.
  FileData :  {0 g : Schema -> Type}
           -> {0 schema : Schema}
           -> (file     : FilePath)
           -> (graphs   : List (g schema))
           -> GraphData g

  ||| In-memory data, which will first have to be
  ||| written to a file.
  TableData :  {0 g : Schema -> Type}
            -> {0 schema : Schema}
            -> (cols     : Atoms schema)
            => (file     : Path Abs)
            -> (table    : Table schema)
            -> (graphs   : List (g schema))
            -> GraphData g

||| A plot displaying several graphs.
public export
record Plot (g : Schema -> Type) where
  constructor MkPlot
  run : Nat -> Path Abs -> (Nat, List $ GraphData g)

export
pure : List (GraphData g) -> Plot g
pure v = MkPlot $ \n,_ => (n,v)

export
Semigroup (Plot g) where
  MkPlot f <+> MkPlot g = MkPlot $ \n,p =>
    let (n1,fs1) = f n p
        (n2,fs2) = g n1 p
     in (n2, fs1 ++ fs2)

export
Monoid (Plot g) where
  neutral = pure []

tmpFile : Nat -> Body
tmpFile n = "curve" <+> natBody <+> ".csv"
  where natBody : Body
        natBody = fromMaybe "0" . body $ show n

export
fromTable : Table s -> Atoms s => List (g s) -> Plot g
fromTable rs gs = MkPlot $ \n,dir =>
  (S n, [TableData (dir /> tmpFile n) rs gs])

export
fromFile : FilePath -> List (g s) -> Plot g
fromFile name gs = pure [FileData name gs]

toFile : GraphData g -> Maybe GPFile
toFile (FileData _ _)      = Nothing
toFile (TableData f tbl _) = Just $ MkFile f (printTable tbl)

toStrs : IsGraph g => GraphData g -> List String
toStrs (FileData f gs)    = map (\g => "\"\{f}\" \{toString g}") gs
toStrs (TableData f _ gs) = map (\g => "\"\{f}\" \{toString g}") gs

||| In contrast to the Display.toScript method instantiation
||| this function leaves the options,
||| and thus can be used to write the Display.toScript instance for Frame.
export
script : IsGraph g => Plot g -> Script
script p@(MkPlot mp) = MkScript $ \n,pl,fp =>
  let (n2,blocks) = mp n fp
      files       = Lin <>< mapMaybe toFile blocks
      graphs      = foldMap toStrs blocks
   in (n2, pl, MkContent files [< "\{command g} \{commaConcat graphs}"])

export
IsGraph g => ToScript (Plot g) where
  toScript p = script p
