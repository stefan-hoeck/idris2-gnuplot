module Gnuplot.Plot2D

import Data.FilePath
import Data.FilePath.Body

import Data.SortedMap
import Gnuplot.Display
import Gnuplot.Options
import Gnuplot.Graph2D
import Gnuplot.File
import Gnuplot.Schema
import Gnuplot.Schema.Expr
import Gnuplot.Util

%default total

||| Data plotted in a graph.
public export
data GraphData : (g : Schema -> Type) -> Type where
  ||| Data stored in a file.
  FileData :  {0 g : Schema -> Type}
           -> (schema   : Schema)
           -> (file     : FilePath)
           -> (graphs   : List (g schema))
           -> GraphData g

  ||| In-memory data, which will first have to be
  ||| written to a file.
  TableData :  {0 g : Schema -> Type}
            -> (schema   : Schema)
            -> (file     : File Abs)
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
        natBody = fromMaybe "0" . parse $ show n

export
fromTable : {s : _} -> Table s -> List (g s) -> Plot g
fromTable rs gs = MkPlot $ \n,dir =>
  (S n, [TableData s (dir /> tmpFile n) rs gs])

export
fromFile : {s : _} -> FilePath -> List (g s) -> Plot g
fromFile name gs = pure [FileData s name gs]

toFile : GraphData g -> Maybe GPFile
toFile (FileData _ _ _)      = Nothing
toFile (TableData _ f tbl _) = Just $ MkFile f (printTable tbl)

toStrs : IsGraph g => GraphData g -> List String
toStrs (FileData _ f gs)    = map (plot $ Just f) gs
toStrs (TableData _ f _ gs) = 
  let fp = FP $ toPath f in map (plot $ Just fp) gs

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

--------------------------------------------------------------------------------
--          2D Plots
--------------------------------------------------------------------------------

||| Plots can be assembled using 'mappend' or 'mconcat'.
||| You can alter attributes of embedded graphs using 'fmap'.
public export
0 Plot2D : (x,y : Universe) -> Type
Plot2D x y =  Plot (Graph x y)

||| Creates a plot from a single graph and a selection of
||| columns from a precalculated table of data.
export
table :  {s  : _}
      -> {us : _}
      -> GraphType x y us
      -> (mods : Mods)
      -> (cols : NP (Expr s) (us ++ AddCols x y mods))
      -> Table s
      -> LineSettings
      -> Plot2D x y
table gt mods cols rows sets =
  fromTable rows [G gt mods cols sets]
