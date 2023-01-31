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
data GraphData :  FunctionType
               -> (g : FunctionType -> Schema -> Type)
               -> Type where
  ||| Data stored in a file.
  FileData :  {0 g : FunctionType -> Schema -> Type}
           -> (schema   : Schema)
           -> (file     : FilePath)
           -> (graphs   : List (g t schema))
           -> GraphData t g

  ||| In-memory data, which will first have to be
  ||| written to a file.
  TableData :  {0 g : FunctionType -> Schema -> Type}
            -> (schema   : Schema)
            -> (file     : File Abs)
            -> (table    : Table schema)
            -> (graphs   : List (g t schema))
            -> GraphData t g

  ||| Calculated functions without the need to read data
  ||| from a file.
  Functions :  {0 g : FunctionType -> Schema -> Type}
            -> (graphs : List (g t []))
            -> GraphData t g

||| A plot displaying several graphs.
public export
record Plot (t : FunctionType) (g : FunctionType -> Schema -> Type) where
  constructor MkPlot
  run : Nat -> Path Abs -> (Nat, List $ GraphData t g)

export
pure : List (GraphData t g) -> Plot t g
pure v = MkPlot $ \n,_ => (n,v)

export
Semigroup (Plot t g) where
  MkPlot f <+> MkPlot g = MkPlot $ \n,p =>
    let (n1,fs1) = f n p
        (n2,fs2) = g n1 p
     in (n2, fs1 ++ fs2)

export
Monoid (Plot t g) where
  neutral = pure []

tmpFile : Nat -> Body
tmpFile n = "curve" <+> natBody <+> ".csv"
  where natBody : Body
        natBody = fromMaybe "0" . parse $ show n

export
fromTable : {s : _} -> Table s -> List (g t s) -> Plot t g
fromTable rs gs = MkPlot $ \n,dir =>
  (S n, [TableData s (dir /> tmpFile n) rs gs])

export
fromFile : {s : _} -> FilePath -> List (g t s) -> Plot t g
fromFile name gs = pure [FileData s name gs]

toFile : GraphData t g -> Maybe GPFile
toFile (FileData _ _ _)      = Nothing
toFile (Functions _)         = Nothing
toFile (TableData _ f tbl _) = Just $ MkFile f (printTable tbl)

toStrs : IsGraph g => GraphData t g -> List String
toStrs (Functions gs)       = map (plot Nothing) gs
toStrs (FileData _ f gs)    = map (plot $ Just f) gs
toStrs (TableData _ f _ gs) = 
  let fp = FP $ toPath f in map (plot $ Just fp) gs

export
script : IsGraph g => Plot t g -> Script
script p@(MkPlot mp) = MkScript $ \n,pl,fp =>
  let (n2,blocks) = mp n fp
      files = Lin <>< mapMaybe toFile blocks
      gs    = foldMap toStrs blocks
   in (n2, pl, MkContent files [< "\{command g} \{commaConcat gs}"])

export
IsGraph g => ToScript (Plot t g) where
  toScript p = script p

--------------------------------------------------------------------------------
--          2D Plots
--------------------------------------------------------------------------------

||| Plots can be assembled using 'mappend' or 'mconcat'.
||| You can alter attributes of embedded graphs using 'fmap'.
public export
0 Plot2D : (t : FunctionType) -> (x,y : Universe) -> Type
Plot2D t x y =  Plot t (Graph x y)

||| Creates a plot from a single graph and a selection of
||| columns from a precalculated table of data.
export
table :  {s  : _}
      -> {us : _}
      -> (0 prf : NonEmpty s)
      => GraphType x y us
      -> (mods : Mods)
      -> (cols : NP (Expr t s) (us ++ AddCols x y mods))
      -> Table s
      -> LineSettings
      -> Plot2D t x y
table gt mods cols rows sets =
  fromTable rows [G gt mods cols sets]

export
functions : List (Graph x y t []) -> Plot2D t x y
functions gs = pure [Functions gs]
