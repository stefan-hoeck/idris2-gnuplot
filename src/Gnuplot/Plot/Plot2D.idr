module Gnuplot.Plot.Plot2D

import Gnuplot.Graph.Graph2D
import Gnuplot.Plot.Types
import Gnuplot.Schema
import Gnuplot.Util

%default total

||| Plots can be assembled using 'mappend' or 'mconcat'.
||| You can alter attributes of embedded graphs using 'fmap'.
public export
0 Plot2D : (x,y : Type) -> Type
Plot2D x y =  Plot (Graph x y)

--------------------------------------------------------------------------------
--          Computed Plots
--------------------------------------------------------------------------------

||| Creates a plot from a single graph and a selection of
||| columns from a precalculated table of data.
export
table :  Atoms s
      => GraphType x y ts
      -> Selection s ts
      -> Table s
      -> Plot2D x y
table gt sel rows = fromTable rows [deflt gt sel]

export
function :  Atom a
         => Atom b
         => GraphType x y [a,b]
         -> List a
         -> (a -> b)
         -> Plot2D x y
function typ args f =
  table {s = ["x" :> a, "y" :> b]} typ ["x","y"] $
    map (\va => [va,f va]) args

0 FunSchema' : Nat -> (b : Type) -> List c -> Schema
FunSchema' k b []        = []
FunSchema' k b (_ :: xs) = show k :> b :: FunSchema' (S k) b xs

0 FunSchema : (a,b : Type) -> List c -> Schema
FunSchema a b cs = "0" :> a :: FunSchema' 1 b cs

funAtoms' : Atom b => (k : Nat) -> (fs : List c) -> Atoms (FunSchema' k b fs)
funAtoms' k []        = []
funAtoms' k (x :: xs) = %search :: funAtoms' (S k) xs

funAtoms :  Atom a => Atom b => (fs : List c) -> Atoms (FunSchema a b fs)
funAtoms fs = %search :: funAtoms' 1 fs

funRow' :  (0 k : Nat)
        -> a
        -> (fs : List (a -> b))
        -> Row (FunSchema' k b fs)
funRow' k x []        = []
funRow' k x (f :: fs) = f x :: funRow' (S k) x fs

funRow : (fs : List (a -> b)) -> a -> Row (FunSchema a b fs)
funRow fs x = x :: funRow' 1 x fs

funTbl : List a -> (fs : List (a -> b)) -> Table (FunSchema a b fs)
funTbl as fs = map (funRow fs) as

sels' :  (0 k : Nat)
      -> (fs : List (a -> b))
      -> List (Sel (FunSchema' k b fs) b)
sels' _ []        = []
sels' k (_ :: xs) = MkSel (show k) Here :: map inc (sels' (S k) xs)

sels : (fs : List (a -> b)) -> List (Selection (FunSchema a b fs) [a,b])
sels fs = map (\s => [1, inc s]) (sels' 1 fs)

export
functions :  Atom a
          => Atom b
          => GraphType x y [a,b]
          -> List a
          -> List (a -> b)
          -> Plot2D x y
functions gt as fs =
  let atom = funAtoms {a} {b} fs
   in fromTable (funTbl as fs) (map (deflt gt) (sels fs))


export
parameterFunction :  Atom a
                  => GraphType x y [a]
                  -> List t
                  -> (t -> a)
                  -> Plot2D x y
parameterFunction gt ts f = 
  table {s = ["x" :> a]} gt ["x"] $ map (\vt => [f vt]) ts

-- fromFile :
--    (Atom.C x, Atom.C y) =>
--    Type.T x y a -> FilePath -> Col.T a -> T x y
-- fromFile typ filename (Col.Cons cs) =
--    Plot.fromGraphs filename [Graph.deflt typ cs]
-- 
-- listFromFile ::
--    (Atom.C i, Atom.C y) =>
--    Type.T i y y -> FilePath -> Int -> T i y
-- listFromFile typ filename column =
--    fromFile typ filename (Col.atom column)
-- 
-- pathFromFile ::
--    (Atom.C x, Atom.C y) =>
--    Type.T x y (x,y) -> FilePath -> Int -> Int -> T x y
-- pathFromFile typ filename columnX columnY =
--    fromFile typ filename (Col.pair (Col.atom columnX) (Col.atom columnY))
