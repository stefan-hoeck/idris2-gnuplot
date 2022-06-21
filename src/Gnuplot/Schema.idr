||| The types in this module allow us to describe
||| tabular data using a simple schema of named columns,
||| each of a specific type (from a small custom universe
||| of types).
|||
||| We use schemata to describe the tabular content of files,
||| as well as the shape of tables computed at runtime.
module Gnuplot.Schema

import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Universe
--------------------------------------------------------------------------------

public export
data Lit = LInt Integer | LDbl Double

public export
interface Atom a where
  toLit : a -> Lit

public export %inline
Atom Double where
  toLit = LDbl

public export %inline
Atom Integer where
  toLit = LInt

public export %inline
Atom Bits8 where
  toLit = LInt . cast

public export %inline
Atom Bits16 where
  toLit = LInt . cast

public export %inline
Atom Bits32 where
  toLit = LInt . cast

public export %inline
Atom Bits64 where
  toLit = LInt . cast

public export %inline
Atom Int8 where
  toLit = LInt . cast

public export %inline
Atom Int16 where
  toLit = LInt . cast

public export %inline
Atom Int32 where
  toLit = LInt . cast

public export %inline
Atom Int64 where
  toLit = LInt . cast

public export %inline
Atom Nat where
  toLit = LInt . cast

export
print : Atom a => a -> String
print v = case toLit v of
  LInt n => show n
  LDbl d => show d

--------------------------------------------------------------------------------
--          Data Tables
--------------------------------------------------------------------------------

infixr 8 :>

||| A single column in a schema
public export
record Column where
  constructor (:>)
  name   : String
  0 type : Type

public export
0 ColType : Column -> Type
ColType = type

public export
0 ColAtom : Column -> Type
ColAtom = Atom . ColType

||| A table schema
public export
0 Schema : Type
Schema = List Column

||| Proof that all types in a schema are atoms
public export
0 Atoms : Schema -> Type
Atoms = NP ColAtom

||| A row in a table
public export
0 Row : (cols : Schema) -> Type
Row cols = NP_ Column ColType cols

||| A table of rows
public export
0 Table : Schema -> Type
Table = List . Row

export
printRow : (prf : Atoms s) => Row s -> String
printRow [] = ""
printRow {prf = [_]}    [v]       = print v
printRow {prf = _ :: _} (v :: vs) = print v ++ " " ++ printRow vs

export
printTable : Atoms s => Table s -> String
printTable = unlines . map printRow

--------------------------------------------------------------------------------
--          Row Selection
--------------------------------------------------------------------------------

||| Proof that a column of the given name and type is in the schema.
public export
data InSchema : (n : String) -> (cs : Schema) -> (t : Type) -> Type where
  [search n cs]
  Here  : InSchema n (n :> t :: cs) t
  There : InSchema n cs t -> InSchema n (c :: cs) t

toNat : InSchema n cs t -> Nat
toNat Here      = 1
toNat (There p) = S $ toNat p

public export
columnName : {s : _} -> InSchema n s t -> String
columnName {s = n :> _ :: _} Here      = n
columnName {s = _ :: _}      (There x) = columnName x


||| Proof that a row with the given (1-based) index in the schema
public export
data IxInSchema : (x : Nat) -> (cs : Schema) -> (t : Type) -> Type where
  [search x cs]
  IxHere  : IxInSchema 1 (n :> t :: cs) t
  IxThere : IxInSchema x cs t -> IxInSchema (S x) (c :: cs) t

||| A column in a Schema, selected by name
public export
record Sel (s : Schema) (t : Type) where
  constructor MkSel
  0 name : String
  prf    : InSchema name s t

public export
inc : Sel s t -> Sel (c :: s) t
inc (MkSel n p) = MkSel n (There p)

public export
colName : {s : _} -> Sel s t -> String
colName sel = columnName sel.prf

export
Interpolation (Sel s t) where
  interpolate (MkSel _ p) = show $ toNat p

public export
fromIndex : IxInSchema x s t -> Sel s t
fromIndex IxHere      = MkSel _ Here
fromIndex (IxThere y) = let MkSel n p = fromIndex y in MkSel n (There p)

||| We can conveniently select columns by name
||| using string literals.
public export
fromString : (0 n : String) -> (prf : InSchema n s t) => Sel s t
fromString n = MkSel n prf

||| We can conveniently select columns by name
||| using integer literals.
public export
fromInteger :  (0 x : Integer)
            -> (prf : IxInSchema (cast x) s t)
            => Sel s t
fromInteger _ = fromIndex prf 

namespace Selection
  ||| A selection of columns in a table.
  public export
  data Selection : (s : Schema) -> List Type -> Type where
    Nil  : Selection s []
    (::) : Sel s t -> Selection s ts -> Selection s (t :: ts)

export
Interpolation (Selection s ts) where
  interpolate []       = ""
  interpolate [x]      = interpolate x
  interpolate (h :: t) = "\{h}:\{t}"

public export
0 SchemaTypes : Schema -> List Type
SchemaTypes []        = []
SchemaTypes (_ :> t :: cs) = t :: SchemaTypes cs

--------------------------------------------------------------------------------
--          Creating Tables
--------------------------------------------------------------------------------

export
functions :  List a
          -> NP_ Column (\c => a -> c.type) cs
          -> Table (n :> a :: cs)
functions xs fs = map (\v =>  v :: mapNP ($ v) fs) xs

export
param :  List a
      -> NP_ Column (\c => a -> c.type) cs
      -> Table cs
param xs fs = map (\v => mapNP ($ v) fs) xs

--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

Prices : Schema
Prices = ["Year" :> Bits16, "PQR" :> Bits32, "XYZ" :> Bits32]

columnsByName : Selection Prices [Bits16, Bits32]
columnsByName = ["Year", "XYZ"]

columnsByIndex : Selection Prices [Bits16, Bits32]
columnsByIndex = [1,3]

trigTable : Table [ "x"      :> Double
                  , "sin(x)" :> Double
                  , "cos(x)" :> Double
                  , "tan(x)" :> Double
                  ]
trigTable = functions (linear 2000 (-4*pi) (4*pi)) [sin,cos,tan]

circle : Table ["x" :> Double, "y" :> Double]
circle = param (linear 2000 (- pi) pi) [sin,cos]
