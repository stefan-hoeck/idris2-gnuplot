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

||| Types supported in Gnuplot expressions
public export
data Universe : Type where
  ||| An unsigned integer
  GNat : Universe

  ||| A signed integer
  GInt : Universe

  ||| An eight bit value often used when working with RGB colors
  GBits8 : Universe

  ||| An floating point number
  GDbl : Universe

  ||| A boolean
  GBool : Universe

  ||| A string
  GString : Universe

||| Idris type corresponding to the types known to Gnuplot
public export
0 IdrisType : Universe -> Type
IdrisType GNat    = Nat
IdrisType GInt    = Integer
IdrisType GBits8  = Bits8
IdrisType GDbl    = Double
IdrisType GBool   = Bool
IdrisType GString = String

||| Witness that a Gnuplot type is a number
public export
data Num : Universe -> Type where
  NumNat   : Num GNat
  NumInt   : Num GInt
  NumBits8 : Num GBits8
  NumDbl   : Num GDbl

||| Witness that a Gnuplot type has an additive inverse
public export
data Neg : Universe -> Type where
  NegInt   : Neg GInt
  NegBits8 : Neg GBits8
  NegDbl   : Neg GDbl

||| Witness that a Gnuplot type is an integer
public export
data Integral : Universe -> Type where
  IntNat   : Integral GNat
  IntInt   : Integral GInt
  IntBits8 : Integral GBits8

public export %hint
integralIsNum : {0 u : Universe} -> (prf : Integral u) => Num u
integralIsNum {prf = IntNat}   = NumNat
integralIsNum {prf = IntInt}   = NumInt
integralIsNum {prf = IntBits8} = NumBits8

public export %hint
negIsNum : {0 u : Universe} -> (prf : Neg u) => Num u
negIsNum {prf = NegDbl}   = NumDbl
negIsNum {prf = NegInt}   = NumInt
negIsNum {prf = NegBits8} = NumBits8

export
print : (u : Universe) -> IdrisType u -> String
print GNat x      = show x
print GInt x      = show x
print GBits8 x    = show x
print GDbl x      = show x
print GBool True  = "1"
print GBool False = "0"
print GString x   = quote x

--------------------------------------------------------------------------------
--          Data Tables
--------------------------------------------------------------------------------

infixr 8 :>

||| A single column in a schema
public export
record Column where
  constructor (:>)
  name : String
  type : Universe

public export
0 ColType : Column -> Type
ColType = IdrisType . type

||| A table schema
public export
0 Schema : Type
Schema = List Column

||| A row in a table
public export
0 Row : (cols : Schema) -> Type
Row cols = NP_ Column ColType cols

||| A table of rows
public export
0 Table : Schema -> Type
Table = List . Row

export
printRow : {s : _} -> Row s -> String
printRow [] = ""
printRow {s = [c]}    [v]       = print c.type v
printRow {s = c :: _} (v :: vs) = print c.type v ++ " " ++ printRow vs

export
printTable : {s : _} -> Table s -> String
printTable = unlines . map printRow

--------------------------------------------------------------------------------
--          Row Selection
--------------------------------------------------------------------------------

||| Proof that a column of the given name and type is in the schema.
public export
data InSchema : (n : String) -> (cs : Schema) -> (u : Universe) -> Type where
  [search n cs]
  Here  : InSchema n (n :> u :: cs) u
  There : InSchema n cs u -> InSchema n (c :: cs) u

toNat : InSchema n cs u -> Nat
toNat Here      = 1
toNat (There p) = S $ toNat p

public export
columnName : {s : _} -> InSchema n s u -> String
columnName {s = n :> _ :: _} Here      = n
columnName {s = _ :: _}      (There x) = columnName x


||| Proof that a row with the given (1-based) index in the schema
public export
data IxInSchema : (x : Nat) -> (cs : Schema) -> (u : Universe) -> Type where
  [search x cs]
  IxHere  : IxInSchema 1 (n :> u :: cs) u
  IxThere : IxInSchema x cs t -> IxInSchema (S x) (c :: cs) u

||| A column in a Schema, selected by name
public export
record Sel (s : Schema) (u : Universe) where
  constructor MkSel
  0 name : String
  prf    : InSchema name s t

public export
inc : Sel s u -> Sel (c :: s) u
inc (MkSel n p) = MkSel n (There p)

public export
colName : {s : _} -> Sel s u -> String
colName sel = columnName sel.prf

export
Interpolation (Sel s u) where
  interpolate (MkSel _ p) = show $ toNat p

public export
fromIndex : IxInSchema x s u -> Sel s u
fromIndex IxHere      = MkSel _ Here
fromIndex (IxThere y) = let MkSel n p = fromIndex y in MkSel n (There p)

||| We can conveniently select columns by name
||| using string literals.
public export
fromString : (0 n : String) -> (prf : InSchema n s u) => Sel s u
fromString n = MkSel n prf

||| We can conveniently select columns by name
||| using integer literals.
public export
fromInteger :  (0 x : Integer)
            -> (prf : IxInSchema (cast x) s u)
            => Sel s u
fromInteger _ = fromIndex prf 

--------------------------------------------------------------------------------
--          Creating Tables
--------------------------------------------------------------------------------

export
functions :  (u : Universe)
          -> List (IdrisType u)
          -> NP_ Column (\c => IdrisType u -> ColType c) cs
          -> Table (n :> u :: cs)
functions u xs fs = map (\v =>  v :: mapNP ($ v) fs) xs

export
param :  List a
      -> NP_ Column (\c => a -> ColType c) cs
      -> Table cs
param xs fs = map (\v => mapNP ($ v) fs) xs

--------------------------------------------------------------------------------
--          Examples
--------------------------------------------------------------------------------

trigTable : Table [ "x"      :> GDbl
                  , "sin(x)" :> GDbl
                  , "cos(x)" :> GDbl
                  , "tan(x)" :> GDbl
                  ]
trigTable = functions GDbl (linear 2000 (-4*pi) (4*pi)) [sin,cos,tan]

circle : Table ["x" :> GDbl, "y" :> GDbl]
circle = param (linear 2000 (- pi) pi) [sin,cos]
