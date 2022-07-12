module Gnuplot.Schema.Expr

import Gnuplot.Schema
import Gnuplot.Util

%default total

infixr 10 ^

public export
data FloFun =
    Exp
  | Log
  | Sin
  | Cos
  | Tan

export
Interpolation FloFun where
  interpolate Exp = "exp"
  interpolate Log = "log"
  interpolate Sin = "sin"
  interpolate Cos = "cos"
  interpolate Tan = "tan"

public export
data NumOp = Plus | Mult | Pow

export
Interpolation NumOp where
  interpolate Plus = "+"
  interpolate Mult = "*"
  interpolate Pow  = "**"

public export
data IntUnary = Fact | Compl

public export
data IntBinary = Mod | Shr | Shl | And | Or | Xor

export
Interpolation IntBinary where
  interpolate Mod  = "%"
  interpolate Shr  = ">>"
  interpolate Shl  = "<<"
  interpolate And  = "&"
  interpolate Or   = "|"
  interpolate Xor  = "^"

public export
data BoolUnary = Not

public export
data BoolBinary = LAnd | LOr

export
Interpolation BoolBinary where
  interpolate LAnd = "&&"
  interpolate LOr  = "||"


public export
data Comp = Eq | Neq | GT | LT | GTE | LTE

export
Interpolation Comp where
  interpolate Eq  = "=="
  interpolate Neq = "!="
  interpolate GT  = ">"
  interpolate LT  = "<"
  interpolate GTE = ">="
  interpolate LTE = "<="

public export
data Expr : (s : Schema) -> (t : Type) -> Type where
  X      : Expr s t
  Lit    : (lit : t) -> Expr s t
  NumCol : (0 num : Num t) => Sel s t -> Expr s t
  StrCol : Sel s String -> Expr s String
  Flo    : FloFun -> Expr s Double -> Expr s Double
  Arith  : (0 num : Num t) => NumOp -> Expr s t -> Expr s t -> Expr s t
  Minus  : (0 neg : Neg t) => Expr s t -> Expr s t -> Expr s t
  Int1   : (0 int : Integral t) => IntUnary -> Expr s t -> Expr s t
  Int2   : (0 int : Integral t) => IntBinary -> Expr s t -> Expr s t -> Expr s t
  Bool1  : BoolUnary -> Expr s Bool -> Expr s Bool
  Bool2  : BoolBinary -> Expr s Bool -> Expr s Bool -> Expr s Bool
  Div    : (0 fra : Fractional t) => Expr s t -> Expr s t -> Expr s t
  Cmp    : (0 num : Num t) => Atom t => Comp -> Expr s t -> Expr s t -> Expr s Bool
  Ite    : Expr s Bool -> Expr s t -> Expr s t -> Expr s t
  Concat : Expr s String -> Expr s String -> Expr s String

export
Num t => Num (Expr s t) where
  fromInteger = Lit . fromInteger
  (+) = Arith Plus
  (*) = Arith Mult

export
Neg t => Neg (Expr s t) where
  (-)    = Minus
  negate = Minus 0

export
Fractional t => Fractional (Expr s t) where
  (/)    = Div

export
FromDouble t => FromDouble (Expr s t) where
  fromDouble = Lit . fromDouble

export
FromString t => FromString (Expr s t) where
  fromString = Lit . fromString

export
(^) : Num t => Expr s t -> Expr s t -> Expr s t
(^) = Arith Pow

export
hasVar : Expr s t -> Bool
hasVar X             = False
hasVar (Lit lit)     = False
hasVar (NumCol x)    = True
hasVar (StrCol x)    = True
hasVar (Flo x y)     = hasVar y
hasVar (Arith x y z) = hasVar y || hasVar z
hasVar (Minus x y)   = hasVar x || hasVar y
hasVar (Div x y)     = hasVar x || hasVar y
hasVar (Bool1 _ x)   = hasVar x
hasVar (Bool2 _ x y) = hasVar x || hasVar y
hasVar (Int1 _ x)    = hasVar x
hasVar (Int2 _ x y)  = hasVar x || hasVar y
hasVar (Cmp _ x y)   = hasVar x || hasVar y
hasVar (Ite x y z)   = hasVar x || hasVar y || hasVar z
hasVar (Concat x y)  = hasVar x || hasVar y

ip : Atom t => Expr s t -> String
ip X              = "x"
ip (Lit lit)      = print lit
ip (NumCol x)     = "column(\{x})"
ip (StrCol x)     = "strcol(\{x})"
ip (Flo x y)      = "\{x}(\{ip y})"
ip (Arith op x y) = "(\{ip x}) \{op} (\{ip y})"
ip (Minus x y)    = "(\{ip x}) - (\{ip y})"
ip (Div x y)      = "(\{ip x}) / (\{ip y})"
ip (Bool1 Not x)  = "!(\{ip x})"
ip (Int1 Fact x)  = "(\{ip x})!"
ip (Int1 Compl x) = "~(\{ip x})"
ip (Int2 op x y)  = "(\{ip x}) \{op} (\{ip y})"
ip (Bool2 op x y) = "(\{ip x}) \{op} (\{ip y})"
ip (Cmp op x y)   = "(\{ip x}) \{op} (\{ip y})"
ip (Ite x y z)    = "(\{ip x}) ? (\{ip y}) : (\{ip z})"
ip (Concat x y)   = "(\{ip x}).(\{ip y})"

export %inline
Atom t => Interpolation (Expr s t) where
  interpolate       = ip

||| A selection of columns in a table.
public export
data Selection : (s : Schema) -> List Type -> Type where
  Nil  : Selection s []
  (::) : Expr s t -> Selection s ts -> Selection s (t :: ts)

export
(as : NP Atom ts) => Interpolation (Selection s ts) where
  interpolate []                      = ""
  interpolate {as = [_]}    [x]       = interpolate x
  interpolate {as = _ :: _} (h :: t)  = "(\{h}):(\{t})"

public export
0 SchemaTypes : Schema -> List Type
SchemaTypes []        = []
SchemaTypes (_ :> t :: cs) = t :: SchemaTypes cs

export
inc : Expr s t -> Expr (h :: s) t
inc X              = X
inc (Lit lit)      = Lit lit
inc (NumCol x)     = NumCol $ inc x
inc (StrCol x)     = StrCol $ inc x
inc (Flo x y)      = Flo x $ inc y
inc (Arith x y z)  = Arith x (inc y) (inc z)
inc (Minus x y)    = Minus (inc x) (inc y)
inc (Div x y)      = Div (inc x) (inc y)
inc (Bool1 x y)    = Bool1 x (inc y)
inc (Bool2 op x y) = Bool2 op (inc x) (inc y)
inc (Int1 x y)     = Int1 x (inc y)
inc (Int2 op x y)  = Int2 op (inc x) (inc y)
inc (Cmp  op x y)  = Cmp op (inc x) (inc y)
inc (Ite  x y z)   = Ite (inc x) (inc y) (inc z)
inc (Concat x y)   = Concat (inc x) (inc y)

--------------------------------------------------------------------------------
--          Syntax
--------------------------------------------------------------------------------

infixl 8 `shiftL`, `shiftR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

export %inline
mod : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
mod = Int2 Mod

export %inline
shiftL : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
shiftL = Int2 Shl

export %inline
shiftR : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
shiftR = Int2 Shr

export %inline
(.&.) : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
(.&.) = Int2 And

export %inline
(.|.) : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
(.|.) = Int2 Or

export %inline
xor : (0 _ : Integral t) => Expr s t -> Expr s t -> Expr s t
xor = Int2 Xor

export %inline
complement : (0 _ : Integral t) => Expr s t -> Expr s t
complement = Int1 Compl

export %inline
factorial : (0 _ : Integral t) => Expr s t -> Expr s t
factorial = Int1 Fact

export %inline
ifelse : Expr s Bool -> Expr s t -> Expr s t -> Expr s t
ifelse = Ite

export %inline
(==) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(==) = Cmp Eq

export %inline
(/=) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(/=) = Cmp Neq

export %inline
(>=) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(>=) = Cmp GTE

export %inline
(<=) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(<=) = Cmp LTE

export %inline
(>) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(>) = Cmp GT

export %inline
(<) : (0 _ : Num t) => Atom t => Expr s t -> Expr s t -> Expr s Bool
(<) = Cmp LT

export %inline
(&&) : Expr s Bool -> Expr s Bool -> Expr s Bool
(&&) = Bool2 LAnd

export %inline
(||) : Expr s Bool -> Expr s Bool -> Expr s Bool
(||) = Bool2 LOr

export %inline
not : Expr s Bool -> Expr s Bool
not = Bool1 Not

export %inline
sin : Expr s Double -> Expr s Double
sin = Flo Sin

export %inline
cos : Expr s Double -> Expr s Double
cos = Flo Cos

export %inline
tan : Expr s Double -> Expr s Double
tan = Flo Tan

export %inline
exp : Expr s Double -> Expr s Double
exp = Flo Exp

export %inline
log : Expr s Double -> Expr s Double
log = Flo Log

export %inline
col : (0 num : Num t) => Sel s t -> Expr s t
col = NumCol

export %inline
strcol : Sel s String -> Expr s String
strcol = StrCol

--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

Prices : Schema
Prices = ["Year" :> Bits16, "PQR" :> Bits32, "XYZ" :> Bits32]

columnsByName : Selection Prices [Bits16, Bits32]
columnsByName = [col "Year", col "XYZ"]

columnsByIndex : Selection Prices [Bits16, Bits32]
columnsByIndex = [col 1, col 3]
