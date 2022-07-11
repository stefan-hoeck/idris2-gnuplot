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
numOp : NumOp -> String
numOp Plus = "+"
numOp Mult = "*"
numOp Pow  = "**"

public export
data Expr : (s : Schema) -> (t : Type) -> Type where
  X     : Expr s t
  Lit   : (lit : t) -> Expr s t
  Var   : Sel s t -> Expr s t
  Flo   : FloFun -> Expr s Double -> Expr s Double
  Arith : (0 num : Num t) => NumOp -> Expr s t -> Expr s t -> Expr s t
  Minus : (0 neg : Neg t) => Expr s t -> Expr s t -> Expr s t
  Div   : (0 fra : Fractional t) => Expr s t -> Expr s t -> Expr s t

export
Num t => Num (Expr s t) where
  fromInteger = Lit . fromInteger
  (+) = Arith Plus
  (*) = Arith Plus

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
(^) : Num t => Expr s t -> Expr s t -> Expr s t
(^) = Arith Pow

export
hasVar : Expr s t -> Bool
hasVar X             = False
hasVar (Lit lit)     = False
hasVar (Var x)       = True
hasVar (Flo x y)     = hasVar y
hasVar (Arith x y z) = hasVar y || hasVar z
hasVar (Minus x y)   = hasVar x || hasVar y
hasVar (Div x y)     = hasVar x || hasVar y

export
Atom t => Interpolation (Expr s t) where
  interpolate X              = "x"
  interpolate (Lit lit)      = print lit
  interpolate (Var x)        = "\{x}"
  interpolate (Flo x y)      = "\{x}(\{y})"
  interpolate (Arith op x y) = "(\{x}) \{numOp op} (\{y})"
  interpolate (Minus x y)    = "(\{x}) - (\{y})"
  interpolate (Div x y)      = "(\{x}) / (\{y})"

||| A selection of columns in a table.
public export
data Selection : (s : Schema) -> List Type -> Type where
  Nil  : Selection s []
  (::) : Expr s t -> Selection s ts -> Selection s (t :: ts)

export
(as : NP Atom ts) => Interpolation (Selection s ts) where
  interpolate []                      = ""
  interpolate {as = [_]}    [x]       = interpolate x
  interpolate {as = _ :: _} (h :: t)  = "\{h}:\{t}"

public export
0 SchemaTypes : Schema -> List Type
SchemaTypes []        = []
SchemaTypes (_ :> t :: cs) = t :: SchemaTypes cs

export
inc : Expr s t -> Expr (h :: s) t
inc X             = X
inc (Lit lit)     = Lit lit
inc (Var x)       = Var $ inc x
inc (Flo x y)     = Flo x $ inc y
inc (Arith x y z) = Arith x (inc y) (inc z)
inc (Minus x y)   = Minus (inc x) (inc y)
inc (Div x y)     = Div (inc x) (inc y)


--------------------------------------------------------------------------------
--          Example
--------------------------------------------------------------------------------

Prices : Schema
Prices = ["Year" :> Bits16, "PQR" :> Bits32, "XYZ" :> Bits32]

columnsByName : Selection Prices [Bits16, Bits32]
columnsByName = [Var "Year", Var "XYZ"]

columnsByIndex : Selection Prices [Bits16, Bits32]
columnsByIndex = [Var 1, Var 3]
