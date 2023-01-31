module Gnuplot.Schema.Expr

import Gnuplot.Schema
import Gnuplot.Util

%default total

infixr 10 ^

public export
0 I : Universe -> Universe
I u = u

public export
0 Const : Universe -> Universe -> Universe
Const u _ = u

public export
data Fun :  (constraint : Universe -> Type)
         -> (result     : Universe -> Universe)
         -> Type where
    Exp   : Fun Num (Const GDbl)
    Log   : Fun Num (Const GDbl)
    Sin   : Fun Num (Const GDbl)
    Cos   : Fun Num (Const GDbl)
    Tan   : Fun Num (Const GDbl)
    Fact  : Fun (=== GNat) I
    Compl : Fun Integral I
    Not   : Fun (=== GBool) I

export
Interpolation (Fun c r) where
  interpolate Exp   = "exp"
  interpolate Log   = "log"
  interpolate Sin   = "sin"
  interpolate Cos   = "cos"
  interpolate Tan   = "tan"
  interpolate Fact  = "!"
  interpolate Compl = "~"
  interpolate Not   = "!"

public export
data BinOp :  (constraint : Universe -> Type)
           -> (result     : Universe -> Universe)
           -> Type where
  Plus  : BinOp Num I
  Mult  : BinOp Num I
  Pow   : BinOp Num I
  Minus : BinOp Neg I
  Div   : BinOp Num (Const GDbl)
  Mod   : BinOp Integral I
  Shr   : BinOp Integral I
  Shl   : BinOp Integral I
  And   : BinOp Integral I
  Or    : BinOp Integral I
  Xor   : BinOp Integral I
  LAnd  : BinOp (=== GBool) I
  LOr   : BinOp (=== GBool) I
  Eq    : BinOp Num (Const GBool)
  Neq   : BinOp Num (Const GBool)
  GT    : BinOp Num (Const GBool)
  LT    : BinOp Num (Const GBool)
  GTE   : BinOp Num (Const GBool)
  LTE   : BinOp Num (Const GBool)
  Dot   : BinOp (=== GString) (Const GString)

export
Interpolation (BinOp c r) where
  interpolate Plus  = "+"
  interpolate Mult  = "*"
  interpolate Pow   = "**"
  interpolate Div   = "/"
  interpolate Minus = "-"
  interpolate Mod   = "%"
  interpolate Shr   = ">>"
  interpolate Shl   = "<<"
  interpolate And   = "&"
  interpolate Or    = "|"
  interpolate Xor   = "^"
  interpolate LAnd  = "&&"
  interpolate LOr   = "||"
  interpolate Eq    = "=="
  interpolate Neq   = "!="
  interpolate GT    = ">"
  interpolate LT    = "<"
  interpolate GTE   = ">= "
  interpolate LTE   = "<="
  interpolate Dot   = "."

public export
data FunctionType : Type where 
  Regular    : FunctionType
  Parametric : FunctionType

public export
data Expr : (t : FunctionType) -> (s : Schema) -> (u : Universe) -> Type where
  T     : Expr Parametric [] u
  X     : Expr Regular [] u
  ColNr : Expr t s GNat
  Lit   : (u : Universe) -> (lit : IdrisType u) -> Expr t s u
  NCol  : (0 _ : Num u) => Sel s u -> Expr t s u
  SCol  : Sel s GString -> Expr t s GString
  EFun  : Fun c f -> Expr t s u -> (0 _ : c u) => Expr t s (f u)
  EOp   : BinOp c f -> Expr t s u -> Expr t s u -> (0 _ : c u) => Expr t s (f u)
  Ite   : Expr t s GBool -> Expr t s u -> Expr t s u -> Expr t s u

export
{u : _} -> Num u => Num (Expr t s u) where
  fromInteger {u = GNat}   = Lit GNat . fromInteger
  fromInteger {u = GInt}   = Lit GInt . fromInteger
  fromInteger {u = GBits8} = Lit GBits8 . fromInteger
  fromInteger {u = GDbl}   = Lit GDbl . fromInteger
  fromInteger {u = GBool} impossible
  fromInteger {u = GString} impossible

  x + y = EOp Plus x y
  x * y = EOp Mult x y

export
{u : _} -> Neg u => Neg (Expr t s u) where
  x - y    = EOp Minus x y
  negate v = EOp Minus 0 v

export %inline
(/) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GDbl
x / y = EOp Div x y

export %inline
FromDouble (Expr t s GDbl) where
  fromDouble = Lit GDbl

export %inline
FromString (Expr t s GString) where
  fromString = Lit GString

export %inline
(^) : Num u => Expr t s u -> Expr t s u -> Expr t s u
x ^ y = EOp Pow x y

ip : Expr t s u -> String
ip T              = "t"
ip X              = "x"
ip ColNr          = "column(0)"
ip (Lit u lit)    = print u lit
ip (NCol x)       = "column(\{x})"
ip (SCol x)       = "strcol(\{x})"
ip (EFun Fact x)  = "(\{ip x})!"
ip (EFun f x)     = "\{f}(\{ip x})"
ip (EOp op x y)   = "(\{ip x}) \{op} (\{ip y})"
ip (Ite x y z)    = "(\{ip x}) ? (\{ip y}) : (\{ip z})"

export %inline
Interpolation (Expr t s u) where
  interpolate       = ip

export
inc : Expr t (x :: s) u -> Expr t (h :: x :: s) u
inc ColNr        = ColNr
inc (Lit u lit)  = Lit u lit
inc (NCol x)     = NCol $ inc x
inc (SCol x)     = SCol $ inc x
inc (EFun x y)   = EFun x $ inc y
inc (EOp x y z)  = EOp x (inc y) (inc z)
inc (Ite x y z)  = Ite (inc x) (inc y) (inc z)

--------------------------------------------------------------------------------
--          Syntax
--------------------------------------------------------------------------------

infixl 8 `shiftL`, `shiftR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

export %inline
mod : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
mod x y = EOp Mod x y

export %inline
shiftL : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
shiftL x y = EOp Shl x y

export %inline
shiftR : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
shiftR x y = EOp Shr x y

export %inline
(.&.) : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
(.&.) x y = EOp And x y

export %inline
(.|.) : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
(.|.) x y = EOp Or x y

export %inline
xor : (0 _ : Integral u) => Expr t s u -> Expr t s u -> Expr t s u
xor x y = EOp Xor x y

export %inline
complement : (0 _ : Integral u) => Expr t s u -> Expr t s u
complement x = EFun Compl x

export %inline
factorial : Expr t s GNat -> Expr t s GNat
factorial v = EFun Fact v

export %inline
ifelse : Expr t s GBool -> Expr t s u -> Expr t s u -> Expr t s u
ifelse = Ite

export %inline
(==) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(==) x y = EOp Eq x y

export %inline
(/=) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(/=) x y = EOp Neq x y

export %inline
(>=) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(>=) x y = EOp GTE x y

export %inline
(<=) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(<=) x y = EOp LTE x y

export %inline
(>) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(>) x y = EOp GT x y

export %inline
(<) : (0 _ : Num u) => Expr t s u -> Expr t s u -> Expr t s GBool
(<) x y = EOp LT x y

export %inline
(&&) : Expr t s GBool -> Expr t s GBool -> Expr t s GBool
(&&) x y = EOp LAnd x y

export %inline
(||) : Expr t s GBool -> Expr t s GBool -> Expr t s GBool
(||) x y = EOp LOr x y

export %inline
not : Expr t s GBool -> Expr t s GBool
not x = EFun Not x

export %inline
sin : (0 _ : Num u) => Expr t s u -> Expr t s GDbl
sin x = EFun Sin x

export %inline
cos : (0 _ : Num u) => Expr t s u -> Expr t s GDbl
cos x = EFun Cos x

export %inline
tan : (0 _ : Num u) => Expr t s u -> Expr t s GDbl
tan x = EFun Tan x

export %inline
exp : (0 _ : Num u) => Expr t s u -> Expr t s GDbl
exp x = EFun Exp x

export %inline
log : (0 _ : Num u) => Expr t s u -> Expr t s GDbl
log x = EFun Log x

public export %inline
col : (0 num : Num u) => Sel s u -> Expr t s u
col = NCol

public export %inline
colNr : Expr t s GNat
colNr = ColNr

public export %inline
strcol : Sel s GString -> Expr t s GString
strcol = SCol
