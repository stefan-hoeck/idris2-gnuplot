module Gnuplot.Value

import Gnuplot.Frame.Option

%default total

public export
record Options (a : Type) where
  constructor MkOptions
  optData : List String
  format  : List String
  others  : List (Option, List String)

public export
data GTpe : Type where
  GDbl : GTpe
  GInt : GTpe
  GNat : GTpe
  W8   : GTpe
  W16  : GTpe
  W32  : GTpe
  W64  : GTpe
  I8   : GTpe
  I16  : GTpe
  I32  : GTpe
  I64  : GTpe

public export
0 IdrisType : GTpe -> Type
IdrisType GDbl = Double
IdrisType GInt = Integer
IdrisType GNat = Nat
IdrisType W8   = Bits8
IdrisType W16  = Bits16
IdrisType W32  = Bits32
IdrisType W64  = Bits64
IdrisType I8   = Int8
IdrisType I16  = Int16
IdrisType I32  = Int32
IdrisType I64  = Int64


public export
interface Atom a where
  options : Options a
  options = MkOptions [] [] []

export Atom Double where
export Atom Integer where
export Atom Nat where
export Atom Bits8 where
export Atom Bits16 where
export Atom Bits32 where
export Atom Bits64 where
export Atom Int8 where
export Atom Int16 where
export Atom Int32 where
export Atom Int64 where

-- {- |
-- We provide a way to specify a set of columns
-- that matches the tuple structure of a certain graph type.
-- -}
-- module Graphics.Gnuplot.Value.ColumnSet
--    (T(Cons), atom, pair, triple, quadruple,
--    ) where
-- 
-- import qualified Graphics.Gnuplot.Value.Atom  as Atom
-- 
-- 
-- newtype T a = Cons [Int]
-- 
-- {-
-- Functor and Applicative instances would be useful
-- for combining column sets,
-- but they are dangerous, because they can bring
-- type and column number out of sync.
-- 
-- instance Functor Column where
--    fmap _ (Cons n) = Cons n
-- 
-- instance Applicative Column where
--    pure _ = Cons []
--    Cons ns <*> Cons ms = Cons (ns++ms)
-- -}
-- 
-- atom :: Atom.C a => Int -> T a
-- atom i = Cons [i]
-- 
-- pair :: T a -> T b -> T (a,b)
-- pair (Cons ai) (Cons bi) = Cons (ai++bi)
-- 
-- triple :: T a -> T b -> T c -> T (a,b,c)
-- triple (Cons ai) (Cons bi) (Cons ci) = Cons (ai++bi++ci)
-- 
-- quadruple :: T a -> T b -> T c -> T d -> T (a,b,c,d)
-- quadruple (Cons ai) (Cons bi) (Cons ci) (Cons di) = Cons (ai++bi++ci++di)

-- {- |
-- Provide a class that renders multiple Haskell values in a text form
-- that is accessible by gnuplot.
-- 
-- Maybe we add a method for the binary interface to gnuplot later.
-- -}
-- module Graphics.Gnuplot.Value.Tuple (
--    C(text, columnCount),
--    ColumnCount(ColumnCount),
--    ) where
-- 
-- import Data.Time.Format (defaultTimeLocale, )
-- import qualified Data.Time as Time
-- 
-- import Data.Word (Word8, Word16, Word32, Word64, )
-- import Data.Int (Int8, Int16, Int32, Int64, )
-- import Data.Ratio (Ratio, )
-- 
-- import Data.Function (id, ($), (.), )
-- import Text.Show (Show, ShowS, shows, showString, )
-- import Prelude (Eq, Ord, Int, Integer, Integral, Float, Double, realToFrac, (+), (++), )
-- 
-- 
-- class C a where
--    {- |
--    For values that are also in Atom class,
--    'text' must generate a singleton list.
--    -}
--    text :: a -> [ShowS]
-- 
--    {- |
--    It must hold @ColumnCount (length (text x)) == columnCount@.
--    -}
--    columnCount :: ColumnCount a
--    columnCount = ColumnCount 1
-- 
-- {- |
-- Count numbers of gnuplot data columns for the respective type.
-- 
-- Somehow a writer monad with respect to Sum monoid
-- without material monadic result.
-- 
-- Cf. ColumnSet module.
-- -}
-- newtype ColumnCount a = ColumnCount Int
--    deriving (Eq, Ord, Show)
-- 
-- {-
-- Functor and Applicative instances would be useful
-- for combining column sets,
-- but they are dangerous, because they can bring
-- type and column columnCount out of sync.
-- -}
-- 
-- pure :: a -> ColumnCount a
-- pure _ = ColumnCount 0
-- 
-- (<*>) :: ColumnCount (a -> b) -> ColumnCount a -> ColumnCount b
-- ColumnCount n <*> ColumnCount m = ColumnCount (n+m)
-- 
-- 
-- singleton :: a -> [a]
-- singleton = (:[])
-- 
-- 
-- instance C Float   where text = singleton . shows
-- instance C Double  where text = singleton . shows
-- instance C Int     where text = singleton . shows
-- instance C Integer where text = singleton . shows
-- instance (Integral a) => C (Ratio a) where
--    text = singleton . shows . (id :: Double->Double) . realToFrac
-- 
-- instance C Int8  where text = singleton . shows
-- instance C Int16 where text = singleton . shows
-- instance C Int32 where text = singleton . shows
-- instance C Int64 where text = singleton . shows
-- instance C Word8  where text = singleton . shows
-- instance C Word16 where text = singleton . shows
-- instance C Word32 where text = singleton . shows
-- instance C Word64 where text = singleton . shows
-- 
-- instance C Time.Day where
--    text d = text $ Time.UTCTime d 0
-- instance C Time.UTCTime where
--    text = singleton . showString . Time.formatTime defaultTimeLocale "%s"
-- 
-- 
-- instance (C a, C b) => C (a,b) where
--    text (a,b) = text a ++ text b
--    columnCount =
--       pure (,)
--          <*> columnCount
--          <*> columnCount
-- 
-- instance (C a, C b, C c) => C (a,b,c) where
--    text (a,b,c) = text a ++ text b ++ text c
--    columnCount =
--       pure (,,)
--          <*> columnCount
--          <*> columnCount
--          <*> columnCount
-- 
-- instance (C a, C b, C c, C d) => C (a,b,c,d) where
--    text (a,b,c,d) = text a ++ text b ++ text c ++ text d
--    columnCount =
--       pure (,,,)
--          <*> columnCount
--          <*> columnCount
--          <*> columnCount
--          <*> columnCount
