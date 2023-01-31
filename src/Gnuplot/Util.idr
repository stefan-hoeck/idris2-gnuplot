module Gnuplot.Util

import public Data.List
import public Data.Maybe
import public Data.SOP
import public Data.SortedMap as SM
import public Data.String
import public Data.Vect

%default total

export
quote : String -> String
quote = show

export
formatBool : Bool -> String -> String
formatBool True s = s
formatBool False s = "no\{s}"

export
formatBoolM : Maybe Bool -> String -> Maybe String
formatBoolM mb s = map (`formatBool` s) mb

export
asList : (a -> b) -> Maybe a -> List b
asList f = toList . map f

export
commaConcat : List String -> String
commaConcat = fastConcat . intersperse ", "

export
semiColonConcat : List String -> String
semiColonConcat = fastConcat . intersperse "; "

export
assembleCells : Foldable t => t (List String) -> String
assembleCells = unlines . map commaConcat . toList

export %inline
concatList : List (List a) -> List a
concatList = join

export
fromIx : (n : Nat) -> (Nat -> a) -> List a
fromIx n f = go 0 n
  where go : (x : Nat) -> (m : Nat) -> List a
        go _ 0     = []
        go x (S k) = f x :: go (S x) k

export
linear :  Neg a 
       => Fractional a
       => (n : Nat)
       -> (0 _ : IsSucc n)
       => (x0, x1 : a)
       -> List a
linear n x0 x1 =
  let n'   = the a (fromInteger $ cast n)
      diff = x1 - x0
   in fromIx (S n) (\x => x0 + (fromInteger $ cast x) * diff / n')

export %inline
Interpolation Nat where interpolate = show
