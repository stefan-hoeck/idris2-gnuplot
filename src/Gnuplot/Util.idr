module Gnuplot.Util

import public Data.List
import public Data.Maybe
import public Data.String

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
