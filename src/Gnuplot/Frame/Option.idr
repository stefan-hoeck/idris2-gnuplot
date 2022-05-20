module Gnuplot.Frame.Option

import Data.SortedMap as SM
import Gnuplot.Util

%default total

||| Every option represents an internal state in gnuplot.
||| It is altered with gnuplot's set command.
||| The first field in 'T' is the name of the option
||| and the name of the according internal state in gnuplot.
||| 
||| Sometimes the addressed state is not explicitly mentioned
||| but is expressed by the syntax of the values.
||| E.g. you can write @set grid xtics@ and @set grid noxtics@,
||| but both commands refer to the same internal boolean variable,
||| that we like to call @xtics@.
||| It is important that the gnuplot Haskell bindings know
||| that these two set commands refer to the same gnuplot state,
||| since we want to simulate a stateless functional interface
||| in front of a stateful imperative one.
||| 
||| In case of a such a hidden state,
||| we manage an identifier in the second field of 'T'.
||| It is mainly used for distinguishing different hidden states,
||| that are accessed by the same @set@ variable.
||| This second field may not contain valid gnuplot identifiers,
||| however you might use the field for formatting boolean options
||| using 'Graphics.Gnuplot.Frame.OptionSet.addBool'.
public export
record Option where
  constructor MkOption
  name : String
  add  : String

toPair : Option -> (String,String)
toPair o = (o.name, o.add)

export
Eq Option where (==) = (==) `on` toPair

export
Ord Option where compare = compare `on` toPair

export
grid   : String -> Option
grid   = MkOption "grid"

export
size   : String -> Option
size   = MkOption "size"

export
key    : String -> Option
key    = MkOption "key"

export
border : String -> Option
border = MkOption "border"

export
pm3d   : String -> Option
pm3d   = MkOption "pm3d"

export
xRange : String -> Option
xRange = MkOption "xrange"

export
yRange : String -> Option
yRange = MkOption "yrange"

export
zRange : String -> Option
zRange = MkOption "zrange"

export
xLabel : String -> Option
xLabel = MkOption "xlabel"

export
yLabel : String -> Option
yLabel = MkOption "ylabel"

export
zLabel : String -> Option
zLabel = MkOption "zlabel"

export
xTicks : String -> Option
xTicks = MkOption "xtics"

export
yTicks : String -> Option
yTicks = MkOption "ytics"

export
zTicks : String -> Option
zTicks = MkOption "ztics"

export
xLogScale : Option
xLogScale = MkOption "logscale x" ""

export
yLogScale : Option
yLogScale = MkOption "logscale y" ""

export
zLogScale : Option
zLogScale = MkOption "logscale z" ""

export
title : Option
title = MkOption "title" ""

export
view  : Option
view  = MkOption "view" ""

export
xFormat : Option
xFormat = MkOption "format x" ""

export
yFormat : Option
yFormat = MkOption "format y" ""

export
zFormat : Option
zFormat = MkOption "format z" ""

export
timeFmt : Option
timeFmt = MkOption "timefmt" ""

export
xData : Option
xData = MkOption "xdata" ""

export
yData : Option
yData = MkOption "ydata" ""

export
zData : Option
zData = MkOption "zdata" ""

export
sizeScale : Option
sizeScale = size "scale"

export
sizeRatio : Option
sizeRatio = size "ratio"

export
keyShow     : Option
keyShow     = key "show"

export
keyPosition : Option
keyPosition = key "position"

export
xRangeBounds : Option
xRangeBounds = xRange "bounds"

export
yRangeBounds : Option
yRangeBounds = yRange "bounds"

export
zRangeBounds : Option
zRangeBounds = zRange "bounds"

export
xLabelText : Option
xLabelText = xLabel "text"

export
yLabelText : Option
yLabelText = yLabel "text"

export
zLabelText : Option
zLabelText = zLabel "text"

export
xTickLabels : Option
xTickLabels = xTicks "labels"

export
yTickLabels : Option
yTickLabels = yTicks "labels"

export
zTickLabels : Option
zTickLabels = zTicks "labels"

export
gridXTicks : Option
gridXTicks = grid "xtics"

export
gridYTicks : Option
gridYTicks = grid "ytics"

export
gridZTicks : Option
gridZTicks = grid "ztics"

export
boxwidth : Option
boxwidth = MkOption "boxwidth" ""

export
styleFillSolid  : Option
styleFillSolid  = MkOption "style fill" "solid"

export
styleFillBorder : Option
styleFillBorder = MkOption "style fill" "border"

export
styleHistogram  : Option
styleHistogram  = MkOption "style histogram" ""

--------------------------------------------------------------------------------
--          OptionSet
--------------------------------------------------------------------------------

public export
0 Plain : Type
Plain = SortedMap Option (List String)

public export
record OptionSet (graph : Type) where
  constructor MkOptionSet
  plain : Plain

||| The default options contain what we expect as default value in gnuplot.
||| We need an entry for every option that cannot be reset by @unset@.
export
deflt : Plain
deflt = fromList [
    (keyShow, [])
  , (sizeRatio, ["noratio"])
  , (xLabelText, [])
  , (yLabelText, [])
  , (zLabelText, [])
  , (xRangeBounds, ["[*:*]"])
  , (yRangeBounds, ["[*:*]"])
  , (zRangeBounds, ["[*:*]"])
  , (xFormat, [])
  , (yFormat, [])
  , (zFormat, [])
  , (xTickLabels, [])
  , (yTickLabels, [])
  , (zTickLabels, [])
  , (gridXTicks, ["noxtics"])
  , (gridYTicks, ["noytics"])
  , (gridZTicks, ["noztics"])
  , (styleFillSolid, ["0"])
  , (styleFillBorder, [])
  , (styleHistogram, ["clustered"])
  , (boxwidth, [])
  ]

export
initial : Plain
initial = merge deflt $ fromList [
    (xData, [])
  , (yData, [])
  , (zData, [])
  ]

||| Add (set) an option with arguments as plain strings.
||| 
||| This is very flexible, but not very safe.
||| Use it only as fall-back,
||| if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
export
add : Option -> List String -> OptionSet graph -> OptionSet graph
add opt args = {plain $= insert opt args}

||| Remove (unset) an option.
||| 
||| This is very flexible, but not very safe.
||| Use it only as fall-back,
||| if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
export
remove : Option -> OptionSet graph -> OptionSet graph
remove opt = {plain $= delete opt}

0 DiffTrpl : Type
DiffTrpl = (Option, Maybe $ List String, Maybe $ List String)

||| Convert the difference between the first and the second option set
||| into a sequence of 'set' and 'unset' commands.
export
diffToString : Plain -> Plain -> List String
diffToString m0 m1 = mapMaybe toString merged
  where merged : List DiffTrpl
        merged = SM.toList $ mergeWith (\x,y => (fst x, snd y))
                   ((,Nothing) . Just <$> m0)
                   ((Nothing,) . Just <$> m1)

        toString : DiffTrpl -> Maybe String
        toString (MkOption p _, o, n) = toMaybe (o /= n) $
          maybe "unset \{p}" (\as => "set \{p} \{unwords as}") n

||| Set or unset option according to a 'Bool'.
||| This is for switches that can be disabled using @unset@.
||| 
||| This is very flexible, but not very safe.
||| Use it only as fall-back,
||| if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
||| 
||| See also: 'addBool', 'add', 'remove'.
export
boolean : Option -> Bool -> OptionSet graph -> OptionSet graph
boolean opt True  = add opt []
boolean opt False = remove opt

||| Add an option with boolean value
||| that is formatted like @set style fill border@ and @set style fill noborder@.
||| The name of the internal state (i.e. @border@)
||| must be stored in the second field of the option.
||| 
||| This is very flexible, but not very safe.
||| Use it only as fall-back,
||| if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
||| 
||| See also 'boolean'.
export
addBool : Option -> Bool -> OptionSet graph -> OptionSet graph
addBool opt arg = add opt [formatBool arg opt.add]
