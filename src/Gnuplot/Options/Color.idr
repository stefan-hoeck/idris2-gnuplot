module Gnuplot.Options.Color

import Gnuplot.Util

%default total

hexChar : Bits8 -> Char
hexChar 0  = '0'
hexChar 1  = '1'
hexChar 2  = '2'
hexChar 3  = '3'
hexChar 4  = '4'
hexChar 5  = '5'
hexChar 6  = '6'
hexChar 7  = '7'
hexChar 8  = '8'
hexChar 9  = '9'
hexChar 10 = 'a'
hexChar 11 = 'b'
hexChar 12 = 'c'
hexChar 13 = 'd'
hexChar 14 = 'e'
hexChar _  = 'f'

hex : Bits8 -> String
hex b = pack [hexChar $ b `div` 16, hexChar $ b `mod` 16]

||| Originally Private.ColorSpecification.T
public export
data Color : Type where
  Name        : String -> Color
  RGB8        : (red, green, blue : Bits8) -> Color
  PaletteFrac : Double -> Color

export
Interpolation Color where
  interpolate (Name n)        = "rgb \{quote n}"
  interpolate (RGB8 r g b)    = "rgb \"0x\{hex r}\{hex g}\{hex b}\""
  interpolate (PaletteFrac x) = "palette frac \{show x}"

export
white : Color
white = Name "white"

export
black : Color
black = Name "black"

export
gray0 : Color
gray0 = Name "gray0"

export
grey0 : Color
grey0 = Name "grey0"

export
gray10 : Color
gray10 = Name "gray10"

export
grey10 : Color
grey10 = Name "grey10"

export
gray20 : Color
gray20 = Name "gray20"

export
grey20 : Color
grey20 = Name "grey20"

export
gray30 : Color
gray30 = Name "gray30"

export
grey30 : Color
grey30 = Name "grey30"

export
gray40 : Color
gray40 = Name "gray40"

export
grey40 : Color
grey40 = Name "grey40"

export
gray50 : Color
gray50 = Name "gray50"

export
grey50 : Color
grey50 = Name "grey50"

export
gray60 : Color
gray60 = Name "gray60"

export
grey60 : Color
grey60 = Name "grey60"

export
gray70 : Color
gray70 = Name "gray70"

export
grey70 : Color
grey70 = Name "grey70"

export
gray80 : Color
gray80 = Name "gray80"

export
grey80 : Color
grey80 = Name "grey80"

export
gray90 : Color
gray90 = Name "gray90"

export
grey90 : Color
grey90 = Name "grey90"

export
gray100 : Color
gray100 = Name "gray100"

export
grey100 : Color
grey100 = Name "grey100"

export
gray : Color
gray = Name "gray"

export
grey : Color
grey = Name "grey"

export
lightGray : Color
lightGray = Name "light-gray"

export
lightGrey : Color
lightGrey = Name "light-grey"

export
darkGray : Color
darkGray = Name "dark-gray"

export
darkGrey : Color
darkGrey = Name "dark-grey"

export
red : Color
red = Name "red"

export
lightRed : Color
lightRed = Name "light-red"

export
darkRed : Color
darkRed = Name "dark-red"

export
yellow : Color
yellow = Name "yellow"

export
lightYellow : Color
lightYellow = Name "light-yellow"

export
darkYellow : Color
darkYellow = Name "dark-yellow"

export
green : Color
green = Name "green"

export
lightGreen : Color
lightGreen = Name "light-green"

export
darkGreen : Color
darkGreen = Name "dark-green"

export
springGreen : Color
springGreen = Name "spring-green"

export
forestGreen : Color
forestGreen = Name "forest-green"

export
seaGreen : Color
seaGreen = Name "sea-green"

export
blue : Color
blue = Name "blue"

export
lightBlue : Color
lightBlue = Name "light-blue"

export
darkBlue : Color
darkBlue = Name "dark-blue"

export
midnightBlue : Color
midnightBlue = Name "midnight-blue"

export
navy : Color
navy = Name "navy"

export
mediumBlue : Color
mediumBlue = Name "medium-blue"

export
royalblue : Color
royalblue = Name "royalblue"

export
skyblue : Color
skyblue = Name "skyblue"

export
cyan : Color
cyan = Name "cyan"

export
lightCyan : Color
lightCyan = Name "light-cyan"

export
darkCyan : Color
darkCyan = Name "dark-cyan"

export
magenta : Color
magenta = Name "magenta"

export
lightMagenta : Color
lightMagenta = Name "light-magenta"

export
darkMagenta : Color
darkMagenta = Name "dark-magenta"

export
turquoise : Color
turquoise = Name "turquoise"

export
lightTurquoise : Color
lightTurquoise = Name "light-turquoise"

export
darkTurquoise : Color
darkTurquoise = Name "dark-turquoise"

export
pink : Color
pink = Name "pink"

export
lightPink : Color
lightPink = Name "light-pink"

export
darkPink : Color
darkPink = Name "dark-pink"

export
coral : Color
coral = Name "coral"

export
lightCoral : Color
lightCoral = Name "light-coral"

export
orangeRed : Color
orangeRed = Name "orange-red"

export
salmon : Color
salmon = Name "salmon"

export
lightSalmon : Color
lightSalmon = Name "light-salmon"

export
darkSalmon : Color
darkSalmon = Name "dark-salmon"

export
aquamarine : Color
aquamarine = Name "aquamarine"

export
khaki : Color
khaki = Name "khaki"

export
darkKhaki : Color
darkKhaki = Name "dark-khaki"

export
goldenrod : Color
goldenrod = Name "goldenrod"

export
lightGoldenrod : Color
lightGoldenrod = Name "light-goldenrod"

export
darkGoldenrod : Color
darkGoldenrod = Name "dark-goldenrod"

export
gold : Color
gold = Name "gold"

export
beige : Color
beige = Name "beige"

export
brown : Color
brown = Name "brown"

export
orange : Color
orange = Name "orange"

export
darkOrange : Color
darkOrange = Name "dark-orange"

export
violet : Color
violet = Name "violet"

export
darkViolet : Color
darkViolet = Name "dark-violet"

export
plum : Color
plum = Name "plum"

export
purple : Color
purple = Name "purple"
