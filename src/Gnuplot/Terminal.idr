module Gnuplot.Terminal

import Gnuplot.File
import Gnuplot.Util

%default total

--------------------------------------------------------------------------------
--          Utility Types
--------------------------------------------------------------------------------

public export
data FontSize = Tiny | Small | Medium | Large | Giant

export
Interpolation FontSize where
  interpolate Tiny   = "tiny"
  interpolate Small  = "small"
  interpolate Medium = "medium"
  interpolate Large  = "large"
  interpolate Giant  = "giant"

public export
data PSMode = Landscape | Portrait | EPS

export
Interpolation PSMode where
  interpolate Landscape = "landscape"
  interpolate Portrait  = "portrait"
  interpolate EPS       = "eps"

public export
data TermType =
    DefTpe
  | PNGTpe
  | PSTpe
  | SVGTpe
  | WXTTpe
  | X11Tpe

--------------------------------------------------------------------------------
--          Terminals
--------------------------------------------------------------------------------

public export
data Terminal : TermType -> Type where
  Default : Terminal DefTpe

  PNG     :  (filename    : FilePath)
          -> (transparent : Maybe Bool) 
          -> (interlace   : Maybe Bool) 
          -> (trueColor   : Maybe Bool) 
          -> (fontSize    : Maybe FontSize) 
          -> Terminal PNGTpe

  PS      :  (filename    : FilePath)
          -> (mode        : Maybe PSMode)
          -> (color       : Maybe Bool)
          -> (fonts       : List FilePath)
          -> (font        : Maybe (String,Nat))
          -> Terminal PSTpe

  SVG     :  (filename    : FilePath) -> Terminal SVGTpe

  WXT     :  (title        : Maybe String)
          -> (persist      : Maybe Bool)
          -> Terminal WXTTpe

  X11     :  (title        : Maybe String)
          -> (persist      : Maybe Bool)
          -> Terminal X11Tpe

public export
record TermInfo where
  constructor MkTermInfo
  options     : List String
  commands    : List String
  interactive : Bool

--------------------------------------------------------------------------------
--          PNG
--------------------------------------------------------------------------------

export
png : FilePath -> Terminal PNGTpe
png p = PNG p Nothing Nothing Nothing Nothing

export
transparent : Bool -> Terminal PNGTpe -> Terminal PNGTpe 
transparent b (PNG p _ i t f) = PNG p (Just b) i t f

export
interlace : Bool -> Terminal PNGTpe -> Terminal PNGTpe 
interlace b (PNG p t _ c f) = PNG p t (Just b) c f

export
trueColor : Bool -> Terminal PNGTpe -> Terminal PNGTpe 
trueColor b (PNG p t i _ f) = PNG p t i (Just b) f

export
fontSize : FontSize -> Terminal PNGTpe -> Terminal PNGTpe 
fontSize f (PNG p t i c _) = PNG p t i c (Just f)

pngToInfo : Terminal PNGTpe -> TermInfo
pngToInfo (PNG p t i c fs) = MkTermInfo {
    options     = catMaybes [ Just "png"
                            , formatBoolM t "transparent"
                            , formatBoolM i "interlace"
                            , formatBoolM c "truecolor"
                            , map interpolate fs
                            ]
  , commands    = ["set output \{p}"]
  , interactive = False
  }

--------------------------------------------------------------------------------
--          PS
--------------------------------------------------------------------------------

export %inline
ps : FilePath -> Terminal PSTpe
ps f = PS f Nothing Nothing [] Nothing

export
mode : PSMode -> Terminal PSTpe -> Terminal PSTpe
mode m (PS f _ c fs fo) = PS f (Just m) c fs fo

export
color : Bool -> Terminal PSTpe -> Terminal PSTpe
color b (PS f m _ fs fo) = PS f m (Just b) fs fo 

export
font : String -> Nat -> Terminal PSTpe -> Terminal PSTpe
font n s (PS f m c fs _) = PS f m c fs (Just (n,s))

export
embedFont : FilePath -> Terminal PSTpe -> Terminal PSTpe
embedFont p (PS f m c fs fo) = PS f m c (p :: fs) fo

psToInfo : Terminal PSTpe -> TermInfo
psToInfo (PS p m c fs fo) = MkTermInfo {
    options     = concat [ ["postscript"]
                         , asList interpolate m
                         , asList (\case True => "color"; False => "monochrome") c
                         , map (\f => "fontfile \{f}") fs
                         , asList (\(n,s) => "font \{quote n} \{show s}") fo
                         ]
  , commands    = ["set output \{p}"]
  , interactive = False
  }

--------------------------------------------------------------------------------
--          SVG
--------------------------------------------------------------------------------

export %inline
svg : FilePath -> Terminal SVGTpe
svg = SVG

svgToInfo : Terminal SVGTpe -> TermInfo
svgToInfo (SVG p) = MkTermInfo {
    options     = ["svg"]
  , commands    = ["set output \{p}"]
  , interactive = False
  }

--------------------------------------------------------------------------------
--          WXT
--------------------------------------------------------------------------------

export
wxt : Terminal WXTTpe
wxt = WXT Nothing Nothing

namespace WXT
  export
  title : String -> Terminal WXTTpe -> Terminal WXTTpe
  title s (WXT _ p) = WXT (Just s) p

  export
  persist : Bool -> Terminal WXTTpe -> Terminal WXTTpe
  persist b (WXT t _) = WXT t (Just b)


wxtToInfo : Terminal WXTTpe -> TermInfo
wxtToInfo (WXT t p) = MkTermInfo {
    options     = catMaybes [Just "wxt", map quote t, formatBoolM p "persist"]
  , commands    = []
  , interactive = True
  }

--------------------------------------------------------------------------------
--          X11
--------------------------------------------------------------------------------

export
x11 : Terminal X11Tpe
x11 = X11 Nothing Nothing

namespace X11
  export
  title : String -> Terminal X11Tpe -> Terminal X11Tpe
  title s (X11 _ p) = X11 (Just s) p

  export
  persist : Bool -> Terminal X11Tpe -> Terminal X11Tpe
  persist b (X11 t _) = X11 t (Just b)


x11ToInfo : Terminal X11Tpe -> TermInfo
x11ToInfo (X11 t p) = MkTermInfo {
    options     = catMaybes [ Just "x11"
                            , map quote t
                            , formatBoolM p "persist"
                            ]
  , commands    = []
  , interactive = True
  }

--------------------------------------------------------------------------------
--          Formatting
--------------------------------------------------------------------------------

export
toInfo : Terminal t -> TermInfo
toInfo Default    = MkTermInfo [] [] True
toInfo p@(PNG {}) = pngToInfo p
toInfo p@(PS {})  = psToInfo p
toInfo p@(SVG {}) = svgToInfo p
toInfo p@(WXT {}) = wxtToInfo p
toInfo p@(X11 {}) = x11ToInfo p

export
format : TermInfo -> List String
format (MkTermInfo [] cs _) = cs
format (MkTermInfo os cs _) = "set terminal \{unwords os}" :: cs
