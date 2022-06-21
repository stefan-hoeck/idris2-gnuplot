module Main

import Control.RIO.App
import Control.RIO.Console
import Control.RIO.File
import Control.RIO.Logging
import Control.RIO.Sys
import Data.List.Quantifiers
import Gnuplot

--------------------------------------------------------------------------------
--          Running the app
--------------------------------------------------------------------------------

handlers : Logger => All (Handler ()) [FileErr,SysErr]
handlers = [ \e => error (printErr e)
           , \e => error (printErr e)
           ]

run : ToScript a => a -> IO ()
run plot =
  let console = stdIO
      fs      = File.local
      sys     = Sys.system
      log     = colorConsoleLogger console
   in runApp handlers $ runScript (WXT Nothing $ Just True) plot

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

trigs : Table [ "x"      :> Double
              , "sin(x)" :> Double
              , "cos(x)" :> Double
              , "tan(x)" :> Double
              ]
trigs = functions (linear 2000 (-2*pi) (2*pi)) [sin,cos,tan]

sinCosTan : Frame2D Double Double
sinCosTan = frame
  [ yrange .= (-2, 2)
  , xlabel .= "x"
  , ylabel .= "y"
  ] $
  fromTable trigs
    [ titled Lines [1,2]
    , titled Lines [1,3]
    , titled Lines [1,4]
    ]

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

main : IO ()
main = run sinCosTan
