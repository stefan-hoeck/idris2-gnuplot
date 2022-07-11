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
  let console = stdOut
      fs      = File.local
      sys     = Sys.system
      log     = filter Debug (colorConsoleLogger console)
   in runApp handlers $ runScript (SVG "test.svg") plot

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

trigs : Table [ "x"            :> Double
              , "sin(x)"       :> Double
              , "cos(x)"       :> Double
              , "tan(x)"       :> Double
              , "exp(x)"       :> Double
              , "3 * exp(1/x)" :> Double
              ]
trigs = functions (linear 4000 (-2*pi) (2*pi))
          [ sin
          , cos
          , tan
          , exp
          , \x => if abs x < 0.0001 then 0 else 0.3 * exp (1 / x)
          ]

sinCosTan : Frame2D Double Double
sinCosTan = frame
  [ yrange (-2) 2
  , xlabel "x"
  , ylabel "y"
  ] $
  fromTable trigs
    [ titled Lines [Var 1,Var 2]
    , titled Lines [Var 1,Var 3]
    , titled Lines [Var 1,Var 4]
    , titled Lines [Var 1,Var 5]
    , titled Lines [Var 1,Var 6]
    ]

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

main : IO ()
main = run sinCosTan
