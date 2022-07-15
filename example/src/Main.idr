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
      log     = filter Trace (colorConsoleLogger console)
   in runApp handlers $ runScript (SVG "test.svg") plot

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

trigs : Table [ "x"            :> GDbl
              , "sin(x)"       :> GDbl
              , "cos(x)"       :> GDbl
              , "tan(x)"       :> GDbl
              , "exp(x)"       :> GDbl
              , "3 * exp(1/x)" :> GDbl
              ]
trigs = functions GDbl (linear 4000 (-2*pi) (2*pi))
          [ sin
          , cos
          , tan
          , exp
          , \x => if abs x < 0.0001 then 0 else 0.3 * exp (1 / x)
          ]

sinCosTan : Frame2D GDbl GDbl
sinCosTan = frame
  [ yrange (-2) 2
  , xlabel "x"
  , ylabel "y"
  ] $
  fromTable trigs
  [ titled {y = GDbl} Lines [col 1,col 2]
  , titled {y = GDbl} Lines [col 1,col 3]
  , titled {y = GDbl} Lines [col 1,col 4]
  , titled {y = GDbl} Lines [col 1,col 5]
  , titled {y = GDbl} Lines [col 1,col 6]
  ]

--------------------------------------------------------------------------------
--          Plot Examples
--------------------------------------------------------------------------------

main : IO ()
main = run sinCosTan
