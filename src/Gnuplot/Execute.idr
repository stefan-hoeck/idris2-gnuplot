module Gnuplot.Execute

import Control.RIO.File
import Control.RIO.Logging

import Data.List1

import Gnuplot.Display
import Gnuplot.File
import Gnuplot.Options
import Gnuplot.Terminal
import Gnuplot.Util

import Control.RIO.File
import Control.RIO.Sys

%default total

public export
0 PlotEnv : List Type -> Type
PlotEnv ts = (Logger, Sys, FS, Has FileErr ts, Has SysErr ts)

--------------------------------------------------------------------------------
--          Files and Directores
--------------------------------------------------------------------------------

||| Tries to write a string to a file.
export
writeGP : Logger => FS => Has FileErr xs => GPFile -> App xs ()
writeGP (MkFile p c) = do
  debug "Writing to \{p}"
  trace "Content\n\{c}"
  write p c

gnuplotDir : FilePath
gnuplotDir = ".gnuplot"

curveFile : FilePath
curveFile = "curve.gp"

export
withTempDir : PlotEnv xs => (run : FilePath -> App xs a) -> App xs a
withTempDir run =
  finally (rmDir gnuplotDir) $ do
    when !(missing gnuplotDir) (mkDir gnuplotDir)
    inDir gnuplotDir (run "")

export
withTempFile : PlotEnv xs => (run : FilePath -> App xs a) -> App xs a
withTempFile run = finally (removeFile curveFile) $ run curveFile

export
runCmd :  PlotEnv xs
       => (FilePath -> (List String, List GPFile))
       -> App xs ()
runCmd f = withTempDir $ \dir =>
  let (commands, files) = f dir
   in do traverse_ writeGP files
         withTempFile $ \fil => do
           writeGP (MkFile fil $ unlines commands)
           sys "gnuplot \{fil}"

export
runScript : PlotEnv xs => ToScript a => Terminal t -> a -> App xs ()
runScript term x =
  let MkScript f := toScript x
      cmds       := format (toInfo term)
   in runCmd $ \fp => case f 0 [] fp of
        (_,ss,MkBody fs cs) => (cmds ++ map interpolate ss ++ cs,fs)
