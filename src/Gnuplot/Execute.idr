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

gnuplotDir : Path Abs -> Path Abs
gnuplotDir dir = dir /> ".gnuplot"

curveFile : Path Abs -> Path Abs
curveFile dir = dir /> "curve.gp"

export
withTempDir :  PlotEnv xs
            => (dir : Path Abs)
            -> (run : Path Abs -> App xs a)
            -> App xs a
withTempDir dir run =
  let gpdir = gnuplotDir dir
   in finally (rmDir gpdir) $ do
        when !(missing gpdir) (mkDir gpdir)
        run gpdir

export
withTempFile :  PlotEnv xs
             => (dir : Path Abs)
             -> (run : Path Abs -> App xs a)
             -> App xs a
withTempFile dir run =
  let cfile = curveFile dir
   in finally (removeFile cfile) $ run cfile

export
runCmd :  PlotEnv xs
       => (dir : Path Abs)
       -> (Path Abs -> (List String, List GPFile))
       -> App xs ()
runCmd dir f = withTempDir dir $ \td =>
  let (commands, files) = f td
   in do traverse_ writeGP files
         withTempFile td $ \fil => do
           writeGP (MkFile fil $ unlines commands)
           sys "gnuplot \{fil}"

export
runScript :  PlotEnv xs
          => ToScript a
          => Terminal t
          -> a
          -> App xs ()
runScript term x =
  let MkScript f := toScript x
      cmds       := format (toInfo term)
   in do
      dir <- curDir
      runCmd dir $ \fp => case f 0 [] fp of
        (_,ss,MkContent fs cs) =>
          (cmds ++ map interpolate ss ++ (cs <>> []),fs <>> [])
