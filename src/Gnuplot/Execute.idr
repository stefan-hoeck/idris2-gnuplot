module Gnuplot.Execute

import Control.RIO.File

import Data.List1

import Gnuplot.Display
import Gnuplot.File
import Gnuplot.Frame.Option
import Gnuplot.Util

import Control.RIO.File
import Control.RIO.Sys

%default total

public export
0 PlotEnv : List Type -> Type
PlotEnv ts = (Sys, FS, Has FileErr ts, Has SysErr ts)

--------------------------------------------------------------------------------
--          Files and Directores
--------------------------------------------------------------------------------

||| Tries to write a string to a file.
export
writeGP : FS => Has FileErr xs => GPFile -> App xs ()
writeGP (MkFile p c) = write p c

gnuplotDir : FilePath
gnuplotDir = ".gnuplot"

curveFile : FilePath
curveFile = "curve.gp"

export
withTempDir : PlotEnv xs => (run : FilePath -> App xs a) -> App xs a
withTempDir run =
  finally (rmDir gnuplotDir) $ do
    when !(missing gnuplotDir) (mkDir gnuplotDir)
    run gnuplotDir

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
           write fil (unlines commands)
           sys "gnuplot \{fil}"

export
runScript : PlotEnv xs => ToScript a => a -> App xs ()
runScript x =
  let MkScript f = toScript x
   in runCmd $ \fp => case f 0 deflt fp of
        (_,_,MkBody fs cs) => (cs,fs)
