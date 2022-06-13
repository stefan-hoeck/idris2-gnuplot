module Gnuplot.Execute

import Control.Monad.Either
import Gnuplot.Display
import Gnuplot.File
import Gnuplot.Frame.Option
import Gnuplot.Util
import System
import System.File

%default total

data Err : Type where
  Sys       : (cmd : String) -> (n : Int) -> Err
  WriteFile : (path : FilePath) -> FileError -> Err

printErr : Err -> String

||| Convert an IO action with the potential of failure
||| to an `EitherT Err`.
export
eitherIO :  HasIO io
         => (toErr : err -> Err)
         -> (act : io (Either err a))
         -> EitherT Err io a
eitherIO toErr = MkEitherT . map (mapFst toErr)

||| Make sure a *cleanup* action is run after
||| an IO action that might fail.
export
finally :  Monad m
        => (cleanup : EitherT err m ())
        -> (act     : EitherT err m a)
        -> EitherT err m a
finally cleanup act = MkEitherT $ do
  res <- runEitherT act
  ignore $ runEitherT cleanup
  pure res

||| Runs a gnuplot program, printing errors to standard out.
export
run : EitherT Err IO () -> IO ()
run (MkEitherT io) = do
  Left err <- io | Right () => pure ()
  putStrLn (printErr err)

--------------------------------------------------------------------------------
--          System Commands
--------------------------------------------------------------------------------

||| Tries to run a system command.
export
sys : HasIO io => (cmd : String) -> EitherT Err io ()
sys cmd = do
  0 <- system cmd | n => throwE (Sys cmd n)
  pure ()

--------------------------------------------------------------------------------
--          Files and Directores
--------------------------------------------------------------------------------

||| Checks if a file at the given location exists.
export
exists : HasIO io => (p : FilePath) -> io Bool
exists = exists . path

||| Checks if a file at the given location is missing.
export
missing : HasIO io => (p : FilePath) -> io Bool
missing = map not . exists

||| Tries to create a director (including parent directories)
export
mkDir : HasIO io => (dir : FilePath) -> EitherT Err io ()
mkDir dir = case path dir of
  "" => pure ()
  s  => sys "mkdir -p \{s}"

||| Forcefully deletes a directory with all its content
export
rmDir : HasIO io => (dir : FilePath) -> EitherT Err io ()
rmDir dir = when !(exists dir) $ sys "rm -rf \{dir}"

||| Deletes a file
export
rmFile : HasIO io => (file : FilePath) -> EitherT Err io ()
rmFile file = when !(exists file) $ sys "rm \{file}"

||| Tries to write a string to a file.
export
write : HasIO io => FilePath -> String -> EitherT Err io ()
write path str = do
  eitherIO (WriteFile path) (writeFile path.path str)

||| Tries to write a string to a file.
export
writeGP : HasIO io => GPFile -> EitherT Err io ()
writeGP (MkFile p c) = write p c

gnuplotDir : FilePath
gnuplotDir = MkFilePath ".gnuplot"

curveFile : FilePath
curveFile = MkFilePath "curve.gp"

export
withTempDir :  HasIO io
            => (run : FilePath -> EitherT Err io a) 
            -> EitherT Err io a
withTempDir run =
  finally (rmDir gnuplotDir) $ do
    when !(missing gnuplotDir) (mkDir gnuplotDir)
    run gnuplotDir

export
withTempFile :  HasIO io
             => (run : FilePath -> EitherT Err io a) 
             -> EitherT Err io a
withTempFile run = finally (rmFile curveFile) $ run curveFile

export
runCmd :  HasIO io
       => (FilePath -> (List String, List GPFile))
       -> EitherT Err io ()
runCmd f = withTempDir $ \dir =>
  let (commands, files) = f dir
   in do traverse_ writeGP files
         withTempFile $ \fil => do
           write fil (unlines commands)
           sys "gnuplot \{fil}"

export
runScript : HasIO io => ToScript a => a -> EitherT Err io ()
runScript x =
  let MkScript f = toScript x
   in runCmd $ \fp => case f 0 deflt fp of
        (_,_,MkBody fs cs) => (cs,fs)
