module Gnuplot.Plot.Types

import Gnuplot.File

%default total

public export
record File graph where
  constructor MkFile
  fileName : FilePath
  content  : Maybe String
  graphs   : List graph


public export
record Plot graph where
  constructor MkPlot
  run : Nat -> FilePath -> (Nat, List $ File graph)

export
pure : List (File graph) -> Plot graph
pure v = MkPlot $ \n,_ => (n,v)

export
Semigroup (Plot graph) where
  MkPlot f <+> MkPlot g = MkPlot $ \n,p =>
    let (n1,fs1) = f n p
        (n2,fs2) = g n1 p
     in (n2, fs1 ++ fs2)

export
Monoid (Plot graph) where
  neutral = pure []

tmpFileStem : String
tmpFileStem = "curve"

export
withUniqueFile : String -> List graph -> Plot graph
withUniqueFile c gs = MkPlot $ \n,dir =>
  (S n, [MkFile (dir /> "\{tmpFileStem}\{show n}" <.> "csv") (Just c) gs])

export
fromGraphs : FilePath -> List graph -> Plot graph
fromGraphs name gs = pure [MkFile name Nothing gs]

-- instance Functor T where
--    fmap f (Cons mp) =
--       Cons $
--       fmap (map (\file -> file{graphs_ = map f $ graphs_ file}))
--       mp
-- 
-- {- |
-- In contrast to the Display.toScript method instantiation
-- this function leaves the options,
-- and thus can be used to write the Display.toScript instance for Frame.
-- -}
-- toScript :: Graph.C graph => T graph -> Display.Script
-- toScript p@(Cons mp) =
--    Display.Script $ do
--       blocks <- AccState.liftT AccTuple.first mp
--       let files =
--              mapMaybe
--                 (\blk -> fmap (File.Cons (filename_ blk)) (content_ blk))
--                 blocks
--           graphs =
--              concatMap
--                 (\blk ->
--                    map
--                       (\gr ->
--                          quote (filename_ blk) ++ " " ++
--                          Graph.toString gr) $ graphs_ blk) $
--              blocks
--       return $
--          Display.Body files
--             [Graph.commandString (plotCmd p) ++ " " ++ commaConcat graphs]
-- 
-- optionsToScript :: Graph.C graph => OptionSet.T graph -> Display.Script
-- optionsToScript opts =
--    Display.Script $
--    AccState.liftT AccTuple.second $ do
--       opts0 <- MS.get
--       let opts1 = OptionSet.decons opts
--       MS.put opts1
--       return $
--          Display.Body [] $
--          OptionSet.diffToString opts0 opts1
-- 
-- defltOpts :: Graph.C graph => T graph -> OptionSet.T graph
-- defltOpts _ = Graph.defltOptions
-- 
-- instance Graph.C graph => Display.C (T graph) where
--    toScript plot =
--       optionsToScript (defltOpts plot)  `mappend`  toScript plot
-- 
-- plotCmd :: Graph.C graph => T graph -> Graph.Command graph
-- plotCmd _plot = Graph.command
