module Main where

import Ppt.Configuration (RunConfig, cfgTarget)
import qualified Ppt.SIParser as Parse
import qualified Ppt.Agent as Agent
import qualified Ppt.StaticInstrumentation as Inst
import qualified Ppt.Storage as S
import qualified Ppt.Generate as Gen
-- import Ppt.Listener as L
import System.Environment (getArgs)
--import LLVM.Core as LC
{- Driver program for pt
   ---------------------

   Implementation of the command-line parser and front-end as a whole.
 -}

showHelp = do
  putStrLn "ppt help -- This message"
  putStrLn "ppt generate [options] <filename> -- Generate static instrumentation"
  putStrLn "    <filename> - The instrumentation spec"
  putStrLn "    options: "
  putStrLn "      -o name --- file base name"
  putStrLn "ppt retrieve <filename> -- Extract generated instrumentation into current directory."

runCommand :: [String] -> RunConfig -> IO ()
runCommand args cfg = do
  if (length args) == 0 then showHelp else
    case head args of
      "gen" -> Gen.generate (tail args) cfg
      "generate" -> Gen.generateCommand (tail args)
      "attach" -> Agent.attach (tail args) cfg
      "?" -> showHelp
      "help" -> showHelp
      "ret" -> Gen.checkout (tail args) cfg
      "retrieve" -> Gen.checkout (tail args) cfg
      otherwise -> showHelp

main = do
  -- TODO: Look at the command first.
  args <- getArgs
  if (length args) == 0 then showHelp else
    case head args of
      "init" -> do
                cfg <- S.createConfig
                putStrLn ("Configuration created, Target = " ++ (show (cfgTarget cfg)))
      otherwise -> do
               config <- S.loadConfig
               case config of
                    Nothing -> putStrLn "Could not find configuration.  Run 'ppt init' to put one here."
                    Just cfg -> do
                              -- L.initialize
                              runCommand args cfg
