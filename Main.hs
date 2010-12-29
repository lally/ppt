module Main where

import Configuration (RunConfig, cfgTarget)
import SIParser as Parse
import StaticInstrumentation as Inst
import Storage as S
import Generate as Gen
import Listener as L
import System (getArgs)   
--import LLVM.Core as LC    
{- Driver program for pt
   ---------------------

   Implementation of the command-line parser and front-end as a whole.
 -}

showHelp = do
  putStrLn "pt help -- This message"
  putStrLn "pt generate [options] <filename> -- Generate static instrumentation"
  putStrLn "   <filename> - The instrumentation spec"
  putStrLn "   options: "
  putStrLn "     -o name --- file base name"


runCommand :: [String] -> RunConfig -> IO ()
runCommand args cfg = do
  if (length args) == 0 then showHelp else 
    case head args of
      "generate" -> Gen.generate (tail args) cfg
      "help" -> showHelp
      otherwise -> showHelp
  
main = do
  -- TODO: Look at the command first.
  args <- getArgs
  case head args of
     "init" -> do
               cfg <- createConfig
               putStrLn ("Configuration created, Target = " ++ (show (cfgTarget cfg)))
     otherwise -> do
               config <- S.loadConfig
               case config of
                    Nothing -> putStrLn "Could not find configuration.  Run 'ppt init' to put one here."
                    Just cfg -> do
                              L.initialize 
                              runCommand args cfg
