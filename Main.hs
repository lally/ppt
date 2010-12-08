module Main where

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


runCommand :: [String] -> S.Config -> IO ()
runCommand args cfg = do
  if (length args) == 0 then showHelp else 
    case head args of
      "generate" -> Gen.generate (tail args) cfg
      "help" -> showHelp
      otherwise -> showHelp
  
main = do
  -- TODO: Look at the command first.
  args <- getArgs
  config <- S.loadConfig
  L.initialize
  runCommand args config
