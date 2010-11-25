module Main where

import SIParser as Parse
import StaticInstrumentation as Inst
import Storage as S
import Listener as L
import System (getArgs)   
    
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

main = do
  -- TODO: Look at the command first.
  args <- getArgs
  config <- S.loadConfig
  L.initialize