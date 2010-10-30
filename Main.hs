module Main where

import SIParser as Parse
import StaticInstrumentation as Inst
import Storage as S
import System (getArgs)   
    
{- Driver program for pt
   ---------------------

   Implementation of the command-line parser and front-end as a whole.
 -}

showHelp = do
  putStrln "pt help -- This message"
  putStrln "pt generate [options] <filename> -- Generate static instrumentation"
  putStrln "   <filename> - The instrumentation spec"
  putStrln "   options: "
  putStrln "     -o name --- file base name"

main = do
  -- Look at the command first.
  config <- S.loadConfig
  args <- getArgs
  