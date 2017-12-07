module Main where

import qualified Ppt.Agent as Agent
import qualified Ppt.Generate as Gen
import qualified Ppt.Decode as Decode
import System.Environment (getArgs)

{- Driver program for pt
   ---------------------

   Implementation of the command-line parser and front-end as a whole.
 -}

showHelp = do
  putStrLn "ppt -- This message"
  putStrLn "ppt generate -- generate instrumentation source code from specification."
  putStrLn "ppt attach -- attach to a running process to collect instrumentation data into a binary file."
  putStrLn "ppt decode -- convert a collected  binary file to readable text."
  putStrLn "Each subcommand has its own help"

runCommand :: [String] -> IO ()
runCommand args = do
  if (length args) == 0 then showHelp else
    case head args of
      "generate" -> Gen.generateCommand (tail args)
      "attach" -> Agent.attach (tail args)
      "decode" -> Decode.decodeCommand (tail args)
      "?" -> showHelp
      "help" -> showHelp
      otherwise -> showHelp

main = do
  -- TODO: Look at the command first.
  args <- getArgs
  runCommand args
