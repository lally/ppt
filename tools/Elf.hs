module Main where
import Ppt.ElfProcess
import System.Environment (getArgs)
import System.Console.GetOpt

data Flag = FString | FNumber | FVerbose deriving (Eq, Show)

data FlagSet = FlagSet { isString :: Bool
                       , verbose :: Bool } deriving (Eq, Show)

doFlags :: [Flag] -> FlagSet
doFlags fs =
  let doFlags' prior (flg:flgs)
        | flg == FString = doFlags' (prior { isString = True }) flgs
        | flg == FNumber = doFlags' (prior { isString = False }) flgs
        | flg == FNumber = doFlags' (prior { verbose = True }) flgs
      doFlags' prior [] = prior
  in doFlags' (FlagSet False False) fs

parseArgs :: [String] -> (FlagSet, [String])
parseArgs args =
  let options :: [OptDescr Flag]
      options =
        [Option ['v'] ["verbose"] (NoArg FVerbose) "verbose",
         Option ['n'] ["numeric"] (NoArg FNumber) "numeric symbol",
         Option ['s'] ["string"] (NoArg FString) "string symbol"]
      (flgs, rest, _) = getOpt Permute options args
  in (doFlags flgs, rest)

main = do
  args <- getArgs
  if length args < 2
  then error "usage: elf pid symbol [newvalue]"
  else return ()
  let pid = (read $ args !! 0) :: Int
  let symname = args !! 1
  elf <- loadProcess pid
  let syms = symbolsWithPrefix elf symname
      sym = head syms
  if length args > 2
  then do setIntegerInProcess pid sym (read $ args !! 2)
  else return False
  value <- integerInProcess pid sym
  putStrLn $ show value

