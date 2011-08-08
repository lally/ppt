module Agent(attach) where
import System.Console.GetOpt as GO
import System.Process
import Configuration
import StaticInstrumentation
import SIParser as SIP
import Numeric
import Data.Char
--import GHC.IO.Exception (ExitCode, ExitFailure)
{-
    Command line processing support
-}
data Flag = Pid Int
          deriving (Eq, Show)

arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['p'] ["pid"] (GO.ReqArg (\t -> Pid (read t)) "pid") "Pid to attach to."]


-- Attach to a running process.
-- Command line arguments:
--  ppt attach -p <pid> -v <version> <spec>
-- Note, -v isn't yet implemented.

attach :: [String] -> RunConfig -> IO ()
attach args cfg = 
       let res = GO.getOpt GO.Permute arglist args in
       case res of
          ([Pid pid], specs, []) -> do
             let processSpec spec = do
                             text <- readFile spec
                             let res = SIP.parseText SIP.commandFile text spec
                             case res of 
                               Right specData@(Spec emit buffer@(Buffer bnm bsz brate) frames)-> do
                                  let hashText = "0x" ++ (concatMap (\v -> showIntAtBase 16 intToDigit v "") $ reverse $ take 4 $ specHash specData)
                                  let hashSym = "_ppt_version_" ++ bnm
                                  let memSym = "_ppt_hmem_" ++ bnm
                                  -- For now, we're going to assume you've externally built the converter
                                  r <- createProcess (proc  "ppt-agent" ["attach", "-p", (show pid), "-s", memSym, 
                                                                         "-v", hashSym, "-N", hashText, 
                                                                         "-i", "./" ++ bnm ++ "_listen out.buf %d 65536", 
                                                                         "-n", "65536", "-V"])
                                  putStrLn "Launched."
                               Left s -> do
                                  putStrLn (show s)
             mapM_ processSpec specs
          (_, _, _) -> putStrLn (show res)                 
                 