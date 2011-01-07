module Generate where
import Configuration
import Storage as S
import Listener as L
import System.IO
import Generate.C (emitC)
import StaticInstrumentation as Inst
import SIParser as SIP
import System.Console.GetOpt as GO
import Data.Char (toUpper)
import Data.List (find)
import Data.List.Utils (replace)
import Data.Word (Word8)
import Data.String.Utils (join)
import System.Directory (copyFile)
--import Scratch


generateC :: Inst.FullSpecification -> String
generateC inst = show inst

{-
   Command line processing support
-}
data Flag = OutputFile String
          deriving (Eq, Show)

-- The argument list accepted by the 'generate' command
arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['o'] ["output"] (GO.ReqArg OutputFile "output") 
           "The output file(s) base name"]


doGenerate :: FullSpecification -> String -> RunConfig -> IO ()
doGenerate spec@(Spec emit _ frames) basefname cfg = do
      let impl = implement cfg spec
--      putStrLn (show impl)
      case emit of
        LangC -> do
          let dstpath = specPath cfg spec
              header_name = (basefname ++ ".h")
              source_name = (basefname ++ ".c")
              (header, source) = 
                emitC cfg impl basefname
          makeSpecPath cfg spec
          writeFile (dstpath ++ header_name) header
          writeFile (dstpath ++ source_name) source
          L.generateReader cfg spec (dstpath ++ basefname ++ ".ll")
    
        LangCpp -> do
          let dstpath = specPath cfg spec
              header_name = basefname ++ ".h"
              source_name = basefname ++ ".cpp"
              (header, source) = emitC cfg impl basefname
          makeSpecPath cfg spec
          writeFile header_name header
          writeFile source_name source
          L.generateReader cfg spec (basefname ++ ".ll")

--  putStrLn (show result) 
  

generate :: [String] -> RunConfig -> IO ()
generate args cfg =  
  let res = GO.getOpt GO.Permute arglist args in
  case res of
    (opts, files, []) -> do
      let file = head files
      text <- readFile file 
      let result = SIP.parseText SIP.commandFile text file
      case result of
           Left err -> putStrLn ("ERROR: " ++ (show err))
           Right spec@(Spec emit _ frames) ->
                 doGenerate spec basename cfg
                 where
                    isfname (OutputFile _) = True
                    basename = case find isfname opts of
                                    Just (OutputFile n) -> n
                                    Nothing -> takeWhile (/= '.') file
    (_, _, _) ->
      putStrLn (show res)
                   

checkout :: [String] -> RunConfig -> IO ()
checkout files cfg = do
      mapM_ (\file -> do
            text <- readFile file 
            let result = SIP.parseText SIP.commandFile text file
            case result of
                 Left err -> putStrLn ("ERROR: " ++ (show err))
                 Right spec -> do
                  let dstpath = specPath cfg spec        
                      basefilename = takeWhile (/= '.') file
                  mapM_ (\suff -> do
                               let n = (basefilename ++ suff)  
                               putStrLn ("Copied " ++ n)
                               copyFile (dstpath ++ n) n) [".h", ".c", ".ll"]) files
