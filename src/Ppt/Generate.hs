module Ppt.Generate where
import Ppt.Configuration
import Ppt.Storage as S
-- import Ppt.Listener as L
import System.IO
import Ppt.Generate.C (emitC)
import Ppt.StaticInstrumentation as Inst
import Ppt.SIParser as SIP
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
              converter_name = (basefname ++ "_convert.c")
              creader_name = (basefname ++ "_listen.c")
              (header, source, converter, creader) = 
                emitC cfg spec impl basefname
          makeSpecPath cfg spec
          writeFile (dstpath ++ header_name) header
          writeFile (dstpath ++ source_name) source
          writeFile (dstpath ++ converter_name) converter
          writeFile (dstpath ++ creader_name) creader
          -- L.generateReader cfg spec (dstpath ++ basefname ++ ".ll")
    
        {- If it isn't obvious, this is just a stub -}
        LangCpp -> do
          let dstpath = specPath cfg spec
              header_name = (basefname ++ ".h")
              source_name = (basefname ++ ".cpp")
              converter_name = (basefname ++ "_convert.cpp")
              creader_name = (basefname ++ "_listen.cpp")
              (header, source, converter, creader) = 
                emitC cfg spec impl basefname {- <--- Obvious? -}
          putStrLn ("Note, this is unsupported, and 'checkout' will not " ++ 
                   "work correctly.")
          makeSpecPath cfg spec
          writeFile (dstpath ++ header_name) header
          writeFile (dstpath ++ source_name) source
          writeFile (dstpath ++ converter_name) converter
          writeFile (dstpath ++ creader_name) creader
          -- L.generateReader cfg spec (dstpath ++ basefname ++ ".ll")

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
                 {- Note that we're assuming C here. -}
                  let dstpath = specPath cfg spec        
                      basefilename = takeWhile (/= '.') file
                  mapM_ (\suff -> do
                               let n = (basefilename ++ suff)  
                               putStrLn ("Copied " ++ n)
                               copyFile (dstpath ++ n) n) [".h", ".c", ".ll", 
                                                           "_convert.c", "_listen.c"]) files
