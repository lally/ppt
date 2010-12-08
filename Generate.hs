module Generate where
import Storage as S
import Listener as L
import System.IO
import StaticInstrumentation as Inst
import SIParser as SIP
import System.Console.GetOpt as GO
import Data.Char (toUpper)
import Data.List (find) 

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


{- 
   Source code generation

   The header file is generated via this schema:

   #header guard
   type decls

   variable decls foreach type: decl { 
    WRITE_MACROs   
    global var
    shmem handle
    shmem ptrs
    frame_write routine 
   }
  
   #header guard

   Source file:
   foreach type: {
    global var
    shmem handle
    shmem ptrs
    frame_write routine 
   }
-}

data LanguageWriter = LangWriter {
  memberWrapper :: FrameElement -> String,
  frameWrapper :: String -> [String] -> String,
  -- writeMacro :: frame name -> ...
  writeMacro :: String -> FrameElement -> String, 
  globalVar :: String -> String,
  shmemHandle :: String -> String,
  shmemPtrs :: String -> String,
  frameWriteDecl :: FrameSpecification -> String,
  frameWriteBody :: FrameSpecification -> String
  }
                      
writerFor LangC = LangWriter { 
  memberWrapper = cMember, 
  frameWrapper = cFrame, 
  writeMacro = cMacro, 
  globalVar = cVar,
  shmemHandle = cHandle, 
  shmemPtrs = cPtrs, 
  frameWriteDecl = cWFrameDecl,
  frameWriteBody = cWFrameBody }
  where
    gVarName name = "pt_frame__" ++ name
    cMember (FrameElement FDouble n) = "double " ++ n 
    cMember (FrameElement FFloat n) = "float " ++ n
    cMember (FrameElement FInt n) = "int " ++ n
    cFrame fname mems = 
      "typedef struct pt_tag_struct " ++ fname ++ 
      " {\n\tint pt___seqno;\n" ++
      (concatMap (\x -> "\t" ++ x ++ ";\n") mems) ++ 
      "} ptframe_" ++ fname ++ "_t;\n"
    cMacro name (FrameElement _ n) = 
      "\n#define WRITE"++ (map toUpper name) ++ "_" ++ (map toUpper n) ++
      "(x) " ++ (gVarName name) ++ "." ++ n ++ "=x\n"
    cVar n = "ptframe_" ++ n ++ "_t " ++ (gVarName n) ++ ";\n"
    cHandle n = "int "
    cPtrs n = undefined
    cWFrameDecl (FrameSpecification n es) = undefined
    cWFrameBody (FrameSpecification n es) = undefined
  
writeDeclsC :: FrameSpecification -> String
writeDeclsC (FrameSpecification name elems) =
  "pt_priv" ++ name

-- spec -> (header_filename, source_filename) -> (header text, source
-- text)
emitC :: FullSpecification -> (String, String) -> (String, String) 
emitC (Spec emit fs) (hname, sname) = 
  let
    typeDecl FDouble = "double"
    typeDecl FFloat = "float"
    typeDecl FInt = "int"
    memDecl (FrameElement t name) = "  " ++ (typeDecl t) 
                                    ++ " " ++ name ++ ";"
    headerBody (FrameSpecification name elems) = 
      "struct " ++ name ++ " {\n" ++ (concatMap memDecl elems) ++ "\n};"
    header = headerGuards hname (concatMap headerBody fs)
    source = "/* implement me too */\n"
    headerGuards fname inner = 
      "#ifndef " ++ sym ++ "\n#define " ++ sym ++ "\n" ++ inner ++ 
      "\n#endif /* #ifdef " ++ sym ++ "*/\n"
      where sym = "INCLUDE_" ++ (map toUpper fname)
  in (header, source)

runParse :: String -> String -> IO ()
runParse file basefname = do
  putStrLn ("Running with file " ++ file)
  text <- readFile file 
  let result = SIP.parseText SIP.commandFile text file
  case result of
    Left err -> putStrLn ("ERROR: " ++ (show err))
    Right spec@(Spec emit frames) ->
      case emit of
        LangC ->
          let header_name = (basefname ++ ".h") in
          let source_name = (basefname ++ ".c") in
          let (header, source) = 
                emitC spec (header_name, source_name)  in
          writeFile header_name header >> 
          writeFile source_name source >>
          putStrLn ("Generated " ++ header_name ++ 
                    " and " ++ source_name) >>
          L.generateReader spec (basefname ++ ".lli")
    
        LangCpp ->
          let header_name = basefname ++ ".h"
              source_name = basefname ++ ".cpp"
              (header, source) = emitC spec (header_name, source_name)
          in
          writeFile header_name header >>
          writeFile source_name source >>
          putStrLn ("Generated " ++ header_name ++ 
                    " and " ++ source_name) >>
          L.generateReader spec (basefname ++ ".lli")

  putStrLn (show result) 
  

generate :: [String] -> S.Config -> IO ()
generate args cfg = do 
  let res = GO.getOpt GO.Permute arglist args
  case res of
    (opts, file, []) -> 
      runParse (head file) basename
      where
        isfname (OutputFile _) = True
        basename = case find isfname opts of
          Just (OutputFile n) -> n
          Nothing -> takeWhile (/= '.') (head file)
    (_, _, _) ->
      putStrLn (show res)
                   