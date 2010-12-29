module Generate where
import Configuration
import Storage as S
import Listener as L
import System.IO
import StaticInstrumentation as Inst
import SIParser as SIP
import System.Console.GetOpt as GO
import Data.Char (toUpper)
import Data.List (find)
import Data.List.Utils (replace)
import Data.Word (Word8)
import Data.String.Utils (join)

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
  -- emit a single member definition
  memberWrapper :: FrameElement -> String,

  -- emit the type definition for a frame (given name, (map memberWrapper members))
  frameWrapper :: String -> [String] -> String,

  -- writeMacro :: frame name -> element -> #define WRITE_****(value)
  writeMacro :: String -> FrameElement -> String,

  -- the global that will store the frame being built.
  globalVar :: String -> String,

  -- the global shared memory handle (to be filled in by the agent)
  shmemHandle :: String -> String,

  -- the global containing the element count of the shared memory handle
  shmemSize :: String -> String,
  
  -- generate a version tag for the specified frame name and hash
  frameVersion :: String -> [Word8] -> String,
  frameVersionDecl :: String -> [Word8] -> String,

  -- Generate the declaration for the frame_write routine.
  frameWriteDecl :: FrameSpecification -> String,
  
  -- Generate the definition for the frame_write routine.
  frameWriteBody :: FrameSpecification -> String
  }
                      
writerFor LangC = LangWriter { 
  memberWrapper = cMember, 
  frameWrapper = cFrame, 
  writeMacro = cMacro, 
  globalVar = cVar,
  shmemHandle = cHandle, 
  shmemSize = cSize,
  frameVersion = cFrameVersion,
  frameVersionDecl = cFrameVersionDecl,
  frameWriteDecl = cWFrameDecl,
  frameWriteBody = cWFrameBody }
  where
    gVarName name = "_ppt_frame__" ++ name
    cMember (FrameElement FDouble n) = "double " ++ n 
    cMember (FrameElement FFloat n) = "float " ++ n
    cMember (FrameElement FInt n) = "int " ++ n
    cMember (FrameElement FTime n) = "struct timeval " ++ n
    cFrame fname mems = 
      "typedef struct ppt_tag_struct_" ++ fname ++ 
      " {\n\tint _ppt__seqno;\n" ++
      (concatMap (\x -> "\t" ++ x ++ ";\n") mems) ++ 
      "} pptframe_" ++ fname ++ "_t;"
    cMacro name (FrameElement _ n) = 
      "\n#define WRITE_"++ (map toUpper name) ++ "_" ++ (map toUpper n) ++
      "(x) " ++ (gVarName name) ++ "." ++ n ++ "=(x)"
    cVar n = "pptframe_" ++ n ++ "_t " ++ (gVarName n) ++ ";"
    cHandle n = "int " ++ "_ppt_hmem_" ++ n ++ ";"
    cSize n = "int " ++ "_ppt_hsize_" ++ n ++ ";"
    cFrameVersion n hash = "unsigned char _ppt_version_" ++ n ++ "[] = {" ++
                  (join "," (map show hash)) ++ "};"

    cFrameVersionDecl n hash = "unsigned char _ppt_version_" ++ n ++ "[" ++
                      (show . length $ (map show hash)) ++ "];"

    cWFrameDecl (FrameSpecification n es) = "/* todo: decl write method for " ++ n ++ "*/"
    cWFrameBody (FrameSpecification n es) = "/* todo: define write method for " ++ n ++ "*/"
  
writeDecls :: EmissionSpec -> FrameSpecification -> String
writeDecls es spec@(FrameSpecification name elems) =
            let w = writerFor es
                e s = "extern " ++ s
                typedecl = (frameWrapper w) name (map (\x -> (memberWrapper w) x) elems)
                gvar = (globalVar w) name
                write_macros = concatMap (\e -> (writeMacro w) name e) elems
                shHandle = (shmemHandle w) name
                shSize = (shmemSize w) name
                verno = (frameVersionDecl w) name (specHash spec)
                writeDecl = (frameWriteDecl w) spec
             in join "\n" [typedecl, e gvar, write_macros, e shHandle, e shSize,
                e verno, writeDecl]
                
-- spec -> (header_filename, source_filename) -> (header text, source
-- text)
emitC :: FullSpecification -> (String, String) -> (String, String) 
emitC (Spec emit fs) (hname, sname) =
  {-
  Each FrameSpec will need:

  // header file
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

  // source file
   foreach type: {
    global var
    shmem handle
    shmem ptrs
    frame_write routine 
  -}
  let
    headerGuards fname inner = 
      "#ifndef " ++ sym ++ "\n#define " ++ sym ++ "\n#include <sys/time.h>\n" ++ inner ++ 
      "\n#endif /* #ifdef " ++ sym ++ "*/\n"
      where sym = "INCLUDE_" ++ (map toUpper (replace "." "_" fname))

      
    header = headerGuards hname (concatMap (writeDecls emit) fs)
    source = "/* implement me too */\n"
  in (header, source)

runParse :: String -> String -> RunConfig -> IO ()
runParse file basefname cfg = do
  text <- readFile file 
  let result = SIP.parseText SIP.commandFile text file
  case result of
    Left err -> putStrLn ("ERROR: " ++ (show err))
    Right spec@(Spec emit frames) ->
      let impl = implement cfg spec in
      putStrLn (show impl) >>
      case emit of
        LangC ->
          let f1 = head frames in
          let dstpath = specPath cfg f1 in
          let header_name = (basefname ++ ".h") in
          let source_name = (basefname ++ ".c") in
          let (header, source) = 
                emitC spec (header_name, source_name)  in
          makeSpecPath cfg f1 >>
          writeFile (dstpath ++ header_name) header >> 
          writeFile (dstpath ++ source_name) source >>
          L.generateReader spec (dstpath ++ basefname ++ ".lli")
    
        LangCpp ->
          let header_name = basefname ++ ".h"
              source_name = basefname ++ ".cpp"
              (header, source) = emitC spec (header_name, source_name)
          in
          writeFile header_name header >>
          writeFile source_name source >>
          L.generateReader spec (basefname ++ ".lli")

--  putStrLn (show result) 
  

generate :: [String] -> RunConfig -> IO ()
generate args cfg = do 
  let res = GO.getOpt GO.Permute arglist args
  case res of
    (opts, file, []) -> 
      runParse (head file) basename cfg
      where
        isfname (OutputFile _) = True
        basename = case find isfname opts of
          Just (OutputFile n) -> n
          Nothing -> takeWhile (/= '.') (head file)
    (_, _, _) ->
      putStrLn (show res)
                   