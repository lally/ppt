{- C Code Generator 
   ----------------

   A header and source file are generated, which are stock C and
   POSIX.  HStringTemplate is used for clarity.

-}

module Generate.C (emitC) where
import Configuration
import StaticInstrumentation
import Data.Char (toUpper)
import Data.List.Utils (replace)
import Text.StringTemplate


hasFrameElement :: ImplMember -> Bool
hasFrameElement (ImplMember (Just _) _) = True
hasFrameElement (ImplMember Nothing _) = False
         

memberName :: ImplMember -> String
memberName (ImplMember (Just (FrameElement _ nm)) _) = nm

typeBody :: RunConfig -> ImplMember -> Int -> String
typeBody cfg (ImplMember Nothing IMSeqno) _ = "int ppt_seqno"
typeBody cfg (ImplMember Nothing (IMPad n)) i= "unsigned char  ppt_seqno" ++ (show i) ++ "["++(show n) ++"]"
typeBody cfg (ImplMember (Just fe@(FrameElement FDouble nm)) IMDouble) _ = "double " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FFloat nm)) IMFloat) _ = "float " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FInt nm)) IMInt) _ = "int " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FTime nm)) IMTime) _ = "struct timeval " ++ nm


makeMacro :: String -> ImplMember -> String
makeMacro frame (ImplMember (Just (FrameElement _ mem)) _) = 
          let tstr = "WRITE_$ufr$_$umem$(x) _ppt_frame_$frame$.$mem$ = (x)"
              t = newSTMP tstr :: StringTemplate String
              u = setManyAttrib [("mem", mem), ("frame", frame), 
                                 ("ufr", (map toUpper frame)), ("umem", (map toUpper mem))] t
          in render u

makeHeader :: RunConfig -> FullImplementation -> String -> String
makeHeader cfg impl@(Impl _ nm fs) fname =
         let tstr = unlines ["#ifndef $sym$",
                    "#define $sym$",
                    "",
                    "#include <sys/time.h>",
                    "",
                    "typedef struct ppt_tag_struct_$nm$ {",
                    "  $names; separator=\";\n  \"$",
                    "} pptframe_$nm$_t __attribute__ ((packed));",
                    "",
                    "#define $macros; separator=\"\n#define \"$",
                    "",
                    "extern ppt_frame_$nm$_t _ppt_frame_$nm$;",
                    "extern int _ppt_hmem_$nm$;",
                    "extern int _ppt_hsize_$nm$;",
                    "extern int _ppt_version_$nm$[16];",
                    "void ppt_write_$nm$_frame();",
                    "#endif /* #ifndef $sym$ */",
                    ""]
             t = newSTMP tstr :: StringTemplate String
             -- now that we have the template, add the members
             nameValues = (map (\(a,b) -> typeBody cfg a b) $ zip fs [1..]) :: [String]
             macroValues =  map (makeMacro nm) $ filter hasFrameElement fs
             sym = "INCLUDE_" ++ (map toUpper (replace "." "_" fname))
             -- put in the scalar attributes
             scalarTempl = setManyAttrib [("sym", sym), ("nm",nm)] t
             fullTempl = setManyAttrib [("names",nameValues), ("macros", macroValues)] scalarTempl
          in render fullTempl
             

makeSource :: RunConfig -> FullImplementation -> String -> String
makeSource c impl@(Impl _ nm fs) fname = -- undefined
           let tstr = unlines ["#include \"$fname$\"",
                               "",
                               "#include <sys/types.h>",
                               "#include <sys/ipc.h>",
                               "#include <sys/shm.h>",
                               "",
                               "static pptframe_$nm$_t *s_start, *s_end, *s_cur;",
                               "",
                               "void ppt_write_$nm$_frame() {",
                               "  if (_ppt_hmem_$nm$) {",
                               "      if (s_start) {",
                               "          $assigns; separator=\";\n          \"$",
                               "          s_cur->a = _ppt_frame_$nm$.a;",
                               "          ...;",
                               "          __sync_synchronize(); // gcc builtin",
                               "          s_cur->ppt_seqno = _ppt_frame_$nm$.seqno++;",
                               "          s_cur++;",
                               "          if (s_cur == s_end) { s_cur = s_start; }",
                               "      } else {",
                               "          int h = _ppt_hmem_$nm$;",
                               "          // determine the size of the shared memory segment, and attach it.",
                               "          struct shmid_ds buf;",
                               "          if (shmctl(h, IPC_STAT, &buf) != 0",
                               "              || ((s_start = (ppt_frame_$nm$_t *) shmat(h, 0, 0600))) == (ppt_frame_$nm$_t *) -1) {",
                               "              _ppt_hmem_$nm$ = 0;",
                               "              return;  // abort attach.",
                               "          }",
                               "          s_end = s_start + (buf.shm_segsz / sizeof(struct ppt_frame_$nm$_t));",
                               "          s_cur = s_start;",
                               "      }",
                               "  } else if (s_start) {",
                               "      shmdt(s_start);",
                               "      s_start = 0;",
                               "  }",
                               "}"]
               t = newSTMP tstr ::StringTemplate String
               mems = map memberName $ filter hasFrameElement fs
               assigns = map (\e -> "s_cur->" ++ e ++ " = _ppt_frame_" ++ nm ++ "." ++ e) mems
           in render $ setAttribute "nm" nm $ setAttribute "assigns" assigns t

emitC :: RunConfig -> FullImplementation -> String -> (String, String)
emitC cfg impl fname = (makeHeader cfg impl fname, makeSource cfg impl fname)
