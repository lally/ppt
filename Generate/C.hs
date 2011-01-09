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
typeBody cfg (ImplMember Nothing IMSeqno) _ = "/* ppt-private */ int ppt_seqno"
typeBody cfg (ImplMember Nothing (IMPad n)) i= "/* ppt-private */ unsigned char  ppt_pad" ++ (show i) ++ "["++(show n) ++"]"
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
                    "  $names; separator=\";\n  \"$;",
                    "} pptframe_$nm$_t __attribute__ ((packed));",
                    "",
                    "#define $macros; separator=\"\n#define \"$",
                    "",
                    "extern pptframe_$nm$_t _ppt_frame_$nm$;",
                    "extern int _ppt_hmem_$nm$;",
                    "extern int _ppt_hsize_$nm$;",
                    "extern int _ppt_version_$nm$[16];",
                    "#ifdef _cplusplus",
                    "extern \"C\" {",
                    "#endif",
                    "void ppt_write_$nm$_frame();",
                    "#ifdef _cplusplus",
                    "}",
                    "#endif",
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
           let tstr = unlines ["#include \"$fname$.h\"",
                               "",
                               "#include <sys/types.h>",
                               "#include <sys/ipc.h>",
                               "#include <sys/shm.h>",
                               "",
                               "static pptframe_$nm$_t *s_start, *s_end, *s_cur;",
                               "pptframe_$nm$_t _ppt_frame_$nm$;",
                               "int _ppt_hmem_$nm$;",
                               "int _ppt_hsize_$nm$;",
                               "int _ppt_version_$nm$[16];",
                               "",
                               "void ppt_write_$nm$_frame() {",
                               "  if (_ppt_hmem_$nm$) {",
                               "      if (s_start) {",
                               "          $assigns; separator=\";\n          \"$;",
                               "          __sync_synchronize(); // gcc builtin",
                               "          s_cur->ppt_seqno = _ppt_frame_$nm$.ppt_seqno++;",
                               "          s_cur++;",
                               "          if (s_cur == s_end) { s_cur = s_start; }",
                               "      } else {",
                               "          int h = _ppt_hmem_$nm$;",
                               "          // determine the size of the shared memory segment, and attach it.",
                               "          struct shmid_ds buf;",
                               "          if (shmctl(h, IPC_STAT, &buf) != 0",
                               "              || ((s_start = (pptframe_$nm$_t *) shmat(h, 0, 0600))) == (pptframe_$nm$_t *) -1) {",
                               "              _ppt_hmem_$nm$ = 0;",
                               "              return;  // abort attach.",
                               "          }",
                               "          s_end = s_start + (buf.shm_segsz / sizeof(pptframe_$nm$_t));",
                               "          s_cur = s_start;",
                               "      }",
                               "  } else if (s_start) {",
                               "      shmdt(s_start);",
                               "      s_start = 0;",
                               "  }",
                               "}",
                               ""]
               t = newSTMP tstr ::StringTemplate String
               mems = map memberName $ filter hasFrameElement fs
               assigns = map (\e -> "s_cur->" ++ e ++ " = _ppt_frame_" ++ nm ++ "." ++ e) mems
           in render $ setAttribute "fname" fname $ setAttribute "nm" nm $ setAttribute "assigns" assigns t

isPartOfOutput :: ImplMember -> Bool
isPartOfOutput (ImplMember (Just _) _) = True
isPartOfOutput (ImplMember Nothing IMSeqno) = True
isPartOfOutput (ImplMember Nothing _) = False

memberNames :: ImplMember -> [String]
memberNames (ImplMember (Just (FrameElement _ nm)) IMTime) = [nm ++ ".tv_sec", nm ++ "tv_usec"]
memberNames (ImplMember (Just (FrameElement _ nm)) _) = [nm]
memberNames (ImplMember Nothing IMSeqno) = ["ppt_seqno"]

memberFormat :: ImplMember -> [String]
memberFormat (ImplMember _ IMDouble) = ["%10.8f"]
memberFormat (ImplMember _ IMFloat)  = ["%10.8f"]
memberFormat (ImplMember _ IMInt) = ["%d"]
memberFormat (ImplMember _ IMSeqno) = ["%d"]
memberFormat (ImplMember _ IMTime) = ["%d", "%d"]



makeConverter :: RunConfig -> FullImplementation -> String -> String
makeConverter c impl@(Impl _ nm fs) fname = 
           let tstr = unlines ["#include <stdio.h>",
                               "#include <stdlib.h>",
                               "",
                               "typedef struct tag_pptframe_$nm$_t {",
                               "  $elements; separator=\";\n  \"$;",
                               "} pptframe_$nm$_t __attribute__ ((packed));",
                               "",
                               "",
                               "int main(int args, char ** argv) {",
                               "    if (args <3) {",
                               "        printf(\"usage: %s infile outfile\\\\n\", argv[0]);",
                               "        puts  (\"  to print out raw $nm$ entries to tab-separated text.\");",
                               "        exit(1);",
                               "    }",
                               "",
                               "    FILE *in, *out;",
                               "    if (!(in = fopen(argv[1], \"r\"))) {",
                               "        puts(argv[1]);",
                               "        exit(1);",
                               "    }",
                               "",
                               "    if (!(out = fopen(argv[2], \"w+\"))) {",
                               "        puts(argv[2]);",
                               "        exit(1);",
                               "    }",
                               "",
                               "    fprintf(out, \"$names; separator=\"\\\\t\"$\\\\n\");",
                               "",
                               "    while (1) {",
                               "        pptframe_$nm$_t buf;",
                               "        if (!fread(&buf, sizeof(pptframe_$nm$_t), 1, in)) {",
                               "            fclose(out);",
                               "            exit(0);",
                               "        }",
                               "        fprintf(out, \"$formats; separator=\"\\\\t\"$\\\\n\", buf.$names; separator=\", buf.\"$);",
                               "    }",
                               "",
                               "    return 0;",
                               "}",
                               ""]
               t = newSTMP tstr ::StringTemplate String
               mems =  filter isPartOfOutput fs
               elements = (map (\(a,b) -> typeBody c a b) $ zip fs [1..]) :: [String]
               names =  concatMap memberNames mems
               formats = concatMap memberFormat mems
           in render $ setAttribute "formats" formats $ setAttribute "names" names $ setAttribute "nm" nm $ setAttribute "elements" elements t

emitC :: RunConfig -> FullImplementation -> String -> (String, String, String)
emitC cfg impl fname = (makeHeader cfg impl fname, makeSource cfg impl fname, makeConverter cfg impl fname)
