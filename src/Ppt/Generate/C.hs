{- C Code Generator 
   ----------------

   A header and source file are generated, which are stock C and
   POSIX.  HStringTemplate is used for clarity.

-}

module Ppt.Generate.C (emitC) where
import Ppt.Configuration

import Ppt.StaticInstrumentation
import Data.Char (toUpper)
import Data.List.Utils (replace)
import Text.StringTemplate
import Text.Printf (printf)

hasFrameElement :: ImplMember -> Bool
hasFrameElement (ImplMember (Just _) _) = True
hasFrameElement (ImplMember Nothing _) = False
         

memberName :: ImplMember -> String
memberName (ImplMember (Just (FrameElement _ nm)) _) = nm

typeBody :: RunConfig -> ImplMember -> Int -> String
typeBody cfg (ImplMember Nothing (IMSeqno SFront)) _ = "/* ppt */ int ppt_seqno"
typeBody cfg (ImplMember Nothing (IMSeqno SBack)) _ = "/* ppt */ int ppt_seqno_back"
typeBody cfg (ImplMember Nothing IMDescriminator) _ = "/* ppt */ int ppt_type"
typeBody cfg (ImplMember Nothing (IMPad n)) i = 
         "/* ppt */ unsigned char  ppt_pad" ++ (show i) ++ "["++(show n) ++"]"
typeBody cfg (ImplMember (Just fe@(FrameElement FDouble nm)) IMDouble) _ = "double " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FFloat nm)) IMFloat) _ = "float " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FInt nm)) IMInt) _ = "int " ++ nm
typeBody cfg (ImplMember (Just fe@(FrameElement FTime nm)) IMTime) _ = "struct timeval " ++ nm


makeMacro :: String -> ImplMember -> String
makeMacro frame (ImplMember (Just (FrameElement _ mem)) _) = 
          let tstr = "WRITE_$ufr$_$umem$(_PARAM_) _ppt_frame_$frame$.$mem$ = (_PARAM_)"
              t = newSTMP tstr :: StringTemplate String
              u = setManyAttrib [("mem", mem), ("frame", frame), 
                                 ("ufr", (map toUpper frame)), ("umem", (map toUpper mem))] t
          in render u

makeFrameHeaderDecl :: RunConfig -> ImplFrame -> String
makeFrameHeaderDecl cfg (ImplFrame frname members) =
              let templstr = unlines [
                                     "//",
                                     "// Frames for $nm$",
                                     "typedef struct ppt_tag_struct_$nm$ {",
                                     "  // $nrnames$ members",
                                     "  $names; separator=\";\n  \"$;",
                                     "} pptframe_$nm$_t;",
                                     "",
                                     "extern pptframe_$nm$_t _ppt_frame_$nm$;",
                                     "#define $macros; separator=\"\n#define \"$",
                                     ""]
                  -- Assemble $names and $macros.
                  nameValues = (map (\(a,b) -> typeBody cfg a b) $ zip members [1..]) :: [String]
                  macroValues = map (makeMacro frname) $ filter hasFrameElement members
                  -- Build our base template
                  scalarTempl = setManyAttrib [("nm", frname), ("nrnames", show (length nameValues))] $ newSTMP templstr
                  -- And shove them into the template.
                  fullTempl = setManyAttrib [("names", nameValues), ("macros", macroValues)] scalarTempl
               in render fullTempl


makeFrameDecl :: RunConfig -> ImplFrame -> String
makeFrameDecl cfg (ImplFrame frname members) =
              let templstr = unlines [
                                     "//",
                                     "// Frames for $nm$",
                                     "typedef struct ppt_tag_struct_$nm$ {",
                                     "  $names; separator=\";\n  \"$;",
                                     "} pptframe_$nm$_t;",
                                     ""]
                  -- Build our base template
                  scalarTempl = setManyAttrib [("nm", frname)] $ newSTMP templstr
                  -- Assemble $names and $macros.
                  nameValues = (map (\(a,b) -> typeBody cfg a b) $ zip members [1..]) :: [String]
                  -- And shove them into the template.
                  fullTempl = setManyAttrib [("names", nameValues)] scalarTempl
               in render fullTempl

makeWriteDecl :: RunConfig -> ImplFrame -> String
makeWriteDecl cfg (ImplFrame frname members) =
              "void ppt_write_" ++ frname ++ "_frame()"


makeHeader :: RunConfig -> FullImplementation -> String -> String
makeHeader cfg impl@(Impl _ nm frames) fname =
         let tstr = unlines ["#ifndef $sym$",
                    "#define $sym$",
                    "",
                    "#include <sys/time.h>",
                    "",
                    "$framedecls; separator=\"\n\n\"$",
                    "",
                    "// Transfer buffer variables for $nm$",
                    "extern int _ppt_hmem_$nm$;",
                    "extern int _ppt_hsize_$nm$;",
                    "extern unsigned char _ppt_version_$nm$[16];",
                    "",
                    "#ifdef _cplusplus",
                    "extern \"C\" {",
                    "#endif",
                    "$writedecls; separator=\";\n\"$;",
                    "#ifdef _cplusplus",
                    "}",
                    "#endif /* #ifdef _cplusplus */",
                    "#endif /* #ifndef $sym$ */",
                    ""]
             t = newSTMP tstr :: StringTemplate String
             -- Build the frame decl templates
             declTemplates = map (makeFrameHeaderDecl cfg) frames
             writeDecls = map (makeWriteDecl cfg) frames
             sym = "INCLUDE_" ++ (map toUpper (replace "." "_" fname))
             -- put in the scalar attributes
             scalarTempl = setManyAttrib [("sym", sym), ("nm",nm)] t
             fullTempl = setManyAttrib [("framedecls", declTemplates), ("writedecls", writeDecls)] scalarTempl
          in render fullTempl
             
makeCReader :: RunConfig -> FullSpecification -> FullImplementation -> String -> String
makeCReader c spec impl@(Impl _ nm fs) fname =
  let tstr = unlines [ "#include <sys/types.h>",
                       "#include <sys/shm.h>",
                       "#include \"$header$\"",
                       "#include <stdio.h>",
                       "#include <stdlib.h>",
                       "#include <unistd.h>",
                       "",
                       "int min(int a, int b) { return a<b? a:b; }",
                       "",
                       "int main(int args, char ** argv) {",
                       "    if (args < 4) return 1;",
                       "    FILE *out = fopen(argv[1], \"w\");",
                       "    int shm_handle = atoi(argv[2]);",
                       "    int shm_sz = atoi(argv[3]);",
                       "    if (!out || shm_handle <= 0 || shm_sz <= 0)  { return 1; }",
                       "    shm_sz /= (sizeof ($structname$));",
                       "    $structname$ *start, *end, *cur;",
                       "    start = ($structname$ *) shmat(shm_handle, 0, SHM_RDONLY);",
                       "    end = &start[shm_sz];",
                       "    cur = start;",
                       "    int last_cur_seqno = 0, last_seqno=0, delay=100, stride=0;",
                       "    int count = 0;",
                       "    int cont_read = 1;",
                       "    while (1) {",
                       "      count = 0;",
                       "      cont_read = 1;",
                       "      stride = 0;",
                       "      while (cur->ppt_seqno > last_cur_seqno",
                       "             || cur->ppt_seqno <= min(last_seqno - shm_sz, 0)",
                       "             || (count == 0 && cur->ppt_seqno != last_cur_seqno)) {",
                       "        last_cur_seqno = cur->ppt_seqno;",
                       "        stride++;",
                       "        count++;",
                       "        cur++;",
                       "        if (end == cur) {",
                       "          fwrite(cur - stride, stride, sizeof ($structname$), out);",
                       "          cur = start;",
                       "          stride = 0;",
                       "        }",
                       "      }",
                       "      fwrite(cur - stride, stride, sizeof ($structname$), out);",
                       "      last_seqno = last_cur_seqno;",
                       "      int old_delay= delay;",
                       "      double rate = count / (0.0 + delay);",
                       "",
                       "      if (!count) {",
                       "        delay = 100;",
                       "      } else if (count < (shm_sz / 8)) {",
                       "        const int desired = shm_sz / 2;",
                       "        // we slept too little.  This past 'delay' got us 'count'",
                       "        // elements.  Multiply up.",
                       "        delay= delay * (desired / count);",
                       "      }",
                       "      else if (count > (7 * shm_sz / 8)) {",
                       "        // we slept too long.  The 'rate' (count/delay) should be",
                       "        // reasonably stable (if possibly too low).  Scale down.",
                       "        // rate eqn = count / delay.  new delay = sz/2 * rate",
                       "        // delay = sz /2 * count / delay = sz * count / (2 * delay)",
                       "        delay = (shm_sz / 2) * rate;",
                       "      }",
                       "      if (delay < 10) delay = 10;",
                       "      if (delay > 2000) delay = 2000;",
                       "      printf (\"Sleeping for %d ms. last_seqno=%d\\\\n\", delay, last_seqno);",
                       "      usleep(delay * 1000);",
                       "    }",
                       "}",
                       "" ]
      t = newSTMP tstr :: StringTemplate String
      (ImplFrame firstNm _) = head fs
      fullTempl = setManyAttrib [("header",  fname ++ ".h"), 
                                 ("structname", "pptframe_" ++ firstNm ++ "_t")] t
  in render fullTempl

makeSource :: RunConfig -> FullSpecification -> FullImplementation -> String -> String
makeSource c spec impl@(Impl _ nm fs) fname = 
           -- Notes: $nm$ here is the implementation's name $first$ here is the
           -- first frame implementation we get.  We use it to act as the basis
           -- for the rest.  We'll statically initialize the static frame data
           -- with their type descriminator values, and just never touch them
           -- again (copying them dumbly over).
           let tstr = unlines ["#include \"$fname$.h\"",
                               "",
                               "#include <sys/types.h>",
                               "#include <sys/ipc.h>",
                               "#include <sys/shm.h>",
                               "#include <string.h>",
                               "#include <stdio.h>",
                               "",
                               "#ifndef __GNUC__",
                               "#define GCC_VERSION 0",
                               "#else",
                               "#define GCC_VERSION (__GNUC__ * 10000 \\\\",
                               "                     + __GNUC_MINOR__ * 100  \\\\",
                               "                     + __GNUC_PATCHLEVEL__)",
                               "#endif",
                               "",
                               "#if GCC_VERSION < 40000",
                               "#define __sync_synchronize()",
                               "#endif",
                               "",
                               "static pptframe_$first$_t *s_start, *s_end, *s_cur;",
                               "",
                               "$typeDecls; separator=\";\n\"$;",
                               "",
                               "int _ppt_hmem_$nm$;",
                               "int _ppt_hsize_$nm$;",
                               "unsigned char _ppt_version_$nm$[16] = {$vbytes; separator=\", \"$};",
                               "",
                               "void ppt_write_$nm$_frame(pptframe_$first$_t *src) {",
                               "  static int ppt_$nm$_seqno = 1;",
                               "  static int ppt_last_handle = 0;",
                               "  if (_ppt_hmem_$nm$) {",
                               "      if (s_start && _ppt_hmem_$nm$ == ppt_last_handle) {",
                               "          s_cur->ppt_seqno = 0;",
                               "          __sync_synchronize(); // gcc builtin",
                               "          memcpy(s_cur, src, sizeof(pptframe_$first$_t));",
                               "          __sync_synchronize(); // gcc builtin",
                               "          if (++ppt_$nm$_seqno < 0) { ppt_$nm$_seqno = 1; }",
                               "          s_cur->ppt_seqno_back = ppt_$nm$_seqno;",
                               "          s_cur->ppt_seqno = ppt_$nm$_seqno;",
                               "          s_cur++;",
                               "          if (s_cur == s_end) { s_cur = s_start; }",
                               "      } else {",
                               "          int h = _ppt_hmem_$nm$;",
                               "          if (ppt_last_handle)",
                               "              shmdt(s_start);",
                               "          ppt_last_handle = _ppt_hmem_$nm$;",
                               "",
                               "          // determine the size of the shared memory segment, and attach it.",
                               "          struct shmid_ds buf;",
                               "          if (shmctl(h, IPC_STAT, &buf) != 0",
                               "              || ((s_start = (pptframe_$first$_t *) shmat(h, 0, 0600))) ",
                               "                     == (pptframe_$first$_t *) -1) {", 
                               "              _ppt_hmem_$nm$ = 0;",
                               "              perror(\"shmat for $nm$\");",
                               "              return;  // abort attach.",
                               "          }",
                               "          s_end = s_start + (buf.shm_segsz / sizeof(pptframe_$first$_t));",
                               "          s_cur = s_start;",
                               "      }",
                               "  } else if (s_start) {",
                               "      shmdt(s_start);",
                               "      s_start = 0;",
                               "  }",
                               "}",
                               "",
                               "#ifdef _cplusplus",
                               "extern \"C\" {",
                               "#endif",
                               "",
                               "$writeDecls; separator=\"\n\n\"$",
                               "",
                               "#ifdef _cplusplus",
                               "}",
                               "#endif",
                               ""]
               t = newSTMP tstr ::StringTemplate String
               ver_bytes = map show $ specHash spec
               (ImplFrame first _) = head fs
               makeTypeDecl (ImplFrame nm _) ty =
                            "pptframe_" ++ nm ++ "_t _ppt_frame_" ++ nm ++ " = { 0, " ++ (show ty) ++ " }"
               makeWriteDecl (ImplFrame fnm _) = 
                             "void ppt_write_" ++ fnm ++ "_frame() {\n"
                             ++ "    ppt_write_" ++ nm ++ "_frame((pptframe_" ++ first ++ "_t*) &_ppt_frame_" ++ fnm ++ ");\n"
                             ++ "}\n"
               writeDecls = map makeWriteDecl fs
               typeDecls = map (\(f,t) -> makeTypeDecl f t) $ zip fs [1..]
               arrayTempl = setManyAttrib [("vbytes", ver_bytes), ("writeDecls", writeDecls), 
                                          ("typeDecls", typeDecls)] t
               scalarTempl = setManyAttrib [("fname", fname), ("nm", nm), ("first", first)] arrayTempl
           in render scalarTempl

isPartOfOutput :: ImplMember -> Bool
isPartOfOutput (ImplMember (Just _) _) = True
isPartOfOutput (ImplMember Nothing (IMSeqno SFront)) = True
isPartOfOutput (ImplMember Nothing _) = False

memberNames :: ImplMember -> [String]
memberNames (ImplMember (Just (FrameElement _ nm)) IMTime) = [nm ++ ".tv_sec", nm ++ ".tv_usec"]
memberNames (ImplMember (Just (FrameElement _ nm)) _) = [nm]
memberNames (ImplMember Nothing (IMSeqno _)) = ["ppt_seqno"]

memberFormat :: ImplMember -> [String]
memberFormat (ImplMember _ IMDouble) = ["%10.8f"]
memberFormat (ImplMember _ IMFloat)  = ["%10.8f"]
memberFormat (ImplMember _ IMInt) = ["%d"]
memberFormat (ImplMember _ (IMSeqno _)) = ["%d"]
memberFormat (ImplMember _ IMTime) = ["%d", "%d"]

makeOpenFunction :: RunConfig -> ImplFrame -> String
makeOpenFunction cfg (ImplFrame name elems) =
                 let templStr = unlines ["    sprintf(namebuf, \"%s_$name$.csv\", argv[2]);",
                                         "    if (!(out_$name$ = fopen(namebuf, \"w+\"))) {",
                                         "      puts(namebuf);",
                                         "      exit(1);",
                                         "    }",
                                         "    fprintf(out_$name$, \"$names; separator=\"\\\\t\"$\\\\n\");",
                                         ""]
                     printables = filter isPartOfOutput elems
                     names = concatMap memberNames printables
                     t = newSTMP templStr :: StringTemplate String
                     t' = setAttribute "names" names t
                     t'' = setAttribute "name" name t'
                  in render t''
                     

makeCase :: RunConfig -> ImplFrame -> Int -> String
makeCase cfg frame@(ImplFrame name members) k = 
         let templStr = unlines [
                           "case $nr$: {",
                           "          pptframe_$name$_t *tmp = (pptframe_$name$_t*) &buf;",
                           "          if (tmp->ppt_seqno && tmp->ppt_seqno == tmp->ppt_seqno_back) {",
                           "            fprintf(out_$name$, \"$formats; separator=\"\\\\t\"$\\\\n\",",
                           "                    tmp->$names; separator=\", tmp->\"$);",
                           "          }",
                           "        } break;"]
             t = newSTMP templStr :: StringTemplate String
             mems = filter isPartOfOutput members
             names = concatMap memberNames mems
             formats = concatMap memberFormat mems
             t' = setManyAttrib [("nr", (show k)), ("name", name)] t
          in render $ setManyAttrib [("names", names), ("formats", formats)] t'

makeConverter :: RunConfig -> FullImplementation -> String -> String
makeConverter cfg impl@(Impl _ nm frames) _ = 
           -- 'nm' is the name of the entire implementation (e.g. the buffer).
           -- 'firstname' is the name of the first frame type.
           let tstr = unlines ["#include <stdio.h>",
                               "#include <stdlib.h>",
                               "",
                               "$framedecls; separator=\"\n\n\"$",
                               "",
                               "",
                               "int main(int args, char ** argv) {",
                               "    char namebuf[128];",
                               "    FILE *in, *$outnames; separator=\", *\"$;",
                               "",
                               "    if (args <3) {",
                               "        printf(\"usage: %s infile outfile_base\\\\n\", argv[0]);",
                               "        puts  (\"  to print out raw $nm$ entries to tab-separated text.\");",
                               "        exit(1);",
                               "    }",
                               "",
                               "    if (!(in = fopen(argv[1], \"r\"))) {",
                               "        puts(argv[1]);",
                               "        exit(1);",
                               "    }",
                               "",
                               "$opens; separator=\"\n\n\"$",
                               "",
                               "    while (1) {",
                               "        pptframe_$firstname$_t buf;",
                               "        if (!fread(&buf, sizeof(pptframe_$firstname$_t), 1, in)) {",
                               "            goto finish;",
                               "        }",
                               "        switch (buf.ppt_type) {",
                               "        $cases; separator=\"\n        \"$",
                               "        }",
                               "    }",
                               "",
                               "finish:",
                               "$closes; separator=\"\n\"$",
                               "    return 0;",
                               "}",
                               ""]
               t = newSTMP tstr ::StringTemplate String
               framedecls = map (makeFrameDecl cfg) frames
               cases = map (\(frame, nr) -> makeCase cfg frame nr) $ zip frames [1..]
               opens = map (makeOpenFunction cfg) frames
               closes = map (\(ImplFrame k _) -> ("    fclose(out_" ++ k ++ ");")) frames
               outnames = map (\(ImplFrame k _) -> ("out_" ++ k)) frames
               arrayTempl = setManyAttrib [("framedecls", framedecls), ("cases", cases), 
                                          ("opens", opens), ("closes", closes), ("outnames", outnames)] t
               (ImplFrame firstname _) = head frames
           in render $ setManyAttrib [("nm", nm), ("firstname", firstname)] arrayTempl

emitC :: RunConfig -> FullSpecification -> FullImplementation -> String -> (String, String, String, String)
emitC cfg spec impl fname = (makeHeader cfg impl fname, 
                             makeSource cfg spec impl fname, 
                             makeConverter cfg impl fname, 
                             makeCReader cfg spec impl fname)
