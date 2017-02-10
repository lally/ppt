{- |C++ Code generator -}
module Ppt.Generate.Cp where

import Ppt.Configuration
import Ppt.Frame.ParsedRep
import Ppt.Frame.Layout

import System.FilePath
import qualified Data.Set as S
import qualified Data.List as L

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>),(<+>))
{-
For input:
  option clock timeval monotonic;

  frame mainloop {
      time start, end;
      int loop_iters;
  }
Exmaple Output:
Header:
 #pragma once

extern volatile int _ppt_saving_buffer_foo;

namespace ppt {
class mainloop {
private:
       int ppt_seqno;
       int ppt_type;
public:
       struct timeval start, end;
       int loop_iters;

       void snapshot_start() {
          clock_gettime(CLOCK_MONOTONIC, &start);
       }


private:
       int ppt_seqno_back;
};

// overloads for each type.
inline void writeout(const mainloop& ml) { if (_ppt_saving_buffer_foo) {  _ppt_save_foo(static_cast<void*>(&ml)); }}

}  // namespace ppt
-}
-- |Derived from EmitOptions for whatever data we need for outputting.
data OutputCfg = OutputCfg { timeType :: String, -- ^Decltype of time vars
                             timeHeader :: String,
                             -- ^Which header to include for time support
                             timeSave :: (String -> PP.Doc), -- ^Save time to a var
                             indent :: Int, -- ^Indentation depth
                             defaultInit :: Bool,
                             -- ^Does the constructor zero out the type?
                             multithreadWrite :: Bool
                             -- ^Use multithreaded write protocol?
                           }

-- |A member may require that a module be generated.  Without any
-- members requiring a module, we don't have to generate it at all.
data GenModule = GMCounters deriving (Eq, Show)

instance Ord GenModule where
  compare a b = EQ -- Only 1 module right now.

data MemberData = MB { mbMethods ::[PP.Doc], mbMembers ::[PP.Doc], mbHeaders :: [String]
                     , mbModules :: [GenModule] }
                 -- ^Declared members in the original frame specification
                | PrivateMem { pmbMembers ::[PP.Doc], pmbHeaders :: [String]
                             , pmbModules :: [GenModule] }
                 -- ^PPT private stuff (sequence numbers, type descriminator)
                  deriving (Eq, Show)

-- |Breakdown of members in a given class declaration.
data Decl  = ClassDecl { cName :: String
                       , publicMethods :: [PP.Doc]
                       , publicMembers :: [PP.Doc]
                       , privateMembers ::[PP.Doc]
                       , cHeaders :: [String]
                       , cModules :: [GenModule]}
           | ToplevelDecl { tHeaders :: [String]
                          , tDecls :: [PP.Doc]
                          , tModules :: [GenModule]}
           deriving (Eq, Show)

classDecl :: String -> [MemberData] -> Decl
classDecl n [] = ClassDecl n [] [] [] [] []
classDecl n ((MB methods mems hdrs mods):rest) =
               let (ClassDecl _ pmeth pmem prmem ns ms) = classDecl n rest
               in ClassDecl n (methods ++ pmeth) (mems ++ pmem) prmem (hdrs ++ ns) (mods ++ ms)
classDecl n ((PrivateMem ms hdrs mods):rest) =
               let (ClassDecl _ pmeth pmem prmem ns mds) = classDecl n rest
               in ClassDecl n pmeth pmem (ms ++ prmem) (hdrs ++ ns) (mods ++ mds)

writeDecl :: OutputCfg -> Decl -> PP.Doc
writeDecl cfg (ClassDecl name pubMeths pubMems privMems _ _) =
  let tag = PP.text $ "class " ++ name
      semify (e:es) = (PP.nest (indent cfg) e <> PP.semi):semify es
      semify [] = []
      indentify (e:es) = (PP.nest (indent cfg) e):indentify es
      indentify [] = []
      privateBlock = if length privMems > 0
        then (PP.text "private:"): semify privMems
        else []
      publicBlock = if length pubMems > 0 || length pubMems > 0
        then (PP.text "public:") : ((semify pubMems) ++ (indentify pubMeths))
        else []
  in PP.hang (tag <+> PP.lbrace) (indent cfg) $ PP.sep $ privateBlock ++ publicBlock ++ [
    PP.rbrace <> PP.semi]
writeDecl cfg (ToplevelDecl _ mems _) = PP.vcat (L.intersperse PP.semi mems)

-- |Clearly can only work for simple types.  No arrays.
dataMember :: String -> String -> PP.Doc
dataMember ty name = PP.text ty <+> PP.text name

-- |Generates a block declaration that's collapsable.
blockdecl :: OutputCfg -> PP.Doc ->PP.Doc -> [PP.Doc] ->PP.Doc
blockdecl cfg name sep elems =
  PP.hang (name <+> PP.lbrace) (indent cfg) $ PP.sep $ elems ++ [PP.rbrace]

funccall :: OutputCfg -> PP.Doc -> [PP.Doc] -> PP.Doc
funccall cfg name args =
  let prefix = name <+> PP.lparen
      prefixlen = length $ PP.render prefix
  in PP.hang (name <+> PP.lparen) prefixlen $ PP.hsep $ (L.intersperse PP.comma args) ++ [PP.rparen]

-- |Make member declarations out of a single literal (layed out) input member.
makeMember :: OutputCfg -> LayoutMember -> MemberData
makeMember cfg (LMember PTime _ _ _ k nm) =
  let timety = timeType cfg
      timeheaders = [timeHeader cfg]
  in (MB [blockdecl cfg (PP.text $ "void snapshot_" ++ nm ++ "()") PP.semi [
             timeSave cfg nm <> PP.semi]]
         [dataMember timety nm] timeheaders [])

makeMember cfg (LMember PCounter _ _ _ k nm) =
  MB [] [dataMember "uint64_t" nm] [] [GMCounters]
makeMember cfg (LMember PByte _ _ _ (LKPadding n) nm) =
  MB [] [dataMember "uint8_t"  (nm ++ "[" ++ show n ++ "]")] ["cstdint"] []
makeMember cfg (LMember ty _ _ _ k nm) =
  let declType = case ty of
        PDouble -> "double"
        PFloat -> "float"
        PInt -> "int"
  in MB [] [dataMember declType nm] [] []

makeFrameDecl :: OutputCfg -> FrameLayout -> Decl
makeFrameDecl cfg (FLayout nm fr layoutmems) =
  let mems = map (makeMember cfg) layoutmems
  in classDecl nm mems

allHeaders :: [Decl] -> [GenModule] -> [String]
allHeaders decls mods =
  let declsOf (ClassDecl _ _ _ _ h _) = h
      declsOf (ToplevelDecl h _ _) = h
      modsOf GMCounters = []
  in S.toList $ S.fromList $ (concatMap declsOf decls) ++ (concatMap modsOf mods)

allModules :: [Decl] -> [GenModule]
allModules decls =
  let modsOf (ClassDecl _ _ _ _ _ m) = m
      modsOf (ToplevelDecl _ _ m) = m
  in S.toList $ S.fromList $ concatMap modsOf decls

makeOutCfg :: EmitOptions -> OutputCfg
makeOutCfg (EmitOptions _ _ ETimeVal (ERuntime mt) _) =
  OutputCfg "struct timespec" "time.h" (\var -> PP.text $ "time(&" ++ var ++ ")") 4 True mt
makeOutCfg (EmitOptions _ _ (ETimeSpec src) (ERuntime mt) _) =
  let clock = case src of
        ETimeClockRealtime ->         "CLOCK_REALTIME"
        ETimeClockRealtimeCoarse ->   "CLOCK_REALTIME_COARSE"
        ETimeClockMonotonic ->        "CLOCK_MONOTONIC"
        ETimeClockMonotonicCoarse ->  "CLOCK_MONOTONIC_COARSE"
        ETimeClockMonotonicRaw ->     "CLOCK_MONOTONIC_RAW"
        ETimeClockBoottime ->         "CLOCK_BOOTTIME"
        ETimeClockProcessCputimeId -> "CLOCK_PROCESS_CPUTIME_ID"
        ETimeClockThreadCputimeId ->  "CLOCK_THREAD_CPUTIME_ID"
  in OutputCfg "struct timeval" "time.h" (
    \var -> PP.text $ "clock_gettime(" ++ clock ++ ", &" ++ var ++ ")") 4 True mt

includeHeaders :: [String] -> [PP.Doc]
includeHeaders (h:hs) =
  (mconcat $ map PP.text ["#include <", h, ">"]):includeHeaders hs
includeHeaders [] = []

-- Parts of the file:
-- The headers
-- Any runtime decls
-- The classes
-- The runtime support

cpFile :: EmitOptions -> [FrameLayout] -> PP.Doc
{-
 Derivation Process
 - Get declarations for frames
 - Get declarations for default runtimes
 - get declarations for enabled modules
 - Get headers and emit
 - Emit declarations for runtimes and modules and 
 - Emit decls (with inline definitions) for Classes (frames)
 - Emit definitions for runtime and enabled modules
-}
cpFile opts flayous =
  let cfg = makeOutCfg opts
      frameDecls = map (makeFrameDecl cfg) flayous
      mods = allModules frameDecls
      headers = allHeaders frameDecls mods
      headerDocs =
        let pfx = PP.text "#include <"
            sfx = PP.char '>'
        in map (\n -> pfx <> PP.text n <> sfx) headers
      allDocs = headerDocs ++ map (writeDecl cfg) frameDecls
  in PP.vcat allDocs


--    blockdecl cfg tag PP.semi (privateBlock ++ publicBlock)



{-
-- |Returns the C++ name of the represented type.
rawType _ IMDouble = "double"
rawType _ IMFloat = "float"
rawType _ IMInt = "int"
rawType cfg IMTime = timeType cfg


saveMember :: OutputCfg -> ImplMemberType -> String -> PP.Doc
saveMember cfg IMTime nm =
  blockdecl cfg (PP.text ("snapshot_" ++ nm)) PP.semi [ PP.text $ (timeSave cfg) nm ]

member :: OutputCfg -> ImplMember -> Int -> MemberData
member cfg (ImplMember Nothing (IMSeqno SFront)) _ = PrivateMem (dataMember "int" "ppt_seqno")
member cfg (ImplMember Nothing (IMSeqno SBack)) _ = PrivateMem (dataMember "int" "ppt_seqno_back")
member cfg (ImplMember Nothing IMDescriminator) _ = PrivateMem  (dataMember "int" "ppt_type")
member cfg (ImplMember Nothing (IMPad n)) i =
  PrivateMem  (dataMember "unsigned char" ("ppt_pad" ++ (show i) ++ "[" ++ (show n) ++ "]"))

member cfg (ImplMember (Just fe@(SingleElement _ nm)) ty) _ = (
  MB PP.empty (dataMember (rawType cfg ty) nm))

-- General plan:
-- - Put together a list ofPP.Docs that'll go through 'vcat' to get put in the final file.
-- - Make that a 'concat' of lists for the different declarations.

formatMembers :: OutputCfg -> [ImplFrame] -> PP.Doc
formatMembers _ [] = PP.empty
formatMembers c (im:ims) =
  let (ImplFrame nm mems) = im
      (ClassDecl pubMethods pubMembers privateMems) = classDecl $ map (
        \(a, b) -> member c a b) $ zip mems [1..]
  in blockdecl c (PP.text $ "class " ++ nm) PP.semi ([PP.text "public:"] ++
                                            pubMethods ++
                                            pubMembers ++
                                            [PP.text "private:"] ++
                                            privateMems)

-- |Generates a C++ header file.
makeHeader :: OutputCfg -> FullImplementation -> FilePath -> String
makeHeader cfg impl fname =
                 let outCfg = undefined :: OutputCfg
                     includeFiles =  []
                     (Impl spec buffer frames) = impl
                 in show $ PP.vcat (
                   includeFiles ++ [blockdecl outCfg (PP.text "namespace ppt") PP.semi [
                                       blockdecl outCfg (PP.text $"namespace " ++ buffer) PP.semi [
                                           formatMembers cfg frames]]])

emitCp :: RunConfig -> FullSpecification -> FullImplementation -> String -> (String, String, String, String)
emitCp cfg spec impl fname =
                   let outCfg = undefined
                   in (makeHeader outCfg impl fname, undefined, undefined, undefined)

-}
