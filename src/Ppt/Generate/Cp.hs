{- |C++ Code generator -}
module Ppt.Generate.Cp (emitCp) where

import Ppt.Configuration
import Ppt.StaticInstrumentation

import System.FilePath
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

data MemberData = MB { mbMethods ::PP.Doc, mbMembers ::PP.Doc }
                | PrivateMem { pmbMembers ::PP.Doc }
                  deriving (Eq, Show)

-- |Derived from FullSpecification for whatever data we need for outputting.
data OutputCfg = OutputCfg { timeType :: String,
                             timeHeader :: String,
                             timeSave :: (String -> String),
                             indent :: Int,
                             defaultInit :: Bool }

-- |Clearly can only work for simple types.  No arrays.
dataMember :: String -> String -> PP.Doc
dataMember ty name = PP.text ty <> PP.text name <> PP.semi

-- |Returns the C++ name of the represented type.
rawType _ IMDouble = "double"
rawType _ IMFloat = "float"
rawType _ IMInt = "int"
rawType cfg IMTime = timeType cfg

-- |Generates a block declaration that's collapsable.
blockdecl :: OutputCfg ->PP.Doc ->PP.Doc -> [PP.Doc] ->PP.Doc
blockdecl cfg name sep elems = PP.hang (name <+> PP.lbrace) (indent cfg) $ PP.sep $ elems ++ [PP.rbrace]

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

-- |Breakdown of members in a given class declaration.
data ClassDecl = ClassDecl { publicMethods :: [PP.Doc]
                           , publicMembers :: [PP.Doc]
                           , privateMembers ::[ PP.Doc] } deriving (Eq, Show)

classDecl :: [MemberData] -> ClassDecl
classDecl [] = ClassDecl [] [] []
classDecl ((MB methods mems):rest) =
               let (ClassDecl pmeth pmem prmem) = classDecl rest
               in ClassDecl (methods:pmeth) (mems:pmem) prmem
classDecl ((PrivateMem ms):mems) =
               let (ClassDecl pmeth pmem prmem) = classDecl mems
               in ClassDecl pmeth pmem (ms:prmem)

makeClass :: OutputCfg -> String -> ClassDecl -> PP.Doc
makeClass cfg name (ClassDecl pubMeths pubMems privMems) =
                 let tag = PP.text $ "class " ++ name
                     privateBlock = [PP.text "private:"] ++ privMems
                     publicBlock = [PP.text "public:"] ++ pubMems ++ pubMeths
                 in blockdecl cfg tag PP.semi (privateBlock ++ publicBlock)
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

