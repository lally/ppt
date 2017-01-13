{- |C++ Code generator -}
module Ppt.Generate.Cp (emitCp) where

import Ppt.Configuration
import Ppt.StaticInstrumentation

import qualified Text.PrettyPrint as PP

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

data MemberData = MB { mbMethods :: Doc, mbMembers :: Doc }
                | PrivateMem { pmbMembers :: Doc }
                  deriving (Eq, Show)

-- |Derived from FullSpecification for whatever data we need for outputting.
data OutputCfg = OutputCfg { timeType :: String, timeSave :: (String -> String), indent :: Int, defaultInit :: Bool } deriving (Eq, Show)

-- |Clearly can only work for simple types.  No arrays.
dataMember :: String -> String -> PP.Doc
dataMember ty name = PP.text ty <> PP.text name <> PP.semi

-- |Returns the C++ name of the represented type.
rawType _ IMDouble = "double"
rawType _ IMFloat = "float"
rawType _ IMInt = "int"
rawType cfg IMTime = timeType cfg

saveMember :: OutputConfig -> ImplMemberType -> String -> PP.Doc
saveMember cfg IMTime nm =
           in blockdecl cfg ("snapshot_" ++ nm) PP.semi [ timeSave nm ]

member :: OutputCfg -> ImplMember -> Int -> MemberData
member cfg (ImplMember Nothing (IMSeqno SFront)) _ = PrivateMem (dataMember "int" "ppt_seqno")
member cfg (ImplMember Nothing (IMSeqno SBack)) _ = PrivateMem (dataMember "int" "ppt_seqno_back")
member cfg (ImplMember Nothing IMDescriminator) _ = PrivateMem  (dataMember "int" "ppt_type")
member cfg (ImplMember Nothing (IMPad n)) i = PrivateMem  (dataMember "unsigned char" ("ppt_pad" ++ (show i) ++ "[" ++ (show n) ++ "]")
member cfg (ImplMember (Just fe@(FrameElement _ nm) ty) _ = MB empty (dataMember (rawType cfg ty) nm)

-- |Generates a block declaration that's collapsable.
blockdecl :: OutputCfg Doc -> Doc -> [Doc]
blockdecl cfg name sep elems = hang (name <+> lbrace) (indent cfg) $ sep $ elems ++ [rbrace]

makeHeader :: RunConfig -> FullImplementation -> Filename -> String
makeHeader cfg impl fname = ""
           -- General plan:
           -- - Put together a list of Docs that'll go through 'vcat' to get put in the final file.
           -- - Make that a 'concat' of lists for the different declarations.
           -- - 

emitCp :: RunConfig -> FullSpecifiction -> FullImplementation -> String -> (String, String, String, String)
emitCp cfg spec impl fname = (makeHeader cfg impl fname, undefined, undefined, undefined)

