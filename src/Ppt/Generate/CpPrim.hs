-- |C++ Code Generator Primitives
module Ppt.Generate.CpPrim where
import Ppt.Frame.ParsedRep
import Ppt.Frame.Layout

import Text.PrettyPrint ((<>),(<+>))

import Ppt.Generate.CpConfig
import qualified Data.List as L

import qualified Text.PrettyPrint as PP

--
-- Class Member Generation
--

-- |Members to go into the final declaration.  Note that these *must*
-- be kept in the order presented, as they've already had memory
-- layout applied.
data MemberData = MB { mbMethods ::[PP.Doc], mbMember :: PP.Doc, mbHeaders :: [String]
                     , mbModules :: [GenModule] }
                 -- ^Declared members in the original frame specification
                | PrivateMem { pmbMember :: PP.Doc, pmbHeaders :: [String]
                             , pmbModules :: [GenModule] }
                 -- ^PPT private stuff (sequence numbers, type descriminator)
                  deriving (Eq, Show)

-- |Breakdown of members in a given class declaration.
data QualMember = PubMember PP.Doc
                | PrivMember PP.Doc
                deriving (Eq, Show)
data Decl  = ClassDecl { cName :: String
                       , cNr :: Maybe Int
                       , cMembers :: [QualMember]
                       , cMethods :: [PP.Doc]
                       , cHeaders :: [String]
                       , cModules :: [GenModule]}
           deriving (Eq, Show)

classDecl :: String -> [MemberData] -> Decl
classDecl n [] = ClassDecl n Nothing [] [] [] []
classDecl n ((MB methods mem hdrs mods):rest) =
               let (ClassDecl _ _ pmem pmeth ns ms) = classDecl n rest
               in ClassDecl n Nothing (PubMember mem:pmem) (methods ++ pmeth) (
                 hdrs ++ ns) (mods ++ ms)
classDecl n ((PrivateMem m hdrs mods):rest) =
               let (ClassDecl _ _ pmem meth ns mds) = classDecl n rest
               in ClassDecl n Nothing (PrivMember m:pmem) meth (hdrs ++ ns) (mods ++ mds)

qualBlock _ [] = []
qualBlock cfg ((PubMember n):ns) =
  let isPub (PubMember _) = True
      isPub _ = False
      pubPrefix = map (\(PubMember t) -> t) $ takeWhile isPub ns
      suffix = drop (length pubPrefix) ns
  in docPublic:(formatMems cfg (n:pubPrefix)) ++ qualBlock cfg suffix
qualBlock cfg ((PrivMember n):ns) =
  let isPub (PrivMember _) = True
      isPub _ = False
      pubPrefix = map (\(PrivMember t) -> t) $ takeWhile isPub ns
      suffix = drop (length pubPrefix) ns
  in docPrivate:(formatMems cfg (n:pubPrefix)) ++ qualBlock cfg suffix


-- |Make member declarations out of a single literal (layed out) input member.
makeMember :: OutputCfg -> LayoutMember -> MemberData
makeMember cfg (LMember (PTime _) _ _ _ _ nm) =
  let timety = timeType cfg
      timeheaders = [timeHeader cfg]
  in (MB [blockdecl cfg (PP.text $ "void snapshot_" ++ nm ++ "()") PP.semi [
             timeSave cfg nm]]
         (dataMember timety nm) timeheaders [])

-- These should have been layed out by now!
makeMember cfg (LMember (PCounter Nothing) _ _ _ k nm) = undefined
--  MB [] (dataMember "uint64_t" (nm ++ "[" ++ (show $ counterCount cfg) ++ "]")) [] [GMCounters]
makeMember cfg (LMember PByte _ _ _ (LKPadding n) nm) =
  PrivateMem (dataMember "uint8_t"  (nm ++ "[" ++ show n ++ "]")) ["cstdint"] []

-- TODO: Put in initializer here if defaultInit outputcfg
makeMember cfg (LMember (PCounter (Just v)) _ _ _ (LKMember _ _) nm) =
  MB [] (dataMember "uint64_t" (nm ++ "_" ++ (show v))) [] [GMCounters]

makeMember cfg (LMember ty _ _ _ (LKMember _ _) nm) =
  let declType = case ty of
        PDouble -> "double"
        PFloat -> "float"
        PInt -> "int"
  in MB [] (dataMember declType nm) [] []

makeMember cfg (LMember ty _ _ _ _ nm) =
  let declType = case ty of
        PDouble -> "double"
        PFloat -> "float"
        PInt -> "int"
  in PrivateMem (dataMember declType nm) [] []

sequenceDecls :: [Decl] -> [Decl]
sequenceDecls frameDecls =
  if length frameDecls > 1
  then map (\(frame, index) -> frame { cNr = Just index }) $ zip frameDecls [1..]
  else frameDecls

makeFrameDecl :: OutputCfg -> FrameLayout -> Decl
makeFrameDecl cfg (FLayout nm fr layoutmems) =
  let mems = map (makeMember cfg) layoutmems
  in classDecl nm mems


--
-- Low Level Text Generation Operations
--
semify cfg (e:es) = (PP.nest (indent cfg) e <> PP.semi):semify cfg es
semify _ [] = []

docPublic = PP.text "public:"
docPrivate = PP.text "private:"

enquote :: String -> String
enquote s =  "\"" ++ s ++ "\""

enbracket :: String -> String
enbracket s =  "<" ++ s ++ ">"

indentify cfg (e:es) = (PP.nest (indent cfg) e):indentify cfg es
indentify _ [] = []

formatMems cfg (e:es) = (PP.nest (indent cfg) e <> PP.semi):formatMems cfg es
formatMems _ [] = []

-- |Clearly can only work for simple types.  No arrays.
dataMember :: String -> String -> PP.Doc
dataMember ty name = PP.text ty <+> PP.text name

-- |Generates a block declaration that's collapsable.
blockdecl :: OutputCfg -> PP.Doc -> PP.Doc -> [PP.Doc] ->PP.Doc
blockdecl cfg name sep elems =
  let sfxElems = map (<> sep) elems
  in PP.sep [(name <+> PP.lbrace), PP.nest (indent cfg) (PP.sep sfxElems), PP.rbrace]

-- |Non-collapsable block decl.
blockdeclV :: OutputCfg -> PP.Doc -> PP.Doc -> [PP.Doc] ->PP.Doc
blockdeclV cfg name sep elems =
  let sfxElems = map (<> sep) elems
  in PP.vcat [(name <+> PP.lbrace), PP.nest (indent cfg) (PP.sep sfxElems), PP.rbrace]

funccall :: OutputCfg -> PP.Doc -> [PP.Doc] -> PP.Doc
funccall cfg name args =
  let prefix = name <+> PP.lparen
      prefixlen = length $ PP.render prefix
  in PP.hang (name <+> PP.lparen) prefixlen $ PP.hsep $ (L.intersperse PP.comma args) ++ [PP.rparen]

includeHeaders :: [String] -> [PP.Doc]
includeHeaders (h:hs) =
  (mconcat $ map PP.text ["#include <", h, ">"]):includeHeaders hs
includeHeaders [] = []

docConcat xs = PP.text $ concat xs
docConcatSp xs = PP.text $ L.intercalate " " xs

quoteString :: String -> String
quoteString str =
  qs str
  where qs (c:cs)
          | c == '"' = "\\\"" ++ (qs cs)
          | c == '\\' = "\\\\" ++ (qs cs)
          | otherwise = c:(qs cs)
        qs [] = []

stmt :: String -> PP.Doc
stmt s = PP.text s <> PP.semi

