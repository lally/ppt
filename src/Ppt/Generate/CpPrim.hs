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

-- |A block of members, prefixed with a public/private block label.
-- All methods are public.
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

makeMember cfg (LMember (PCounter (Just 0)) _ _ _ (LKMember frmem side) nm) =
  {-
   -- For native:
    if (data_BUFNAME::ppt_counter_fd[2] != -1) {
       goto __ppt_NM_Load3;
    } else if (data_BUFNAME::ppt_counter_fd[1] != -1) {
       goto __ppt_NM_Load2;
    } else if (data_BUFNAME::ppt_counter_fd[0] != -1) {
       goto __ppt_NM_Load1;
    } else {
      return;
    }

    uint32_t a, d;

  __ppt_NMLoad3:
    __asm__ volatile("rdpmc" :  "=a" (a), "=d" (d) : "c" (data_BUFNAME::ppt_counter_rcx[2]));
    v2 = a | (static_cast<uint64_t>(d) << 32);
  __ppt_NMLoad2:
    __asm__ volatile("rdpmc" :  "=a" (a), "=d" (d) : "c" (data_BUFNAME::ppt_counter_rcx[1]));
    v1 = a | (static_cast<uint64_t>(d) << 32);
  __ppt_NMLoad1:
    __asm__ volatile("rdpmc" :  "=a" (a), "=d" (d) : "c" (data_BUFNAME::ppt_counter_rcx[0]));
    v0 = a | (static_cast<uint64_t>(d) << 32);

   -- For syscall:
   save_counters(&v0, &v1, &v2);

  -}
  let maxCounterIdx = (counterCount cfg) - 1
      indices = reverse [0 .. maxCounterIdx]
      baseName = fmName frmem
      memSfx = if defaultInit cfg then "= 0" else ""
      counterFor n = case side of
              Nothing -> baseName ++  "_" ++ show n
              Just (IntBegin a b) -> baseName ++ (if b > 1 then ("_" ++ show n) else "") ++ "_start"
              Just (IntEnd a b) -> baseName ++ (if b > 1 then ("_" ++ show n) else "") ++ "_end"
      functionsBaseName = case side of
        Nothing -> baseName
        Just (IntBegin _ _) -> baseName ++ "_start"
        Just (IntEnd _ _) -> baseName ++ "_end"
      saveFn = if nativeCounters cfg
               then
                 let labelFor n = "__ppt_" ++ nm ++ "_Load" ++ show (n+1)
                     condFor n = blockdecl cfg (PP.text $ "if (data_" ++ (bufName cfg) ++ "::ppt_counter_fd[" ++
                                                show n ++ "] != -1)") PP.semi [
                       stmt $ "goto " ++ labelFor n
                       ]
                     condCat conds = PP.vcat (head conds : (map (\c -> PP.text "else " <> c) $ tail conds))
                     loadFor n = [(PP.text $ labelFor n) <> ":",
                                  stmt $ "__asm__ volatile(\"rdpmc\" :  \"=a\" (a), \"=d\" (d) : \"c\" (data_" ++
                                     (bufName cfg) ++ "::ppt_counter_rcx[" ++ show n ++ "]));",
                                  stmt $ nm ++ "_" ++ (show n) ++ "= a | (static_cast<uint64_t>(d) << 32);" ]
                 in blockdecl cfg (PP.text $ "void snapshot_" ++ functionsBaseName ++ "()") PP.semi (
                   condCat (map condFor indices) : (stmt "uint32_t a,d;":(concatMap loadFor indices)))
               else
                 let args = L.intercalate ", " $ map (\i -> "&" ++ (counterFor i)) indices
                 in blockdecl cfg (PP.text $ "void snapshot_" ++ functionsBaseName ++ "()") PP.empty [
                   stmt $ "save_counters(" ++ args ++ ")"]
  in MB [saveFn] (dataMember "uint64_t" (nm ++ memSfx)) [] [GMCounters]

makeMember cfg (LMember (PCounter (Just v)) _ _ _ (LKMember _ _) nm) =
  let memSfx = if defaultInit cfg then "= 0" else ""
  in MB [] (dataMember "uint64_t" (nm ++ memSfx)) [] [GMCounters]

-- |Regular member types.
makeMember cfg (LMember ty _ _ _ (LKMember _ _) nm) =
  let declType = case ty of
        PDouble -> "double"
        PFloat -> "float"
        PInt -> "int"
      memSfx = if defaultInit cfg then "= 0" else ""
  in MB [] (dataMember declType (nm ++ memSfx)) [] []

-- |Non-member types.  Should just be ints.  Padding was defined in another case up above.
makeMember cfg (LMember ty _ _ _ _ nm) =
  let declType = case ty of
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

