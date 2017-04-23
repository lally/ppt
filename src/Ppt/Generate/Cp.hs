{- |C++ Code generator -}
module Ppt.Generate.Cp where

import Ppt.Configuration
import Ppt.Frame.ParsedRep
import Ppt.Frame.Layout

import Text.PrettyPrint ((<>),(<+>))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

import System.FilePath
import qualified Data.Set as S
import qualified Data.List as L
import qualified Control.Lens as CL
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
inline void writeout(const mainloop& ml) {
    if (_ppt_saving_buffer_foo) {
      _ppt_save_foo(static_cast<void*>(&ml));
    }
}
}  // namespace ppt
-}

-- |Derived from EmitOptions for whatever data we need for outputting.
data OutputCfg = OutputCfg { timeType :: String -- ^Decltype of time vars
                           , timeHeader :: String
                             -- ^Which header to include for time support
                           , timeSave :: (String -> PP.Doc) -- ^Save time to a var
                           , indent :: Int -- ^Indentation depth
                           , defaultInit :: Bool
                             -- ^Does the constructor zero out the type?
                           , multithreadWrite :: Bool
                             -- ^Use multithreaded write protocol?
                           , bufName :: String
                           , sourceSuffix :: String
                           , headerSuffix :: String
                           , filePrefix :: String
                           , namespace :: [String]
                           , emitOpts :: EmitOptions
                           , frames :: [FrameLayout]
                           }

-- |A member may require that a module be generated.  Without any
-- members requiring a module, we don't have to generate it at all.
data GenModule = GMCounters
               | GMSaveBuffer LayoutIOSpec Bool
               -- ^Store layout, and whether it's multithreaded.
               deriving (Eq, Show)

instance Ord GenModule where
  compare GMCounters GMCounters = EQ
  compare (GMSaveBuffer (LayoutIO lsz loff) lm) (GMSaveBuffer (LayoutIO rsz roff)  rm) =
    let szcomp = compare lsz rsz
        offcomp = compare loff roff
        mcomp = compare lm rm
    in if szcomp == EQ
       then if offcomp == EQ
            then mcomp
            else offcomp
       else szcomp
  compare GMCounters ( GMSaveBuffer _ _) = LT
  compare (GMSaveBuffer _ _) GMCounters = GT

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


-- Low level operations

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

writeDecl :: OutputCfg -> String -> Decl -> PP.Doc
writeDecl cfg firstName (ClassDecl name typeIdx clsMems clsMeths _ _) =
  let tag = PP.text $ "class " ++ name
      publicTail [] = [docPublic]
      publicTail s =
        case (last s) of
          PubMember _ -> []
          PrivMember _ -> [docPublic]
      memBody = qualBlock cfg clsMems
      withMeths meths = (publicTail clsMems) ++ (indentify cfg ((saveFn firstName cfg typeIdx):clsMeths)) 
  in PP.vcat ((tag <+> PP.lbrace):( memBody ++ (withMeths clsMeths)) ++ [
    PP.rbrace <> PP.semi])

-- |Clearly can only work for simple types.  No arrays.
dataMember :: String -> String -> PP.Doc
dataMember ty name = PP.text ty <+> PP.text name

-- |Generates a block declaration that's collapsable.
blockdecl :: OutputCfg -> PP.Doc -> PP.Doc -> [PP.Doc] ->PP.Doc
blockdecl cfg name sep elems =
  let sfxElems = map (<> sep) elems
  in (PP.sep [(name <+> PP.lbrace), PP.nest (indent cfg) (PP.sep sfxElems), PP.rbrace])
--    PP.hang (name <+> PP.lbrace) (indent cfg) $ PP.sep $ sfxElems) <> PP.rbrace

funccall :: OutputCfg -> PP.Doc -> [PP.Doc] -> PP.Doc
funccall cfg name args =
  let prefix = name <+> PP.lparen
      prefixlen = length $ PP.render prefix
  in PP.hang (name <+> PP.lparen) prefixlen $ PP.hsep $ (L.intersperse PP.comma args) ++ [PP.rparen]

-- |Returns the option-based elements of OutputCfg
filePrefs :: EmitOptions -> OutputCfg
filePrefs e@(EmitOptions b _ _ _ _ _) =
  OutputCfg "" "" undefined 4 True False (ebName b) ".cc" ".hh" "" ["ppt", ebName b] e []

-- | Returns (sourceSuffix, headerSuffix, filePrefix, namespace) from an array of EOption
cppOpts :: [EOption] -> (String, String, String, [String])
cppOpts _ = (".cc", ".hh", "ppt-", ["ppt"])

makeOutCfg :: EmitOptions -> [FrameLayout] -> OutputCfg
makeOutCfg e@(EmitOptions b _ ETimeVal (ERuntime mt) _ eOpts) flayouts=
  let (ssfx, hsfx, fpfx, ns) = cppOpts eOpts
  in (OutputCfg "struct timeval" "time.h" (\var -> PP.text $ "time(&" ++ var ++ ")") 4 True mt
   (ebName b) ssfx hsfx fpfx ns e flayouts)

makeOutCfg e@(EmitOptions b _ (ETimeSpec src) (ERuntime mt) _ eOpts) flayouts =
  let (ssfx, hsfx, fpfx, ns) = cppOpts eOpts
      clock = case src of
        ETimeClockRealtime ->         "CLOCK_REALTIME"
        ETimeClockRealtimeCoarse ->   "CLOCK_REALTIME_COARSE"
        ETimeClockMonotonic ->        "CLOCK_MONOTONIC"
        ETimeClockMonotonicCoarse ->  "CLOCK_MONOTONIC_COARSE"
        ETimeClockMonotonicRaw ->     "CLOCK_MONOTONIC_RAW"
        ETimeClockBoottime ->         "CLOCK_BOOTTIME"
        ETimeClockProcessCputimeId -> "CLOCK_PROCESS_CPUTIME_ID"
        ETimeClockThreadCputimeId ->  "CLOCK_THREAD_CPUTIME_ID"
  in (OutputCfg "struct timespec" "time.h" (
         \var -> PP.text $ "clock_gettime(" ++ clock ++ ", &" ++ var ++ ")")
       4 True mt (ebName b) ssfx hsfx fpfx ns e flayouts)

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

-- Mid level (structures, functions, etc)

makeJSON :: OutputCfg -> String
makeJSON cfg =
  let json = JsonRep "1.0.0" (emitOpts cfg) (frames cfg) [] []
  in quoteString $ unpack $ encode json

structDecl :: OutputCfg -> String -> [(String, String)] -> PP.Doc
structDecl cfg name members =
  let leftwidth :: Int
      leftwidth = 1 + (foldl max 0 $ map (length . fst) members)
      -- type, member
      formatMem (t, m) = (PP.text t PP.$$ (PP.nest leftwidth $ PP.text m)) <> PP.semi
  in mconcat [blockdecl cfg (docConcatSp [ "struct",  name ]) PP.empty $ map formatMem members, PP.semi]

externDecl :: String -> [String] -> PP.Doc
externDecl typ nameMems = docConcatSp [ "extern \"C\"", typ, L.intercalate "_" nameMems ] <> PP.semi

externDecl' :: String -> [String] -> PP.Doc
externDecl' typ nameMems = docConcatSp [ "extern \"C\"", typ, L.intercalate "_" nameMems ]

externDeclAttr :: String -> [String] -> String -> PP.Doc
externDeclAttr typ nameMems attr = docConcatSp [ "extern \"C\"", typ, L.intercalate "_" nameMems,
                                                 attr ] <> PP.semi

staticDecl :: String -> [String] -> PP.Doc
staticDecl typ nameMems =
  docConcatSp [ "static", typ, L.intercalate "_" nameMems ] <> PP.semi

controlDecl :: OutputCfg -> PP.Doc
controlDecl cfg =
  let size_t = "size_t"
      uint64_t = "uint64_t"
  in structDecl cfg "ppt_control" [ (size_t, "control_blk_sz")
                                  , (uint64_t, "data_block_hmem")
                                  , (uint64_t, "data_block_hmem_attached")
                                  , ("void*", "data_block")
                                  , (size_t, "data_block_sz")
                                  , ("uint32_t", "nr_perf_ctrs")
                                  , ("struct perf_event_attr", "counterdata[3]")
                                  , (uint64_t, "client_flags") ]

dataDecl :: OutputCfg -> String -> PP.Doc
dataDecl cfg first =
  let x = x
  in structDecl cfg ("data_" ++ bufName cfg) [ ("static " ++ first ++ " *", "ppt_buf"),
                                               ("static int", "ppt_bufsz")]



-- |Wraps a 'body' in a namespace decl and #includes.  The 'headers' list elements must already
-- have either angle brackets or quotes around them.
fileWrap :: OutputCfg -> [String] -> [PP.Doc] -> PP.Doc
fileWrap cfg headers body =
        let hpfx = PP.text "#include "
            nsFront = PP.text "namespace"
            nsBack = PP.char '}'
            headerBlock =  map (\n -> hpfx <> PP.text n) headers
            namespaces = namespace cfg
            nsName = PP.text $ L.intercalate "::" namespaces
            nsPrefix = map (\s -> nsFront <+> PP.text s <+> PP.char '{')  namespaces
            nsSuffix =
              (mconcat $ take (length namespaces) $ repeat nsBack) <+> (
              (PP.text "// namespace ") <> nsName)
            blank = [PP.empty]
        in PP.sep (headerBlock ++ blank ++ nsPrefix ++ blank ++ body ++ blank ++ [nsSuffix, PP.empty])

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
makeMember cfg (LMember PTime _ _ _ _ nm) =
  let timety = timeType cfg
      timeheaders = [timeHeader cfg]
  in (MB [blockdecl cfg (PP.text $ "void snapshot_" ++ nm ++ "()") PP.semi [
             timeSave cfg nm]]
         (dataMember timety nm) timeheaders [])

makeMember cfg (LMember PCounter _ _ _ k nm) =
  MB [] (dataMember "uint64_t" nm) [] [GMCounters]
makeMember cfg (LMember PByte _ _ _ (LKPadding n) nm) =
  PrivateMem (dataMember "uint8_t"  (nm ++ "[" ++ show n ++ "]")) ["cstdint"] []

-- TODO: Put in initializer here if defaultInit outputcfg
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

-- High level (per header/ per source file)


makeFrameDecl :: OutputCfg -> FrameLayout -> Decl
makeFrameDecl cfg (FLayout nm fr layoutmems) =
  let mems = map (makeMember cfg) layoutmems
  in classDecl nm mems

moduleHeaders :: OutputCfg -> [Decl] -> [GenModule] -> [String]
moduleHeaders cfg decls mods =
  let declsOf (ClassDecl _ _ _ _ h _) = h
      modsOf GMCounters = []
      modsOf (GMSaveBuffer _ _) = ["string.h", "sys/types.h", "sys/shm.h", "unistd.h", "atomic", "stdio.h",
                                   "linux/perf_event.h", "linux/hw_breakpoint.h" ]
  in S.toList $ S.fromList $ (concatMap declsOf decls) ++ (concatMap modsOf mods)

allModules :: [Decl] -> [GenModule]
allModules decls =
  let modsOf (ClassDecl _ _ _ _ _ m) = m
  in S.toList $ S.fromList $ concatMap modsOf decls

saveFn :: String -> OutputCfg -> Maybe Int -> PP.Doc
saveFn firstName cfg typeIdx =
  let bufp = "data_" ++ (bufName cfg) ++ "::ppt_buf"
      bufsz = "data_" ++ (bufName cfg) ++ "::ppt_bufsz"
  in blockdecl cfg (docConcat ["void save()"])  PP.empty [
       blockdecl cfg (docConcat [ "if (!", bufp, " && !_ppt_hmem_", bufName cfg, ")" ]) PP.semi [PP.text "return"],
       blockdecl cfg (docConcat [ "if (try_attach())"]) PP.semi $ concat [
           [ "int index = nextIndex()",
             "__ppt_seqno = index",
             "__ppt_seqno_back = index" ] ++
           (case typeIdx of
              Nothing -> []
              Just idx -> [PP.text $ "__ppt_type = " ++ show idx]) ++
           [ PP.text $ "const int modidx = (index-1) % " ++ bufsz],
           (if multithreadWrite cfg
            then [PP.text $ bufp ++ "[modidx].__ppt_seqno = 0",
                  PP.text $ bufp ++ "[modidx].__ppt_seqno_back = 0",
                  writeBarrier]
            else []),
           [ PP.text $ bufp ++ "[modidx].__ppt_seqno = index",
             writeBarrier,
             docConcat ["memcpy(((uint8_t*)&", bufp, "[modidx]) + sizeof(__ppt_seqno), ",
                        "((uint8_t*)this) + sizeof(__ppt_seqno), ",
                         "sizeof(*this) - 2*sizeof(__ppt_seqno))"],
             writeBarrier,
             PP.text $ bufp ++ "[modidx].__ppt_seqno_back = index",
             writeBarrier
           ]
       ],
       PP.empty
    ]
  where writeBarrier = PP.text "std::atomic_thread_fence(std::memory_order_release)"

modDecls :: OutputCfg -> String -> [GenModule] -> PP.Doc
modDecls cfg firstName mods =
  let eachMod mod = case mod of
                      GMSaveBuffer (LayoutIO sz off) mt ->
                        PP.vcat $ [ docConcat ["class ", firstName, ";"],
                                    externDecl "int" ["_ppt_hmem", bufName cfg],
                                    dataDecl cfg firstName,
                                    PP.text "bool try_attach();",
                                    docConcat ["class ", firstName, ";"],
                                    docConcatSp ["int", "nextIndex();"]]
                      GMCounters -> PP.text "/* counters */"
  in mconcat $ map eachMod mods


modInlineDefs :: OutputCfg -> String -> [GenModule] -> PP.Doc
modInlineDefs cfg firstName mods = PP.empty

attachFn :: String -> OutputCfg -> Bool -> PP.Doc
attachFn firstName cfg hasCounters =
  let bufp = "data_" ++ (bufName cfg) ++ "::ppt_buf"
      bufsz = "data_" ++ (bufName cfg) ++ "::ppt_bufsz"
  in blockdecl cfg (PP.text "bool try_attach()") PP.empty [
        blockdecl cfg (docConcat ["if (",bufp," && ", "_ppt_hmem_", bufName cfg, ")"]) PP.semi [
            PP.text "return true"
            ],
        blockdecl cfg (docConcat ["if (_ppt_hmem_",buf," && !",bufp,")"]) PP.empty ([
            stmt "struct shmid_ds ds",
            blockdecl cfg (docConcat [
                              "if (shmctl(_ppt_hmem_",buf,", IPC_STAT, &ds) != 0)"]) PP.semi [
                docConcat ["perror(\"failed ppt attach of ", buf, ": shmctl\")"],
                docConcat ["_ppt_hmem_",buf," = 0"],
                PP.text "return false"
                ],
            docConcat ["off_t elem_offset = sizeof(ppt_control) + (sizeof(ppt_control) % sizeof(",
                       firstName, "));"],
            docConcat ["_ppt_ctrl = reinterpret_cast<ppt_control*>(shmat(_ppt_hmem_",
                       buf,", nullptr, 0))"] <> PP.semi,
            blockdecl cfg (PP.text "if (_ppt_ctrl == nullptr)") PP.semi [
                docConcat ["perror(\"failed ppt attach of ", buf, ": shmat\")"],
                docConcat ["_ppt_hmem_",buf," = 0"],
                PP.text "return false"
                ],
            docConcat [bufp, " = reinterpret_cast<", firstName, "*>(reinterpret_cast<uint8_t*>(_ppt_ctrl) + elem_offset);"],
            stmt $ "_ppt_ctrl->data_block = " ++ bufp,
            docConcat ["_ppt_ctrl->data_block_sz = (ds.shm_segsz - elem_offset) / sizeof(",
                       firstName,")" ] <> PP.semi,
            docConcat [bufsz, " = _ppt_ctrl->data_block_sz" ] <> PP.semi,
            docConcat ["_ppt_ctrl->data_block_hmem_attached = _ppt_hmem_",
                       buf ] <> PP.semi]
            ++ (if hasCounters
                then [stmt "setupCounters()"]
                else []) ++
            [stmt "return true"
            ]),
        blockdecl cfg (docConcat ["else if (",bufp," && !_ppt_hmem_",buf,")"]) PP.empty ([
            blockdecl cfg (PP.text $ "if (shmdt(_ppt_ctrl) != 0)") PP.semi [
                docConcat ["perror(\"failed ppt detach of ",buf,": shmdt\")"]
                ],
            stmt $ bufp ++ " = nullptr",
            docConcat ["_ppt_hmem_", buf, " = 0"] <> PP.semi]
            ++ (if hasCounters
                then [stmt "closeCounters()"]
                else []) ++
            [
            ]),
            stmt "return false"
        ]
    where buf = bufName cfg

nextIdxFn :: String -> OutputCfg -> PP.Doc
nextIdxFn firstName cfg =
    (if multithreadWrite cfg
      then blockdecl cfg (PP.text "int nextIndex()") PP.semi [
                       "static std::atomic<int> s_index(1)",
                       "return s_index.fetch_add(1, std::memory_order_release)" ]
      else docConcat [ "int nextIndex() { static int s_index = 0; return ++s_index; }" ])

moduleSource firstName cfg hasCounters (GMSaveBuffer (LayoutIO sz off) mt) =
  let attrused = " __attribute__((used))"
      statDecl cfg =
        [ docConcat [ "struct ppt_stat_t { pid_t ppt_agent_pid; }"] <> PP.semi,
          externDecl "ppt_stat_t" ["_ppt_stat", bufName cfg] <> PP.semi,
          docConcat ["ppt_stat_t _ppt_stat_", bufName cfg, attrused] <> PP.semi]
  in PP.sep ([
    externDecl "const char*" ["_ppt_json", bufName cfg],
    stmt $  firstName  ++ " * data_" ++ (bufName cfg) ++ "::ppt_buf",
    stmt $  "int data_" ++ (bufName cfg) ++ "::ppt_bufsz",
    PP.hsep [docConcat ["const char* _ppt_json_", bufName cfg],
             PP.text attrused, PP.text "=", PP.text (enquote $ makeJSON cfg)] <> PP.semi,
    docConcat ["int _ppt_hmem_", bufName cfg, " ", attrused, ";"]]
     ++ statDecl cfg ++
    [ controlDecl cfg,
      staticDecl "ppt_control" ["*_ppt_ctrl"],
      nextIdxFn firstName cfg,
      attachFn firstName cfg hasCounters
    ])

moduleSource firstName cfg _ GMCounters  = PP.vcat [
  -- A clear placeholder.
  stmt "void setupCounters() {}",
  stmt "void closeCounters() {}"
  ]

statLayouts :: [FrameLayout] -> (String, LayoutIOSpec)
statLayouts flayouts= case flayouts of
        [] -> ("void", LayoutIO  0 0)
        (x:_) -> (flName x, layoutSpec x)

modImpls :: OutputCfg -> String -> [GenModule] -> PP.Doc
modImpls cfg firstName mods =
  PP.sep $ map (moduleSource firstName cfg hasCounters) mods
  where
    hasCounters = (length $ filter (== GMCounters) mods) > 0

cppHeader :: OutputCfg -> String -> [Decl] -> [GenModule] -> [String] -> (FilePath, PP.Doc)
cppHeader cfg firstName frameDecls mods headers =
  let classes = map (writeDecl cfg firstName) $ sequenceDecls frameDecls
      body = ((modDecls cfg firstName mods):PP.empty:classes) ++ [PP.empty, modInlineDefs cfg firstName mods]
      fileName = (filePrefix cfg) ++ (bufName cfg) ++ (headerSuffix cfg)
  in (fileName, fileWrap cfg headers body)

cppSource :: OutputCfg -> String -> [Decl] -> [GenModule] -> [String] -> (FilePath, PP.Doc)
cppSource cfg firstName frameDecls mods baseHeaders =
  let headerName = "\"" ++ (filePrefix cfg) ++ (bufName cfg) ++ (headerSuffix cfg) ++ "\""
      headers = headerName: baseHeaders
      fileName = (filePrefix cfg) ++ (bufName cfg) ++ (sourceSuffix cfg)
      body = modImpls cfg firstName mods
  in (fileName, fileWrap cfg headers [body])

cppFiles :: EmitOptions -> [FrameLayout] -> [(FilePath, PP.Doc)]
cppFiles opts flayous =
  let cfg = makeOutCfg opts flayous
      frameDecls = map (makeFrameDecl cfg) flayous
      isMultithreaded = erMultithread $ CL.view eRuntime opts
      (firstName, lspec) = statLayouts flayous
      mods = (GMSaveBuffer lspec isMultithreaded ):allModules frameDecls
      headers = map enbracket $ moduleHeaders cfg frameDecls mods
  in [cppHeader cfg firstName frameDecls mods headers,
      cppSource cfg firstName frameDecls mods headers]

