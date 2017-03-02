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
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
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
               | GMSaveBuffer LayoutIOSpec Bool -- ^Store layout, and whether it's multithreaded.
               deriving (Eq, Show)

instance Ord GenModule where
  compare GMCounters GMCounters = EQ
  compare (GMSaveBuffer (LayoutIO lsz loff) lm) ( GMSaveBuffer (LayoutIO rsz roff)  rm) =
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

classDecl :: String -> [MemberData] -> Decl
classDecl n [] = ClassDecl n Nothing [] [] [] []
classDecl n ((MB methods mem hdrs mods):rest) =
               let (ClassDecl _ _ pmem pmeth ns ms) = classDecl n rest
               in ClassDecl n Nothing (PubMember mem:pmem) (methods ++ pmeth) (hdrs ++ ns) (mods ++ ms)
classDecl n ((PrivateMem m hdrs mods):rest) =
               let (ClassDecl _ _ pmem meth ns mds) = classDecl n rest
               in ClassDecl n Nothing (PrivMember m:pmem) meth (hdrs ++ ns) (mods ++ mds)

semify cfg (e:es) = (PP.nest (indent cfg) e <> PP.semi):semify cfg es
semify _ [] = []

docPublic = PP.text "public:"
docPrivate = PP.text "private:"

indentify cfg (e:es) = (PP.nest (indent cfg) e):indentify cfg es
indentify _ [] = []

formatMems cfg (e:es) = (PP.nest (indent cfg) e <> PP.semi):formatMems cfg es
formatMems _ [] = []

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

writeDecl :: OutputCfg -> String -> Decl -> PP.Doc
writeDecl cfg firstName (ClassDecl name typeIdx clsMems clsMeths _ _) =
  let tag = PP.text $ "class " ++ name
      publicTail [] = [docPublic]
      publicTail s =
        case (last s) of
          PubMember _ -> []
          PrivMember _ -> [docPublic]
      memBody = qualBlock cfg clsMems
      saveMethod = blockdecl cfg (PP.text "void save()")  PP.semi  (
        [ "int index = nextIndex()",
          "__ppt_seqno = index",
          "__ppt_seqno_back = index" ] ++
        (case typeIdx of
          Nothing -> []
          Just idx -> [PP.text $ "__ppt_type = " ++ show idx]) ++ [
        if (name /= firstName)
        then PP.text $ concat ["save(reinterpret_cast<", firstName, "*>(this))"]
        else PP.text "save(this)"
        ])
      withMeths meths = (publicTail clsMems) ++ (indentify cfg (saveMethod:clsMeths))
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
--  MB [] (dataMember "uint8_t"  (nm ++ "[" ++ show n ++ "]")) ["cstdint"] []

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

makeFrameDecl :: OutputCfg -> FrameLayout -> Decl
makeFrameDecl cfg (FLayout nm fr layoutmems) =
  let mems = map (makeMember cfg) layoutmems
  in classDecl nm mems

allHeaders :: OutputCfg -> [Decl] -> [GenModule] -> [String]
allHeaders cfg decls mods =
  let declsOf (ClassDecl _ _ _ _ h _) = h
      modsOf GMCounters = []
      modsOf (GMSaveBuffer _ _) = ["sys/types.h", "sys/shm.h", "unistd.h"] ++
        if multithreadWrite cfg
        then ["atomic"]
        else []
  in S.toList $ S.fromList $ (concatMap declsOf decls) ++ (concatMap modsOf mods)

allModules :: [Decl] -> [GenModule]
allModules decls =
  let modsOf (ClassDecl _ _ _ _ _ m) = m
  in S.toList $ S.fromList $ concatMap modsOf decls

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
  in (OutputCfg "struct timespec" "time.h" (\var -> PP.text $ "time(&" ++ var ++ ")") 4 True mt
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
  in (OutputCfg "struct timeval" "time.h" (
         \var -> PP.text $ "clock_gettime(" ++ clock ++ ", &" ++ var ++ ")")
       4 True mt (ebName b) ssfx hsfx fpfx ns e flayouts)

includeHeaders :: [String] -> [PP.Doc]
includeHeaders (h:hs) =
  (mconcat $ map PP.text ["#include <", h, ">"]):includeHeaders hs
includeHeaders [] = []

docConcat xs = PP.text $ concat xs
docConcatSp xs = PP.text $ L.intercalate " " xs
-- Parts of the file:
-- The headers
-- Any runtime decls
-- The classes
-- The runtime support

modDecls :: EmitOptions -> String -> [GenModule] -> PP.Doc
modDecls opts firstName mods =
--  PP.text $ "/* Insert  decls for runtime here: " ++ (L.intercalate ", " $ map show mods) ++ " */"
  let eachMod mod = case mod of
                      GMSaveBuffer (LayoutIO sz off) mt ->
                        PP.vcat $ [ docConcat ["class ", firstName, ";"],
                                    docConcatSp ["void","save(const", firstName, "* buf);"],
                                    docConcatSp ["int", "nextIndex();"]]
                      GMCounters -> PP.text "/* counters */"
  in mconcat $ map eachMod mods

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

makeJSON :: OutputCfg -> String
makeJSON cfg =
  let json = JsonRep "1.0.0" (emitOpts cfg) (frames cfg) [] []
  in quoteString $ unpack $ encode json

structDecl :: OutputCfg -> String -> [(String, String)] -> PP.Doc
structDecl cfg name members =
  let leftwidth :: Int
      leftwidth = foldl max 0 $ map (length . fst) members
      -- type, member
      formatMem (t, m) = (PP.sizedText leftwidth t) <> PP.text m <> PP.semi
  in blockdecl cfg (docConcatSp [ "struct",  name ]) PP.empty $ map formatMem members

externDecl :: String -> [String] -> PP.Doc
externDecl typ nameMems = docConcatSp [ "extern \"C\"", typ, L.intercalate "_" nameMems ] <> PP.semi

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


eachMod firstName cfg (GMSaveBuffer (LayoutIO sz off) mt) =
  let writeBarrier = PP.text "std::atomic_thread_fence(std::memory_order_release)"
      attrused = " __attribute__((used))"
      qt = "\""
  in PP.sep [
    externDecl "const char*" ["_ppt_json", bufName cfg, attrused, "=", (qt ++ makeJSON cfg ++ qt)],
    externDecl "int" ["_ppt_hmem", bufName cfg, attrused],
    docConcat [ "extern \"C\" struct { pid_t ppt_agent_pid; } _ppt_stat_", bufName cfg, attrused
              ] <> PP.semi,
    staticDecl "int" ["_ppt_bufsz"],
    -- The _mem member points to our control block (mapped in).  The _buf member points to our
    -- array.  For non-resizable configurations, these both point into the same shared memory
    -- segment.
    controlDecl cfg,
    staticDecl "ppt_control" ["*_ppt_ctrl"],
    staticDecl firstName ["*_ppt_buf"],
    (if multithreadWrite cfg
      then blockdecl cfg (PP.text "static int nextIndex()") PP.semi [
                       "static std::atomic<int> s_index(1)",
                       "return std::fetch_add(&s_index, 1, std::memory_order_release)" ]
      else docConcat [ "static int nextIndex() { static int s_index = 0; return ++s_index; }" ]),
    blockdecl cfg (docConcat ["void save(const ", firstName, " * buf, int index)"])  PP.empty [
       blockdecl cfg (docConcat [ "if (_ppt_buf || try_attach())"]) PP.semi $ concat [
           [ PP.text "int modidx = index % _ppt_bufsz" ],
           (if multithreadWrite cfg
            then [PP.text "_ppt_buf[modidx].__ppt_seqno = 0",
                  PP.text "_ppt_buf[modidx].__ppt_seqno_back = 0",
                  writeBarrier]
            else []),
           [ PP.text "_ppt_buf[modidx].__ppt_seqno = modidx",
             writeBarrier,
             PP.text ("memcpy(&_ppt_buf[modidx], buf + sizeof(buf->__ppt_seqno), " ++
                       "sizeof(*buf) - 2*sizeof(buf->__ppt_seqno))"),
             writeBarrier,
             PP.text "_ppt_buf[modidx].__ppt_seqno_back = modidx",
             writeBarrier
           ]
       ],
       PP.empty
    ],
    blockdecl cfg (PP.text "bool try_attach()") PP.empty [
        blockdecl cfg (docConcat ["if (_ppt_hmem_",bufName cfg," && !_ppt_buf)"]) PP.empty [
            stmt "struct shmid_ds ds",
            blockdecl cfg (docConcat [
                              "if (shmctl(_ppt_hmem_",bufName cfg,", IPC_STAT, &ds) != 0)"]) PP.semi [
                docConcat ["perror(\"failed ppt attach of ", bufName cfg, ": shmctl\")"],
                docConcat ["_ppt_hmem_",bufName cfg," = 0"] <> PP.semi,
                stmt "return false"
                ],
            docConcat ["off_t elem_offset = sizeof(ppt_control) + (sizeof(ppt_control) % sizeof(", firstName, "))"],
            docConcat ["_ppt_bufsz = (ds.shm_segsz - elem_offset) / sizeof(",firstName,")"] <> PP.semi,
            docConcat ["_ppt_control = reinterpret_cast<",firstName,">(shmat(_ppt_hmem_",
                       bufName cfg,", nullptr, 0))"] <> PP.semi,
            blockdecl cfg (PP.text "if (_ppt_control == nullptr)") PP.semi [
                docConcat ["perror(\"failed ppt attach of ", bufName cfg, ": shmat\");"],
                docConcat ["_ppt_hmem_",bufName cfg," = 0"],
                PP.text "return false"
                ],
            stmt "_ppt_buf = _ppt_control + elem_offset",
            stmt "return true"
            ],
        blockdecl cfg (docConcat ["else if (_ppt_buf && !_ppt_hmem_",bufName cfg,")"]) PP.empty [
            blockdecl cfg (PP.text "if (shmdt(_ppt_buf) != 0)") PP.semi [
                docConcat ["perror(\"failed ppt detach of ",bufName cfg,": shmdt\")"],
                PP.text "_ppt_buf = nullptr"
                ]
            ],
            stmt "return false"
        ]
    ]
eachMod _ cfg GMCounters = PP.text "/* counters */"


modImpls :: OutputCfg -> String -> [GenModule] -> PP.Doc
modImpls cfg firstName mods = PP.sep $ map (eachMod firstName cfg) mods

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
  let cfg = makeOutCfg opts flayous
      frameDecls = map (makeFrameDecl cfg) flayous
      isMultithreaded = erMultithread $ eRuntime opts
      (firstName, lspec) = case flayous of
        [] -> ("void", LayoutIO  0 0)
        (x:_) -> (flName x, layoutSpec x)
      mods = (GMSaveBuffer lspec isMultithreaded ):allModules frameDecls
      headers = allHeaders cfg frameDecls mods
      headerDocs =
        let pfx = PP.text "#include <"
            sfx = PP.char '>'
        in map (\n -> pfx <> PP.text n <> sfx) headers
      ourModDelcs = modDecls opts firstName mods
      ourModImpls = modImpls cfg firstName mods
      sequencedDecls =
        if length frameDecls > 1
        then map (\(frame, index) -> frame { cNr = Just index }) $ zip frameDecls [1..]
        else frameDecls
      allDocs =
        headerDocs ++ [ourModDelcs] ++ map (writeDecl cfg firstName) sequencedDecls ++ [ourModImpls]
  in PP.vcat allDocs

