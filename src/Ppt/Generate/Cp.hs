{- |C++ Code generator -}
module Ppt.Generate.Cp where

import Ppt.Frame.ParsedRep
import Ppt.Frame.Layout
import Ppt.Generate.CpConfig
import Ppt.Generate.CpPrim

import Control.Exception (throw, NoMethodError(..))
import Text.PrettyPrint ((<>),(<+>))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)

import System.FilePath
import qualified Data.Set as S
import qualified Data.List as L
import qualified Control.Lens as CL
import qualified Text.PrettyPrint as PP

--
-- Mid level (structures, functions, etc)
--
makeJSON :: OutputCfg -> String
makeJSON cfg =
  let json = JsonRep "1.0.0" (emitOpts cfg) x64Layout (frames cfg) [] []
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
      uint32_t = "uint32_t"
      ppt_cntr_entry = structDecl cfg "perf_counter_entry" [ (uint32_t, "ecx"),
                                                             ("struct perf_event_attr", "event_attr") ]
      ppt_ctrl_decl = structDecl cfg "ppt_control" [ (size_t, "control_blk_sz")
                                  , (uint64_t, "data_block_hmem")
                                  , (uint64_t, "data_block_hmem_attached")
                                  , ("void*", "data_block")
                                  , (size_t, "data_block_sz")
                                  , (uint64_t, "client_flags")
                                  , ("uint32_t", "nr_perf_ctrs")
                                  , ("struct perf_counter_entry", "counterdata[3]")]
  in ppt_cntr_entry <> ppt_ctrl_decl

dataDecl :: OutputCfg -> String -> [GenModule] -> PP.Doc
dataDecl cfg first mods =
  let hasCounters (GMCounters) = True
      hasCounters _ = False
      enableCounters :: Bool
      enableCounters = any hasCounters mods
      enableNativeCounters = enableCounters && (nativeCounters cfg)
      nrCounters = show $ counterCount cfg
      counterMem = if enableCounters
                   then  ([("static int", "ppt_counter_fd[" ++ nrCounters ++ "]")] ++
                          if (nativeCounters cfg) 
                          then [("static void *", "ppt_counter_mmap"),
                                ("static uint64_t", "ppt_counter_rcx[" ++ nrCounters ++ "]")]
                          else [])
                   else []
  in structDecl cfg ("data_" ++ bufName cfg) ([("static " ++ first ++ " *", "ppt_buf"),
                                               ("static int", "ppt_bufsz"),
                                               ("static int", "ppt_offset")] ++ counterMem)

--
-- High-Level Generation Primitives
--

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


moduleHeaders :: OutputCfg -> [Decl] -> [GenModule] -> [String]
moduleHeaders cfg decls mods =
  let declsOf (ClassDecl _ _ _ _ h _) = h
      modsOf GMCounters = ["algorithm", "syscall.h", "linux/perf_event.h"]
      modsOf (GMSaveBuffer _ _) = ["string.h", "sys/types.h", "sys/shm.h", "unistd.h", "atomic", "stdio.h" ]
  in S.toList $ S.fromList $ (concatMap declsOf decls) ++ (concatMap modsOf mods)

allModules :: [Decl] -> [GenModule]
allModules decls =
  let modsOf (ClassDecl _ _ _ _ _ m) = m
  in S.toList $ S.fromList $ concatMap modsOf decls

writeDecl :: OutputCfg -> String -> Decl -> PP.Doc
writeDecl cfg firstName (ClassDecl name typeIdx clsMems clsMeths _ _) =
  let tag = PP.text $ "class " ++ name
      publicTail [] = [docPublic]
      publicTail s =
        case (last s) of
          PubMember _ -> []
          PrivMember _ -> [docPublic]
      memBody = qualBlock cfg clsMems
      chosenSaveFn = if firstName == name
                     then saveFn firstName cfg typeIdx
                     else restSaveFn firstName cfg

      withMeths meths = (publicTail clsMems) ++ (indentify cfg ((chosenSaveFn):clsMeths)) 
  in PP.vcat ((tag <+> PP.lbrace):( memBody ++ (withMeths clsMeths)) ++ [
    PP.rbrace <> PP.semi])

restSaveFn :: String -> OutputCfg -> PP.Doc
restSaveFn firstName cfg =
  blockdecl cfg (docConcat ["void save()"]) PP.empty [
    stmt $ "reinterpret_cast<" ++ firstName ++ "*>(this)->save()"
  ]

saveFn :: String -> OutputCfg -> Maybe Int -> PP.Doc
saveFn firstName cfg typeIdx =
  let bufp = "data_" ++ (bufName cfg) ++ "::ppt_buf"
      bufsz = "data_" ++ (bufName cfg) ++ "::ppt_bufsz"
      offset = "data_" ++ (bufName cfg) ++ "::ppt_offset"
  in blockdecl cfg (docConcat ["void save()"])  PP.empty [
       blockdecl cfg (docConcat [ "if (!", bufp, " && !_ppt_hmem_", bufName cfg, ")" ]) PP.semi [
           PP.text "return"],
       blockdecl cfg (docConcat [ "if (try_attach())"]) PP.semi $ concat [
           [ "int index = nextIndex()",
             "__ppt_seqno = index",
             "__ppt_seqno_back = index" ] ++
           (case typeIdx of
              Nothing -> []
              Just idx -> [PP.text $ "__ppt_type = " ++ show idx]) ++
           [ PP.text $ "const int modidx = (" ++ offset ++ " + index-1) % " ++ bufsz],
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
                                    dataDecl cfg firstName mods,
                                    PP.text "bool try_attach();",
                                    docConcat ["class ", firstName, ";"],
                                    docConcatSp ["int", "nextIndex();"],
                                    PP.text ""]
                      GMCounters ->
                        let args = L.intercalate "," $ take (counterCount cfg) $ repeat "uint64_t*"
                        in PP.vcat [ PP.text $ "void save_counters(" ++ args ++ ");" ]
  in mconcat $ map eachMod mods

modInlineDefs :: OutputCfg -> String -> [GenModule] -> PP.Doc
modInlineDefs cfg firstName mods = PP.empty

attachFn :: String -> OutputCfg -> Bool -> PP.Doc
attachFn firstName cfg hasCounters =
  let bufp = "data_" ++ (bufName cfg) ++ "::ppt_buf"
      bufsz = "data_" ++ (bufName cfg) ++ "::ppt_bufsz"
      offset = "data_" ++ (bufName cfg) ++ "::ppt_offset"
  in PP.vcat $ (if hasCounters then [stmt "void setupCounters()", stmt "void closeCounters()"] else []) ++ [
    blockdecl cfg (PP.text "bool try_attach()") PP.empty [
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
            docConcat [bufp, " = reinterpret_cast<", firstName,
                       "*>(reinterpret_cast<uint8_t*>(_ppt_ctrl) + elem_offset);"],
            stmt $ "_ppt_ctrl->data_block = " ++ bufp,
            docConcat ["_ppt_ctrl->data_block_sz = (ds.shm_segsz - elem_offset) / sizeof(",
                       firstName,")" ] <> PP.semi,
            docConcat [bufsz, " = _ppt_ctrl->data_block_sz" ] <> PP.semi,
            docConcat [offset, " = ", bufsz, " - ((s_index.load()-1) % ", bufsz, ")"] <> PP.semi,
            docConcat ["_ppt_ctrl->data_block_hmem_attached = _ppt_hmem_",
                       buf ] <> PP.semi]
            ++ (if hasCounters
                then [stmt "setupCounters()"]
                else []) ++
            [stmt "return true"
            ]),
        blockdecl cfg (docConcat ["else if (",bufp," && !_ppt_hmem_",buf,")"]) PP.empty (
            (if hasCounters
              then [stmt "closeCounters()"]
              else []) ++ [
                blockdecl cfg (PP.text $ "if (shmdt(_ppt_ctrl) != 0)") PP.semi [
                    docConcat ["perror(\"failed ppt detach of ",buf,": shmdt\")"]
                    ],
                stmt $ bufp ++ " = nullptr",
                stmt $ "_ppt_ctrl = nullptr",
                docConcat ["_ppt_hmem_", buf, " = 0"] <> PP.semi,
                stmt "return false"])
        ]
    ]
    where buf = bufName cfg

nextIdxFn :: String -> OutputCfg -> PP.Doc
nextIdxFn firstName cfg =
  -- TODO: Fix this to generate an accesor function for curIndex()
  -- that wraps s_index and the access method for it.
    (if multithreadWrite cfg
      then PP.sep ([ stmt "static std::atomic<int> s_index(1)",
                     blockdecl cfg (PP.text "int nextIndex()") PP.semi [
                       "return s_index.fetch_add(1, std::memory_order_release)" ]])
      else docConcat [ "int nextIndex() { static int s_index = 1; return s_index++; }" ])

moduleSource firstName cfg hasCounters (GMSaveBuffer (LayoutIO sz off) mt) =
  let attrused = " __attribute__((used))"
      statDecl cfg =
        [ docConcat [ "struct ppt_stat_t { pid_t ppt_agent_pid; }"] <> PP.semi,
          externDecl "ppt_stat_t" ["_ppt_stat", bufName cfg] <> PP.semi,
          docConcat ["ppt_stat_t _ppt_stat_", bufName cfg, attrused] <> PP.semi]
      enableNativeCounters = hasCounters && (nativeCounters cfg)
      nCounters = show $ counterCount cfg
      counterMem = if hasCounters
                   then  ([ stmt $ "int data_" ++ (bufName cfg) ++ "::ppt_counter_fd["++ nCounters ++ "]"] ++
                          if (nativeCounters cfg)
                          then [stmt $ "void * data_" ++ (bufName cfg) ++ "::ppt_counter_mmap",
                                stmt $ "uint64_t data_" ++ (bufName cfg) ++ "::ppt_counter_rcx["++ nCounters ++ "]"]
                          else [])
                   else []
      debugMem = if debugOutput cfg
                 then [stmt $ "ppt_control *get_ctrl_ptr() { return _ppt_ctrl; }"]
                 else []
  in PP.vcat ([
    externDecl "const char*" ["_ppt_json", bufName cfg],
    stmt $  firstName  ++ " * data_" ++ (bufName cfg) ++ "::ppt_buf",
    stmt $  "int data_" ++ (bufName cfg) ++ "::ppt_bufsz",
    stmt $  "int data_" ++ (bufName cfg) ++ "::ppt_offset"]
            ++ counterMem ++ [
    PP.hsep [docConcat ["const char* _ppt_json_", bufName cfg],
             PP.text attrused, PP.text "=", PP.text (enquote $ makeJSON cfg)] <> PP.semi,
    docConcat ["int _ppt_hmem_", bufName cfg, " ", attrused, ";"]]
     ++ statDecl cfg ++
    [ controlDecl cfg,
      staticDecl "ppt_control" ["*_ppt_ctrl"],
      nextIdxFn firstName cfg,
      attachFn firstName cfg hasCounters
    ] ++ debugMem)

moduleSource firstName cfg _ GMCounters  =
  let isNative = nativeCounters cfg in
  PP.vcat ([
    blockdeclV cfg (PP.text "void setupCounters()") PP.empty [
        stmt "int prior_fd = -1",
        stmt "if (_ppt_ctrl == nullptr) return",
        blockdecl cfg (PP.text "for (int i = 0 ; i < std::min<int>(_ppt_ctrl->nr_perf_ctrs, 3); i++)"
                      ) PP.empty ([
            stmt $ ("data_" ++ (bufName cfg) ++ "::ppt_counter_fd[i] = syscall(__NR_perf_event_open, " ++
                    "&_ppt_ctrl->counterdata[i].event_attr, 0, -1, prior_fd, 0);"),
            stmt $ "prior_fd = data_"++(bufName cfg)++"::ppt_counter_fd[0]",
            blockdeclV cfg (
                PP.text $ "if (prior_fd < 0 || data_" ++ (bufName cfg) ++ "::ppt_counter_fd[i] < 0)") PP.empty [
                stmt "perror(\"perf_event_open\")",
                stmt "_ppt_ctrl->nr_perf_ctrs = 0",
                stmt $ ("for (int j = i; j >= 0; j--) { close(data_" ++ (bufName cfg) ++
                        "::ppt_counter_fd[j]); data_" ++ (bufName cfg) ++
                        "::ppt_counter_fd[j] = 0; }"),
                stmt "return;"
                ]
            ] ++ (if isNative
                  then [
                     stmt ( "if (i == 0) data_" ++ (bufName cfg) ++
                            "::ppt_counter_mmap = mmap(NULL, sysconf(_SC_PAGESIZE), PROT_READ, MAP_SHARED, prior_fd, 0)")
                      ]
                  else []))

        ],
    blockdecl cfg (PP.text "void closeCounters()") PP.empty ([
        stmt "if (_ppt_ctrl == nullptr) return",
        stmt $ ("for (int j = std::min<int>(_ppt_ctrl->nr_perf_ctrs, 3); j >= 0; j--) { close(data_" ++
                (bufName cfg) ++ "::ppt_counter_fd[j]); data_" ++ (bufName cfg) ++
                 "::ppt_counter_fd[j] = 0;}")
        ] ++ if isNative then [ stmt $ "munmap(data_" ++ (bufName cfg) ++ "::ppt_counter_mmap, sysconf(_SC_PAGESIZE))" ] else [])
  ] ++ if isNative
       then [] -- native counters are saved in inline decls.
       else [
    let args = L.intercalate ", " $ map (\n -> "uint64_t* " ++ [n]) $ take (counterCount cfg) ['a'..'z']
        pfx = "data_" ++ (bufName cfg) ++ "::"
        fd = pfx ++ "ppt_counter_fd[0]"
    in blockdecl cfg (PP.text $ "void save_counters(" ++ args ++ ")") PP.empty [
        blockdeclV cfg (PP.text $ "if (_ppt_ctrl != nullptr && _ppt_ctrl->nr_perf_ctrs > 0 && " ++ fd ++ " > -1)") PP.empty ([
          stmt $ "uint64_t buf[" ++ show (1 + counterCount cfg) ++ "]",
          stmt $ "read(" ++ fd ++ ", buf, (1 + _ppt_ctrl->nr_perf_ctrs) * sizeof(uint64_t))",
          blockdeclV cfg (PP.text $ "switch (_ppt_ctrl->nr_perf_ctrs)") PP.empty (
              let letters = ['a'..'z']
                  indices = take (counterCount cfg) [0..]
              in ((concatMap (\n -> [PP.text $ "case " ++ show (n+1) ++ ":",
                                   stmt $ ' ':' ':'*':(letters !! n):(" = buf[" ++ (show (n + 1)) ++ "]")]) $ reverse indices)
                   ++ [stmt "default: break"])
              )
          ]) -- switch(nr ctrs)
        ] -- void save_counters()
    ]) -- else ...

statLayouts :: [FrameLayout] -> (String, LayoutIOSpec)
statLayouts flayouts= case flayouts of
        [] -> ("void", LayoutIO  0 0)
        (x:_) -> (flName x, layoutSpec x)

modImpls :: OutputCfg -> String -> [GenModule] -> PP.Doc
modImpls cfg firstName mods =
  PP.sep $ map (moduleSource firstName cfg hasCounters) sortMods
  where
    sortMods = L.sort mods
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

