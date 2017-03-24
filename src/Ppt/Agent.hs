{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ppt.Agent(attach) where
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Char
import Data.Maybe
import Foreign.C.Error
import Numeric
import Ppt.Configuration
import Ppt.ElfProcess
import Ppt.Frame.Layout
import Ppt.StaticInstrumentation
import Ppt.SIParser as SIP
import System.Exit
import System.Process
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Elf as E
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified System.Console.GetOpt as GO
import qualified System.Posix.Process as POS

C.include "ppt-control.h"
C.include "<sys/ipc.h>"
C.include "<sys/shm.h>"

{-
    Command line processing support
-}
data Flag = Pid Int
          deriving (Eq, Show)

arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['p'] ["pid"] (GO.ReqArg (\t -> Pid (read t)) "pid") "Pid to attach to."]

-- Determine the single-member element size of the shared memory segment.
sharedMemSz :: JsonRep -> Maybe Int
sharedMemSz json =
  let last :: [a] -> Maybe a
      last [] = Nothing
      last [x] = Just x
      last (a:as) = last as
      emit = jsBufferEmit json
      frames = jsBufferFrames json
      size = do
        aFrame <- last frames
        lastMem <- last $ flLayout aFrame
        return (lOffset lastMem + lSize lastMem)
  in size

roundUp :: Int -> Int -> Int
roundUp elemSz blockSz =
  let min = blockSz `div` elemSz
      additional = if (blockSz `mod` elemSz) > 0
        then 1
        else 0
  in (min + additional) * elemSz

-- Attach to a running process.
-- Command line arguments:
--  ppt attach -p <pid> -v <version> <spec>
-- Note, -v isn't yet implemented.
--
-- attachSetup pid symbolsWithPrefix

check :: String -> Bool -> Maybe String
check _ True = Nothing
check desc False = Just desc

-- Lazy evaluates to first error (or to the end of list)
checkErrors :: [Maybe String] -> IO ()
checkErrors ((Just s):ss) = die s
checkErrors (Nothing:ss) = checkErrors ss
checkErrors [] = return ()

numElementsInBuffer = 16384

attach :: [String] -> RunConfig -> IO ()
attach [] _ = do
  putStrLn "usage: ppt attach <pid>"

attach (a:as) cfg = do
  let pid = read a :: Int
  process <- loadProcess pid
  let hmem_pfx = "_ppt_hmem_"
      stat_pfx = "_ppt_stat_"
      json_pfx = "_ppt_json_"
      pfx_syms = symbolsWithPrefix process hmem_pfx
      hmem_syms = map BSC.unpack $ mapMaybe (snd . E.steName) $ pfx_syms
      buffers = map (drop (L.length hmem_pfx)) hmem_syms
      bufs = HM.fromList $ mapMaybe (\s -> (,) <$> (fmap BSC.unpack $ snd (E.steName s)) <*> (pure s)) pfx_syms
      bufferName = head as
      sym = HM.lookup (hmem_pfx ++ head as) bufs
      (Just hmem_sym) = sym
      statSyms = symbolsWithPrefix process (stat_pfx ++ bufferName)
      jsonSyms = symbolsWithPrefix process (json_pfx ++ bufferName)
      -- These aren't evaluated until after checkErrors validates the lists.
      statSym = head statSyms
      jsonSym = head jsonSyms
  checkErrors [
    check "No symbols found" $ length hmem_syms > 0,
    check "Symbol not found" $ isJust sym,
    check (concat ["Could not find ", stat_pfx, bufferName]) $ length statSyms == 1,
    check (concat ["Could not find ", json_pfx, bufferName]) $ length jsonSyms == 1
    ]
  ourPid <- POS.getProcessID
  moldPid <- swapIntegerInProcess pid statSym 0 (fromIntegral ourPid)
  mabiStr <- stringInProcess pid jsonSym

  let (Just oldPid) = moldPid
      (Just abiStr) = mabiStr
      jsonVal = (decode $ BSL.fromStrict abiStr) :: Maybe JsonRep
      (Just json) = jsonVal
      melemSize = sharedMemSz json
      (Just elemSize) = melemSize
      ctrlStructSz = [C.pure| int{ sizeof(struct ppt_control) } |]
      controlSize = roundUp  392 elemSize
      totalBufferSize = fromIntegral $ controlSize + elemSize * numElementsInBuffer
  checkErrors [
    check ("Failed to read symbol in pid " ++ show pid) $ isJust moldPid,
    check ("Process appears busy with ppt pid " ++ show oldPid) $ oldPid /= 0,
    check ("Failed to read buffer metadata from " ++ show pid) $ isJust mabiStr,
    check ("Failed to decode JSON metadata") $ isJust jsonVal
    ]
  -- Ok, we have the lock in this process nad the JSON read.  Now create the shared mem block
  -- and try to attach it.
  shmId <- throwErrnoIfMinus1 "Failed to allocate shared memory" (
    [C.exp| int {shmget(IPC_PRIVATE, $(size_t totalBufferSize), IPC_CREAT | IPC_EXCL)} |])
  -- TODO: put this all in a handle block that nukes the shared mem segment
  shmAddr <- throwErrnoIfMinus1 "Failed to attach shared memory block" (
    [C.exp| uintptr_t {(uintptr_t) shmat($(int shmId), NULL, 0)} |])
  [C.block| void {
    struct ppt_control *pc = (struct ppt_control*) $(uintptr_t shmAddr);
    pc->control_blk_sz = sizeof(struct ppt_control);
    pc->data_block_hmem = 0;
    pc->nr_perf_ctrs = 0;
    pc->client_flags = 0; } |]
  moldHandle <- swapIntegerInProcess pid hmem_sym 0 (fromIntegral shmId)
  checkErrors [
    let (Just oldHandle) = moldHandle in check ("Got back old memory handle " ++ show oldHandle) $ (fromIntegral oldHandle) == shmId
    ]
  putStrLn "Shared memory attached."

  if ((L.length as) > 0) && (isJust $ (HM.lookup (hmem_pfx ++ head as) bufs))
  then
   do let bufname = head as
      putStrLn ("FAKE: attaching to buffer " ++ bufname)
      return ()
  else
   do putStrLn $ "Buffers: " ++ (L.intercalate "," buffers) ++ ".  Use ppt attach <pid> <buffer> to attach to that buffer."



