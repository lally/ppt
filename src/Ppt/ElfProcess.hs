{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ppt.ElfProcess where
import Data.Bits (shiftR, (.&.))
import Foreign.C.Types
import Control.Concurrent (runInBoundThread)
import qualified Data.Elf as E
import qualified Data.List as L
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.include "<stdio.h>"
C.include "<inttypes.h>"
C.include "<endian.h>"
C.include "<sys/ptrace.h>"
C.include "<sys/types.h>"
C.include "<sys/wait.h>"

loadProcess :: Int -> IO E.Elf
loadProcess pid = do {
  ret <- IO.withFile ("/proc/" ++ show pid ++ "/exe") IO.ReadMode (
      \h -> do { fileContents <- BS.hGetContents h
               ; return $ E.parseElf fileContents
               })
  ; return ret
  }

symbolsWithPrefix :: E.Elf -> String -> [E.ElfSymbolTableEntry]
symbolsWithPrefix elf nm =
  let symbols = concat $ E.parseSymbolTables elf
      startsWith :: BS.ByteString -> E.ElfSymbolTableEntry -> Bool
      startsWith name sym =
        case E.steName sym of
          (_, Just nm) -> BS.isPrefixOf name  nm
          _ -> False
  in filter (startsWith (pack nm)) symbols

{-
From the linux-ptrace package:
-- | Sadly, only the OS thread which performed the ptrace_attach is allowed
-- to mess with the traced process. This means that users of this module will
-- need to forkOS or runInBoundThread in order to get reliable behaviour.
-}
bytesOf :: CULong -> [Int] -> BS.ByteString
bytesOf value bytes =
  BS.pack $ map (\nr -> fromIntegral $ 0xFF .&. (shiftR value ((8-nr-1) * 8))) bytes

-- |Most significant Zero Byte
mzb :: CULong -> Maybe Int
mzb v
  | v .&. 0xFF00000000000000 == 0 = Just 0
  | v .&. 0x00FF000000000000 == 0 = Just 1
  | v .&. 0x0000FF0000000000 == 0 = Just 2
  | v .&. 0x000000FF00000000 == 0 = Just 3
  | v .&. 0x00000000FF000000 == 0 = Just 4
  | v .&. 0x0000000000FF0000 == 0 = Just 5
  | v .&. 0x000000000000FF00 == 0 = Just 6
  | v .&. 0x00000000000000FF == 0 = Just 7
  | otherwise = Nothing

stringFrom :: [CULong] -> BS.ByteString
stringFrom (v:vs) =
  case mzb v of
    Nothing -> bytesOf v [0..7] `BS.append` stringFrom vs
    Just n -> bytesOf v [0..(n-1)]

stringFrom_ :: [IO (CULong)] -> IO BS.ByteString
stringFrom_ (fn:fns) =
  do v <- fn
     case mzb v of
       Nothing -> do
         vs <- stringFrom_ fns
         return (bytesOf v [0..7] `BS.append` vs)
       Just n -> do
         return $ bytesOf v [0..(n-1)]

runInPtrace :: Int -> (CInt -> IO a) -> IO (Maybe a)
runInPtrace ipid fn =
  let op = do let pid = fromIntegral ipid
              ptrace_ret <- [C.exp| int {ptrace(PTRACE_ATTACH, $(int pid), 0, 0)}|]
              if ptrace_ret /= 0
              then do
                 [C.exp| void { perror("PTRACE_ATTACH") }|]
                 putStrLn "Have a look at /etc/sysctl.d/10-ptrace.conf"
                 return Nothing
              else do
                  [C.block| void {
                     int unused_status;
                     waitpid($(int pid), &unused_status, 0);
                   }|]
                  value <- fn pid
                  [C.exp| int {ptrace(PTRACE_DETACH, $(int pid), 0, 0)}|]
                  return $ Just value
  in runInBoundThread op

integerInProcess :: Int -> E.ElfSymbolTableEntry -> IO (Maybe Int)
integerInProcess pi sym =
  let addr = fromIntegral $ E.steValue sym
  in runInPtrace pi (\pid -> do
         value <- [C.exp| int {ptrace(PTRACE_PEEKDATA, $(int pid), (void *) $(uintptr_t addr), 0)}|]
         return $ fromIntegral value)

setIntegerInProcess :: Int -> E.ElfSymbolTableEntry  -> Int -> IO (Bool)
setIntegerInProcess pi sym value =
  do let addr = fromIntegral $ E.steValue sym
         rawVal = fromIntegral value
     ret <- runInPtrace pi (\pid -> do
         value <- [C.exp| int {ptrace(PTRACE_POKEDATA, $(int pid), (void *) $(uintptr_t addr), $(int rawVal))}|]
         return $ fromIntegral value)
     return $ maybe False (\a -> a == 0) ret

swapIntegerInProcess :: Int -> E.ElfSymbolTableEntry -> Int -> Int -> IO (Maybe Int)
swapIntegerInProcess pi sym oldValue newValue =
  do let addr = fromIntegral $ E.steValue sym
         rawVal = fromIntegral newValue
         coldValue = fromIntegral oldValue
     ret <- runInPtrace pi (\pid -> do
         value <- [C.block| int {
                      int result = ptrace(PTRACE_PEEKDATA, $(int pid), (void *) $(uintptr_t addr), 0);
                      if (result == $(int coldValue)) {
                           ptrace(PTRACE_POKEDATA, $(int pid), (void *) $(uintptr_t addr), $(int rawVal));
                           return $(int rawVal);
                      } else {
                           return result;
                      } }|]
         return $ fromIntegral value)
     return ret

stringInProcess :: Int -> E.ElfSymbolTableEntry -> IO (Maybe BS.ByteString)
stringInProcess pi sym =
-- NOTE: remember to do a (BS.length value) `seq` value from calling stringFrom_
  do let addr :: CUIntPtr
         addr = fromIntegral $ E.steValue sym
         fetchBytes :: CInt -> CUIntPtr -> IO (CULong)
         fetchBytes pid off = do
           [C.block| unsigned long {
               return be64toh(ptrace(PTRACE_PEEKDATA, $(int pid), (void *) $(uintptr_t off), 0));
               } |]
     ret <- runInPtrace pi (\pid -> do
         ptr <- [C.exp| unsigned long { ptrace(PTRACE_PEEKDATA, $(int pid), (void *) $(uintptr_t addr), 0)} |]
         res <- stringFrom_ (map (\o -> fetchBytes pid ((fromIntegral ptr) + fromIntegral (8*o))) [0..])
         return res)

     return ret

