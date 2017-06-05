{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ppt.Agent.Command where
import Ppt.Agent.ElfProtocol
import Control.Lens                 hiding (element, noneOf, sizeOf)
import Control.Exception (handle, displayException)
import Control.Exception.Base
import Control.Monad.State
import Control.Concurrent
import Control.Applicative
import Data.Foldable hiding (length)
import Data.Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy         hiding (readFile, map, head, tail, putStrLn, putStr)
import Data.Int
import Data.Maybe
import Data.Time
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Ppt.Frame.Layout             hiding (sizeOf)
import Prelude                      hiding (length)
import System.IO
import Numeric
import qualified Data.Binary as B
import qualified Data.List as L
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified System.Console.GetOpt        as GO

data Flag = Pid { _pid :: Int }
          | Filename { _fileName :: String }
          | Desc { _desc :: String }
          | DescFile { _descFile :: String }
          | TimeOffset { _timeOffset :: Double }
            -- ^Time to offset from the system clock.  In Hours.
          | BufferName { _bufName :: String }
          | ListBuffers { _listBuffersDummy :: Int }
          | ShowHelp { _showHelpDummy :: Int }
          | Verbose { _verbLevel :: Int }
          | Remainder { _remain :: String }
          deriving (Eq, Show)
makeLenses ''Flag


arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['p'] ["pid"] (GO.ReqArg (\t -> Pid (read t)) "pid")
            "Pid to attach to. (required)"
          , GO.Option ['b'] ["buffer"] (GO.ReqArg (\t -> BufferName t) "buffer name")
            "Name of buffer to read.  (required)"
          , GO.Option ['l'] ["list-buffers"] (GO.NoArg (ListBuffers 0))
            "List buffers in process."
          , GO.Option ['h', '?'] ["help"] (GO.NoArg (ShowHelp 0))
            "Print help."
          , GO.Option ['d'] ["desc"] (GO.ReqArg (\t -> Desc t) "description")
            "A comment on the collection session."
          , GO.Option ['D'] ["descfile"] (GO.ReqArg (\t -> DescFile t) "descriptionfile")
            "File containing comment on the collection session."
          , GO.Option ['t'] ["time-offset"] (GO.ReqArg (\t -> TimeOffset (read t)) "delta-hours")
            "Apply this change to the timestamps on this machine."
          , GO.Option ['o'] ["output-file"] (GO.ReqArg (\t -> Filename t) "output-file")
            "Output file to save buffer contents. (required)"
          , GO.Option ['v'] ["verbose"] (GO.NoArg (Verbose 0))
            "Verbose output."]

data Execution = Exec { ePid        :: Int
                      , eFilename   :: String
                      , eDesc       :: String
                      , eTimeOff    :: Double
                      , eBufferName :: String
                      , eVerbose    :: Int
                      }
               | ExecBuffers { ePid :: Int }
               | ExecShowHelp
               deriving (Show)
makeLenses ''Execution

tryLens :: (a -> Maybe b) -> [a] -> Maybe b
tryLens fn list =
  let results = catMaybes $ map fn list
  in case results of
    (x:_) -> Just x
    [] -> Nothing

countLens :: (a -> Maybe b) -> [a] -> Int
countLens fn list =
  let results = catMaybes $ map fn list
  in L.length results

theDesc :: [Flag] -> IO String
theDesc [] = return ""
theDesc (Desc s:_) = return s
theDesc (DescFile n:_) = do c <- readFile n; return c
theDesc (_:ps) = theDesc ps

combine :: String -> [Flag] -> Maybe Execution
combine desc flags =
 let pId = tryLens (preview pid) flags
     filename = tryLens (preview fileName) flags
     timeoffset = tryLens (preview timeOffset) flags
     bufname = tryLens (preview bufName) flags
     remainders = catMaybes $ map (preview remain) flags
     verbosity = countLens (preview verbLevel) flags
     tryTake :: Read a => Maybe a -> State [String] (Maybe a)
     tryTake (Just s) = return $ Just s
     tryTake Nothing = do
       s <- get
       case s of
         [] -> return Nothing
         _ -> do
           let rest = tail s
           put rest
           return $ Just (read $ head s)
     takeDefault :: Read a => Maybe a -> a -> State [String] (a)
     takeDefault (Just s) def = return s
     takeDefault Nothing def = do
       s <- get
       case s of
         [] -> return def
         _ -> do
           let rest = tail s
           put rest
           return (read $ head s)

     (result, _) = flip runState remainders $ do
       _pid <- tryTake pId
       _bufname <- tryTake bufname
       _filename <- tryTake filename
       _timeOff <- takeDefault timeoffset 0.0
       return $ Exec <$> _pid <*> _filename <*> (pure desc) <*> (pure _timeOff) <*> _bufname <*> (
         pure verbosity)
 in result

parseArgs :: [String] -> IO (Execution)
parseArgs args = do
  let (flags, rest, errs) = GO.getOpt (GO.ReturnInOrder (\s -> Remainder s)) arglist args
  desc <- theDesc flags
  return $ fromJust $ ((combine desc flags) <|>
                       (tryLens (preview listBuffersDummy) flags >> (ExecBuffers <$> tryLens (preview pid) flags)) <|>
                        (pure ExecShowHelp))

-- |File Format for output:
-- 4 bytes Magic Number (0x50505431, 'PPT1')
-- 4 Byte header length (little endian), named 'k'
-- k bytes of JSON
-- remainder of file is saved buffers.

saveFile :: FilePath -> FileRecord -> IO (Handle)
saveFile name header = do
  let text = encode header
      textLen = (fromIntegral $ length text) :: Int32
      magic = 0x50505431 :: Int32
  h <- openBinaryFile name WriteMode
  hSetBuffering h (BlockBuffering $ Just 8)
  hPut h (B.encode  magic)
  hPut h (B.encode textLen)
  hPut h text
  hFlush h
  return h

writeBufferToFile :: (Storable a, Show a, Integral a, FiniteBits a) => Handle -> VM.IOVector a -> Int -> Int -> Int -> IO ()
writeBufferToFile h vec startIndex nelements verb = do
  let hexList [] = []
      hexList xs = (L.concatMap hex thisWord) ++ (' ':(hexList rest))
        where hex n = (L.take (countLeadingZeros n `div` 4) $ L.repeat '0') ++ (showHex n " ")
              (thisWord, rest) = L.splitAt 4 xs
      readFn :: (Storable a, Integral a) => Ptr a-> IO ([a])
      readFn p = do
            -- Don't actually need the data, just a value of 'a' from Ptr a to get its size.
            first <- peek p
            let size = sizeOf first
                start = plusPtr p startIndex :: Ptr a
                end = plusPtr start nelements :: Ptr a
                toList :: (Integral a, Storable a) => Ptr a -> Ptr a -> Int -> IO ([a])
                toList s e sz
                  | s == e = return []
                  | otherwise = do val <- peek s
                                   rest <- toList (plusPtr s sz) e sz
                                   return (val:rest)
            valList <- toList start end size -- :: (Storable a, Integral a) => IO ([a])
            if verb > 0
              then do putStrLn $ "calling hPutBuf with p=" ++ (show p) ++ " start=" ++ (show start) ++ "  size=" ++ (
                        show $ size * nelements) ++ " and startIndex=" ++ show startIndex
              else return ()
            hPutBuf h start nelements
            return valList
  lst <- VM.unsafeWith vec readFn
  if verb > 1
    then putStrLn $ hexList lst
    else return ()


-- Actual command implementations of Agent-mode commands (attach)
processBufferValues :: String -> Double -> String -> Int -> IntPtr -> JsonRep -> Int -> IO ()
processBufferValues desc timeOffset fileName verbose shmPtr json bufElems = do
  let (Just elemSize) = frameSize json
      elemSizeInWords = elemSize `div` 4
      destIntPtr = ((roundUp 392 elemSize) + fromIntegral shmPtr)
      destP = intPtrToPtr $ fromIntegral destIntPtr
      verbPutLn s = if verbose > 0 then putStrLn s else return ()
      execLoop file cursor seqno saveBuffer = do
            (count', cursor', seqno') <- readBuffer verbose destP cursor seqno bufElems elemSizeInWords saveBuffer
            verbPutLn $ ">> (Raw) Count: " ++ show count' ++ ", Cursor: " ++ show cursor' ++ ", seqno: " ++ show seqno'
            verbPutLn $ ">> bufElems: " ++ show bufElems
            verbPutLn $ ">> shmPtr: " ++ (showHex shmPtr ", destIntPtr: ") ++ (showHex  destIntPtr "")
            verbPutLn $ ">> that's an offset of " ++ show (destIntPtr - fromIntegral shmPtr)
            verbPutLn $ ">> Size of an object: " ++ (show elemSize) ++ ", " ++ (show elemSizeInWords) ++ " words."
            let count = min bufElems (max count' 0)
            if (count > 0)
            then writeBufferToFile file saveBuffer 0 (count * elemSize) verbose
            else return ()
            -- TODO: make this dynamic timing based on count vs bufsize.
            hFlush file -- TODO: only do this occasionally.
            threadDelay 250000
            execLoop file cursor' seqno' saveBuffer
      flushHandler :: Handle -> AsyncException -> IO ()
      flushHandler file UserInterrupt = do verbPutLn ">> Closing file"
                                           hClose file
      flushHandler file ex = do self <- myThreadId
                                hClose file
                                throwTo self ex
  file <- saveFile fileName (FileRecord "1.0.0" "now" desc (round $ timeOffset * 3600.0) json)
  destBuffer <- VM.new (bufElems * elemSizeInWords)
  handle (flushHandler file) $ execLoop file 0 1 destBuffer
  return ()


attach :: [String] -> IO ()
attach args = do
  command <- parseArgs args
--  putStrLn $ "got args: " ++ show args
--  putStrLn $ " read as : " ++ show command
  case command of
    Exec pid fname desc toff bufname verbosity ->
      attachAndRun pid bufname (processBufferValues desc toff fname) verbosity
    ExecBuffers pid -> do
      buffers <- listBuffers pid
      putStrLn $ "Buffers: " ++ L.intercalate ", " buffers
    ExecShowHelp ->
      putStr $ GO.usageInfo "ppt attach " arglist
  return ()
