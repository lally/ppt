{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ppt.Agent.Command where
import Ppt.Agent.ElfProtocol
import Control.Lens                 hiding (element, noneOf, sizeOf)
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
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Ppt.Frame.Layout             hiding (sizeOf)
import Prelude                      hiding (length)
import System.IO
import Numeric
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
          | Remainder { _remain :: String }
          deriving (Eq, Show)
makeLenses ''Flag


arglist :: [GO.OptDescr Flag]
arglist = [GO.Option ['p'] ["pid"] (GO.ReqArg (\t -> Pid (read t)) "pid") "Pid to attach to."
          , GO.Option ['b'] ["buffer"] (GO.ReqArg (\t -> BufferName t) "buffer name") "Name of buffer to read."
          , GO.Option ['l'] ["list-buffers"] (GO.NoArg (ListBuffers 0)) "List buffers in process."
          , GO.Option ['h', '?'] ["help"] (GO.NoArg (ShowHelp 0)) "Print help."
          , GO.Option ['d'] ["desc"] (GO.ReqArg (\t -> Desc t) "description") "Comment on the collection session."
          , GO.Option ['D'] ["descfile"] (GO.ReqArg (\t -> DescFile t) "descriptionfile") "File containing comment on the collection session."
          , GO.Option ['t'] ["time-offset"] (GO.ReqArg (\t -> TimeOffset (read t)) "delta-hours") "Apply this change to the timestamps on this machine." ]

data Execution = Exec { ePid        :: Int
                      , eFilename   :: String
                      , eDesc       :: String
                      , eTimeOff    :: Double
                      , eBufferName :: String
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
     (result, _) = flip runState remainders $ do
       _pid <- tryTake pId
       _filename <- tryTake filename
       _timeOff <- tryTake timeoffset
       _bufname <- tryTake bufname
       return $ Exec <$> _pid <*> _filename <*> (pure desc) <*> _timeOff <*> _bufname
 in result

parseArgs :: [String] -> IO (Execution)
parseArgs args = do
  let (flags, rest, errs) = GO.getOpt (GO.ReturnInOrder (\s -> Remainder s)) arglist args
  desc <- theDesc flags
  return $ fromJust $ ((combine desc flags) <|>
                       (tryLens (preview listBuffersDummy) flags >> (ExecBuffers <$> tryLens (preview pid) flags)) <|>
                        (pure ExecShowHelp))

data FileRecord = FileRecord { frFormat    :: String
                             , frDate      :: String
                             , frComment   :: String
                             , frDeltaTime :: Int    -- in seconds
                             , frJson      :: JsonRep }
                  deriving (Generic, Eq, Show)
instance ToJSON FileRecord
instance FromJSON FileRecord

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
  hSetBuffering h (BlockBuffering $ Just 1048576)
  hPut h (toByteString magic)
  hPut h (toByteString textLen)
  hPut h text
  return h

writeBufferToFile :: Storable a => Handle -> VM.IOVector a -> Int -> Int -> IO ()
writeBufferToFile h vec startIndex nelements = do
  VM.unsafeWith vec $ \p -> do
    -- Don't actually need the data, just a value of 'a' from Ptr a to get its size.
    let start = plusPtr p startIndex
    first <- peek p
    let size = sizeOf first
    hPutBuf h start (size * nelements)

-- Actual command implementations of Agent-mode commands (attach)
processBufferValues :: String -> Double -> String -> IntPtr -> JsonRep -> Int -> IO ()
processBufferValues desc timeOffset fileName shmPtr json bufElems = do
  let (Just elemSize) = frameSize json
      elemSizeInWords = elemSize `div` 4
      destIntPtr = ((roundUp 392 elemSize) + fromIntegral shmPtr)
      destP = intPtrToPtr $ fromIntegral destIntPtr
      execLoop file cursor seqno saveBuffer = do
            (count, cursor', seqno') <- readBuffer destP cursor seqno bufElems elemSizeInWords saveBuffer
            putStrLn $ ">> Count: " ++ show count ++ ", Cursor: " ++ show cursor' ++ ", seqno: " ++ show seqno'
            putStrLn $ ">> shmPtr: " ++ (showHex shmPtr ", destIntPtr: ") ++ (showHex  destIntPtr "")
            putStrLn $ ">> that's an offset of " ++ show (destIntPtr - fromIntegral shmPtr)
            putStrLn $ ">> Size of an object: " ++ (show elemSize) ++ ", " ++ (show elemSizeInWords) ++ " words."
            if (bufElems < cursor + count)
              then writeBufferToFile file saveBuffer cursor count
              else do
                let endPart = bufElems - cursor
                    firstPart = count - endPart
                writeBufferToFile file saveBuffer cursor endPart
                writeBufferToFile file saveBuffer 0 firstPart
            -- TODO: make this dynamic timing based on count vs bufsize.
            threadDelay 250000
            execLoop file cursor' seqno' saveBuffer
  file <- saveFile fileName (FileRecord "1.0.0" "now" desc (round $ timeOffset * 3600.0) json)
  destBuffer <- VM.new (bufElems * elemSizeInWords)
  execLoop file 0 1 destBuffer
  return ()


attach :: [String] -> IO ()
attach args = do
  command <- parseArgs args
  case command of
    Exec pid fname desc toff bufname ->
      attachAndRun pid bufname (processBufferValues desc toff fname)
    ExecBuffers pid -> do
      buffers <- listBuffers pid
      putStrLn $ "Buffers: " ++ L.intercalate ", " buffers
    ExecShowHelp ->
      putStr $ GO.usageInfo "ppt attach " arglist
  return ()
