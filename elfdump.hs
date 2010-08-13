
import Data.Elf
import Data.ByteString
import Data.Binary.Get
import Data.Word
import Data.ByteString.Lazy.UTF8
import System.Environment (getArgs, getProgName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

{- A convenience function for ghci -}

elf :: String -> IO Elf
elf filename = do (liftM parseElf (B.readFile filename))

{- Read String Tables -}
strSections :: Elf -> [ElfSection]
strSections elf = Prelude.filter isStringSect $ elfSections elf
                  where isStringSect x = (elfSectionType x == SHT_STRTAB)
                                         && (elfSectionName x == ".strtab")

stringAtIndex :: ElfSection -> Int -> String
stringAtIndex sect n = let dropped =  B.drop n $ elfSectionData sect
                           bstring = Prelude.head $ B.split 0 dropped
                        in toString $ LB.fromChunks [bstring]
                  
stringsOf :: ElfSection -> (String, [B.ByteString])
stringsOf sect = (elfSectionName sect, B.split 0 $ elfSectionData sect)

{- Read Symbol Tables -}
readElfWord :: Get (Word32)
readElfWord = do
            w <- getWord32le
            return w

readElfChar :: Get (Word8)
readElfChar = do { w <- getWord8; return w }
readElfShort :: Get (Word16)
readElfShort = do { w <- getWord16le; return w }

data ElfSectionIndex = ElfSectUndefined
                     | ElfSectLoReserveOrLoProc -- these two are indistinguishable
                     | ElfSectProcessor Int
                     | ElfSectHiProc
                     | ElfSectABS
                     | ElfSectCommon
                     | ElfSectHiReserve
                     | ElfSectOther Int
                     deriving (Eq, Show)

data ElfSymbol = ElfSymbol {
       name :: String
     , value :: Int
     , size :: Int
     , info :: Int
     , other :: Int
     , shndx :: ElfSectionIndex
     } deriving (Eq, Show)

readElfSymTableEnt :: (Int -> String) -> Get ElfSymbol
readElfSymTableEnt conv = do
                        name <- liftM conv $ liftM fromIntegral readElfWord
                        value <- fromIntegral `liftM` readElfWord
                        size <- fromIntegral `liftM` readElfWord
                        info <- fromIntegral `liftM` readElfChar
                        skip 1 -- 'other' is always zero, so far.
                        shndx_key <- fromIntegral `liftM` readElfShort
                        -- from table 4.7 of the elf sepc. (gabi41.pdf)
                        let shndx = case shndx_key of
                                         0 -> ElfSectUndefined
                                         0xff00 -> ElfSectLoReserveOrLoProc
                                         0xff1f -> ElfSectHiProc
                                         0xfff1 -> ElfSectABS
                                         0xfff2 -> ElfSectCommon
                                         0xffff -> ElfSectHiReserve
                                         x | x >= 0xff00 && x <= 0xff1f -> ElfSectProcessor x
                                           | otherwise -> ElfSectOther x
                                         
                        return ElfSymbol { name = name,
                               value = value,
                               size = size,
                               info = info,
                               other=0,
                               shndx=shndx }
                        


readElfSymTable :: (Int -> String) -> B.ByteString -> [ElfSymbol]
readElfSymTable conv bs = readElfSymTable' conv (LB.fromChunks [bs])
readElfSymTable' conv bs | LB.null bs = []
                         | otherwise = let readFunc = readElfSymTableEnt conv
                                           (curSym, remain, nextOff) = runGetState readFunc bs 0
                                       in curSym : (readElfSymTable' conv remain)

loadElfSymbols :: Elf -> M.Map String ElfSymbol
loadElfSymbols elf =
     let conv = stringAtIndex (Prelude.head $ strSections elf)
         symtab = Prelude.filter (\x -> (SHT_SYMTAB == elfSectionType x )) $ elfSections elf
         symtabdata = elfSectionData $ Prelude.head $ symtab
         symbols = readElfSymTable conv symtabdata
         map = Prelude.map (\x -> (name x, x)) symbols
      in M.fromList map

                                       
main = do 
--     elf <- liftM parseElf (B.readFile "/research/phd/libmet/test_target")
     loadElfSymbols `liftM` (elf "/research/phd/libmet/test_target")
     
     return 0
     
       
        