{-# LANGUAGE DeriveGeneric #-}
module Ppt.Frame.Layout where
import GHC.Generics
import Data.Aeson
import Data.Typeable
import Ppt.Frame.ParsedRep
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Either
import Debug.Trace
import Data.List as L
import qualified Data.HashMap.Strict as HM

data LayoutException = InvalidInputException Frame String
                     | InternalFailureException String
    deriving (Show, Typeable)

instance Exception LayoutException

-- |Indicates sizes of machine types on the runtime platform.  Also
-- includes other layout-related values due to options.
data TargetInfo = TargetInfo { tDouble :: Int
                             , tFloat :: Int
                             , tInt :: Int
                             , tTime :: Int
                             , tCounter :: Int
                             , tPadTo :: Int
                               -- ^Minimum size to pad an object to.
                               -- Normally the same as tInt, unless
                               -- there's an option to pad it higher,
                               -- to take advantage of specialized
                               -- instructions.
                             } deriving (Generic, Eq, Show)

-- |Machine Layout
data SeqNoSide = FrontSeq | BackSeq deriving (Generic, Eq, Show)

data IntervalSide = IntBegin Int Int -- ^Begin of Num of Num
                  | IntEnd Int Int deriving (Generic, Eq, Show)

data LayoutKind = LKSeqno SeqNoSide
                  -- ^Sequence numbers.  Front and back
                | LKTypeDescrim Int
                  -- ^Number of elements to discriminate by.
                | LKMember FrameMember (Maybe IntervalSide)
                  -- ^An actual declared member.  It may be one of a
                  -- group.  (Current, Total), where Current < Total,
                  -- always.  For counters, we have some number of
                  -- them (3) that get generated for each 'counter'
                  -- member.  Interval counters get twice as many.
                 | LKPadding Int
                   -- ^Padding between members, or at end.  Param is
                   -- number of bytes
                deriving (Generic, Eq, Show)
makeTraversals ''LayoutKind

data LayoutMember = LMember { _lType :: Primitive
                            , _lOffset :: Int
                            , _lAlignment :: Int
                              -- ^Typically the same as lSize, unless it's padding
                            , _lSize :: Int
                              -- ^Purely a function of TargetInfo and lType
                            , _lKind :: LayoutKind
                            , _lName :: String }
                   deriving (Generic, Eq, Show)

makeLenses ''LayoutMember

data LayoutIOSpec = LayoutIO { _lioSize :: Int
                             , _lioBackOffset :: Int
                             } deriving (Eq, Show)

makeLenses ''LayoutIOSpec

data FrameLayout = FLayout { _flName :: String
                           , _flFrame :: Frame
                           , _flLayout :: [LayoutMember]
                           } deriving (Generic, Eq, Show)

makeLenses ''FrameLayout

data JsonRep = JsonRep { jsAbi :: String
                       , jsBufferEmit :: EmitOptions
                       , jsTarget :: TargetInfo
                       , jsBufferFrames :: [FrameLayout]
                       , jsTags :: [(String, String)]
                       , jsMetadata :: [String]
                       } deriving (Generic, Eq, Show)

layoutSpec :: FrameLayout -> LayoutIOSpec
layoutSpec layout =
  let sumSize = sum $ map lSize $ flLayout layout
      lastMem = last $ flLayout layout
      backOff = case lastMem of
                  (LMember _ off _ _ (LKSeqno BackSeq) _) -> off
  in LayoutIO sumSize backOff

sizeOf :: TargetInfo -> Primitive -> Int
sizeOf info PDouble = tDouble info
sizeOf info PFloat = tFloat info
sizeOf info PInt = tInt info
sizeOf info (PTime _) = tTime info
sizeOf info (PCounter _) = tCounter info
sizeOf info PByte = 1

alignOf :: TargetInfo -> Primitive -> Int
alignOf info PDouble = tDouble info
alignOf info PFloat = tFloat info
alignOf info PInt = tInt info
alignOf info (PTime _) = tCounter info
alignOf info (PCounter _)= tCounter info
alignOf info PByte = 1

alignRemainder :: Int -> Int -> Int
alignRemainder align amt
  | align == amt = 0
  | otherwise = align - (amt `mod` align)

roundTo :: (Integral n) => n -> n -> n
roundTo align val
 | (val `mod` align) == 0 = val
 | otherwise = val + (align - (val `mod` align))

serializeOffsets :: TargetInfo -> [LayoutMember] -> [LayoutMember]
serializeOffsets target inmems =
  let serialize :: TargetInfo -> Int -> [LayoutMember] -> [LayoutMember]
      serialize tinfo pfx (m:mems) =
        let nextElemSize = pfx + (sizeOf tinfo (lType m))
        in (m {lOffset = pfx }):(serialize tinfo nextElemSize mems)
  in serialize target 0 inmems

mlast :: [a] -> Maybe a
mlast [] = Nothing
mlast [x] = Just x
mlast (a:as) = mlast as

-- |Determine the single-member element size of the shared memory
-- segment.  In units of 32-bit words.
frameSize :: JsonRep -> Maybe Int
frameSize json =
  let emit = jsBufferEmit json
      frames = jsBufferFrames json
  in do aFrame <- mlast frames
        lastMem <- mlast $ flLayout aFrame
        return (lOffset lastMem + lSize lastMem)

-- Then get theem padded to the same size.
instance ToJSON SeqNoSide
instance ToJSON IntervalSide
instance ToJSON LayoutKind
instance ToJSON LayoutMember
instance ToJSON FrameLayout
instance ToJSON TargetInfo

instance FromJSON SeqNoSide
instance FromJSON IntervalSide
instance FromJSON LayoutKind
instance FromJSON LayoutMember
instance FromJSON FrameLayout
instance FromJSON TargetInfo

instance ToJSON JsonRep where
  toJSON j = object [ "abi" .= jsAbi j
                    , "tags" .= toJSON (
                        map (\(k,v) -> object [ "key" .= k, "value" .= v ]) $ jsTags j)
                    , "metadata" .= jsMetadata j
                    , "emit" .= jsBufferEmit j
                    , "target" .= jsTarget j
                    , "frames" .= jsBufferFrames j
                    ]

instance FromJSON JsonRep where
  parseJSON (Object o) = do
    abi <- o .:? "abi" .!= "not found"
    emit <- o .: "emit"
    frames <- o .:? "frames" .!= []
    tags <- o .:? "tags" .!= []
    md <- o .:? "metadata" .!= []
    tgt <- o .: "target"
    return $ JsonRep abi emit tgt frames tags md


data FileRecord = FileRecord { frFormat    :: String
                             , frDate      :: String
                             , frComment   :: String
                             , frDeltaTime :: Int    -- in seconds
                             , frCounters  :: [String]
                             , frJson      :: JsonRep }
                  deriving (Generic, Eq, Show)
instance ToJSON FileRecord
instance FromJSON FileRecord

