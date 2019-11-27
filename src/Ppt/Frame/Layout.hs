{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ppt.Frame.Layout where
import Control.Applicative
import Control.Exception
import Control.Lens (makePrisms, makeLenses, view, folded, (^.), (^..) )
import Control.Monad
import Data.Aeson
import Data.Either
import Data.Typeable
import Debug.Trace
import GHC.Generics
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM

import Ppt.Frame.Types
import Ppt.Frame.ParsedRep

data LayoutException = InvalidInputException Frame String
                     | InternalFailureException String
    deriving (Show, Typeable)

instance Exception LayoutException


data JsonRep = JsonRep { jsAbi :: String
                       , jsBufferEmit :: EmitOptions
                       , jsTarget :: TargetInfo
                       , jsBufferFrames :: [FrameLayout]
                       , jsTags :: [(String, String)]
                       , jsMetadata :: [String]
                       } deriving (Generic, Eq, Show)

layoutSpec :: FrameLayout -> LayoutIOSpec
layoutSpec layout =
  let sumSize = sum $ layout ^.. flLayout . folded . lSize -- map (view (flLayout . lSize)) layout
      lastMem = last $ layout ^. flLayout
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
        let nextElemSize = pfx + (sizeOf tinfo (m ^. lType))
        in (m {_lOffset = pfx }):(serialize tinfo nextElemSize mems)
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
        lastMem <- mlast $ aFrame ^. flLayout
        return (lastMem ^. lOffset + lastMem ^. lSize)

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

