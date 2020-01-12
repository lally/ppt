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
import Safe
import Debug.Trace
import GHC.Generics
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM

import Ppt.Frame.Prim
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
  let sumSize = sum $ layout ^.. flLayout . folded . lSize
      lastMem = last $ layout ^. flLayout
      backOff = case lastMem of
                  (LMember _ off _ _ (LKSeqno BackSeq) _) -> off
  in LayoutIO sumSize backOff

sizeOf :: TargetInfo -> Prim -> Int
sizeOf info p =
  case (pType p) of
    PRational PPDouble _ -> tDouble info
    PRational PPFloat _ -> tFloat info
    PIntegral PPInt _ -> tInt info
    PIntegral PPByte _ -> 1
    PTime _ -> tTime info
    PCounter _ _-> tCounter info

alignOf :: TargetInfo -> Prim -> Int
alignOf info p =
  case (pType p) of
    PRational PPDouble _ -> tDouble info
    PRational PPFloat _ -> tFloat info
    PIntegral PPInt _ -> tInt info
    PIntegral PPByte _ -> 1
    PTime _ -> tTime info
    PCounter _ _-> tCounter info

-- |Indicates the amount of space in the last 'align'-sized block that 'amt' takes.
alignRemainder :: Int -> Int -> Int
alignRemainder align amt
  | align == amt = 0
  | otherwise = align - (amt `mod` align)

serializeOffsets :: TargetInfo -> [LayoutMember] -> [LayoutMember]
serializeOffsets target inmems =
  let serialize :: TargetInfo -> Int -> [LayoutMember] -> [LayoutMember]
      serialize tinfo pfx (m:mems) =
        let nextElemSize = pfx + (sizeOf tinfo (m ^. lType))
        in (m {_lOffset = pfx }):(serialize tinfo nextElemSize mems)
  in serialize target 0 inmems

-- |Determine the single-member element size of the shared memory
-- segment.  In units of 32-bit words.
frameSize :: JsonRep -> Maybe Int
frameSize json =
  let emit = jsBufferEmit json
      frames = jsBufferFrames json
  in do aFrame <- lastMay frames
        lastMem <- lastMay $ aFrame ^. flLayout
        return (lastMem ^. lOffset + lastMem ^. lSize)

instance ToJSON SeqNoSide
instance ToJSON IntervalSide
instance (ToJSON d) => ToJSON (GenLayoutKind d)
instance (ToJSON d) => ToJSON (GenLayoutMember d)
instance (ToJSON d, ToJSON f) => ToJSON (GenFrameLayout f d)
instance ToJSON TargetInfo

instance FromJSON SeqNoSide
instance FromJSON IntervalSide
instance (FromJSON d) => FromJSON (GenLayoutKind d)
instance (FromJSON d) => FromJSON (GenLayoutMember d)
instance (FromJSON d, FromJSON f) => FromJSON (GenFrameLayout d f)
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

