{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Ppt.Frame.Types where
import Control.Lens (makePrisms, makeLenses, view)
import GHC.Generics

import Ppt.Frame.ParsedRep

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

-- |Everything necessary to read values from a stored representation.
data ReadConfig = ReadConfig { rcTargetInfo :: TargetInfo
                             , rcTimeRep :: ETimeRep
                             , rcCounterConfig :: PCounterConfig
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

makePrisms ''LayoutKind

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
