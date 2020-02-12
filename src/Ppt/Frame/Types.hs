{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Ppt.Frame.Types ( module Ppt.Frame.Types,
                         module Ppt.Frame.Prim) where
import Control.Lens (makePrisms, makeLenses, view, (^.))
import GHC.Generics
import Ppt.Frame.Prim


-- |Machine Layout
data SeqNoSide = FrontSeq | BackSeq deriving (Generic, Eq, Show)

data IntervalSide = IntBegin Int Int
                    -- ^Begin of Num of Num (e.g., interval start for counter 1 of 3)
                  | IntEnd Int Int
                    -- ^End of Num of Num (interval end for counter 1 of 3)
                  deriving (Generic, Eq, Show)

-- |layout kind for a decl type d.
data GenLayoutKind d = LKSeqno SeqNoSide
                       -- ^Sequence numbers.  Front and back
                     | LKTypeDescrim Int
                       -- ^Number of elements to discriminate by.
                     | LKMember d (Maybe IntervalSide)
                       -- ^An actual declared member.  It may be one of a
                       -- group.  (Current, Total), where Current < Total,
                       -- always.  For counters, we have some number of
                       -- them (3) that get generated for each 'counter'
                       -- member.  Interval counters get twice as many.
                      | LKPadding Int
                        -- ^Padding between members, or at end.  Param is
                        -- number of bytes
                     deriving (Generic, Eq, Show)

makePrisms ''GenLayoutKind

data GenLayoutMember d = LMember { _lType :: Prim
                                 -- ^The type as declared.
                                 , _lOffset :: Int
                                 , _lAlignment :: Int
                                   -- ^Typically the same as lSize, unless it's padding
                                 , _lSize :: Int
                                   -- ^Purely a function of TargetInfo and lType
                                 , _lKind :: GenLayoutKind d
                                   -- ^Representation.
                                 , _lName :: String }
                        deriving (Generic, Eq, Show)

makeLenses ''GenLayoutMember


data LayoutIOSpec = LayoutIO { _lioSize :: Int
                             , _lioBackOffset :: Int
                             } deriving (Eq, Show)

makeLenses ''LayoutIOSpec

data GenFrameLayout f d = FLayout { _flName :: String
                                  , _flFrame :: f
                                  , _flLayout :: [GenLayoutMember d]
                                  } deriving (Generic, Eq, Show)

makeLenses ''GenFrameLayout
