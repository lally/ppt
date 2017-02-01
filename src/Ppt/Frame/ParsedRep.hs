{-# LANGUAGE DeriveGeneric #-}

module Ppt.Frame.ParsedRep where
import GHC.Generics
import Data.Aeson
{- |Parsing, Calculation, and Layout

 Ppt builds a buffer specification from its individual Frames.  It
 parses a Frame specification into a Layout that's got enough
 information for code generation and for placing into the buffer.
-}

-- |Common to both Parsed Rep and Machine Layout.  Presumed x86_64 type sizes.
data Primitive = PDouble | PFloat | PInt | PTime
               | PCounter | PByte deriving (Generic, Eq, Show)

-- |Used in evaluation.  Some types are promoted.  PrimType paired with a value.
data PrimitiveValue = PVRational Double
                    | PVIntegral Int
                    | PVTime Int
                    | PVCounter [Int]
                    deriving (Generic, Eq, Show)

-- |Parsed Representation
data ELanguage = ELangC | ELangCpp deriving (Generic, Eq, Show)

-- |For ETimeSPec, which kind of clock to ask from clock_gettime()
data ETimeSource = ETimeClockRealtime
                 | ETimeClockRealtimeCoarse
                 | ETimeClockMonotonic
                 | ETimeClockMonotonicCoarse
                 | ETimeClockMonotonicRaw
                 | ETimeClockBoottime
                 | ETimeClockProcessCputimeId
                 | ETimeClockThreadCputimeId
                 deriving (Generic, Eq, Show)

-- |gettimeofday() vs clock_gettime()
data ETimeRep = ETimeVal | ETimeSpec ETimeSource deriving (Generic, Eq, Show)
data ERuntime = ERuntime { erMultithread :: Bool } deriving (Generic, Eq, Show)
data ETag = Tag String String deriving (Generic, Eq, Show) -- ^Key, Value
data EBuffer = EBuffer { ebName :: String, ebMinElements :: Maybe Int }
             deriving (Generic, Eq, Show)
data EmitOptions = EmitOptions { eBuffer ::EBuffer
                               , eLanguage :: ELanguage
                               , eTimeRep :: ETimeRep
                               , eRuntime:: ERuntime
                               , eTags :: [ETag] }
                   deriving (Generic, Eq, Show)

-- |Literal data members of the Frame.  These can be single,
-- standalone members /or/ a differential.  In the latter case, a
-- single differential declaration in the original specification can
-- result in multiple members in the layout.
data FrameMember = FMember { fmType :: Primitive
                           , fmName :: String
                           , fmDifferential :: Bool}
                 deriving (Generic, Eq, Show)

-- |Simple binary operation on two values.  We don't do composite
-- operations on series, just the built-in ops.
data FBinOperation = FAdd | FSub | FMul | FDiv deriving (Generic, Eq, Show)


-- |Operation on a series of values.  Should be implementable in accumulateArray
data FSeriesOperation = FSum | FMean | FVar deriving (Generic, Eq, Show)

data FrameCalculation = FValue FrameMember
                      | FBinOp FBinOperation FrameCalculation FrameCalculation
                      | FSeriesOp FSeriesOperation FrameCalculation
                      deriving (Generic, Eq, Show)

-- |Note that we allow calculations to occur on differentials this
-- way, which is a powerful part of the language.
data FrameElement = FMemberElem FrameMember
                    -- ^Saved at runtime.
                  | FCalculatedElem { fcResultType :: Primitive
                                    , fcOperation :: FrameCalculation
                                    , fcFieldName :: String
                                    , fcLiveReport :: Bool
                                    , fcInExport :: Bool}
                    -- ^Calculated field, with the resulting type
                    -- stored here.  `fcLiveReport` indictes whether
                    -- it should be repored in 'monitor' mode.
                    -- `fcInExport` indicates whether it should be in
                    -- converted output.
                  deriving (Generic, Eq, Show)

-- Statistics are a bit more complex.  A tree of binary opreations is
-- simple, but it gets a bit more complex on series operations.  We
-- need a digraph of data dependencies for determining the evaluation
-- order of the FrameCalculations.

-- It's not a tree.  It's a list that's been created by traversing the
-- data-dependency tree.  So we need a Plan - a list of evaluations
-- where they've been resolved, depenency wise.  This also lets me
-- validate any circular references.

--data FrameEvaluator = FEvaluate { feValues :: Map String PrimitiveValue }
data Frame = Frame String [FrameElement] deriving (Generic, Eq, Show)

-- Look into AESON-ing this thing, to serialize into the generated
-- source, and to shove into generated output for python
-- post-processing.
data Buffer = Buffer EmitOptions [Frame] deriving (Generic, Eq, Show)


instance ToJSON Primitive
instance ToJSON PrimitiveValue
instance ToJSON ELanguage
instance ToJSON ETimeSource
instance ToJSON ETimeRep
instance ToJSON ERuntime
instance ToJSON ETag
instance ToJSON EBuffer
instance ToJSON EmitOptions
instance ToJSON FrameMember
instance ToJSON FBinOperation
instance ToJSON FSeriesOperation
instance ToJSON FrameCalculation
instance ToJSON FrameElement
instance ToJSON Frame
instance ToJSON Buffer

instance FromJSON Primitive
instance FromJSON PrimitiveValue
instance FromJSON ELanguage
instance FromJSON ETimeSource
instance FromJSON ETimeRep
instance FromJSON ERuntime
instance FromJSON ETag
instance FromJSON EBuffer
instance FromJSON EmitOptions
instance FromJSON FrameMember
instance FromJSON FBinOperation
instance FromJSON FSeriesOperation
instance FromJSON FrameCalculation
instance FromJSON FrameElement
instance FromJSON Frame
instance FromJSON Buffer
