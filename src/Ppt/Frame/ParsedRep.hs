{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Ppt.Frame.ParsedRep where
import GHC.Generics
import Data.Aeson
import Data.Hashable
import Data.Word
import Ppt.Frame.Types
import Control.Lens hiding (element, noneOf)
{- |Parsing, Calculation, and Layout

 Ppt builds a buffer specification from its individual Frames.  It
 parses a Frame specification into a Layout that's got enough
n information for code generation and for placing into the buffer.
-}


-- |Parsed Representation
data ELanguage = ELangC | ELangCpp deriving (Generic, Eq, Show)


data ERuntime = ERuntime { erMultithread :: Bool } deriving (Generic, Eq, Show)
data ETag = Tag String String -- ^Key, Value
            deriving (Generic, Eq, Show)
data EBuffer = EBuffer { ebName :: String, ebMinElements :: Maybe Int }
             deriving (Generic, Eq, Show)

data EOption = ENamespace [String]
             | EHeaderSuffix String
             | ESourceSuffix String
             | EFilePrefix String
             | ENativeCounter Bool
             | EDebug Bool
             deriving (Generic, Eq, Show)

data EmitOptions = EmitOptions { _eBuffer ::EBuffer
                               , _eLanguage :: ELanguage
                               , _eTimeRep :: ETimeRep
                               , _eRuntime:: ERuntime
                               , _eTags :: [ETag]
                               , _eOptions :: [EOption] }
                   deriving (Generic, Eq, Show)
makeLenses ''EmitOptions

-- |Literal data members of the Frame.  These can be single,
-- standalone members /or/ a differential.  In the latter case, a
-- single differential declaration in the original specification can
-- result in multiple members in the layout.
data FrameMember = FMember { fmType :: Prim
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

-- |A higher level component of a semantic Frame.  FrameElements
-- contain literal members of the data type (FMemberElem), and
-- calculated values (FCalculatedElems).  Note that we allow
-- calculations to occur on differentials this way, which is a
-- powerful part of the language.
data FrameElement = FMemberElem FrameMember
                    -- ^Saved at runtime.
                  | FCalculatedElem { fcResultType :: Prim
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
data Frame = Frame { _frameName :: String
                   , _frameElements :: [FrameElement]
                   } deriving (Generic, Eq, Show)
makeLenses ''Frame

type LayoutKind = GenLayoutKind FrameMember
type LayoutMember = GenLayoutMember FrameMember
type FrameLayout = GenFrameLayout Frame FrameMember

memPrim :: Lens' LayoutMember Prim
memPrim = lens read write
  where read (LMember t _ _ _ _ _) = t
        write m@(LMember t _ _ _ _ _) u = m{ _lType = u }

-- Look into AESON-ing this thing, to serialize into the generated
-- source, and to shove into generated output for python
-- post-processing.
data Buffer = Buffer EmitOptions [Frame] deriving (Generic, Eq, Show)

instance Hashable Frame where
  hashWithSalt salt f = hashWithSalt salt (_frameName f)

instance ToJSON PIPrecision
instance ToJSON PFPPrecision
instance ToJSON ExpansionInfo
instance ToJSON PCounterConfig
instance ToJSON Prim
instance ToJSON ELanguage
instance ToJSON ETimeSource
instance ToJSON ETimeRep
instance ToJSON ERuntime
instance ToJSON ETag
instance ToJSON EBuffer
instance ToJSON EOption
instance ToJSON EmitOptions
instance ToJSON FrameMember
instance ToJSON FBinOperation
instance ToJSON FSeriesOperation
instance ToJSON FrameCalculation
instance ToJSON FrameElement
instance ToJSON Frame
instance ToJSON Buffer

instance FromJSON PIPrecision
instance FromJSON PFPPrecision
instance FromJSON ExpansionInfo
instance FromJSON PCounterConfig
instance FromJSON Prim
instance FromJSON ELanguage
instance FromJSON ETimeSource
instance FromJSON ETimeRep
instance FromJSON ERuntime
instance FromJSON ETag
instance FromJSON EBuffer
instance FromJSON EOption
instance FromJSON EmitOptions
instance FromJSON FrameMember
instance FromJSON FBinOperation
instance FromJSON FSeriesOperation
instance FromJSON FrameCalculation
instance FromJSON FrameElement
instance FromJSON Frame
instance FromJSON Buffer
