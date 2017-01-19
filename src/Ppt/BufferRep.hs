module Ppt.BufferRep where

{- |Parsing and Layout

 Ppt builds a buffer specification from its individual Frames.  It
 parses a Frame specification into a Layout that's got enough
 information for code generation and for placing into the buffer.
-}


-- |Common to both Parsed Rep and Machine Layout
data Primitive = PDouble | PFloat | PInt | PTime | PCounter deriving (Eq, Show)

-- |Used in evaluation.  Some types are promotoed.
data PrimitiveValue = PVRational Double
                    | PVIntegral Int
                    | PVTime Int
                    | PVCounter [Int]
                    deriving (Eq, Show)

-- |Parsed Representation
data ELanguage = ELangC | ELangCpp deriving (Eq, Show)

data ETimeSource = ETimeClockRealtime
                 | ETimeClockRealtimeCoarse
                 | ETimeClockMonotonic
                 | ETimeClockMonotonicCoarse
                 | ETimeClockMonotonicRaw
                 | ETimeClockBoottime
                 | ETimeClockProcessCputimeId
                 | ETimeClockThreadCputimeId
                 deriving (Eq, Show)

-- |gettimeofday() vs clock_gettime()
data ETimeRep = ETimeVal | ETimeSpec ETimeSource deriving (Eq, Show)
data ERuntime = ERuntime { erMultithread :: Bool } deriving (Eq, Show)
data ETag = Tag String String deriving (Eq, Show) -- ^Key, Value
data EBuffer = EBuffer { ebName :: String, ebMinElements :: Maybe Int } deriving (Eq, Show)
data EmitOptions = Emit EBuffer ELanguage ETimeRep ERuntime [ETag] deriving (Eq, Show)

-- |Literal data members of the Frame.  These can be single,
-- standalone members /or/ a differential.  In the latter case, a
-- single differential declaration in the original specification can
-- result in multiple members in the layout.
data FrameMember = FMember { fmType :: Primitive
                           , fmName :: String
                           , fmDifferential :: Bool}
                 deriving (Eq, Show)

-- |Simple binary operation on two values.  We don't do composite
-- operations on series, just the built-in ops.
data FBinOperation = FAdd | FSub | FMul | FDiv deriving (Eq, Show)

-- |Operation on a series of values.  Should be implementable in accumulateArray
data FSeriesOperation = FSum | FMean | FVar deriving (Eq, Show)

data FrameCalculation = FValue FrameMember
                      | FBinOp FBinOperation FrameCalculation FrameCalculation
                      | FSeriesOp FSeriesOperation FrameCalculation
                      deriving (Eq, Show)

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
                  deriving (Eq, Show)

-- |Machine Layout
data LayoutMember = LMember { lOffset :: Int
                            , lType :: Primitive
                            , lVisible :: Bool
                              -- ^Invisible members are padding or
                              -- synchronization overhead.
                            , lName :: String }
                   deriving (Eq, Show)

-- |Statistics are a bit more complex.  A tree of binary opreations is
-- simple, but it gets a bit more complex on series operations.  We
-- need a digraph of data dependencies for determining the evaluation
-- order of the FrameCalculations.

--data FrameEvaluator = FEvaluate { feValues :: Map String PrimitiveValue }

data Frame = Frame String [FrameElement] deriving (Eq, Show)



