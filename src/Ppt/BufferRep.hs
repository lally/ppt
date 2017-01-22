{-# LANGUAGE DeriveGeneric #-}

module Ppt.BufferRep where
import GHC.Generics
import Data.Aeson
{- |Parsing and Layout

 Ppt builds a buffer specification from its individual Frames.  It
 parses a Frame specification into a Layout that's got enough
 information for code generation and for placing into the buffer.
-}


-- |Common to both Parsed Rep and Machine Layout
data Primitive = PDouble | PFloat | PInt | PTime | PCounter deriving (Generic, Eq, Show)

-- |Used in evaluation.  Some types are promotoed.
data PrimitiveValue = PVRational Double
                    | PVIntegral Int
                    | PVTime Int
                    | PVCounter [Int]
                    deriving (Generic, Eq, Show)

-- |Parsed Representation
data ELanguage = ELangC | ELangCpp deriving (Generic, Eq, Show)

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
data EmitOptions = Emit EBuffer ELanguage ETimeRep ERuntime [ETag]
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

{- Operations
   ----------

   This isn't trivial to figure out.  We have a few types:
   - A singular stat based on a single frame's data.
   - A series stat based on all frames' data.
   - A gap/delta based on differences between frames' data

   So, what's allowable?  There's a high-level type system here.
   Unerlying it all, I want a single-pass algorithm for calculating
   these.

   - I can base a stat on just a single frame.
   - I can base a stat on all frames, creating a series stat.
   - I can base a stat on applying a single-frame stat on all frames, creating a series stat.

   What requires amore than one pass?
   - x / var(x) : A series based on a series.  *That's what I can't do.*

   I can do a series based on a singular stat.
   I can do a singular stat based on a series.

   So, the type system has a primitive value and a series-or-singular
   indication.  When composing, some trees aren't allowed.  I also
   have a purely max-of-children value of minimum frames to get a
   value.

   There are two evaluation scopes:
   - The current buffer (used by series scope)
   - The current buffer + series values (used at top level)

   I bucket the evaluation order that way.
   Example calculations:
   - (func_time / work) - mean( func_time / work )
     ^^ current frame     ^^^^^^^^ mean of all frames

   When displaying values, I can just show (insufficient data) on the live display.
   When saving data, I have two options:
   - Wait until I have enough data for a full row, and eat data to get there
   - Save only raw data
   - Use invalid values for the cells with insufficient data to calculate

   Configuration
   -------------
   Use TOML.
   Accept key-path=value options on a command line flag.
   Accept alternative configuration files as a flag.
   Accept ignore-configuration as a flag.

   Accept /new statistics/ on a flag.
-}

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

-- |Machine Layout
data LayoutMember = LMember { lOffset :: Int
                            , lType :: Primitive
                            , lVisible :: Bool
                              -- ^Invisible members are padding or
                              -- synchronization overhead.
                            , lName :: String }
                   deriving (Generic, Eq, Show)

-- |Statistics are a bit more complex.  A tree of binary opreations is
-- simple, but it gets a bit more complex on series operations.  We
-- need a digraph of data dependencies for determining the evaluation
-- order of the FrameCalculations.

-- It's not a tree.  It's a list that's been created by traversing the
-- data-dependency tree.  So we need a Plan - a list of evaluations
-- where they've been resolved, depenency wise.  This also lets me
-- validate any circular references.

-- Compilation
--  - Get the expressions for the entire frame at once.
--  - Use a blackhole system for determining the dependencies (and validating)
--  - When traversing the graph, build a list of things to evaluate upon exit of traversal from a node.
--    - Look for something that matches (Eq!) in the list already, and use that.  (CSE built-in)
--    - Increment a refcount for it.
--    - Mark reportable ones as such - to indicate which elements
--      aren't reportable, and can be folded into their dependents.

--  - After the full list, (LATER ON) look for nonreportable things
--    with refcount 1 to fold into their dependents.

--  - Each node puts out evaluator templates that wrap the accumulateArray slots or the pure function
--    - We generally want to have one array for all stats.  So each
--      series element just allocates slots in the array, and then
--      generates expressions that add into the array.
 

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
instance ToJSON LayoutMember
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
instance FromJSON LayoutMember
instance FromJSON Frame
instance FromJSON Buffer           

