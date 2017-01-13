module Ppt.StaticInstrumentation where
import qualified Data.Digest.Pure.MD5 as MD5
-- import Data.ByteString ()
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (unpack)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Data.Binary (encode)
-- Datatype declarations used by a few other modules

{-
 Desired additions to the specification language:

 - An 'interval' type.  Which is just two FTimes, but refer to a
   struct that has a 'begin', 'end' time range 
 - a 'calc' declaration: (in the frame)
   'calc interval duration = end - start'
   A calculated value from the frame.
   Auto calculated when exported.
 - We may also have a 'statistic' which is outside of the frame declarations,
   Operated on the series.  But that's based on the entire buffer contents.
   So I'll need a syntax for a simple calculation algebra (needed for 'calc') I guess,
   and built-in series.
   - Can be live-reported in the listener.
   - So each member will have some basic stats available to use in the expression
     language of the statistic.
   - And similarly, each member will be available in singular form in the expression
     language of the calculated values.
   - Operators Needed: +-/* (runtime support can be in the either monad for div-zeros)
   - Streams needed: depends on data type.
       Intervals: min, max, sum, mean
       Values: min, max, sum, mean
       Times: min, max, mean
-}


{- A frame has a specification and an implementation.  The
  specification is the original parsed form from the input .spec file.
  The implementation is the final, optimized, and aligned in-memory
  format with the sequence number added.  -}

-- |A primitive datatype
data FrameType = FDouble
               | FFloat
               | FInt
               | FTime
               | FCounter
                 deriving Show

-- |A single variable in a frame declaration
data FrameElement = SingleElement FrameType String -- ^A single value to collect.
                  | DifferentialElement FrameType String
                    -- ^ A pair of values to collect, in a pairwise
                    -- sub-structure, named 'start' and 'finish'
                    deriving Show

data EmissionSpec = LangC
                  | LangCpp
                    deriving Show

{- A buffer is a unified container of all frames within it.
 Ultimately we'd like to allow automatic calculation of the write rate
 and the read rate, to feed the initial reading process's sleep. 
-}

-- Buffer(name of buffer, size, rate (in Hz))
data Buffer = Buffer String (Maybe Int) (Maybe Int)
              deriving Show

data Frame = Frame String [FrameElement]
             deriving Show

data FullSpecification = Spec EmissionSpec Buffer [Frame]
                         deriving Show

specHash :: FullSpecification -> [Word8]
specHash spec =
  let digest :: MD5.MD5Digest
      digest = MD5.hash (pack $ show spec)
  in unpack $ encode digest

data SeqnoLoc = SFront | SBack
              deriving Show

-- |Underlying numeric types of frame members.
data ImplMemberType = IMDouble -- ^double
                    | IMFloat -- ^float
                    | IMCounter -- ^uint64_t
                    | IMInt -- ^int
                    | IMTime -- ^struct timeval or timespec.
                    | IMSeqno SeqnoLoc -- ^Added sequence number
                    | IMDescriminator -- ^Type descriminator
                    | IMPair ImplMemberType -- ^Pair of members for a differential.
                    | IMPad Int -- ^Padding, with byte count
                    deriving Show

data ImplMember = ImplMember (Maybe FrameElement) ImplMemberType
                deriving Show

data ImplFrame = ImplFrame String [ImplMember]
                 deriving Show

data FullImplementation = Impl EmissionSpec String [ImplFrame]
                        deriving Show
