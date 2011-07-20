module StaticInstrumentation where
import qualified Data.Digest.MD5 as MD5
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
-- Datatype declarations used by a few other modules


{- A frame has a specification and an implementation.  The
  specification is the original parsed form from the input .spec file.
  The implementation is the final, optimized, and aligned in-memory
  format with the sequence number added.  -}

data FrameType = FDouble 
               | FFloat 
               | FInt
               | FTime
                 deriving Show

data FrameElement = FrameElement FrameType String
                    deriving Show

data EmissionSpec = LangC 
                  | LangCpp
                    deriving Show

data Frame = Frame String [FrameElement]
             deriving Show

data FullSpecification = Spec EmissionSpec [Frame]
                         deriving Show

specHash :: FullSpecification -> [Word8]
specHash spec = MD5.hash (map c2w (show spec))

data ImplMemberType = IMDouble
                    | IMFloat
                    | IMInt
                    | IMTime -- struct timeval.
                    | IMSeqno -- added sequence number.
                    | IMPad Int -- padding, with byte count
                    deriving Show

data ImplMember = ImplMember (Maybe FrameElement) ImplMemberType
                deriving Show

data FullImplementation = Impl EmissionSpec String [ImplMember]
                        deriving Show