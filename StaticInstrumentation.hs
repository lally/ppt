module StaticInstrumentation where
import qualified Data.Digest.MD5 as MD5
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
-- Datatype declarations used by a few other modules

data FrameType = FDouble 
               | FFloat 
               | FInt
                 deriving Show

data FrameElement = FrameElement FrameType String
                    deriving Show

data FrameSpecification = FrameSpecification String [FrameElement]
                          deriving Show

data EmissionSpec = LangC 
                  | LangCpp
                    deriving Show
data FullSpecification = Spec EmissionSpec [FrameSpecification]
                         deriving Show

specHash :: FrameSpecification -> [Word8]
specHash spec = MD5.hash (map c2w (show spec))