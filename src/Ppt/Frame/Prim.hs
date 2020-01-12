{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Ppt.Frame.Prim where
-- import Ppt.Frame.Types
import Data.Word
import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Vector.Storable as V
import Control.Lens

--
-- Type Parameters
data PCounterConfig = PPCNone
                    | PPIntelCounter String String String
                    -- ^Three counter names (libpfm4 syntax)
                    deriving (Generic, Eq, Show)

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

data PFPPrecision = PPDouble | PPFloat deriving (Generic, Eq, Show)
data PIPrecision = PPInt | PPByte deriving (Generic, Eq, Show)

-- |Unified primitive type.  Use with 'Nothing' for a pure type value.
data Prim = PRational PFPPrecision (Maybe Double)
          | PIntegral PIPrecision (Maybe  Int)
          | PTime           (Maybe (ETimeRep, Int, Int))
          | PCounter        (Maybe Int) (Maybe (PCounterConfig, Word64))
            -- ^index in config, config and values it's filling in.
          deriving (Generic, Eq, Show)

makePrisms ''Prim

-- |Strips a 'Prim' of its value for type equality check.
pType :: Prim -> Prim
pType (PRational p _) = PRational p Nothing
pType (PIntegral p _) = PIntegral p Nothing
pType (PTime _)       = PTime Nothing
pType (PCounter _ _)  = PCounter Nothing Nothing

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
                             , tCounterCount :: Int
                             } deriving (Generic, Eq, Show)

-- |Everything necessary to read values from a stored representation.
data ReadConfig = ReadConfig { rcTargetInfo :: TargetInfo
                             , rcTimeRep :: ETimeRep
                             , rcCounterConfig :: PCounterConfig
                             } deriving (Generic, Eq, Show)

-- |Type to read, target of writer, base vector containing value,
-- offset from basis containing this member.  Is this a good idea?
-- It's not very lens like.  Should it return a lens?
readValue :: Prim -> ReadConfig -> V.Vector Word8 -> Int -> IO Prim
readValue pt rconf vec off =
  let time = rcTimeRep rconf
      counter = rcCounterConfig rconf
      pullPointer :: Ptr Word8 -> IO Prim
      pullPointer ptr = do
        let ptrAdded = plusPtr ptr off
        case pt of
          PRational PPDouble _ ->
            do value <- peekElemOff (castPtr ptrAdded :: Ptr Double) 0
               return $ PRational PPDouble (Just value)
          PRational PPFloat _ ->
            do value <- peekElemOff (castPtr ptrAdded :: Ptr Float) 0
               return $ PRational PPFloat (Just $ unsafeCoerce value)
          PIntegral PPByte _ ->
            do val <- peekElemOff (castPtr ptrAdded :: Ptr Word8) 0
               return $ PIntegral PPByte (Just $ fromIntegral val)
          PIntegral PPInt _ ->
            do value <- peekElemOff (castPtr ptrAdded :: Ptr Word32)  0
               return $ PIntegral PPInt  (Just $ fromIntegral value)
          PTime _ ->
            do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
               low <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 1
               return $ PTime (Just (time, fromIntegral high, fromIntegral low))
          PCounter (Just i) _ ->
            case counter of
              PPIntelCounter _ _ _ ->
                do val <- peekElemOff (castPtr ptrAdded :: Ptr Word64) i
                   return $ PCounter (Just i) (Just (counter, val))
              PPCNone -> return $ PCounter (Just 0) (Just (PPCNone, 0))
          _ -> fail ("Invalid value type: " ++ show pt)
  in V.unsafeWith vec pullPointer

