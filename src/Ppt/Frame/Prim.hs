{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Ppt.Frame.Prim (PrimType, PCounterConfig, PrimValue, valueType, readValue) where

data PrimType =  PDouble
               | PFloat
               | PInt
               | PTime ETimeRep  -- ^The format stored.
               | PCounter PCounterConfig  -- ^The counter being stored.
               | PByte
               deriving (Generic, Eq, Show)

data PCounterConfig = PCNone
                    | PIntelCounter String String String
                    -- ^Three counter names (libpfm4 syntax)
                    deriving (Generic, Eq, Show)

-- |Common to both Parsed Rep and Machine Layout.  Presumed x86_64 type sizes.
-- |Used in evaluation.  Some types are promoted.  PrimType paired with a value.
data PrimValue = PVRational Double
               | PVIntegral Int
               | PVTime ETimeRep Int Int
               | PVCounter PCounterConfig [Word64]
                 -- ^values and config it's filling in.
               deriving (Generic, Eq, Show)

valueType :: PrimValue -> PrimType
valueType (PVRational _) = PDouble
valueType (PVIntegral _) = PInt
valueType (PVTime tr _ _) = PTime tr
valueType (PVCounter cfg _) = PCounter cfg

-- |Type to read, target of writer, base vector containing value,
-- offset from basis containing this member.  Is this a good idea?
-- It's not very lens like.  Should it return a lens?
readValue :: PrimType -> ReadConfig -> V.Vector Word8 -> Int -> PrimValue
readValue pt rconf vec off =
  V.unsafeWith vec $ \ptr -> do
    let ptrAdded = plusPtr ptr off
        time = rcTimeRep rconf
    case pt of
         PDouble -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Double) 0
                       return $ PVRational value
         PFloat -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Float) 0
                      return $ PVRational $ float2Double value
         PInt -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Word32)  0
                    return $ PVIntegral $ fromIntegral value
         PTime (ETimeSpec _) -> do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                                   low <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 1
                                   return (
                                     PVTime time (fromIntegral high) (fromIntegral low))
         PTime ETimeVal-> do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                             return $ PVTime time (fromIntegral high) 0
         -- WARN(lally): Make sure these are always actually laid out sequentially
         PCounter cnf@(PIntelCounter _ _ _) ->
           do val0 <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
              val1 <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 1
              val2 <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 2
              return $ PVCounter cnf [val0, val1, val2]
         PCounter PCNone -> return $ PVCounter 0 0 PCNone
         PByte -> do val <- peekElemOff (castPtr ptrAdded :: Ptr Word8) 0
                     return $ PVIntegral (fromIntegral val)

