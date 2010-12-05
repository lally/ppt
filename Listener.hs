module Listener where
import StaticInstrumentation
import LLVM.Core
import Data.Word
--
-- A wrapper around LLVM initialization, so that we don't have
-- to force LLVM deps outside of this module.
initialize :: IO ()
initialize = initializeNativeTarget

buildReaderFun :: String -> CodeGenModule (Function (IO ()))
buildReaderFun nm = do
  puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
  greetz <- createStringNul nm
  func <- createFunction ExternalLinkage $ do
    tmp <- getElementPtr0 greetz (0 :: Word32, ())
    call puts  tmp -- Throw away return value.
    ret ()
  return func


-- Format spec -> filename 12
generateReader :: FullSpecification -> String -> IO ()
generateReader (Spec kid specs) filename = do
  let pullName (FrameSpecification nm _) = nm 
  let names = map pullName specs 
  mod <- newNamedModule "TestModule"
  defineModule mod (buildReaderFun (head names))
  writeBitcodeToFile filename mod
  
-- TODO: put out a minimal .lli file, then make it more like my ideal listener
-- then run the optimizer.
