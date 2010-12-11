{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification #-}
module Listener (initialize, generateReader) where
import StaticInstrumentation
import LLVM.Core
--import LLVM.Core.FFI
--import LLVM.Core.Util as CU
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
import Data.Int
import Data.Word
--
-- A wrapper around LLVM initialization, so that we don't have
-- to force LLVM deps outside of this module.
initialize :: IO ()
initialize = initializeNativeTarget

determineStruct :: FrameSpecification -> TypeDesc
determineStruct (FrameSpecification nm elems) =
  TDStruct (map llvmType $ map pullType elems) False
  where
    llvmType FDouble = TDDouble
    llvmType FFloat = TDFloat
    llvmType FInt = TDInt True 32
    pullType (FrameElement t _) = t
      
buildReaderFormat :: FrameSpecification -> String
buildReaderFormat (FrameSpecification nm specs) =
  join " " $ map formatString specs
  where
    join delim (x:xs) = x ++ (concatMap (\y -> delim ++ y) xs)
    formatString (FrameElement FDouble _) = "%f"
    formatString (FrameElement FFloat _) = "%f"
    formatString (FrameElement FInt _) = "%d"
    
            
type MainFunction = Function (Int32 -> Ptr (Ptr Word8) -> IO Int32)

{- Access to getIxList and getArg aren't allowed from here.  I'd
have to modify the LLVM distro.

I guess I'll have to look at some pointer math?  Or, an array of
arrays? -}

buildReaderFun :: String -> CodeGenModule (MainFunction)
buildReaderFun nm  = do
  puts <- newNamedFunction ExternalLinkage "puts" :: 
    TFunction (Ptr Word8 -> IO Word32)
  let callPuts format = (
        createNamedFunction ExternalLinkage "main" $ 
        \ argc argv -> do
--          tmp <- getElementPtr (argv ::Value (Ptr (Ptr Word8)))
--                 (0 :: Int32,  (1 :: Int32, ()))
          tmp0 <- getElementPtr (argv ::Value (Ptr (Ptr Word8))) (0 :: Int32, ())
          tmp1 <- load tmp0
          tmp  <- getElementPtr tmp1 (1 :: Int32, ())

          _ <- call puts tmp
          ret (0 :: Int32)) :: CodeGenModule (MainFunction)

  withStringNul nm callPuts 
  
     --   tmp <- getElementPtr format (0 :: Word32,(0 :: Word32, ()))
--  return func
--        frameInst <- alloca :: TValue (
--   printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (VarArgs (Ptr Word8))
--   where
--        frameT = determineStruct spec 


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
