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

--
-- These will have to be generated from a configure script, probably from
-- a platform-specific generation script.
--
-- Hmm, I could put these into a tuple type that I take as an argument,
-- similar to the CPS-style I'll have to do for my structure types.
type LibCOpenType = Function (Ptr Word8 -> Int32 -> IO Int32)
type LibCCloseType = Function (Int32 -> IO Int32)
type LibCWriteType = Function (Int32 -> Ptr Word8 -> Int64 -> IO Int64)

buildReaderFun :: String -> CodeGenModule (MainFunction)
buildReaderFun nm  = do
  printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Word32)
  puts <- newNamedFunction ExternalLinkage "puts" :: 
    TFunction (Ptr Word8 -> IO Word32)
  open <- newNamedFunction ExternalLinkage "open" :: CodeGenModule(LibCOpenType)
  close <- newNamedFunction ExternalLinkage "close" :: CodeGenModule(LibCCloseType)
  write <- newNamedFunction ExternalLinkage "write" :: CodeGenModule(LibCWriteType)
  pattern <- createStringNul "Loading file %s\n" 
  
  
  let callPuts format = (
        createNamedFunction ExternalLinkage "main" $ 
        \ argc argv -> do
          exit <- newBasicBlock
          cont <- newBasicBlock

          --
          -- First, verify that we have sufficient arguments
          sufficient_args <- icmp IntSGE argc (2::Int32)
          condBr sufficient_args cont exit
          
          defineBasicBlock exit
          ret (1::Int32)

          defineBasicBlock cont
          p_arg1 <- getElementPtr (argv ::Value (Ptr (Ptr Word8))) (1 :: Int32, ())
          arg1 <- load p_arg1
          tmp  <- getElementPtr arg1 (0 :: Int32, ()) -- ptr to first char of argv[1]
          pattern_p <- getElementPtr (pattern :: Global (Array D16 Word8)) (0 :: Word32, (0 :: Word32, ()))

          let printf_s = castVarArgs printf :: Function (Ptr Word8 -> Ptr Word8 -> IO Word32)
          _ <- call  printf_s pattern_p tmp
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
