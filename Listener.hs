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
--
-- We're glancing over the fact that we don't have FILE defined.  Just Ptr Word8
type FILE = Word8 -- ok, this is easier to read
type LibCFOpenType = Function (Ptr Word8 -> Ptr Word8 -> IO (Ptr FILE))
type LibCFCloseType = Function (Ptr FILE -> IO Int32)
type LibCFWriteType = Function (Ptr Word8 -> Int32 -> Int32 ->Ptr FILE -> IO Int32)
type LibCSetBufType = Function (Ptr FILE -> Ptr Word8 -> Int32 -> IO ())
type LibCFPrintf = Function (Ptr Word8 -> Ptr Word8 -> VarArgs Word32)
type LibCatoi = Function (Ptr Word8 -> IO Int32)
type LibCperror = Function (Ptr Word8 -> IO ())

  {- program:
     | Block           | Task                                 |
     |-----------------+--------------------------------------|
     | default         | check argc                           |
     | exit            | exit                                 |
     | args            | fopen the output                     |
     |                 | malloc & setbuf it                   |
     |                 | load shmid, shmsz                    |
     |                 | attach                               |
     |                 | init startp, endp, curp, last_seqno  |
     |                 | init read_cnt = 0                    |
     |                 | init delay = 100.0 (ms)              |
     |                 | calc low = size/8, hi = size-size/8  |
     | loop_head       | load curp, seqno                     |
     |                 | compare seqno                        |
     |                 | goto either writebuf,determine_sleep |
     | writebuf        | fprintf the thing                    |
     |                 | inc read_cnt                         |
     |                 | jump back to loop_head               |
     | determine_sleep | if count < low or count > hi         |
     |                 | > delay = size/2 * delay / count     |
     |                 | > delay = max(delay, 100)            |
     |                 | > delay = min(delay, 2000)           |
     | do_sleep        | read_cnt = 0                         |
     |                 | sleep (delay)                        |

    note, a second perror() version may be useful, with a phi'd string
arg.  as we're always flushing, we can die easily; no need for a
sighup/sigint handler.  perhaps a third 'nsamples' arg?


This proc can be stupid-small, and can be nice'd *far* below the one
it runs against.


   -}
             
getArgv :: Word32 -> Value (Ptr (Ptr Word8)) -> CodeGenFunction r (Value (Ptr Word8))
getArgv n args = do
  pp_arg <- getElementPtr args (n, ())
  p_arg <- load pp_arg
  arg <- getElementPtr p_arg (0::Word32, ())
  return arg
  
buildReaderFun :: String -> CodeGenModule (MainFunction)
buildReaderFun nm  = do
  printf <- (newNamedFunction ExternalLinkage "printf" 
             :: TFunction (Ptr Word8 -> VarArgs Word32))
            
  puts <- (newNamedFunction ExternalLinkage "puts" 
           :: TFunction (Ptr Word8 -> IO Word32))

  fprintf <- (newNamedFunction ExternalLinkage "fprintf" 
              :: CodeGenModule(LibCFPrintf))
             
  fopen <- (newNamedFunction ExternalLinkage "fopen" 
            :: CodeGenModule(LibCFOpenType))
           
  fclose <- (newNamedFunction ExternalLinkage "fclose" 
             :: CodeGenModule(LibCFCloseType))
            
  fwrite <- (newNamedFunction ExternalLinkage "fwrite" 
             :: CodeGenModule(LibCFWriteType))
            
  setbuf <- (newNamedFunction ExternalLinkage "setbuf" 
             :: CodeGenModule(LibCSetBufType))
            
  atoi <- (newNamedFunction ExternalLinkage "atoi"
           :: CodeGenModule(LibCatoi))
          
  perror <- (newNamedFunction ExternalLinkage "perror"
             :: CodeGenModule(LibCperror))
            
  pattern <- createStringNul "Loading file %s\n" 
  fopen_args <- createStringNul "a"
  
             
  let callPuts format = (
        createNamedFunction ExternalLinkage "main" $ 
        \ argc argv -> do
          exit <- newBasicBlock
          load_args <- newBasicBlock
          p_exit <- newBasicBlock
          loop_head <- newBasicBlock
          writebuf <- newBasicBlock
          determine_sleep <- newBasicBlock
          do_sleep <- newBasicBlock
          try_shmat <- newBasicBlock
          try_fopen <- newBasicBlock
          

          --
          -- First, verify that we have sufficient arguments
          sufficient_args <- icmp IntSGE argc (2::Int32)
          condBr sufficient_args load_args exit
          
          --
          -- exit: just exit(1)
          defineBasicBlock exit
          ret (1::Int32)

          --
          -- load_args: call atoi on the shared memory args
          defineBasicBlock load_args
          s_shmid <- getArgv 2 argv
          s_shmsz <- getArgv 3 argv
          shmid <- call atoi s_shmid
          shmsz <- call atoi s_shmsz
          shmsz' <- icmp IntSGT shmsz (0 :: Int32)
          condBr shmsz' try_shmat exit -- validate the size as > 0.
          
          --
          -- try_fopen: try to fopen the output file.
          defineBasicBlock try_fopen
          
          pp_arg1 <- getElementPtr (argv ::Value (Ptr (Ptr Word8))) (1 :: Int32, ())
          p_arg1 <- load pp_arg1
          -- arg1 = ptr to first char of argv[1]
          arg1  <- getElementPtr p_arg1 (0 :: Int32, ()) 
          fopen_arg <- getElementPtr0 (fopen_args :: Global (Array D2 Word8)) (
            0 :: Word32, ())
                       
          outfile <- call fopen arg1 fopen_arg
          nullptr_file <- (inttoptr (valueOf 0)) 
          outfile_ok <- icmp IntNE outfile (nullptr_file :: Value (Ptr FILE))
          condBr outfile_ok try_shmat p_exit
          
          pattern_p <- getElementPtr (pattern :: Global (Array D16 Word8)) (0 :: Word32, (0 :: Word32, ()))
          let printf_s = castVarArgs printf :: Function (Ptr Word8 -> Ptr Word8 -> IO Word32)
          _ <- call  printf_s pattern_p fopen_arg
          ret (0 :: Int32)

          {- exit with a call to perror().  Our string is phi:
           | Block     | Var  |
           |-----------+------|
           | try_fopen | arg1 |
           -}
          defineBasicBlock p_exit
          err_str <- phi [(arg1, try_fopen)]
          call perror err_str
          ret (1 :: Int32)
          
          ) :: CodeGenModule (MainFunction)
  
  withStringNul nm callPuts 
  
-- Format spec -> filename 12
generateReader :: FullSpecification -> String -> IO ()
generateReader (Spec kid specs) filename = do
  let pullName (FrameSpecification nm _) = nm 
  let names = map pullName specs 
  mod <- newNamedModule (head names)
  defineModule mod (buildReaderFun (head names))
  writeBitcodeToFile filename mod
  
-- TODO: put out a minimal .lli file, then make it more like my ideal listener
-- then run the optimizer.
