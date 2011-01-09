{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification #-}
module Listener (initialize, generateReader) where
import StaticInstrumentation
import LLVM.Core
import LLVM.Util.Optimize
import Configuration
import SIParser (implement)
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
import Data.Int
import Data.Word

--
-- A wrapper around LLVM initialization, so that we don't have
-- to force LLVM deps outside of this module.
initialize :: IO ()
initialize = initializeNativeTarget
      
            
type MainFunction = Function (Int32 -> Ptr (Ptr Word8) -> IO Int32)

-- These will have to be generated from a configure script, probably from
-- a platform-specific generation script.
--
-- We're glancing over the fact that we don't have FILE defined.  Just
-- Ptr Word8.  Well, we're glancing over the fact that I'm defining
-- these to my convenience.  E.g. shmat's returning a uint8* instead
-- of void*.
type FILE = Word8 -- ok, this is easier to read
type LibCFOpenType = Function (Ptr Word8 -> Ptr Word8 -> IO (Ptr FILE))
type LibCFCloseType = Function (Ptr FILE -> IO Int32)
type LibCFWriteType = Function (Ptr Word8 -> Int32 -> Int32 ->Ptr FILE -> IO Int32)
type LibCSetBufType = Function (Ptr FILE -> Ptr Word8 -> IO ())
type LibCFPrintf = Function (Ptr Word8 -> Ptr Word8 -> VarArgs Word32)
type LibCFFlush = Function (Ptr Word8 -> IO Int32)
type LibCatoi = Function (Ptr Word8 -> IO Int32)
type LibCperror = Function (Ptr Word8 -> IO ())
type LibCshmat = Function (Int32 -> Ptr Word8 -> Int32 -> IO (Ptr Word8))
type LibCusleep = Function (Int32 -> IO ())

--
-- Convenience methods
--
getArgv :: Word32 -> Value (Ptr (Ptr Word8)) -> CodeGenFunction r (Value (Ptr Word8))
getArgv n args = do
  pp_arg <- getElementPtr args (n, ())
  p_arg <- load pp_arg
  arg <- getElementPtr p_arg (0::Word32, ())
  return arg

s_min :: (IsIntegerOrPointer a, CmpRet a d1, IsFirstClass a) => 
      Value a -> Value a -> CodeGenFunction r (Value a)
s_min a b = do
    t1 <- icmp IntSLE a b
    t2 <- select t1 a b
    return t2

s_max :: (IsIntegerOrPointer a, CmpRet a d1, IsFirstClass a) => 
      Value a -> Value a -> CodeGenFunction r (Value a)
s_max a b = do
    t1 <- icmp IntSGE a b
    t2 <- select t1 a b
    return t2


buildReaderFun :: String -> Int32 -> CodeGenModule (MainFunction)
buildReaderFun nm skip = do
  -- NOTE: should I make these unlocked stdio calls?               
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

  fflush <- (newNamedFunction ExternalLinkage "fflush"
             :: CodeGenModule(LibCFFlush))
             
  setbuf <- (newNamedFunction ExternalLinkage "setbuf" 
             :: CodeGenModule(LibCSetBufType))
            
  atoi <- (newNamedFunction ExternalLinkage "atoi"
           :: CodeGenModule(LibCatoi))
          
  perror <- (newNamedFunction ExternalLinkage "perror"
             :: CodeGenModule(LibCperror))

  shmat <- (newNamedFunction ExternalLinkage "shmat"
             :: CodeGenModule(LibCshmat))
            
  usleep <- (newNamedFunction ExternalLinkage "usleep"
             :: CodeGenModule(LibCusleep))

  pattern <- createStringNul "Writing %d entries\n" 
  fopen_args <- createStringNul "a"
  

  let callPuts format = (
        createNamedFunction ExternalLinkage "main" $ 
        \ argc argv -> do
          {- NOTE: For some unearthly reason, these have to be in the
              exact order of the 'defineBasicBlock' calls below.  -}
          load_args <- newNamedBasicBlock "load_args"
          try_fopen <- newNamedBasicBlock "try_fopen"
          try_shmat <- newNamedBasicBlock "try_shmat"
          init_vars <- newNamedBasicBlock "init_vars"
          sleep_loop_head <- newNamedBasicBlock "sleep_loop_head"
          read_loop_head <- newNamedBasicBlock "read_loop_head"
          read_loop_head2 <- newNamedBasicBlock "read_loop_head2"
          read_loop_head3 <- newNamedBasicBlock "read_loop_head3"

          failed_read <- newNamedBasicBlock "failed_read"
          accept_frame <- newNamedBasicBlock "accept_frame"
          back_to_front <- newNamedBasicBlock "back_to_front"
          write_buffer <- newNamedBasicBlock "write_buffer"
          finish_read <- newNamedBasicBlock "finish_read"
          finish_read2 <- newNamedBasicBlock "finish_read2"
          recalc <- newNamedBasicBlock "recalc"
          recalc2 <- newNamedBasicBlock "recalc2"
          recalc3 <- newNamedBasicBlock "recalc3"
          do_sleep <- newNamedBasicBlock "do_sleep"   
          p_exit <- newNamedBasicBlock "p_exit"
          exit <- newNamedBasicBlock "exit"
          
          let printf_pattern =(castVarArgs printf 
                      :: Function (Ptr Word8 -> Int32 -> IO Word32))

          v_stride <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_count <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_seqno <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_last_seqno <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_last_cur_seqno <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_delay <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_cont_read <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_p_cur <- (alloca :: CodeGenFunction r (Value (Ptr (Ptr Int32))))
          v_p_stride <- (alloca :: CodeGenFunction r (Value (Ptr (Ptr Int32))))
          v_p_perror <-  (alloca :: CodeGenFunction r (Value (Ptr (Ptr Word8))))

          --
          -- First, verify that we have sufficient arguments
          sufficient_args <- icmp IntSGE argc (3::Int32)
          condBr sufficient_args load_args exit
          
          --
          -- load_args: call atoi on the shared memory args
          --
          defineBasicBlock load_args
          s_shmid <- getArgv 2 argv
          s_shmsz <- getArgv 3 argv
          store s_shmid v_p_perror
          shmid <- call atoi s_shmid
          shmsz <- call atoi s_shmsz
          shmsz' <- icmp IntSGT shmsz (0 :: Int32)
          condBr shmsz' try_fopen exit -- validate the size as > 0.
          
          --
          -- try_fopen: try to fopen the output file.
          --
          defineBasicBlock try_fopen
          tf_arg1  <- getArgv 1 argv 
          store tf_arg1 v_p_perror
          tf_fopen_arg <- getElementPtr0 (fopen_args :: Global (Array D2 Word8)) (
            0 :: Word32, ())
                       
          tf_outfile <- call fopen tf_arg1 tf_fopen_arg
          nullptr_file <- inttoptr (valueOf (0 :: Int32))
          tf_outfile_ok <- icmp IntNE tf_outfile (nullptr_file :: Value (Ptr FILE))
          condBr tf_outfile_ok try_shmat p_exit

          --
          -- try_shmat: open up the shared memory
          --
          defineBasicBlock try_shmat
          nullptr_word8 <- inttoptr (valueOf (0 :: Int32))
          _ <- call setbuf tf_outfile nullptr_word8
          array_start_b <- call shmat shmid nullptr_word8 (valueOf 0o400)
          array_start <- bitcast array_start_b -- Ptr Word8 ->  Ptr Int32
          ts_shmat_ok <- icmp IntNE array_start_b (nullptr_word8 :: Value (Ptr Word8))
          condBr ts_shmat_ok init_vars p_exit

          --
          -- init_vars: set up loop variables
          --
          defineBasicBlock init_vars
          store array_start v_p_cur
          store array_start v_p_stride
          array_length <- mul shmsz (valueOf skip)
          array_end <- getElementPtr array_start (array_length, ())
          store (valueOf 0) v_seqno 
          store (valueOf 0) v_last_cur_seqno
          store (valueOf 0) v_last_seqno
          store (valueOf 100000) v_delay
          store (valueOf 0) v_stride
          br sleep_loop_head

          --
          -- sleep_loop_head
          --
          defineBasicBlock sleep_loop_head
          store (valueOf 0) v_count 
          store (valueOf 1) v_cont_read
--          slh_last_seqno <- load v_last_seqno
--          store slh_last_seqno v_seqno
          br read_loop_head
          
          --
          -- read_loop_head [2,3]
          --
          defineBasicBlock read_loop_head
             -- cur->seqno > seqno
          rlh_seqno <- load v_seqno
          rlh_read_seqno_p <- load v_p_cur 
          rlh_read_seqno <- (load rlh_read_seqno_p :: CodeGenFunction r (Value (Int32)))
          rlh_seqno_gt <- icmp IntUGE rlh_read_seqno rlh_seqno
          condBr rlh_seqno_gt accept_frame read_loop_head2

          -- read_loop_head2
            -- or cur->seqno <= min (seqno - size, 0)
          defineBasicBlock read_loop_head2
          rlh2_seqno <- load v_seqno
          rlh2_min_0 <- sub rlh2_seqno array_length
--          rlh2_p_cur_seqno <- s_min rlh2_min_0 (valueOf (0 :: Int32))
          rlh2_read_seqno_p <- load v_p_cur 
          rlh2_read_seqno <- (load rlh2_read_seqno_p 
                              :: CodeGenFunction r (Value (Int32)))
          rlh2_min_fp <- (sitofp rlh2_min_0 :: CodeGenFunction r (Value Double))
          rlh2_seqno_fp <- (uitofp rlh2_read_seqno :: CodeGenFunction r (Value Double))
--          rlh2_seqno_lt <- icmp IntSLE rlh2_read_seqno rlh2_min_0 --rlh2_p_cur_seqno
          rlh2_seqno_lt <- fcmp FPOLT rlh2_seqno_fp rlh2_min_fp
          condBr rlh2_seqno_lt accept_frame read_loop_head3 

          -- read_loop_head3
            -- or count == 0 && cur->seqno != last_cur_seqno
          defineBasicBlock read_loop_head3
          rlh3_count <- load v_count
          rlh3_count_0 <- icmp IntEQ rlh3_count (valueOf (0 ::Int32))
          rlh3_read_seqno_p <- load v_p_cur
          rlh3_read_seqno <- (load rlh3_read_seqno_p 
                             :: CodeGenFunction r (Value (Int32)))
          rlh3_seqno <- load v_last_cur_seqno
          rlh3_seqno_ne <- icmp IntNE rlh3_read_seqno rlh3_seqno
             -- did the underlying value change since last time we checked?
          rlh3_lower_changed <- LLVM.Core.and rlh3_count_0 rlh3_seqno_ne
          condBr rlh3_lower_changed accept_frame failed_read

          --
          -- failed_read
          --
          defineBasicBlock failed_read
          store (valueOf 0) v_cont_read
          br write_buffer
          
          --
          -- accept_frame
          --
          defineBasicBlock accept_frame
          af_stride_t <- load v_stride
          af_stride_t' <- add af_stride_t (1 :: Int32)
          store af_stride_t' v_stride

          af_count <- load v_count
          af_count' <- add af_count (1 :: Int32)
          store af_count' v_count

          
          af_p_seqno <- load v_p_cur
          af_seqno <- load af_p_seqno
          store af_seqno v_seqno

          af_cur <- getElementPtr af_p_seqno (skip, ())
          store af_cur v_p_cur

          af_cur_as_int <- (ptrtoint af_cur :: CodeGenFunction r (Value Int32))
          af_end_as_int <- (ptrtoint array_end :: CodeGenFunction r (Value Int32))
          af_cur_at_end <- icmp IntEQ af_cur_as_int af_end_as_int
          condBr af_cur_at_end back_to_front read_loop_head

          --
          -- back_to_front
          --
          store array_start v_p_cur
          br write_buffer

          -- 
          -- write_buffer
          --
          defineBasicBlock write_buffer
          wb_p_stride' <- load v_p_stride
          wb_p_stride <- bitcast wb_p_stride'
          wb_stride <- load v_stride
          _ <- call fwrite wb_p_stride wb_stride (valueOf (skip*4)) tf_outfile
          wb_pattern <- getElementPtr0 (pattern :: Global (Array D20 Word8)) (
            0 :: Word32, ())
          _ <- call printf_pattern wb_pattern wb_stride

          wb_p_cur <- load v_p_cur
          store wb_p_cur v_p_stride
          store (valueOf 0) v_stride

          wb_cont_read <- load v_cont_read
          wb_done_reading <- icmp IntEQ wb_cont_read (valueOf (0 :: Int32))
          condBr wb_done_reading finish_read read_loop_head

          --
          -- finish_read [2]
          --
          defineBasicBlock finish_read
          fr_p_cur <- load v_p_cur
          fr_p_last_cur_seqno <- load fr_p_cur
          store fr_p_last_cur_seqno v_last_cur_seqno
          fr_seqno <- load v_seqno
          store fr_seqno v_last_seqno
          fr_count <- load v_count
          fr_remain <- sub fr_count shmsz
          fr_lower_threshold <- ashr shmsz (valueOf (3 :: Int32))
          fr_too_low <- icmp IntSLE fr_remain fr_lower_threshold
          condBr fr_too_low recalc finish_read2

          defineBasicBlock finish_read2
          fr_upper_threshold <- mul fr_lower_threshold (valueOf (7 :: Int32))
          fr_too_high <- icmp IntSGE fr_remain fr_upper_threshold
          condBr fr_too_high recalc do_sleep

          --
          -- recalc [2,3]
          --
          defineBasicBlock recalc
          r_count <- load v_count
          r_none_read <- icmp IntEQ r_count (valueOf (0 :: Int32))
          condBr r_none_read recalc2 recalc3

          -- we didn't read anything. sleep for 2 secs.
          defineBasicBlock recalc2
          store (valueOf (2000000 :: Int32)) v_delay
          br do_sleep

          -- we read something, but we're out of the comfort zone.
          defineBasicBlock recalc3
          r3_delay <- load v_delay
          r3_count <- load v_count
          r3_period_num <- (mul shmsz r3_delay :: CodeGenFunction r (Value Int32))
          r3_period_den <- (mul (r3_count :: Value Int32) (valueOf (2 :: Int32))
                          :: CodeGenFunction r (Value Int32))
          r3_period <- ((udiv (r3_period_num :: Value Int32) (r3_period_den :: Value Int32))
                       :: CodeGenFunction r (Value Int32))
          r3_period_afterlow <- s_min r3_period (valueOf (2000000 :: Int32))
          r3_period_hi <- s_max r3_period_afterlow (valueOf (100000 :: Int32))
          store r3_period_hi v_delay
          br do_sleep

          --
          -- do_sleep
          --
          defineBasicBlock do_sleep
          ds_delay <- load v_delay
          _ <- call usleep ds_delay
          ds_should_die <- icmp IntEQ ds_delay (valueOf (0 :: Int32))
          condBr ds_should_die exit sleep_loop_head

          --
          -- p_exit
          --
          defineBasicBlock p_exit
          err_str <- load v_p_perror
          call perror err_str
          ret (1 :: Int32)
          
          --
          -- exit: just exit(1)
          defineBasicBlock exit
          ret (1::Int32)

          ) :: CodeGenModule (MainFunction)
  
  withStringNul nm callPuts
  
generateReader :: RunConfig -> FullSpecification -> String -> IO ()
generateReader cfg s@(Spec _ nm specs) filename = do
  let impl@(Impl _ _ mems) = implement cfg s
      total = sum $ map (implSize cfg) mems
      -- nwords is the size of the struct, in multiples of (sizeof int)
      nwords = (fromIntegral (total `div` 4)) :: Int32
  mod <- newNamedModule nm
  defineModule mod (buildReaderFun nm nwords)
--  optimizeModule 2 mod
  writeBitcodeToFile filename mod
  