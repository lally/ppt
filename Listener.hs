{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification #-}
module Listener (initialize, generateReader) where
import StaticInstrumentation
import LLVM.Core
import Configuration
import SIParser (implement)
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
      
            
type MainFunction = Function (Int32 -> Ptr (Ptr Word8) -> IO Int32)

--
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
type LibCSetBufType = Function (Ptr FILE -> Ptr Word8 -> Int32 -> IO ())
type LibCFPrintf = Function (Ptr Word8 -> Ptr Word8 -> VarArgs Word32)
type LibCFFlush = Function (Ptr Word8 -> IO Int32)
type LibCatoi = Function (Ptr Word8 -> IO Int32)
type LibCperror = Function (Ptr Word8 -> IO ())
type LibCshmat = Function (Int32 -> Ptr Word8 -> Int32 -> IO (Ptr Word8))
type LibCusleep = Function (Int32 -> IO ())

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

s_min :: (IsIntegerOrPointer a, CmpRet a d1, IsFirstClass a) => Value a -> Value a -> CodeGenFunction r (Value a)
s_min a b = do
    t1 <- icmp IntSLE a b
    t2 <- select t1 a b
    return t2

s_max :: (IsIntegerOrPointer a, CmpRet a d1, IsFirstClass a) => Value a -> Value a -> CodeGenFunction r (Value a)
s_max a b = do
    t1 <- icmp IntSGE a b
    t2 <- select t1 a b
    return t2


{-
--outputStruct :: Struct (a) ->
  
outFrameAtOff :: [FrameElement] -> Value (Ptr Word8) -> (Value (Ptr FILE)) -> CodeGenFunction r (Value Word32)
outFrameAtOff fr p f = buildOutput fr (reverse fr) (length fr) p f
              where
                buildOutput structValue  [] fr n p f = do
                  
                buildOutput tail (x:xs) fr n p f = 
                  case x of
                       FrameElement FDouble _ = buildOutput ( Double :& tail ) xs n p f
                       FrameElement FFloat _ = buildOutput (Float :& tail) xs n p f
                       FrameElement FInt _ = buildOutput (Int :& tail) xs n p f

  -}               


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

  pattern <- createStringNul "Loading file %s\n" 
  fopen_args <- createStringNul "a"
             
  let callPuts format = (
        createNamedFunction ExternalLinkage "main" $ 
        \ argc argv -> do
          exit <- newBasicBlock
          load_args <- newBasicBlock
          try_fopen <- newBasicBlock
          try_shmat <- newBasicBlock
          init_vars <- newBasicBlock
          p_exit <- newBasicBlock
          sleep_loop_head <- newBasicBlock
          read_loop_head <- newBasicBlock
          read_loop_head2 <- newBasicBlock
          read_loop_head3 <- newBasicBlock

          failed_read <- newBasicBlock
          accept_frame <- newBasicBlock
          finish_frames <- newBasicBlock
          write_buffer <- newBasicBlock
          finish_read <- newBasicBlock
          finish_read2 <- newBasicBlock
          recalc <- newBasicBlock
          do_sleep <- newBasicBlock          
          
          -- alloc our vars
{-          v_seqno <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_count <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_last_seqno <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_delay <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_stride <- (alloca :: CodeGenFunction r (Value (Ptr Int32)))
          v_p_cur <- (alloca :: CodeGenFunction r (Value (Ptr (Ptr Int32))))
          v_p_stride <- (alloca :: CodeGenFunction r (Value (Ptr (Ptr Int32))))
-}

          --
          -- First, verify that we have sufficient arguments
          sufficient_args <- icmp IntSGE argc (2::Int32)
          condBr sufficient_args load_args exit
          
          --
          -- load_args: call atoi on the shared memory args
          --
          defineBasicBlock load_args
          s_shmid <- getArgv 2 argv
          s_shmsz <- getArgv 3 argv
          shmid <- call atoi s_shmid
          shmsz <- call atoi s_shmsz
          shmsz' <- icmp IntSGT shmsz (0 :: Int32)
          condBr shmsz' try_shmat exit -- validate the size as > 0.
          
          --
          -- try_fopen: try to fopen the output file.
          --
          defineBasicBlock try_fopen
          tf_arg1  <- getArgv 1 argv 
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
          array_start_b <- call shmat shmid nullptr_word8 (valueOf 0o400)
          array_start <- bitcast array_start_b -- Ptr Word8 ->  Ptr Int32
          ts_shmat_ok <- icmp IntNE array_start_b (nullptr_word8 :: Value (Ptr Word8))
          condBr ts_shmat_ok init_vars p_exit

          --
          -- init_vars: set up loop variables
          --
          defineBasicBlock init_vars
          init_p_cur <- phi [(array_start, try_shmat)]
          init_p_stride <- phi [(array_start, try_shmat)]
          array_length <- mul shmsz (valueOf skip)
          array_end <- getElementPtr array_start (array_length, ())
          init_seqno <- return (valueOf (0 :: Int32))
          init_last_seqno <- return (valueOf (0 :: Int32))
          init_delay <- return (valueOf 100000)
          init_stride <- return (valueOf 0)
          br sleep_loop_head

          --
          -- sleep_loop_head
          --
          defineBasicBlock sleep_loop_head
          slh_stride <- phi [(init_stride, init_vars)]
          slh_count <- return (valueOf (0 :: Int32))
          slh_cont_read <- return (valueOf 1) -- whether to continue the read or not
          
          --
          -- read_loop_head [2,3]
          --
          defineBasicBlock read_loop_head
          rlh_p_cur <- phi [(init_p_cur, init_vars)]
          rlh_p_stride <- phi [(init_p_stride, init_vars)]
          rlh_seqno <- phi [(init_seqno, init_vars)]
          rlh_count <- phi [(slh_count, sleep_loop_head)]
          rlh_last_seqno <- phi [(init_last_seqno, init_vars)]
             -- cur->seqno > seqno
          rlh_read_seqno_p <- getElementPtr rlh_p_cur (0 :: Int32, ())
          rlh_read_seqno <- load rlh_read_seqno_p
          rlh_seqno_gt <- icmp IntUGE rlh_read_seqno rlh_seqno
          condBr rlh_seqno_gt accept_frame read_loop_head2

          -- read_loop_head2
            -- or cur->seqno <= min (slh_seqno - size, 0)
          defineBasicBlock read_loop_head2
          rlh_min_0 <- sub rlh_seqno array_length
          rlh_p_cur_seqno <- s_min rlh_min_0 (valueOf (0 :: Int32))
          rlh_seqno_lt <- icmp IntULE rlh_read_seqno rlh_p_cur_seqno
          condBr rlh_seqno_lt accept_frame read_loop_head3

          -- read_loop_head3
            -- or count == 0 && cur->seqno != last_seqno
          defineBasicBlock read_loop_head3
          rlh_count_0 <- icmp IntEQ rlh_count (valueOf (0 ::Int32))
          rlh_seqno_ne <- icmp IntNE rlh_read_seqno rlh_seqno
             -- did the underlying value change since last time we checked?
          rlh_lower_changed <- LLVM.Core.and rlh_count_0 rlh_seqno_ne
          condBr rlh_lower_changed accept_frame failed_read

          --
          -- failed_read
          --
          defineBasicBlock failed_read
          fr_cont_read <- return (valueOf 0)
          br write_buffer
          
          --
          -- accept_frame
          --
          defineBasicBlock accept_frame
          af_stride <- add slh_stride (1 :: Int32)
          af_count <- add rlh_count (1 :: Int32)
          af_p_seqno <- getElementPtr rlh_p_cur (0 :: Int32, ())
          af_seqno <- load af_p_seqno
          af_cur <- getElementPtr rlh_p_cur (skip, ())
          addPhiInputs rlh_p_cur [(af_cur, accept_frame)]
          addPhiInputs rlh_seqno [(af_seqno, accept_frame)]
          addPhiInputs rlh_count [(af_count, accept_frame)]
          addPhiInputs slh_stride [(af_stride, accept_frame)]

          af_cur_as_int <- (ptrtoint af_cur :: CodeGenFunction r (Value Int32))
          af_end_as_int <- (ptrtoint array_end :: CodeGenFunction r (Value Int32))
          af_cur_at_end <- icmp IntEQ af_cur_as_int af_end_as_int
          condBr af_cur_at_end write_buffer read_loop_head

          -- 
          -- write_buffer
          --
          defineBasicBlock write_buffer
          wb_cont_read <- (phi [(fr_cont_read, failed_read), 
                               (slh_cont_read, sleep_loop_head)] 
                               :: CodeGenFunction r (Value Int32))
          wb_p_stride' <- phi [(rlh_p_cur, read_loop_head)]
          wb_p_stride <- bitcast wb_p_stride'
          _ <- call fwrite wb_p_stride slh_stride (valueOf (skip*4)) tf_outfile

          addPhiInputs rlh_p_stride [(wb_p_stride', write_buffer)]

          wb_done_reading <- icmp IntEQ wb_cont_read (valueOf (0 :: Int32))
          condBr wb_done_reading finish_read read_loop_head

          --
          -- finish_read [2]
          --
          defineBasicBlock finish_read
          fr_count <- phi [(rlh_count, read_loop_head)]
          fr_remain <- sub fr_count shmsz
          fr_lower_threshold <- ashr shmsz (valueOf (3 :: Int32))
          fr_too_low <- icmp IntSLE fr_remain fr_lower_threshold
          condBr fr_too_low recalc finish_read2

          defineBasicBlock finish_read2
          fr_upper_threshold <- mul fr_lower_threshold (valueOf (7 :: Int32))
          fr_too_high <- icmp IntSGE fr_remain fr_upper_threshold
          condBr fr_too_high recalc do_sleep

          --
          -- recalc
          --
          defineBasicBlock recalc
          r_delay <- phi [(init_delay, init_vars)]
          r_shmsz <- phi [(shmsz, load_args)]
          r_period_num <- mul r_shmsz r_delay
          r_period_den <- (mul (fr_count :: Value Int32) (valueOf (2 :: Int32))
                          :: CodeGenFunction r (Value Int32))
          r_period <- ((udiv (r_period_num :: Value Int32) (r_period_den :: Value Int32))
                       :: CodeGenFunction r (Value Int32))
          r_period_afterlow <- s_min r_period (valueOf (2000000 :: Int32))
          r_period_hi <- s_max r_period_afterlow (valueOf (100000 :: Int32))
          addPhiInputs r_delay [(r_period_hi, recalc)]
          br do_sleep

          --
          -- do_sleep
          --
          defineBasicBlock do_sleep
          _ <- call usleep r_delay
          br sleep_loop_head

          {- 
          PUT IN LOOP HERE.  Actually, two loops.  An outer one that
          does the read-sleep cycle, and an inner one that does the
          actual reading.

          - defineBasicBlock superLoophead
          - slh_seqno <- seqno
          - count <- 0
          - stride_start <- cur
          - cont_read <- 1
          - br loophead

          - defineBasicBlock loophead

130         while (cur->seqno > seqno
131                || (cur->seqno <= min(slh_seqno - size, 0))
132                || (count == 0 && cur->seqno != last_seqno)) {
          
          - read seqno
          - run the four comparisons, 3 logical ops, and the function call.
          - if ok -- br acceptFrame
          - cont_read <- 0
          - else br writeBuffer

          - defineBasicBlock acceptFrame
          - increment stride count (for a later fwrite)
          - increment total count (for rate analysis)
          - increment ptr
          - update seqno         
          - compare to end ptr -- yes? writeBuffer
          - br loophead

          - I don't need this one.
            - defineBasicBlock finishFrames
            - compare total count, 0
            - branchNZ writeBuffer, finishRead
          
          - defineBasicBlock writeBuffer
          - call fwrite stride_start stride_cnt (4*skip)
          - stride_start = cur
          - cmp cont_read 0:finishRead 1:loophead
          
          - defineBasicBlock finishRead
          - remain <- sub count, size
          - threshold_low <- size << 3
          - cmp remain, size/8
          - jl recalc
          - threshold_up <- threshold_low * 7
          - cmp remain, 7*size/8
          - jg recalc
          - br sleep

          - defineBasicBlock recalc
          - delay <- size * delay / (count * 2)
          - delay2 <- min (delay, 2000000)
          - delay3 <- max (delay2, 100000)
          - br sleep
          
          - defineBasicBlock sleep
          - call usleep delay3 
          - br superLoopHead
          -}
          
          {- exit with a call to perror().  Our string is phi:
           | Block     | Var     |
           |-----------+---------|
           | try_fopen | arg1    |
           | try_shmat | s_shmid | Note: s_shmid isn't *in* try_shmat, but it's the
           |           |         | string version of shmid.
           -}
          defineBasicBlock p_exit
          err_str <- phi [(tf_arg1, try_fopen), (s_shmid, try_shmat)]
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
  writeBitcodeToFile filename mod
  