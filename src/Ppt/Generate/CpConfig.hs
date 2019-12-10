module Ppt.Generate.CpConfig where
import Ppt.Frame.ParsedRep
import Ppt.Frame.Types
import Ppt.Frame.Layout

import qualified Text.PrettyPrint as PP

--
-- Module Configuration
--
x64Layout = TargetInfo 8 4 4 16 8 4 3

-- |Derived from EmitOptions for whatever data we need for outputting.
data OutputCfg = OutputCfg { timeType :: String -- ^Decltype of time vars
                           , timeHeader :: String
                             -- ^Which header to include for time support
                           , timeSave :: (String -> PP.Doc) -- ^Save time to a var
                           , indent :: Int -- ^Indentation depth
                           , defaultInit :: Bool
                             -- ^Does the constructor zero out the type?
                           , multithreadWrite :: Bool
                             -- ^Use multithreaded write protocol?
                           , bufName :: String
                           , sourceSuffix :: String
                           , headerSuffix :: String
                           , filePrefix :: String
                           , namespace :: [String]
                           , emitOpts :: EmitOptions
                           , frames :: [FrameLayout]
                           , nativeCounters :: Bool
                           , counterCount :: Int
                           , debugOutput :: Bool
                           }

-- | Returns (sourceSuffix, headerSuffix, filePrefix, namespace, nativeCounters) from an array of EOption
cppOpts :: EBuffer -> [EOption] -> (String, String, String, [String], Bool, Bool)
cppOpts buf opts =
  let checkNative (ENativeCounter b) = b
      checkNative _ = False
      checkDebug (EDebug b) = b
      checkDebug _ = False
      native = any checkNative opts
      debug = any checkDebug opts
  in (".cc", ".hh", "ppt-", ["ppt", ebName buf], native, debug)

makeOutCfg :: EmitOptions -> [FrameLayout] -> OutputCfg
makeOutCfg e@(EmitOptions b _ ETimeVal (ERuntime mt) _ eOpts) flayouts=
  let (ssfx, hsfx, fpfx, ns, native, debug) = cppOpts b eOpts
  in (OutputCfg "struct timeval" "time.h" (\var -> PP.text $ "time(&" ++ var ++ ")") 4 True mt
   (ebName b) ssfx hsfx fpfx ns e flayouts native 3 debug)

makeOutCfg e@(EmitOptions b _ (ETimeSpec src) (ERuntime mt) _ eOpts) flayouts =
  let (ssfx, hsfx, fpfx, ns, native, debug) = cppOpts b eOpts
      clock = case src of
        ETimeClockRealtime ->         "CLOCK_REALTIME"
        ETimeClockRealtimeCoarse ->   "CLOCK_REALTIME_COARSE"
        ETimeClockMonotonic ->        "CLOCK_MONOTONIC"
        ETimeClockMonotonicCoarse ->  "CLOCK_MONOTONIC_COARSE"
        ETimeClockMonotonicRaw ->     "CLOCK_MONOTONIC_RAW"
        ETimeClockBoottime ->         "CLOCK_BOOTTIME"
        ETimeClockProcessCputimeId -> "CLOCK_PROCESS_CPUTIME_ID"
        ETimeClockThreadCputimeId ->  "CLOCK_THREAD_CPUTIME_ID"
  in (OutputCfg "struct timespec" "time.h" (
         \var -> PP.text $ "clock_gettime(" ++ clock ++ ", &" ++ var ++ ")")
       4 True mt (ebName b) ssfx hsfx fpfx ns e flayouts native 3 debug)

--
-- Code Generation Modules
--
-- To tell what bits of code we need to generate.

-- |A member may require that a module be generated.  Without any
-- members requiring a module, we don't have to generate it at all.
data GenModule = GMCounters
               | GMSaveBuffer LayoutIOSpec Bool
               -- ^Store layout, and whether it's multithreaded.
               deriving (Eq, Show)

instance Ord GenModule where
  compare GMCounters GMCounters = EQ
  compare (GMSaveBuffer (LayoutIO lsz loff) lm) (GMSaveBuffer (LayoutIO rsz roff)  rm) =
    let szcomp = compare lsz rsz
        offcomp = compare loff roff
        mcomp = compare lm rm
    in if szcomp == EQ
       then if offcomp == EQ
            then mcomp
            else offcomp
       else szcomp
  compare GMCounters ( GMSaveBuffer _ _) = GT
  compare (GMSaveBuffer _ _) GMCounters = LT
