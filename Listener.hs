module Listener where

import LLVM.Core

--
-- A wrapper around LLVM initialization, so that we don't have
-- to force LLVM deps outside of this module.
initialize :: IO ()
initialize = initializeNativeTarget

