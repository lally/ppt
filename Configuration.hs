module Configuration where

import StaticInstrumentation

data MachineTarget = Target8 | Target16 | Target32 | Target64
     deriving (Eq, Read, Show)
     

-- The path of the .ppt/ folder, and the loaded target machine
data RunConfig = RunConfig String MachineTarget
                 deriving Show

cfgTarget :: RunConfig -> MachineTarget
cfgTarget (RunConfig _ mt) = mt 

--
-- And, the static configuration data
defaultTarget = Target64

frameSize :: RunConfig -> FrameType -> Int
frameSize _ FDouble = 8
frameSize _ FFloat = 4
frameSize _ FInt = 4
frameSize _ FTime = 16

implSize :: RunConfig -> ImplMember -> Int
implSize c (ImplMember _ t) = memSize c t
      where memSize :: RunConfig -> ImplMemberType -> Int
            memSize _ IMDouble = 8
            memSize _ IMFloat = 4
            memSize _ IMInt = 4
            memSize _ IMTime = 16
            memSize _ IMSeqno = 4
            memSize _ (IMPad n) = n

