module Ppt.Layout.Util where
import Ppt.Frame.Layout
import Ppt.Frame.ParsedRep
import Ppt.Frame.Parser
import Ppt.Generate.Cp

target = TargetInfo 8 4 8 16 8 1

opts = (EmitOptions
        (EBuffer "test" Nothing)
        ELangCpp
        (ETimeSpec ETimeClockRealtime)
        (ERuntime True)
        [])


