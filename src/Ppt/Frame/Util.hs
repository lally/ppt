module Ppt.Layout.Util where
import Ppt.Frame.Layout
import Ppt.Frame.ParsedRep
import Ppt.Frame.Parser
import Ppt.Generate.Cp
import qualified Text.PrettyPrint as PP

target = TargetInfo 8 4 8 16 8 1

opts = (EmitOptions
        (EBuffer "test" Nothing)
        ELangCpp
        (ETimeSpec ETimeClockRealtime)
        (ERuntime True)
        [])
cfg = makeOutCfg opts
x86_64 = TargetInfo 8 4 4 8 8 1

parseMember :: String -> [MemberData]
parseMember str =
  let (Right mem) = tparse frameMember (str ++ ";")
      makeLMember m =
        let (FMemberElem frm) = m
            ty = fmType frm
            nm = fmName frm
        in LMember ty 0 0 0 (LKMember m Nothing) nm
  in map (makeMember cfg . makeLMember) mem

parseFrame :: String -> Either String [FrameLayout]
parseFrame str =
  let res = tparse frame str
  in case res of
    Left s -> Left $ show s
    Right fr -> compileFrames x86_64 opts [fr]

genFrame :: String -> Either String PP.Doc
genFrame str =
  let parseRes = parseFrame str
       -- Take advantage of lazy evaluation here.
  in case parseRes of
    (Left s) -> Left s
    (Right flayout) -> Right $ cpFile opts flayout
