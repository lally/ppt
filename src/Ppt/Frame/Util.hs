module Ppt.Frame.Util where
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
x64 = TargetInfo 8 4 4 8 8 1

bimap :: Either a b -> (a -> c) -> (b -> d) -> Either c d
bimap (Left x) f g = Left (f x)
bimap (Right r) f g = Right (g r)

rcompose (Left x) f g = Left (f x)
rcompose (Right r) f g = g r

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
  in rcompose res show (\n -> compileFrames x64 opts [n])

genFrame :: String -> Either String PP.Doc
genFrame str =
  let parseRes = parseFrame str
  in bimap parseRes id (cpFile opts)

ls :: Show a => Either a b -> Either String b
ls (Left s) = Left (show s)
ls (Right r) = Right r

genFile :: String -> Either String PP.Doc
genFile str = do
  (Buffer emitopts frames) <- ls $ tparse fileParser str
  compiled <- compileFrames x64 emitopts frames
  return $ cpFile emitopts compiled
--  in undefined
