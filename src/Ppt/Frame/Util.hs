module Ppt.Frame.Util (showLayoutData, showLayout) where
import Ppt.Frame.Layout
import Ppt.Frame.LayoutAlgo
import Ppt.Frame.ParsedRep
import Ppt.Frame.Parser
import Ppt.Generate.CpConfig
import Ppt.Generate.CpPrim
import Ppt.Generate.Cp
import Data.Bits (shiftR, (.&.))
import Foreign.C.Types
import Text.Printf
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Text.PrettyPrint as PP

--target = TargetInfo 8 4 8 16 8 1

-- TODO(lally): Use inline-c and sizeof-ops to get this data.  Find a
-- place for this.
x64    = TargetInfo 8 4 4 8 8 1

opts :: EmitOptions
opts = (EmitOptions
        (EBuffer "test" Nothing)
        ELangCpp
        (ETimeSpec ETimeClockRealtime)
        (ERuntime True)
        []
        [])
cfg = makeOutCfg opts []

bimap :: Either a b -> (a -> c) -> (b -> d) -> Either c d
bimap (Left x) f g = Left (f x)
bimap (Right r) f g = Right (g r)

rcompose (Left x) f g = Left (f x)
rcompose (Right r) f g = g r

parseMember :: String -> [MemberData]
parseMember str =
  let (Right mem) = tparse (frameMember opts) (str ++ ";")
      makeLMember m =
        let (FMemberElem frm) = m
            ty = fmType frm
            nm = fmName frm
        in LMember ty 0 0 0 (LKMember frm Nothing) nm
  in map (makeMember cfg . makeLMember) mem

parseFrame :: String -> Either String [FrameLayout]
parseFrame str =
  let res = tparse (frame opts) str
  in rcompose res show (\n -> compileFrames' x64 [n])

genFrame :: String -> Either String [(String, PP.Doc)]
genFrame str =
  let parseRes = parseFrame str
  in bimap parseRes id (cppFiles opts)

ls :: Show a => Either a b -> Either String b
ls (Left s) = Left (show s)
ls (Right r) = Right r

genFile :: String -> Either String [(String, PP.Doc)]
genFile str = do
  (Buffer emitopts frames) <- ls $ tparse (fileParser []) str
  compiled <- compileFrames' x64  frames
  return $ cppFiles emitopts compiled

mkInt n = FMemberElem $ FMember PInt n False
mkTime n = FMemberElem $ FMember (PTime (ETimeSpec ETimeClockRealtime)) n True

lshow :: LayoutMember -> String
lshow (LMember ty off algn sz knd nm) = printf "%-20s off=%3d algn=%3d sz=%2d name=%-25s kind=%s" (show ty) off algn sz nm (show knd) 

evalLayout :: LayoutMember -> String
evalLayout lmem =
  let alignCheck m =
        let sz = sizeOf x64 $ lType m in
          (if (lKind m == LKPadding (lSize m) || (lSize m == sz))
           then (if ((lOffset m) `mod` (lAlignment m) == 0)
                  then 0
                  else 2)
           else 1)
      prefix = let result = alignCheck lmem in (if result == 0
                                                then " > good   "
                                                else  (" > BAD(" ++ show result ++ ") "))
  in prefix ++ lshow lmem

layoutData :: String -> [LayoutMember] -> IO ()
layoutData n elems = do
      let paddingSz' (LKPadding n) = n
          paddingSz' _ = 0
          paddingSz mem = paddingSz' $ lKind mem
          showPad :: Int -> String
          showPad 0 = "   "
          showPad n = printf "%3d" n
      putStrLn $ " -- " ++ n ++ " -- "
      putStrLn $ L.intercalate "\n" $ map (\s -> (showPad $paddingSz s) ++ evalLayout s) elems
      putStrLn $ " Padding sum: " ++ (show $ sum $ map paddingSz elems) ++ ", alignment: " ++ (
        show $ maximum $ map lSize elems)

showLayout :: [Frame] -> IO ()
showLayout frames = do
  let result = compileFrames' x64 frames
  case result of
    Left error -> do putStrLn error
    Right flayouts -> do
      let paddingSz' (LKPadding n) = n
          paddingSz' _ = 0
          paddingSz mem = paddingSz' $ lKind mem
      mapM_ (\(FLayout n _ elems) -> layoutData n elems) flayouts
      return ()

showFrameLayouts :: [FrameLayout] -> IO ()
showFrameLayouts layouts =
  mapM_ (\(FLayout n _ elems) -> layoutData n elems) layouts

showLayoutData :: JsonRep -> IO ()
showLayoutData j = do
  putStrLn "Frame Layouts"
  showFrameLayouts (jsBufferFrames j)
  putStrLn "\nUnderlying Frames"
  showLayout (map flFrame $ jsBufferFrames j)
  return ()


breakCsv :: String -> [String]
breakCsv s =
 cons (case break (== ',') s of
          (l, s') -> (l, case s' of
                           []      -> []
                           _:s''   -> lines s''))
 where
   cons ~(h, t)        =  h : t
   
