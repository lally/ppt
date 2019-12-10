module Ppt.Frame.Util (showLayoutData, showLayout) where
import Ppt.Frame.Layout
import Ppt.Frame.Types
import Ppt.Frame.LayoutAlgo
import Ppt.Frame.ParsedRep
import Ppt.Frame.Parser
import Ppt.Generate.CpConfig
import Ppt.Generate.CpPrim
import Ppt.Generate.Cp
import Control.Lens hiding (bimap)
import Data.Bits (shiftR, (.&.))
import Foreign.C.Types
import Text.Printf
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Text.PrettyPrint as PP

-- TODO(lally): Use inline-c and sizeof-ops to get this data.  Find a
-- place for this.
x64    = TargetInfo 8 4 4 8 8 1 3

opts :: EmitOptions
opts = EmitOptions
        (EBuffer "test" Nothing)
        ELangCpp
        (ETimeSpec ETimeClockRealtime)
        (ERuntime True)
        []
        []
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

mkInt n = FMemberElem $ FMember (PIntegral PPInt Nothing) n False
mkTime n = FMemberElem $ FMember (PTime (Just (ETimeSpec ETimeClockRealtime, 0, 0))) n True

lshow :: LayoutMember -> String
lshow (LMember ty off algn sz knd nm) = printf "%-20s off=%3d algn=%3d sz=%2d name=%-25s kind=%s" (show ty) off algn sz nm (show knd)

evalLayout :: LayoutMember -> String
evalLayout lmem =
  let alignCheck m =
        let sz = sizeOf x64 $ m ^. lType in
          (if m ^.lKind == LKPadding (m ^. lSize) || (m ^. lSize == sz)
           then (if (m ^. lOffset) `mod` (m ^. lAlignment) == 0
                  then 0
                  else 2)
           else 1)
      prefix = let result = alignCheck lmem in (if result == 0
                                                then " > good   "
                                                else  " > BAD(" ++ show result ++ ") ")
  in prefix ++ lshow lmem

layoutData :: String -> [LayoutMember] -> IO ()
layoutData n elems = do
      let paddingSz' (LKPadding n) = n
          paddingSz' _ = 0
          paddingSz mem = paddingSz' $ mem ^. lKind
          showPad (LKPadding n)
            | n == 0 = "   "
            | otherwise = printf "%03d" n
          showPad _ = showPad (LKPadding 0)
      putStrLn $ " -- " ++ n ++ " -- "
      putStrLn $ L.intercalate "\n" $ map (\s -> showPad (s ^. lKind) ++ evalLayout s) elems
      putStrLn $ " Padding sum: " ++ show (sum $ map paddingSz elems) ++ ", alignment: " ++ show (maximum $  elems ^..folded.lSize)

showLayout :: [Frame] -> IO ()
showLayout frames = do
  let result = compileFrames' x64 frames
  case result of
    Left error -> putStrLn error
    Right flayouts ->
      mapM_ (\(FLayout n _ elems) -> layoutData n elems) flayouts

showFrameLayouts :: [FrameLayout] -> IO ()
showFrameLayouts = mapM_ (\(FLayout n _ elems) -> layoutData n elems)

showLayoutData :: JsonRep -> IO ()
showLayoutData j = do
  putStrLn "Frame Layouts"
  showFrameLayouts (jsBufferFrames j)
  putStrLn "\nUnderlying Frames"
  showLayout (jsBufferFrames j ^..folded.flFrame)
  return ()


breakCsv :: String -> [String]
breakCsv s =
 cons (case break (== ',') s of
          (l, s') -> (l, case s' of
                           []      -> []
                           _:s''   -> lines s''))
 where
   cons ~(h, t)        =  h : t

