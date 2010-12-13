module Storage (Config, loadConfig, binaryPath, specPath, makeSpecPath) where
{- Imlements basic path storage policy -}

{- Currently just the ./.ppt/ folder path, but will hold more settings
   later (presumably from a config file) -}
import System.FilePath.Posix
import System.Posix.Directory
import System.Posix.Files
import qualified StaticInstrumentation as SI
import Data.ByteString.Internal (c2w)
import qualified Data.Digest.MD5 as MD5
import Data.String.Utils (split, join)
import System.IO (FilePath)
import Monad (filterM)
import Control.Monad (liftM)
import Data.List (inits)
data Config = Config FilePath

loadConfig :: IO (Config)
loadConfig = return (Config "./.ppt/")
            
-- binaryPath "~/.ppt" binaryVersion
binaryPath :: Config -> String -> FilePath
binaryPath (Config cfg) nm = cfg ++ "generated/" ++ nm

-- specPath returns a folder name for the specified FrameSpecification
specPath :: Config -> SI.FrameSpecification -> FilePath
specPath (Config cfg) spec@(SI.FrameSpecification name elems) =
         let hash = concatMap show (SI.specHash spec)
          in cfg ++ "static/" ++ name ++ "/" ++ hash ++ "/"

unknownPaths :: [FilePath] -> IO ([FilePath])
unknownPaths paths = do filterM ((liftM not) . fileExist) paths

-- create (if needed) the path for 'specPath' for this
-- config/framespec pair to work.
makeSpecPath :: Config -> SI.FrameSpecification -> IO ()
makeSpecPath cfg spec = do
             let paths = map (flip (++) "/") $ filter (\x -> length x > 0) $ split "/" (specPath cfg spec)
             let prefices = tail (map concat $ inits paths)
             unknown_paths <- filterM ((liftM not) . fileExist) prefices
             mapM_ ((flip createDirectory) 0o775) unknown_paths
