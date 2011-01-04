module Storage (loadConfig, binaryPath, specPath, makeSpecPath, createConfig) where
{- Imlements basic path storage policy -}

{- Currently just the ./.ppt/ folder path, but will hold more settings
   later (presumably from a config file) -}
import System.FilePath.Posix
--import System.Posix.Directory
import System.Directory
import Control.Monad (liftM, liftM2)
import System.Posix.Files
import qualified StaticInstrumentation as SI
import Data.ByteString.Internal (c2w)
import qualified Data.Digest.MD5 as MD5
import Data.String.Utils (split, join)
import System.IO (FilePath)
import Monad (filterM)
import Control.Monad (liftM)
import Data.List (inits, tails)
import Configuration

readTargetSpec :: FilePath -> IO (MachineTarget)
readTargetSpec dir = do
               let fname = dir ++ "/target"
               exists <- doesFileExist fname
               target <- if exists then (liftM read (readFile fname)) else return Target64
               return target
               

{-
        Find, from the current directory up, the .ppt directory.
        When found, load up the target information
-}
loadConfig :: IO (Maybe RunConfig)
loadConfig = do
           cwd <- getCurrentDirectory
           let up_dirs = map (join "/") $ reverse $ tail $ inits $ tail $ split "/" cwd
           let up_paths = map (\x -> "/" ++ x) $ up_dirs
           proper_dirs <- liftM (take 1) $ filterM (\x -> doesDirectoryExist (x ++ "/.ppt/")) up_paths
           case proper_dirs of
                [] -> return Nothing
                -- load the target data
                otherwise -> do let dir = head proper_dirs
                                target  <- readTargetSpec dir
                                return (Just (RunConfig (dir ++ "/.ppt/") target))

createConfig :: IO (RunConfig)
createConfig = do
             createDirectory "./.ppt" 
             writeFile "./.ppt/target" (show defaultTarget)
             cwd <- getCurrentDirectory
             let ppt_dir = cwd ++ "/.ppt/"
             return (RunConfig ppt_dir defaultTarget)
             
-- binaryPath "~/.ppt" binaryVersion
binaryPath :: RunConfig -> String -> FilePath
binaryPath (RunConfig cfg _) nm = cfg ++ "generated/" ++ nm

-- specPath returns a folder name for the specified FrameSpecification
specPath :: RunConfig -> SI.FullSpecification -> FilePath
specPath (RunConfig cfg _) spec@(SI.Spec _ name elems) =
         let hash = concatMap show (SI.specHash spec)
          in cfg ++ "static/" ++ name ++ "/" ++ hash ++ "/"

unknownPaths :: [FilePath] -> IO ([FilePath])
unknownPaths paths = do filterM ((liftM not) . fileExist) paths

-- create (if needed) the path for 'specPath' for this
-- config/framespec pair to work.
makeSpecPath :: RunConfig -> SI.FullSpecification -> IO ()
makeSpecPath cfg spec = do
             let backward_path_segs = tails $ tail $reverse $ tail $ split "/" (specPath cfg spec)
             let path_candidates = map (\xs -> "/" ++ (join "/" $ reverse xs) ++ "/") backward_path_segs
             not_exist_rev <- filterM ((liftM not) . doesDirectoryExist) path_candidates
             mapM_ createDirectory $ reverse not_exist_rev