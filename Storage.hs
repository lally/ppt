module Storage where
{- Imlements basic path storage policy -}

{- Currently just the ./.pt/ folder path, but will hold more settings
   later (presumably from a config file) -}
import System.FilePath.Posix

data Config = Config FilePath

loadConfig :: IO (Config)
loadConfig = return (Config "./.pt/")
            
-- binaryPath "~/.pt" binaryVersion
binaryPath :: Config -> String -> FilePath
binaryPath (Config cfg) str = cfg ++ str
