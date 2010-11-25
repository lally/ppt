module Storage where
{- Imlements basic path storage policy -}

{- Currently just the ./.ppt/ folder path, but will hold more settings
   later (presumably from a config file) -}

import System.IO (FilePath)
data Config = Config FilePath

loadConfig :: IO (Config)
loadConfig = return (Config "./.ppt/")
            
-- binaryPath "~/.ppt" binaryVersion
binaryPath :: Config -> String -> FilePath
binaryPath (Config cfg) nm = cfg ++ "generated/" ++ nm